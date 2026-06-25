"""Load cleaned parquet panels and expose fast per-date option lookups.

The backtests need two access patterns against an option chain:

1. **At a (monthly) trade open** -- given a trade date, list the available
   expirations and, for a chosen expiration, the strikes (with mid price and
   delta) so we can pick legs by %-moneyness or by delta.
2. **Every day a position is held** -- look up the mid of a *specific*
   ``(cp_flag, exdate, strike)`` leg on the current date; fall back to the last
   value if the quote is missing.

Both patterns only ever touch *one* trade date at a time and the panel is sorted
by date, so :class:`OptionChain` slices a single day on demand (via a binary
search over the contiguous date blocks) and memoizes the most recent day. This
keeps memory flat regardless of the 17M-row panel size -- no global price dict.
"""

from __future__ import annotations

import numpy as np
import pandas as pd

from vix_hedge import config

_COLS = ["date", "exdate", "cp_flag", "strike", "mid", "delta", "forward", "dte"]


def load_spot_prices() -> pd.DataFrame:
    """Return the daily SPX/VIX close panel indexed by ``date`` (Timestamp)."""
    if not config.SPOT_PARQUET.exists():
        raise FileNotFoundError(f"{config.SPOT_PARQUET} missing; run `python -m vix_hedge.data.build spot`")
    df = pd.read_parquet(config.SPOT_PARQUET)
    df["date"] = pd.to_datetime(df["date"])
    return df.set_index("date").sort_index()


class DayChain:
    """One trade date's option chain.

    Holds the day's raw block and builds per-``(cp_flag, exdate)`` strike tables
    lazily -- the backtest only ever touches the held/selected expiration, so we
    avoid grouping all ~40 legs when 1-2 are needed.
    """

    __slots__ = ("date", "exdates", "_block", "_legs",
                 "_cp", "_exarr", "_strike", "_mid", "_delta", "_forward")

    def __init__(self, date: pd.Timestamp, block: pd.DataFrame):
        self.date = date
        self._block = block
        self.exdates: np.ndarray = np.sort(block["exdate"].unique())
        self._legs: dict[tuple[str, np.datetime64], pd.DataFrame] = {}
        self._strike = None  # column arrays extracted lazily on first leg() lookup

    def _extract(self) -> None:
        """Pull the day's columns into plain numpy arrays once. The panel is
        arrow/categorical-backed, so routing every per-leg filter through pandas
        (``b[mask].dropna().set_index().sort_index()``) pays a pyarrow take +
        dtype-sanitize + ``__finalize__`` on each of the ~10k+ lookups a backtest
        makes -- the hot path. Doing the filter/sort in numpy avoids all of it.
        Lazy (not in ``__init__``) so a caller that edits ``_block`` before its
        first lookup still sees the edit; native dtypes are preserved (mid/delta/
        forward stay float32) so values are byte-identical to the pandas path."""
        b = self._block
        self._cp = b["cp_flag"].to_numpy()        # object 'C'/'P'
        self._exarr = b["exdate"].to_numpy()      # datetime64
        self._strike = b["strike"].to_numpy()     # float64
        self._mid = b["mid"].to_numpy()           # float32 (NaN where unquoted)
        self._delta = b["delta"].to_numpy()
        self._forward = b["forward"].to_numpy()

    def leg(self, cp_flag: str, exdate) -> pd.DataFrame | None:
        """Strike table for one expiration: DataFrame indexed by strike with
        columns mid/delta/forward (non-null mid only), or ``None`` if absent."""
        ex = np.datetime64(exdate)
        key = (cp_flag, ex)
        cached = self._legs.get(key)
        if cached is not None:
            return cached if len(cached) else None
        if self._strike is None:
            self._extract()
        # non-null mid only, sorted ascending by strike -- same contract as the old
        # b[mask].dropna(subset=["mid"]).set_index("strike").sort_index().
        mask = (self._cp == cp_flag) & (self._exarr == ex) & ~np.isnan(self._mid)
        strike = self._strike[mask]
        order = np.argsort(strike, kind="stable")
        tbl = pd.DataFrame(
            {"mid": self._mid[mask][order],
             "delta": self._delta[mask][order],
             "forward": self._forward[mask][order]},
            index=pd.Index(strike[order], name="strike"),
        )
        self._legs[key] = tbl
        return tbl if len(tbl) else None

    def mid(self, cp_flag: str, exdate, strike: float) -> float:
        tbl = self.leg(cp_flag, exdate)
        if tbl is None:
            return np.nan
        try:
            return float(tbl.at[float(strike), "mid"])
        except KeyError:
            return np.nan


class OptionChain:
    """Fast lazy per-date views over a cleaned option panel."""

    def __init__(self, df: pd.DataFrame):
        df = df.sort_values(["date", "cp_flag", "exdate", "strike"]).reset_index(drop=True)
        self.df = df
        self._dates = df["date"].to_numpy()  # sorted; contiguous blocks per day
        self.trade_dates = np.unique(self._dates)
        self._cache: dict[np.datetime64, DayChain] = {}

    def day(self, date) -> DayChain:
        key = np.datetime64(pd.Timestamp(date))
        cached = self._cache.get(key)
        if cached is not None:
            return cached
        lo, hi = np.searchsorted(self._dates, [key, key + np.timedelta64(1, "D")])
        chain = DayChain(pd.Timestamp(date), self.df.iloc[lo:hi])
        # Cache every day. The ensemble harness replays the same chain over the same
        # dates once per staggered cohort (9-21x), so without this each cohort rebuilds
        # all the per-day strike tables from scratch -- the backtest hot path. Day slices
        # are views into the parent frame and the lazily-built leg tables are tiny, so
        # the cache costs ~nothing in memory (bounded by the chain's own trade-date count).
        self._cache[key] = chain
        return chain


def load_spx_chain() -> OptionChain:
    if not config.SPX_OPTIONS_PARQUET.exists():
        raise FileNotFoundError(f"{config.SPX_OPTIONS_PARQUET} missing; run `python -m vix_hedge.data.build spx`")
    return OptionChain(pd.read_parquet(config.SPX_OPTIONS_PARQUET, columns=_COLS))


def load_vix_chain() -> OptionChain:
    if not config.VIX_OPTIONS_PARQUET.exists():
        raise FileNotFoundError(f"{config.VIX_OPTIONS_PARQUET} missing; run `python -m vix_hedge.data.build vix`")
    return OptionChain(pd.read_parquet(config.VIX_OPTIONS_PARQUET, columns=_COLS))
