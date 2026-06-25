"""VIX regime transitions and single-option (VIX call) return distributions.

Two questions from ``calculate_returns_and_transitions.R``:

1. **Regime transitions** -- bucket VIX into levels at 15/30/50 and count how
   often it moves between buckets day to day. (How "sticky" is each vol regime?)
2. **Option return distribution** -- for a fixed recipe (e.g. a 120-DTE, 0.1-delta
   VIX call), how do held-to-expiry returns distribute? What fraction expire
   worthless, and how often does the max return exceed 2x/5x/10x/...? This is the
   convex-payoff case for a VIX-call tail hedge.
"""

from __future__ import annotations

import numpy as np
import pandas as pd

from vix_hedge.data.load import OptionChain
from vix_hedge.vxth.backtest import select_call

DEFAULT_THRESHOLDS = (15, 30, 50)
DEFAULT_MULTIPLES = (2, 5, 10, 20, 50)


def regime_series(vix: pd.Series, thresholds=DEFAULT_THRESHOLDS) -> pd.Series:
    """Map each VIX level to a regime index = number of thresholds it exceeds."""
    th = np.asarray(thresholds)
    return pd.Series((vix.to_numpy()[:, None] > th).sum(axis=1), index=vix.index, name="regime")


def transition_matrix(regime: pd.Series) -> pd.DataFrame:
    """Day-over-day regime transition counts (rows: from, cols: to)."""
    return pd.crosstab(
        pd.Series(regime.to_numpy()[:-1], name="from"),
        pd.Series(regime.to_numpy()[1:], name="to"),
    )


def select_calls_per_date(chain: OptionChain, spot: pd.DataFrame, dte: int, delta: float) -> pd.DataFrame:
    """For every trade date, the (tenor, strike) of the chosen VIX call."""
    rows = []
    dates = pd.DatetimeIndex(np.intersect1d(chain.trade_dates, spot.index.to_numpy()))
    for d in dates:
        pick = select_call(chain.day(d), dte, delta)
        if pick is not None:
            rows.append({"date": d, "tenor": pick[0], "strike": pick[1]})
    return pd.DataFrame(rows)


def _contract_paths(chain: OptionChain, contracts: pd.DataFrame) -> dict:
    """Mid-price path (date-indexed Series) for each (tenor, strike) call, pulled
    in one filtered pass over the panel rather than day-by-day lookups."""
    calls = chain.df[chain.df["cp_flag"] == "C"]
    want = calls.merge(contracts[["tenor", "strike"]], left_on=["exdate", "strike"], right_on=["tenor", "strike"])
    paths = {}
    for (tenor, strike), g in want.groupby(["exdate", "strike"], observed=True):
        s = g.dropna(subset=["mid"]).set_index("date")["mid"].sort_index()
        if len(s):
            paths[(pd.Timestamp(tenor), float(strike))] = s
    return paths


def option_return_stats(
    chain: OptionChain,
    spot: pd.DataFrame,
    *,
    dte: int = 120,
    delta: float = 0.10,
    multiples=DEFAULT_MULTIPLES,
) -> dict:
    """Distribution of max returns for held-to-expiry VIX calls of one recipe.

    Returns a dict with the per-contract ``max_returns`` frame and summary stats
    (count, worthless fraction, exceedance counts/percentages per multiple).
    """
    picks = select_calls_per_date(chain, spot, dte, delta)
    # one row per distinct contract, at its first selection date (R's bo.uniq)
    uniq = picks.drop_duplicates(subset=["tenor", "strike"], keep="first")
    # only contracts whose expiration is within the sample (so we can settle them)
    last = spot.index.max()
    uniq = uniq[uniq["tenor"] <= last]
    paths = _contract_paths(chain, uniq)

    records = []
    for _, row in uniq.iterrows():
        key = (pd.Timestamp(row["tenor"]), float(row["strike"]))
        path = paths.get(key)
        if path is None or path.empty:
            continue
        path = path[path.index >= row["date"]]
        if path.empty:
            continue
        entry = float(path.iloc[0])
        if entry <= 0:
            continue
        vix_exp = spot.at[row["tenor"], "VIX"] if row["tenor"] in spot.index else np.nan
        settle = max(0.0, vix_exp - row["strike"]) if np.isfinite(vix_exp) else np.nan
        life = np.append(path.to_numpy(), settle) if np.isfinite(settle) else path.to_numpy()
        records.append({
            "date": row["date"],
            "tenor": row["tenor"],
            "strike": row["strike"],
            "entry": entry,
            "settle": settle,
            "max_return": float(np.nanmax(life) / entry),
            "worthless": bool(np.isfinite(settle) and settle == 0.0),
        })

    mr = pd.DataFrame(records)
    n = len(mr)
    worthless = int(mr["worthless"].sum()) if n else 0
    exceed = {m: int((mr["max_return"] >= m).sum()) for m in multiples} if n else {}
    return {
        "max_returns": mr,
        "n_contracts": n,
        "n_worthless": worthless,
        "frac_worthless": worthless / n if n else np.nan,
        "exceed_counts": exceed,
        "exceed_pct": {m: round(100 * c / n, 2) for m, c in exceed.items()} if n else {},
        "params": {"dte": dte, "delta": delta},
    }
