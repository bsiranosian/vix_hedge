"""Forward-VIX regime signal + VIX-option selection primitives (CBOE VXTH style).

Shared building blocks for the tail-hedge engine and its sleeves -- this module is
*not* a backtest itself (the simulation loop lives in
:mod:`vix_hedge.vxth.engine`):

* :func:`forward_vix_series` / :func:`forward_vix_signal` -- the ~1-month-forward
  VIX used as the regime signal (a put-call-parity forward, or the real CBOE VX1 /
  CMF30 future where available, 2013+).
* :func:`regime_of` with ``REGIME_BOUNDS`` / ``REGIME_ALLOC`` -- the official CBOE
  VXTH 4-level hedge-weight schedule on that forward (below).
* :func:`select_calls` / :func:`select_call` -- pick the out-of-the-money VIX
  call(s) nearest a target tenor and delta, optionally restricted to the monthly
  settlement calendar.

======================  ============
forward VIX             hedge weight
======================  ============
<= 15                   0.0%
15 - 30                 1.0%   (official CBOE VXTH)
30 - 50                 0.5%
> 50                    0.0%
======================  ============
"""

from __future__ import annotations

import numpy as np
import pandas as pd

from vix_hedge.data.load import DayChain, OptionChain

# (low, high) bounds and hedge weight per regime -- official VXTH schedule.
REGIME_BOUNDS = ((0, 15), (15, 30), (30, 50), (50, 1000))
REGIME_ALLOC = (0.000, 0.010, 0.005, 0.000)


def forward_vix_series(chain: OptionChain, spot: pd.DataFrame) -> pd.Series:
    """Daily ~1-month-forward VIX (proxy for the VX1 future the report used).

    Derived from the options themselves via put-call parity: for the ~30-DTE
    expiration each day, ``F = K + (C - P)`` at the strike where ``|C - P|`` is
    smallest (the ATM strike), with ``r`` taken as 0 over the short tenor. This
    matters because in contango (e.g. early 2020) the forward sits well above
    spot VIX, which determines whether the hedge is on going into a crash.
    Falls back to the OptionMetrics ``forward_price`` column, then spot VIX.
    """
    df = chain.df[["date", "exdate", "cp_flag", "strike", "mid", "dte", "forward"]]
    calls = df[df["cp_flag"] == "C"]
    puts = df[df["cp_flag"] == "P"]
    m = calls.merge(puts, on=["date", "exdate", "strike"], suffixes=("_c", "_p")).dropna(subset=["mid_c", "mid_p"])
    m["cmp"] = m["mid_c"] - m["mid_p"]
    # ATM strike per (date, exdate): smallest |C - P| -> parity forward
    atm = m.loc[m.assign(a=m["cmp"].abs()).groupby(["date", "exdate"])["a"].idxmin()].copy()
    atm["fwd"] = atm["strike"] + atm["cmp"]
    atm["d30"] = (atm["dte_c"] - 30).abs()
    parity = atm.loc[atm.groupby("date")["d30"].idxmin()].set_index("date")["fwd"]

    # (the OptionMetrics ``forward_price`` column is mostly -99.99 sentinels, so
    # parity is the reliable source; fall back to spot VIX only where missing.)
    out = parity.reindex(spot.index).fillna(spot["VIX"])
    out.name = "forward_vix"
    return out


def forward_vix_signal(chain: OptionChain, spot: pd.DataFrame, source: str = "hybrid_vx1") -> pd.Series:
    """Daily forward-VIX regime signal from a chosen source.

    * ``parity``        -- put-call-parity forward from the VIX options; the only
                           source covering the full 2006-2020 span.
    * ``hybrid_vx1``    -- the **real CBOE front-month VIX future (VX1)** where
                           available (2013+), parity before. This is the report's
                           apparent basis and the best replication.
    * ``hybrid_cmf30``  -- real 30-day constant-maturity VIX forward (2013+),
                           parity before. (CMF30 ~ parity to ~0.3 vol points.)

    Falls back to pure parity if the VX-futures cache is absent, so the rest of
    the pipeline never hard-depends on the futures download.
    """
    from vix_hedge import config

    parity = forward_vix_series(chain, spot)
    if source == "parity" or not config.VIX_FUTURES_PARQUET.exists():
        # No surprise network builds: fall back to parity unless the cache exists
        # (build it explicitly with `python -m vix_hedge.data.vix_futures`).
        return parity
    from vix_hedge.data import vix_futures as vf

    panel = vf.load_panel()
    measure = "cmf30" if "cmf30" in source else "vx1"
    real = panel[measure].reindex(spot.index)
    out = real.combine_first(parity)  # real futures where present, parity elsewhere
    out.name = f"forward_vix_{source}"
    return out


def regime_of(value: float) -> int:
    """Regime index 0..3 from the forward-VIX bounds."""
    for i, (lo, hi) in enumerate(REGIME_BOUNDS):
        if lo < value <= hi:
            return i
    return 0 if value <= 0 else len(REGIME_BOUNDS) - 1


def _third_friday(year: int, month: int) -> pd.Timestamp:
    first = pd.Timestamp(year, month, 1)
    return first + pd.Timedelta(days=(4 - first.weekday()) % 7 + 14)


def is_monthly_vix_expiry(date) -> bool:
    """True for a *standard monthly* VIX settlement — the Wednesday 30 days before the
    following month's third Friday — and False for a VIX **weekly** (in the chain from
    2016 on).

    Why this exists: :func:`select_calls` picks the expiration *nearest* ``dte_target``.
    On a weekly-inclusive chain that nearest pick can land on a weekly phased a week off
    the monthly VXTH roll cycle, which flips a single call's crash payoff — e.g. for COVID
    the engine grabbed the Mar-10 *weekly* (expired 6 trading days before the Mar-16 peak
    and re-bought at the top) instead of VXTH's Mar-18 *monthly* (held cheap through the
    peak). Restricting selection to monthlies (``monthly_only=True``) restores VXTH's roll
    calendar, so a single 30Δ call replicates the index. The tenor ladder doesn't need it —
    its 60/90-day rungs span any spike regardless of the front rung's phase.
    """
    t = pd.Timestamp(date).normalize() + pd.Timedelta(days=30)
    return _third_friday(t.year, t.month) == t


def select_calls(
    day: DayChain, dte_target: int, delta_targets: tuple[float, ...], *, monthly_only: bool = False
) -> tuple[pd.Timestamp, list[float]] | None:
    """Pick (expiration, [strikes]) at the call expiration nearest ``dte_target``,
    one strike per target delta (the listed strike whose delta is closest). Skips an
    expiration whose deltas are all NaN. ``None`` if no expiration qualifies.

    This is the strike-laddering primitive: passing several deltas returns several
    rungs **at the same tenor** so the ladder can diversify across strikes (not just
    tenors). :func:`select_call` is the single-delta special case.

    ``monthly_only`` restricts the candidate expirations to standard monthly VIX
    settlements (:func:`is_monthly_vix_expiry`) — the VXTH roll calendar — so the
    nearest-DTE pick can't grab a VIX weekly. Falls back to the full set on the rare day
    no monthly is listed, so early/sparse data never returns ``None`` spuriously.
    """
    if len(day.exdates) == 0:
        return None
    exdates = day.exdates
    if monthly_only:
        keep = np.array([is_monthly_vix_expiry(e) for e in exdates], dtype=bool)
        if keep.any():
            exdates = exdates[keep]
    d = np.datetime64(day.date, "D")
    diffs = np.abs((exdates.astype("datetime64[D]") - d).astype(int) - dte_target)
    order = exdates[np.argsort(diffs)]
    for ex in order[:3]:  # try nearest few expirations
        calls = day.leg("C", ex)
        if calls is None:
            continue
        deltas = calls["delta"].to_numpy()
        if np.all(np.isnan(deltas)):
            continue
        ks = calls.index.to_numpy()
        strikes = [float(ks[np.nanargmin(np.abs(deltas - dt))]) for dt in delta_targets]
        return pd.Timestamp(ex), strikes
    return None


def select_call(
    day: DayChain, dte_target: int, delta_target: float, *, monthly_only: bool = False
) -> tuple[pd.Timestamp, float] | None:
    """Pick (expiration, strike) of the call nearest ``dte_target`` whose delta is
    closest to ``delta_target`` (single-delta case of :func:`select_calls`)."""
    res = select_calls(day, dte_target, (delta_target,), monthly_only=monthly_only)
    return (res[0], res[1][0]) if res is not None else None
