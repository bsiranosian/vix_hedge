"""Lag-jitter harness — the *right* robustness channel for the forward-VIX gate.

The entry-date ensemble (:mod:`vix_hedge.vxth.ensemble`) staggers *cohort start
dates*, but cohorts re-sync onto the same monthly expiry grid and read the **same**
gate prints, so it is blind to the gate's single-print sensitivity. The
real luck is *which day the gate threshold is read*: near a VIX-future expiry the
forward sawtooths across the 15 boundary, so a one-day shift can flip the whole
sleeve on or off going into a crash (winter 2019-20 / COVID).

This harness probes exactly that. It re-runs a strategy with the gate **signal
shifted by ±k trading days** and reports the *spread* of the headline metrics
across those shifts. A gate that is robust to single-print noise (a ramp, or a
hysteresis dead band) collapses that spread; the hard 0↔1% cliff does not.

It also measures **gate-driven trading** directly off the weight path ``w(F_t)``.
Because the engine rebalances the ladder every roll regardless of the gate, the
honest "did we cut trading?" number is the variation of the *target weight*: a
monthly roll back to the **same** weight contributes zero, so total-variation of
``w(F_t)`` isolates the deploy→unwind round-trips the soft gate is meant to kill.

Nothing here changes a Wave-0 signature: it only calls ``engine.simulate`` with a
pre-lagged ``signal_series`` and reads :mod:`vix_hedge.vxth.gate` weights.
"""

from __future__ import annotations

import numpy as np
import pandas as pd

from vix_hedge import metrics
from vix_hedge.vxth import engine, episodes
from vix_hedge.vxth.gate import make_gate

#: Headline metrics tracked across lags. Each maps a curve -> float.
METRIC_FNS = {
    "cagr": metrics.cagr,
    "sharpe": metrics.annualized_sharpe,
    "sortino": metrics.sortino,
    "maxdd": metrics.max_drawdown,
    "covid": lambda c: episodes.window_return(c, *episodes.CRASH_EPISODES["COVID 2020"]),
}


def lag_signal(signal: pd.Series, lag: int) -> pd.Series:
    """Shift the gate signal by ``lag`` trading days on its own index.

    ``lag > 0`` = a **stale** read (the gate sees info ``lag`` days old — the
    realistic, deployable robustness probe). ``lag < 0`` = a peek ahead (lookahead;
    included only to map two-sided single-print sensitivity, never as a strategy).
    """
    return signal.shift(lag)


def curve_metrics(curve: pd.Series) -> dict[str, float]:
    """The headline metrics for one equity curve."""
    return {name: float(fn(curve)) for name, fn in METRIC_FNS.items()}


def lag_jitter(
    spot: pd.DataFrame,
    chain,
    *,
    base: dict,
    cfg,
    signal_series: pd.Series,
    lags: tuple[int, ...] = (-2, -1, 0, 1, 2),
    start: str | None = "2006-03-22",
    end: str | None = None,
) -> dict:
    """Run ``cfg`` across ``signal_series`` shifted by each lag; report the spread.

    Returns ``{per_lag, spread, std}``:

    * ``per_lag`` — DataFrame indexed by lag (days), one column per metric.
    * ``spread``  — ``max − min`` across lags per metric (the *lag jitter*; smaller
      is more robust). The sign convention is raw: for ``maxdd``/``covid`` a smaller
      spread still means "less sensitive to which day the gate is read".
    * ``std``     — population std across lags per metric.
    """
    per_lag: dict[int, dict[str, float]] = {}
    for k in lags:
        sig = lag_signal(signal_series, k)
        curve = engine.simulate(spot, chain, base=base, cfg=cfg, start=start, end=end, signal_series=sig)
        per_lag[k] = curve_metrics(curve)
    table = pd.DataFrame(per_lag).T.sort_index()
    table.index.name = "lag_days"
    return {
        "per_lag": table,
        "spread": table.max() - table.min(),
        "std": table.std(ddof=0),
    }


def gate_turnover(
    signal_series: pd.Series,
    *,
    alloc: tuple[float, ...],
    gate_spec: dict | None = None,
    dates: pd.DatetimeIndex | None = None,
) -> dict[str, float]:
    """Gate-driven trading off the weight path ``w(F_t)`` (per year).

    Isolates *gate* churn from the always-on monthly roll: a roll back to an
    unchanged weight contributes nothing to weight total-variation.

    * ``deploys_per_yr``       — 0→positive transitions per year (each pairs with an
      unwind: the deploy→unwind round-trips the proposal targets).
    * ``sleeves_traded_per_yr``— ``Σ|Δw| / full / years`` = full-sleeve-equivalents
      of notional turned over by the gate per year (the honest trade-volume number).
    * ``frac_on``              — fraction of evaluations with the gate open.
    """
    gate = make_gate(alloc, gate_spec)
    s = signal_series.dropna() if dates is None else signal_series.reindex(dates).dropna()
    if len(s) < 2:
        return {"deploys_per_yr": 0.0, "sleeves_traded_per_yr": 0.0, "frac_on": 0.0}
    w = np.array([gate.weight(float(f)) for f in s.to_numpy()])
    years = (s.index[-1] - s.index[0]).days / 365.25
    full = alloc[1] or 1.0  # normalize TV to "sleeve-equivalents"; guard div-by-0
    on = w > 0.0
    deploys = int(((~on[:-1]) & on[1:]).sum())
    tv = float(np.abs(np.diff(w)).sum())
    return {
        "deploys_per_yr": deploys / years,
        "sleeves_traded_per_yr": tv / full / years,
        "frac_on": float(on.mean()),
    }
