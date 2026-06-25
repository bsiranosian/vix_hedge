"""Roll-phase timing-luck harness — the axis that actually breaks a single call.

The entry-date ensemble (:mod:`vix_hedge.vxth.ensemble`) measures one axis of
timing luck and finds it *small* (~0.1pp full-window): staggered start dates re-sync at
the next monthly roll, so where you start barely matters. The axis that dominates a
**single** call is the *roll phase* — which expiration cycle it happens to hold into a
spike. On a weekly-inclusive VIX chain (weeklys from 2016 on), a single call rolled on
nearest-DTE can sit a week off VXTH's monthly settlement, so its lone option expires
just *before* a spike peak and re-buys at the top — which is exactly why the naive single
call missed COVID while VXTH (the monthly track) caught it.

This module sweeps that phase explicitly. It runs a single-call config on each of several
expiry *tracks* — ``offset_days`` from the monthly settlement Wednesday — and reports how
far the crash payoff swings. The 30/60/90 tenor ladder is phase-invariant (its 60/90-day
rungs span any spike regardless of the front rung's phase), so running it through the same
sweep gives a flat line. That contrast — single call swings ~50pp on COVID over a one-week
roll offset, ladder doesn't move — is the robustness result the ladder is *for*.
"""

from __future__ import annotations

from dataclasses import replace

import pandas as pd

from vix_hedge import metrics
from vix_hedge.data.load import OptionChain
from vix_hedge.vxth import episodes as ep
from vix_hedge.vxth.backtest import is_monthly_vix_expiry
from vix_hedge.vxth.engine import HedgeConfig, simulate

#: days from the monthly VIX settlement Wednesday; 0 = the VXTH monthly track,
#: ±7 = the weekly roughly one week before / after, ±14 = two weeks.
DEFAULT_OFFSETS: tuple[int, ...] = (-14, -7, 0, 7, 14)


def phase_filtered_chain(chain: OptionChain, offset_days: int) -> OptionChain:
    """A chain keeping only expirations ``offset_days`` after a monthly settlement
    (0 → the monthly track; −7 → the weekly ~1wk before monthly; +7 → ~1wk after).

    Use this to expose a *single* call to a chosen roll phase. It is **not** the right
    tool for a ladder: filtering to one track also strands the ladder on that track, so its
    tenor diversification (the thing that makes it phase-robust) is destroyed. A ladder's
    phase-robustness is shown on the *full* chain via a cadence shift (:func:`cadence_sweep`).

    The weekly tracks only exist where VIX weeklys are listed (2016 on), so an off-monthly
    track has no rows on earlier days — run the sweep over a window where every track is
    populated (e.g. 2017+) when comparing across offsets.
    """
    raw = chain.df
    uniq = pd.to_datetime(pd.unique(raw["exdate"]))  # is_monthly on ~hundreds of dates, not every row
    keep_ex = {e for e in uniq if is_monthly_vix_expiry(e - pd.Timedelta(days=offset_days))}
    mask = pd.to_datetime(raw["exdate"]).isin(keep_ex).to_numpy()
    return OptionChain(raw[mask].copy())


def phase_curves(
    spot,
    chain: OptionChain,
    *,
    base: dict,
    cfg: HedgeConfig,
    signal_series: pd.Series,
    offsets: tuple[int, ...] = DEFAULT_OFFSETS,
    start: str | None = None,
    end: str | None = None,
) -> dict[int, pd.Series]:
    """Equity curve of a *single-call* ``cfg`` run on each phase track (the phase is applied
    via the chain, so leave ``params['expiry']`` at its default). Don't pass a ladder here —
    see :func:`phase_filtered_chain`. ``signal_series`` is computed from the *full* chain (the
    regime read doesn't depend on which expiry the sleeve trades).
    """
    return {
        off: simulate(spot, phase_filtered_chain(chain, off), base=base, cfg=cfg,
                      start=start, end=end, signal_series=signal_series)
        for off in offsets
    }


def phase_sweep(
    spot,
    chain: OptionChain,
    *,
    base: dict,
    cfg: HedgeConfig,
    signal_series: pd.Series,
    offsets: tuple[int, ...] = DEFAULT_OFFSETS,
    start: str | None = None,
    end: str | None = None,
    episode: str = "COVID 2020",
) -> pd.DataFrame:
    """Per-offset CAGR / Sharpe / crash-window payoff for ``cfg`` across phase tracks,
    indexed by ``offset_days`` (a ``range (max-min)`` row appended). The spread of the
    crash-payoff column is the roll-phase timing luck — large for a single call, ~0 for
    the ladder.
    """
    s, e = ep.CRASH_EPISODES[episode]
    col = f"{episode} %"
    curves = phase_curves(spot, chain, base=base, cfg=cfg, signal_series=signal_series,
                          offsets=offsets, start=start, end=end)
    rows = {
        off: {"CAGR %": 100 * metrics.cagr(c), "Sharpe": metrics.annualized_sharpe(c),
              col: 100 * ep.window_return(c, s, e)}
        for off, c in curves.items()
    }
    df = pd.DataFrame(rows).T
    df.index.name = "offset_days"
    df.loc["range"] = df.max() - df.min()
    return df


def cadence_sweep(
    spot,
    chain: OptionChain,
    *,
    base: dict,
    cfg: HedgeConfig,
    signal_series: pd.Series,
    offsets: tuple[int, ...] = DEFAULT_OFFSETS,
    start: str | None = None,
    end: str | None = None,
    episode: str = "COVID 2020",
) -> pd.DataFrame:
    """Roll-phase robustness on the **full** chain by shifting ``cfg.ladder_dtes`` by each
    offset (e.g. (30,60,90)→(37,67,97)). Unlike :func:`phase_sweep` this keeps the whole
    chain, so a *ladder* still spans tenors and stays phase-invariant — the crash-payoff
    ``range`` row comes out ~0, the foil to the single call's large :func:`phase_sweep` range.
    """
    s, e = ep.CRASH_EPISODES[episode]
    col = f"{episode} %"
    rows = {}
    for off in offsets:
        shifted = replace(cfg, ladder_dtes=tuple(d + off for d in cfg.ladder_dtes), params=dict(cfg.params or {}))
        c = simulate(spot, chain, base=base, cfg=shifted, start=start, end=end, signal_series=signal_series)
        rows[off] = {"CAGR %": 100 * metrics.cagr(c), "Sharpe": metrics.annualized_sharpe(c),
                     col: 100 * ep.window_return(c, s, e)}
    df = pd.DataFrame(rows).T
    df.index.name = "cadence_shift_days"
    df.loc["range"] = df.max() - df.min()
    return df
