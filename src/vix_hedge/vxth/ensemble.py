"""Entry-date ensemble and tranching — measure and fix timing luck.

Running a hedge config across staggered *start* offsets produces N cohorts that
roll on different monthly cycles. Two outputs:

* **Measure:** the spread of per-cohort metrics (CAGR / Sharpe / crash payoff)
  is a direct read on entry/rebalance "timing luck" — the thing that dominates
  single-call backtests (Braun/Hoffstein/Israelov 2023 found >400 bps/yr of it on
  identical put-spread collars).
* **Fix:** the equal-weight average of the cohort curves is the **tranched**,
  RTL-reduced strategy (timing luck falls ~1/N at no cost to mean return;
  Hoffstein/Faber/Braun 2020; Newfound).

This is the keystone every later item compares against. The strike ladder extends
the laddering to strikes × tenors jointly and sweeps N for the saturation point.
"""

from __future__ import annotations

import numpy as np
import pandas as pd

from vix_hedge import metrics
from vix_hedge.config import STARTING_BALANCE
from vix_hedge.vxth import episodes as ep
from vix_hedge.vxth.engine import HedgeConfig, simulate

#: A VXTH front-month roll is ~monthly ≈ 21 trading days. Staggering one cohort
#: per trading day across this span covers one full roll cycle, so the spread of
#: per-cohort metrics is the entry-date "timing luck" and an N-tranche portfolio
#: spaced ``ROLL_CYCLE_TD / N`` apart de-phases the roll.
ROLL_CYCLE_TD = 21


def _offset_start(start: str, k: int, step_days: int) -> str:
    return (pd.Timestamp(start) + pd.Timedelta(days=k * step_days)).strftime("%Y-%m-%d")


def common_trade_dates(spot: pd.DataFrame, chain, start: str, end: str | None = None) -> pd.DatetimeIndex:
    """Trade dates the engine can actually open on (chain ∩ spot), within range."""
    td = pd.DatetimeIndex(np.intersect1d(chain.trade_dates, spot.index.to_numpy()))
    td = td[td >= pd.Timestamp(start)]
    if end:
        td = td[td <= pd.Timestamp(end)]
    return td


def run_cohorts(
    spot: pd.DataFrame,
    chain,
    *,
    base: dict,
    cfg: HedgeConfig | None,
    n_offsets: int = 21,
    step_days: int = 1,
    start: str = "2006-03-22",
    end: str | None = None,
    signal_series: pd.Series | None = None,
    starting_balance: float = STARTING_BALANCE,
) -> dict[int, pd.Series]:
    """Run ``cfg`` across ``n_offsets`` staggered start dates -> {offset: curve}."""
    curves: dict[int, pd.Series] = {}
    for k in range(n_offsets):
        s = _offset_start(start, k, step_days)
        curves[k] = simulate(
            spot, chain, base=base, cfg=cfg, start=s, end=end,
            signal_series=signal_series, starting_balance=starting_balance,
        )
    return curves


def _metrics_row(curve: pd.Series) -> dict:
    row = {
        "CAGR %": 100 * metrics.cagr(curve),
        "Sharpe": metrics.annualized_sharpe(curve),
        "MaxDD %": 100 * metrics.max_drawdown(curve),
    }
    for name, r in ep.episode_returns(curve).items():
        row[f"ep:{name}"] = 100 * r
    return row


def per_offset_metrics(curves: dict[int, pd.Series]) -> pd.DataFrame:
    """Per-cohort metrics table (one row per entry-date offset)."""
    return pd.DataFrame({k: _metrics_row(c) for k, c in curves.items()}).T.sort_index()


def tranche(curves: dict[int, pd.Series]) -> pd.Series:
    """Equal-weight average of the cohort curves over their common dates."""
    norm = [c / c.dropna().iloc[0] for c in curves.values()]
    df = pd.concat(norm, axis=1, join="inner")
    return df.mean(axis=1).rename("tranched")


def timing_luck(per: pd.DataFrame) -> dict[str, float]:
    """Robustness read-out: spread of the per-cohort metrics (max-min and IQR)."""
    out = {}
    for col in ("CAGR %", "Sharpe"):
        out[f"{col} range"] = float(per[col].max() - per[col].min())
        out[f"{col} IQR"] = float(per[col].quantile(0.75) - per[col].quantile(0.25))
    return out


def ensemble(
    spot: pd.DataFrame,
    chain,
    *,
    base: dict,
    cfg: HedgeConfig | None,
    n_offsets: int = 21,
    step_days: int = 1,
    start: str = "2006-03-22",
    end: str | None = None,
    signal_series: pd.Series | None = None,
    starting_balance: float = STARTING_BALANCE,
) -> dict:
    """Full ensemble: cohort curves, per-offset metric distribution, the tranched
    (RTL-reduced) curve, and a timing-luck read-out. See module docstring."""
    curves = run_cohorts(
        spot, chain, base=base, cfg=cfg, n_offsets=n_offsets, step_days=step_days,
        start=start, end=end, signal_series=signal_series, starting_balance=starting_balance,
    )
    per = per_offset_metrics(curves)
    summary = per.describe(percentiles=[0.1, 0.5, 0.9]).loc[
        ["mean", "std", "min", "10%", "50%", "90%", "max"]
    ]
    return {
        "curves": curves,
        "per_offset": per,
        "summary": summary,
        "tranched": tranche(curves),
        "timing_luck": timing_luck(per),
    }


# --- trading-day cohorts + overlapping tranches -----------------------------
#
# Calendar-day offsets (above) can collapse over weekends/holidays — two start
# dates land on the same first tradeable day, giving duplicate cohorts. For the
# 1/N tranche construction we want *distinct, evenly spaced* roll phases, so we
# stagger by trading day instead.


def run_cohorts_td(
    spot: pd.DataFrame,
    chain,
    *,
    base: dict,
    cfg: HedgeConfig | None,
    n: int = ROLL_CYCLE_TD,
    start: str = "2006-03-22",
    end: str | None = None,
    signal_series: pd.Series | None = None,
    starting_balance: float = STARTING_BALANCE,
) -> dict[int, pd.Series]:
    """Run ``cfg`` from each of the first ``n`` trade dates on/after ``start``.

    One cohort per trading day → distinct, evenly spaced entry phases spanning a
    ~monthly roll cycle at ``n = ROLL_CYCLE_TD``. The pool this returns feeds both
    the per-cohort distribution and the :func:`tranche_robustness` tranching sweep.
    """
    starts = [d.strftime("%Y-%m-%d") for d in common_trade_dates(spot, chain, start, end)[:n]]
    return {
        k: simulate(spot, chain, base=base, cfg=cfg, start=s, end=end,
                    signal_series=signal_series, starting_balance=starting_balance)
        for k, s in enumerate(starts)
    }


def _tranche_members(n: int, n_tranches: int, anchor: int) -> list[int]:
    """Indices of ``n_tranches`` cohorts evenly spaced (stride ``n / n_tranches``)
    around the cycle, starting at ``anchor`` (wrapping)."""
    stride = n / n_tranches
    return [int(round(anchor + j * stride)) % n for j in range(n_tranches)]


def tranche_of(curves: dict[int, pd.Series], members: list[int]) -> pd.Series:
    """Equal-weight tranche of a subset of a precomputed cohort pool."""
    return tranche({m: curves[m] for m in members})


def tranche_robustness(
    curves: dict[int, pd.Series],
    n_values: tuple[int, ...] = (1, 3, 7, 21),
) -> pd.DataFrame:
    """Tranching read-out: residual timing luck of the N-tranche strategy vs N.

    For each ``N`` in ``n_values`` (each should divide ``len(curves)`` for clean
    spacing), build the N-tranche portfolio at every anchor phase from the same
    precomputed ``curves`` pool — no re-simulation — and report the *spread* of
    its CAGR/Sharpe across anchors. The spread is the strategy's own entry-date
    luck; it falls ~1/N while mean CAGR is ~unchanged. Index = N.
    """
    n = len(curves)
    keys = sorted(curves)
    rows: dict[int, dict] = {}
    for nt in n_values:
        cagrs, sharpes = [], []
        for a in range(n):
            c = tranche_of(curves, [keys[i] for i in _tranche_members(n, nt, a)])
            cagrs.append(100 * metrics.cagr(c))
            sharpes.append(metrics.annualized_sharpe(c))
        cagrs, sharpes = np.asarray(cagrs), np.asarray(sharpes)
        rows[nt] = {
            "CAGR mean": float(cagrs.mean()),
            "CAGR range": float(cagrs.max() - cagrs.min()),
            "CAGR std": float(cagrs.std(ddof=1)),
            "Sharpe mean": float(sharpes.mean()),
            "Sharpe range": float(sharpes.max() - sharpes.min()),
        }
    return pd.DataFrame(rows).T.rename_axis("N")


def tranched_strategy(
    spot: pd.DataFrame,
    chain,
    *,
    base: dict,
    cfg: HedgeConfig,
    n_tranches: int | None = None,
    anchor: int = 0,
    cycle: int = ROLL_CYCLE_TD,
    start: str = "2006-03-22",
    end: str | None = None,
    signal_series: pd.Series | None = None,
    starting_balance: float = STARTING_BALANCE,
) -> pd.Series:
    """The deployable N-tranche portfolio: equal-weight mean of ``cfg.n_tranches``
    cohorts spaced ``cycle / N`` trading days apart from ``anchor``.

    Reads ``cfg.n_tranches`` (override via ``n_tranches``). Runs exactly N
    simulations. ``simulate`` is untouched — the tranching lives here.
    """
    nt = n_tranches if n_tranches is not None else cfg.n_tranches
    td = common_trade_dates(spot, chain, start, end)
    span = min(cycle, len(td))  # cycle length to spread the tranches over
    starts = [td[i].strftime("%Y-%m-%d") for i in _tranche_members(span, nt, anchor)]
    curves = {
        k: simulate(spot, chain, base=base, cfg=cfg, start=s, end=end,
                    signal_series=signal_series, starting_balance=starting_balance)
        for k, s in enumerate(starts)
    }
    return tranche(curves).rename(f"{cfg.label} ×{nt}")
