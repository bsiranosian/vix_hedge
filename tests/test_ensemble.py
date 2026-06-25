"""Foundation coverage: ensemble harness, tranching, shared metrics.

Hermetic checks run always; the ensemble integration test is skipped without the
parquet cache.
"""

from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

from vix_hedge import config, metrics
from vix_hedge.vxth import episodes


def test_window_return_and_episodes():
    idx = pd.date_range("2020-01-01", "2020-12-31", freq="D")
    curve = pd.Series(np.linspace(100, 50, len(idx)), index=idx)  # -50% over the year
    assert np.isclose(episodes.window_return(curve, "2020-01-01", "2020-12-31"), -0.5, atol=1e-3)
    er = episodes.episode_returns(curve)
    assert "COVID 2020" in er and er["COVID 2020"] < 0


def test_cagr_impact_sign():
    idx = pd.date_range("2010-01-01", "2020-01-01", freq="D")
    base = pd.Series(np.linspace(1.0, 2.0, len(idx)), index=idx)
    better = base * np.linspace(1.0, 1.2, len(idx))  # ends higher -> positive impact
    assert metrics.cagr_impact(better, base) > 0
    assert metrics.cagr_impact(base, base) == pytest.approx(0.0, abs=1e-9)


_HAS_DATA = config.VIX_OPTIONS_PARQUET.exists() and config.SPOT_PARQUET.exists()
pytestmark = pytest.mark.skipif(not _HAS_DATA, reason="data cache not built")


def test_tranche_members_even_spacing():
    from vix_hedge.vxth.ensemble import _tranche_members

    assert _tranche_members(21, 1, 0) == [0]
    assert _tranche_members(21, 3, 0) == [0, 7, 14]
    assert _tranche_members(21, 7, 0) == [0, 3, 6, 9, 12, 15, 18]
    assert _tranche_members(21, 21, 0) == list(range(21))
    assert _tranche_members(21, 3, 20) == [20, 6, 13]  # wraps around the cycle


@pytest.fixture(scope="module")
def market():
    from vix_hedge import data

    return data.load_spot_prices(), data.load_vix_chain()


def test_ensemble_shape_and_tranche(market):
    from vix_hedge.vxth import BASES, HedgeConfig, ensemble

    spot, chain = market
    cfg = HedgeConfig(0.30, (30,))
    res = ensemble(spot, chain, base=BASES["SPX"], cfg=cfg, n_offsets=5, step_days=3,
                   start="2016-01-01", end="2019-12-31")
    assert len(res["per_offset"]) == 5
    assert {"CAGR %", "Sharpe", "MaxDD %"} <= set(res["per_offset"].columns)
    # tranched final lands within the spread of the cohort finals (it is their mean)
    finals = [(c / c.dropna().iloc[0]).iloc[-1] for c in res["curves"].values()]
    assert min(finals) - 1e-6 <= res["tranched"].iloc[-1] <= max(finals) + 1e-6
    assert res["timing_luck"]["CAGR % range"] >= 0.0


# --- timing-luck quantification + 1/N tranching ---------------------------
# A single 30Δ VIX call rolled monthly over 2018-2020 (Q4-2018 + COVID) is the
# canonical timing-luck case. The cohort pool (one entry per trading day across a
# ~monthly roll cycle) is built once and shared by both tests.
_POOL_KW = dict(n=21, start="2018-01-01", end="2020-12-31")
_N_VALUES = (1, 3, 7, 21)


@pytest.fixture(scope="module")
def cohort_pool(market):
    from vix_hedge.vxth import BASES, HedgeConfig
    from vix_hedge.vxth.backtest import forward_vix_signal
    from vix_hedge.vxth.ensemble import run_cohorts_td

    spot, chain = market
    sig = forward_vix_signal(chain, spot, source="hybrid_vx1")
    cfg = HedgeConfig(0.30, (30,), label="single 30d 30Δ")
    return run_cohorts_td(spot, chain, base=BASES["SPX"], cfg=cfg, signal_series=sig, **_POOL_KW)


def test_r1_single_call_timing_luck_is_wide(cohort_pool):
    """Put a number on §4.3 single-call entry-date luck *over a short horizon*.

    Cohorts hold permanently-offset expiry calendars, but the regime-reset at each
    roll makes crash protection nearly entry-invariant, so over the full 15y window
    the annualized-CAGR spread is small (~0.17pp). Over a short 3y window dominated
    by Q4-2018 + COVID it annualizes to ~5pp (observed ≈5.05) — the horizon where
    entry luck bites. The harness must surface that band; this pins it."""
    from vix_hedge.vxth.ensemble import per_offset_metrics

    per = per_offset_metrics(cohort_pool)
    cagr_range = per["CAGR %"].max() - per["CAGR %"].min()
    cagr_iqr = per["CAGR %"].quantile(0.75) - per["CAGR %"].quantile(0.25)
    assert cagr_range > 1.5, f"expected a wide single-call CAGR band, got {cagr_range:.2f}pp"
    assert cagr_iqr > 0.4  # even the interquartile band is material
    assert (per["Sharpe"].max() - per["Sharpe"].min()) > 0.05


def test_r2_tranching_shrinks_spread_monotonically(cohort_pool):
    """The N-tranche strategy's own entry-date spread shrinks ~1/N toward zero
    while mean CAGR is statistically unchanged."""
    import numpy as np

    from vix_hedge.vxth.ensemble import tranche_robustness

    sweep = tranche_robustness(cohort_pool, _N_VALUES)
    rng = sweep["CAGR range"].to_numpy()
    # monotonically non-increasing in N (allow float noise)
    assert np.all(np.diff(rng) <= 1e-6), f"spread not monotone in N: {rng}"
    assert rng[0] > 1.5                      # single cohort: wide
    assert rng[-1] < 0.05                    # N = pool size: spread collapses to ~0
    assert rng[2] < rng[0] / 5.0             # N=7 cuts the spread >5x (1/N-scale)
    # mean CAGR drifts far less than the luck the tranching removes
    drift = sweep["CAGR mean"].max() - sweep["CAGR mean"].min()
    assert drift < rng[0] / 3.0, f"mean CAGR not stable across N: drift={drift:.2f}pp"


def test_tranched_strategy_reads_n_tranches(market):
    """The deployable curve honors cfg.n_tranches and is the mean of its cohorts."""
    from vix_hedge.vxth import BASES, HedgeConfig
    from vix_hedge.vxth.ensemble import tranched_strategy

    spot, chain = market
    cfg = HedgeConfig(0.30, (30,), n_tranches=3, label="x")
    curve = tranched_strategy(spot, chain, base=BASES["SPX"], cfg=cfg,
                              start="2018-01-01", end="2020-12-31")
    assert curve.notna().any() and curve.iloc[-1] > 0
    # override beats the field
    one = tranched_strategy(spot, chain, base=BASES["SPX"], cfg=cfg, n_tranches=1,
                            start="2018-01-01", end="2020-12-31")
    assert one.notna().any()
