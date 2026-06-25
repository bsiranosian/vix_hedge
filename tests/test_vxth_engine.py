"""VIX-call hedge engine: unit checks + data-dependent reproduction of the report.

The data-dependent tests are skipped if the parquet cache is not built.
"""

from __future__ import annotations

import numpy as np
import pytest

from vix_hedge import config, data, metrics
from vix_hedge.vxth import engine
from vix_hedge.vxth.engine import ALLOC_OFFICIAL, ALLOC_REVERSED, BASES, HedgeConfig


def test_alloc_schedules():
    # official: 1% in 15-30, 0.5% in 30-50; reversed swaps the two
    assert ALLOC_OFFICIAL == (0.0, 0.010, 0.005, 0.0)
    assert ALLOC_REVERSED == (0.0, 0.005, 0.010, 0.0)


def test_bases_sum_to_one():
    for name, w in BASES.items():
        assert abs(sum(w.values()) - 1.0) < 1e-9, name


_HAS_DATA = config.VIX_OPTIONS_PARQUET.exists() and config.SPOT_PARQUET.exists()
pytestmark = pytest.mark.skipif(not _HAS_DATA, reason="data cache not built")


@pytest.fixture(scope="module")
def spot():
    return data.load_spot_prices()


@pytest.fixture(scope="module")
def vix_chain():
    return data.load_vix_chain()


def test_baseline_reproduces_vxth_replication(spot, vix_chain):
    """report §4.1 single 30d/30Δ replication on VXTH's *monthly* roll calendar ~ CAGR 10.6%.

    VXTH rolls a single ~30-day 30Δ VIX call on the standard monthly VIX settlement cycle. The
    chain also lists VIX *weeklys* (2016+); the nearest-DTE pick would otherwise grab one phased a
    week off that cycle, which dropped this number to ~5.3% and made it *miss* the 2020 COVID spike
    (the lone call expired days before the peak and re-bought at the top). Pinning
    ``params={"expiry": "monthly"}`` restores VXTH's calendar, so the single call replicates the
    index (CAGR ~10.6 vs official 12.2, Sharpe ~0.58 vs 0.67) and catches COVID (+29%). The residual
    gap to the official index is methodology (signal source, strike rounding), not roll phase."""
    from vix_hedge.vxth import episodes as ep

    cfg = HedgeConfig(0.30, (30,), ALLOC_OFFICIAL, params={"expiry": "monthly"})
    curve = engine.simulate(spot, vix_chain, base=BASES["SPX"], cfg=cfg)
    assert np.isclose(metrics.cagr(curve), 0.106, atol=0.015)  # stable on the monthly calendar
    assert ep.window_return(curve, *ep.CRASH_EPISODES["COVID 2020"]) > 0.10  # catches COVID (+29%)
    assert (curve.to_numpy() > 0).all()


def test_vxth_replication_ladder_is_robust(spot, vix_chain):
    """report §4.5: the 30/60/90 ladder (the paper's own timing-luck fix) ~ CAGR 9.89%.
    Unlike the single call this is luck-stable -- holding 30/60/90-day calls keeps an
    option live across any spike, so the +16.6% COVID payoff and ~10.3% CAGR don't swing
    with the roll phase. This is the engine's stable validation anchor.

    Pinned to ``sizing="equal_contracts"`` -- the published MSE448 figure was computed
    that way (one shared contract count across the ladder). The engine's *default* is
    now equal-dollar sizing (see ``scripts/run_ladder_sizing.py`` / RESULTS), so this
    test keeps reproducing the original number rather than tracking the new default."""
    from vix_hedge.vxth import episodes as ep

    cfg = HedgeConfig(0.30, (30, 60, 90), ALLOC_OFFICIAL, params={"sizing": "equal_contracts"})
    curve = engine.simulate(spot, vix_chain, base=BASES["SPX"], cfg=cfg)
    assert np.isclose(metrics.cagr(curve), 0.103, atol=0.012)  # 10.28%, matches §4.5 ~9.89
    assert ep.window_return(curve, *ep.CRASH_EPISODES["COVID 2020"]) > 0.10  # pays in COVID (+16.6%)


def test_ladder_robust_to_roll_phase(spot, vix_chain):
    """report §4.5: the tenor ladder is robust to the roll *phase* a single call is exposed to.
    A single call rolled on nearest-DTE can land on a VIX weekly and miss the COVID spike when its
    one option expires before the peak; the 30/60/90 ladder always holds a 60/90-day rung alive
    through the spike, so it catches COVID regardless of phase. (On VXTH's *monthly* calendar the
    single call catches COVID too -- see test_baseline_reproduces_vxth_replication -- so the ladder's
    edge is robustness to that calendar choice, not raw CAGR.)"""
    from vix_hedge.vxth import episodes as ep

    # nearest-DTE single (no expiry knob) -> the weekly-phase exposure the ladder immunizes against
    naive_single = engine.simulate(spot, vix_chain, base=BASES["SPX"], cfg=HedgeConfig(0.30, (30,), ALLOC_OFFICIAL))
    ladder = engine.simulate(spot, vix_chain, base=BASES["SPX"], cfg=HedgeConfig(0.30, (30, 60, 90), ALLOC_OFFICIAL))
    covid = lambda c: ep.window_return(c, *ep.CRASH_EPISODES["COVID 2020"])  # noqa: E731
    assert covid(naive_single) < 0 < covid(ladder)  # naive single misses COVID; the ladder catches it


def test_unhedged_equals_base(spot, vix_chain):
    # cfg=None is the plain base benchmark; SPX base should track SPX returns
    spx = engine.simulate(spot, vix_chain, base=BASES["SPX"], cfg=None)
    realized = spx.iloc[-1] / spx.iloc[0]
    px = spot.loc[spx.index, "SPX"]
    assert np.isclose(realized, px.iloc[-1] / px.iloc[0], rtol=1e-6)


def test_custom_named_base_column(spot, vix_chain):
    # any spot column named in the base weights is usable (e.g. a precomputed
    # blended index) and reproduces SPX exactly
    custom = spot.copy()
    custom["BLEND"] = custom["SPX"]
    cfg = HedgeConfig(0.30, (30, 60, 90), ALLOC_OFFICIAL)
    a = engine.simulate(spot, vix_chain, base=BASES["SPX"], cfg=cfg, end="2008-12-31")
    b = engine.simulate(custom, vix_chain, base={"BLEND": 1.0}, cfg=cfg, end="2008-12-31")
    assert np.allclose(a.to_numpy(), b.to_numpy())


def test_monetization_lowers_return_raises_sharpe(spot, vix_chain):
    # report §4.6: selling at a multiple trades absolute return for risk-adjusted
    base = HedgeConfig(0.10, (30, 60, 90), ALLOC_REVERSED)
    mon = HedgeConfig(0.10, (30, 60, 90), ALLOC_REVERSED, monetize_mult=100)
    c0 = engine.simulate(spot, vix_chain, base=BASES["SPX"], cfg=base)
    c1 = engine.simulate(spot, vix_chain, base=BASES["SPX"], cfg=mon)
    assert metrics.cagr(c1) < metrics.cagr(c0)
    assert metrics.annualized_sharpe(c1) >= metrics.annualized_sharpe(c0) - 1e-9
