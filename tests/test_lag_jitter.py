"""Lag-jitter harness: hermetic turnover/lag checks + data-dependent validation."""

from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

from vix_hedge import config, data
from vix_hedge.vxth import lag_jitter as LJ
from vix_hedge.vxth.backtest import forward_vix_signal
from vix_hedge.vxth.engine import ALLOC_OFFICIAL, BASES, HedgeConfig


def _osc_signal(n: int = 600) -> pd.Series:
    """A forward-VIX path that oscillates 14<->16 across the F=15 gate every ~5 days
    (the Dec/Jan/Feb whipsaw), on a daily business-day index."""
    idx = pd.bdate_range("2010-01-01", periods=n)
    f = 15.0 + 1.5 * np.sin(np.arange(n) * (2 * np.pi / 10))  # 13.5 .. 16.5
    return pd.Series(f, index=idx, name="fwd")


def test_lag_signal_shifts_on_own_index():
    s = pd.Series([1.0, 2.0, 3.0, 4.0], index=pd.bdate_range("2020-01-01", periods=4))
    lagged = LJ.lag_signal(s, 1)  # stale: value at t = value at t-1
    assert np.isnan(lagged.iloc[0])
    assert lagged.iloc[1] == 1.0 and lagged.iloc[3] == 3.0
    lead = LJ.lag_signal(s, -1)  # peek ahead
    assert lead.iloc[0] == 2.0


def test_turnover_hysteresis_cuts_deploys_vs_hard():
    sig = _osc_signal()
    hard = LJ.gate_turnover(sig, alloc=ALLOC_OFFICIAL, gate_spec=None)
    hyst = LJ.gate_turnover(sig, alloc=ALLOC_OFFICIAL, gate_spec={"mode": "hysteresis"})
    # the 14<->16 wobble round-trips the hard gate every cycle; hysteresis latches on once
    assert hard["deploys_per_yr"] > 5 * hyst["deploys_per_yr"]
    assert hard["sleeves_traded_per_yr"] > hyst["sleeves_traded_per_yr"]
    assert hyst["frac_on"] >= hard["frac_on"]  # hysteresis holds the on-state longer


def test_turnover_ramp_has_no_full_round_trips_but_trades_gradually():
    sig = _osc_signal()
    ramp = LJ.gate_turnover(sig, alloc=ALLOC_OFFICIAL, gate_spec={"mode": "ramp"})
    # the 13.5..16.5 wobble stays > ramp floor (13) the whole time -> ~no on/off deploys
    assert ramp["deploys_per_yr"] < 1.0
    assert ramp["sleeves_traded_per_yr"] > 0.0  # but it does trade continuously


# ---- data-dependent (skip without the parquet cache) ----------------------------
_HAS_DATA = config.VIX_OPTIONS_PARQUET.exists() and config.SPOT_PARQUET.exists()
pytestmark = pytest.mark.skipif(not _HAS_DATA, reason="data cache not built")


@pytest.fixture(scope="module")
def market():
    spot = data.load_spot_prices()
    chain = data.load_vix_chain()
    return spot, chain


def test_lag_jitter_table_shape(market):
    spot, chain = market
    sig = forward_vix_signal(chain, spot, source="hybrid_cmf30")
    cfg = HedgeConfig(0.50, (30, 60, 90), ALLOC_OFFICIAL)
    res = LJ.lag_jitter(spot, chain, base=BASES["SPX"], cfg=cfg, signal_series=sig, lags=(-1, 0, 1))
    assert list(res["per_lag"].index) == [-1, 0, 1]
    assert set(res["per_lag"].columns) == set(LJ.METRIC_FNS)
    assert (res["spread"] >= -1e-12).all()  # max-min is non-negative


def test_cmf30_hard_gate_is_lag_robust_into_covid(market):
    """The vx1 gate has a COVID knife edge under a 1-day lag; cmf30 removes
    it even for the *hard* gate. This is the load-bearing claim — robustness is a
    signal lever, not a gate-shape lever."""
    spot, chain = market
    cfg = HedgeConfig(0.50, (30, 60, 90), ALLOC_OFFICIAL)
    vx1 = forward_vix_signal(chain, spot, source="hybrid_vx1")
    cmf = forward_vix_signal(chain, spot, source="hybrid_cmf30")
    sp_vx1 = LJ.lag_jitter(spot, chain, base=BASES["SPX"], cfg=cfg, signal_series=vx1, lags=(0, 1))["spread"]
    sp_cmf = LJ.lag_jitter(spot, chain, base=BASES["SPX"], cfg=cfg, signal_series=cmf, lags=(0, 1))["spread"]
    assert sp_vx1["covid"] > 0.2  # vx1 hard gate: large COVID swing under +1 lag (the knife edge)
    assert sp_cmf["covid"] < 0.02  # cmf30 hard gate: essentially lag-flat


def test_softening_does_not_cut_cost_drag(market):
    """The honest counter-result: gate softening cuts deploy *events* but not the
    roll-driven transaction cost. Hysteresis sits 'on' more, so its drag is no lower."""
    from vix_hedge.vxth.costs import ProportionalCost
    from vix_hedge.vxth.engine import simulate

    spot, chain = market
    sig = forward_vix_signal(chain, spot, source="hybrid_cmf30")
    kw = dict(base=BASES["SPX"], signal_series=sig)
    cost = ProportionalCost()

    def drag(spec):
        cfg = HedgeConfig(0.50, (30, 60, 90), ALLOC_OFFICIAL,
                          params={"gate": spec} if spec else {})
        from vix_hedge import metrics
        g = simulate(spot, chain, cfg=cfg, cost_model=None, **kw)
        n = simulate(spot, chain, cfg=cfg, cost_model=cost, **kw)
        return metrics.cagr(g) - metrics.cagr(n)

    assert drag({"mode": "hysteresis"}) >= drag(None) - 1e-4  # no cost saving from softening
