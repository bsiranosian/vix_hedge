"""Roll-phase timing-luck harness — the single call swings, the ladder doesn't."""

from __future__ import annotations

import pandas as pd
import pytest

from vix_hedge import config, data
from vix_hedge.vxth import roll_phase
from vix_hedge.vxth.backtest import forward_vix_signal, is_monthly_vix_expiry
from vix_hedge.vxth.engine import ALLOC_OFFICIAL, BASES, HedgeConfig


def test_is_monthly_vix_expiry():
    # standard monthly VIX settlements (Wed 30d before the next month's 3rd Friday) ...
    assert is_monthly_vix_expiry(pd.Timestamp("2020-02-19"))
    assert is_monthly_vix_expiry(pd.Timestamp("2020-03-18"))
    # ... weeklys are not
    assert not is_monthly_vix_expiry(pd.Timestamp("2020-02-26"))
    assert not is_monthly_vix_expiry(pd.Timestamp("2020-03-10"))


_HAS_DATA = config.VIX_OPTIONS_PARQUET.exists() and config.SPOT_PARQUET.exists()
pytestmark = pytest.mark.skipif(not _HAS_DATA, reason="data cache not built")


@pytest.fixture(scope="module")
def spot():
    return data.load_spot_prices()


@pytest.fixture(scope="module")
def vix_chain():
    return data.load_vix_chain()


def test_phase_filtered_chain_keeps_only_monthlies(vix_chain):
    monthly = roll_phase.phase_filtered_chain(vix_chain, 0)
    ex = pd.to_datetime(monthly.df["exdate"]).unique()
    assert len(ex) > 0
    assert all(is_monthly_vix_expiry(e) for e in ex)


def test_single_call_swings_with_phase_ladder_invariant(spot, vix_chain):
    """The single call's COVID payoff swings tens of pp across one-week roll-phase shifts
    (some phases catch the spike, some miss it); the 30/60/90 ladder, kept on the full chain,
    is ~invariant to the same shift. Run over 2018-2020 (weeklys populate every track, and the
    window contains COVID). Signal-robust, shown with cmf30."""
    sig = forward_vix_signal(vix_chain, spot, source="hybrid_cmf30")
    offs = (-7, 0, 7)
    kw = dict(base=BASES["SPX"], signal_series=sig, offsets=offs, start="2018-01-01", end="2020-12-31")
    # single: which weekly/monthly track the lone option sits on -> big COVID swing
    single = roll_phase.phase_sweep(spot, vix_chain, cfg=HedgeConfig(0.30, (30,), ALLOC_OFFICIAL), **kw)
    # ladder: same roll-cadence shift, full chain -> spans tenors -> ~invariant
    ladder = roll_phase.cadence_sweep(spot, vix_chain, cfg=HedgeConfig(0.30, (30, 60, 90), ALLOC_OFFICIAL), **kw)
    col = "COVID 2020 %"
    assert single.loc["range", col] > 20.0  # single COVID swings tens of pp with roll phase
    assert ladder.loc["range", col] < 5.0  # ladder ~phase-invariant
    # some phases catch COVID, some miss it (the monthly track, offset 0, is the lucky one)
    rows = single.drop(index="range")
    assert rows[col].max() > 0.0 > rows[col].min()
    assert rows.loc[0, col] > 0.0  # the monthly (VXTH) track catches it
