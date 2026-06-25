"""Transaction-cost model + the bid/ask-spread fill wiring.

The hermetic block pins the :class:`ProportionalCost` arithmetic and — at the
sleeve boundary — the money-conservation identity (``deployed − value() ==`` the
charged half-spread). The data-dependent block checks that costs strictly lower
returns, that frictionless is byte-identical to today, and the headline: the 1/N
tranching robustness gain survives realistic costs.
"""

from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

from vix_hedge import config
from vix_hedge.data.load import OptionChain
from vix_hedge.vxth.costs import DEFAULT_RATES, NO_COST, ProportionalCost, bps
from vix_hedge.vxth.engine import HedgeConfig
from vix_hedge.vxth.sleeves import make_sleeve

# --- hermetic: the cost model arithmetic -----------------------------------


def test_proportional_cost_is_rate_times_notional():
    c = ProportionalCost()
    assert c.on_trade(1000.0, instrument="vix_call") == pytest.approx(1000 * DEFAULT_RATES["vix_call"])
    assert c.on_trade(-1000.0, instrument="vix_call") == pytest.approx(1000 * DEFAULT_RATES["vix_call"])  # |notional|
    assert c.on_trade(1000.0, instrument="unlisted") == pytest.approx(1000 * 0.05)  # default fallback


def test_scale_and_bps_and_nocost():
    assert ProportionalCost(scale=0.0).on_trade(1e6, instrument="vix_call") == 0.0  # scale=0 -> frictionless
    assert ProportionalCost(scale=2.0).on_trade(100.0, instrument="vix_call") == pytest.approx(2 * 100 * 0.06)
    assert bps(50).on_trade(1000.0) == pytest.approx(1000 * 0.005)  # 50 bps of notional
    assert NO_COST.on_trade(1e9, instrument="vix_call") == 0.0


# --- hermetic: sleeve-level conservation (cost leaves the portfolio) --------


def _chain(rows: list[dict]) -> OptionChain:
    """Build an OptionChain from explicit option rows (one trade date is enough)."""
    df = pd.DataFrame(rows)
    for col, default in (("delta", np.nan), ("forward", np.nan)):
        if col not in df:
            df[col] = default
    df["date"] = pd.to_datetime(df["date"])
    df["exdate"] = pd.to_datetime(df["exdate"])
    return OptionChain(df)


def _vix_chain():
    """One date, a 30Δ-ish call ladder; deltas let select_call pick by delta."""
    date, ex30 = "2015-06-01", "2015-07-01"
    rows = []
    for k, dlt, mid in [(18.0, 0.55, 2.0), (20.0, 0.30, 1.0), (22.0, 0.15, 0.5)]:
        rows.append({"date": date, "exdate": ex30, "cp_flag": "C", "strike": k,
                     "mid": mid, "delta": dlt, "dte": 30})
    return _chain(rows)


def test_vix_call_roll_charges_half_spread_and_conserves():
    chain = ProportionalCost()
    cfg = HedgeConfig(0.30, (30,), instrument="vix_call")
    sleeve = make_sleeve("vix_call", _vix_chain(), cfg, cost_model=chain)
    date = pd.Timestamp("2015-06-01")
    px = {"VIX": 18.0}
    deployed = sleeve.roll_and_fund(date, px, 1000.0)
    sleeve.mark(date, px)
    # value() is the premium established (~target); deployed is premium + the spread,
    # so the difference the engine debits from the base is exactly the half-spread.
    assert sleeve.value() == pytest.approx(1000.0, rel=1e-9)
    assert deployed - sleeve.value() == pytest.approx(1000.0 * DEFAULT_RATES["vix_call"], rel=1e-9)


def test_frictionless_sleeve_deploys_exactly_the_premium():
    """cost_model=None must leave deployed == value() (today's behavior, unchanged)."""
    cfg = HedgeConfig(0.30, (30,), instrument="vix_call")
    for cm in (None, NO_COST, ProportionalCost(scale=0.0)):
        sleeve = make_sleeve("vix_call", _vix_chain(), cfg, cost_model=cm)
        d, px = pd.Timestamp("2015-06-01"), {"VIX": 18.0}
        deployed = sleeve.roll_and_fund(d, px, 1000.0)
        sleeve.mark(d, px)
        assert deployed == pytest.approx(sleeve.value(), rel=1e-12), cm


# --- data-dependent: costs bite, frictionless unchanged, tranching survives -


_HAS_DATA = config.VIX_OPTIONS_PARQUET.exists() and config.SPOT_PARQUET.exists()
pytestmark = pytest.mark.skipif(not _HAS_DATA, reason="data cache not built")


@pytest.fixture(scope="module")
def market():
    from vix_hedge import data

    return data.load_spot_prices(), data.load_vix_chain()


def _cohort_pool(spot, chain, *, base, cfg, n, start, end, signal, cost_model):
    """A precomputed cohort pool (one sim per trade-day offset) at a given cost —
    feeds tranche_robustness without touching the ensemble signatures."""
    from vix_hedge.vxth.engine import simulate
    from vix_hedge.vxth.ensemble import common_trade_dates

    starts = [d.strftime("%Y-%m-%d") for d in common_trade_dates(spot, chain, start, end)[:n]]
    return {
        k: simulate(spot, chain, base=base, cfg=cfg, start=s, end=end,
                    signal_series=signal, cost_model=cost_model)
        for k, s in enumerate(starts)
    }


def test_costs_lower_return_and_frictionless_is_identical(market):
    from vix_hedge import metrics
    from vix_hedge.vxth import BASES
    from vix_hedge.vxth.engine import ALLOC_OFFICIAL, simulate

    spot, chain = market
    cfg = HedgeConfig(0.30, (30,), ALLOC_OFFICIAL)
    sig = spot["VIX"]
    kw = dict(base=BASES["SPX"], cfg=cfg, start="2015-01-02", end="2018-12-31", signal_series=sig)

    gross = simulate(spot, chain, cost_model=None, **kw)
    zero = simulate(spot, chain, cost_model=ProportionalCost(scale=0.0), **kw)
    net = simulate(spot, chain, cost_model=ProportionalCost(), **kw)

    # frictionless (None) and a scale-0 model are byte-identical to today.
    assert np.allclose(gross.to_numpy(), zero.to_numpy(), rtol=1e-12)
    # a real half-spread strictly lowers the hedged curve's compound growth.
    assert metrics.cagr(net) < metrics.cagr(gross)
    assert (net.to_numpy() > 0).all()


def test_tranching_robustness_survives_costs(market):
    """The headline: costs are a ~level drag (every cohort trades alike), so the
    cross-offset CAGR *spread* still falls toward 1/N under realistic costs — the
    timing-luck reduction is cost-free even though the absolute level drops."""
    from vix_hedge.vxth import BASES
    from vix_hedge.vxth.engine import ALLOC_OFFICIAL
    from vix_hedge.vxth.ensemble import tranche_robustness

    spot, chain = market
    cfg = HedgeConfig(0.30, (30,), ALLOC_OFFICIAL)
    sig = spot["VIX"]
    kw = dict(base=BASES["SPX"], cfg=cfg, n=6, start="2015-01-02", end="2019-12-31", signal=sig)

    gross = _cohort_pool(spot, chain, cost_model=None, **kw)
    net = _cohort_pool(spot, chain, cost_model=ProportionalCost(), **kw)
    rg = tranche_robustness(gross, n_values=(1, 3))
    rn = tranche_robustness(net, n_values=(1, 3))

    # costs drag the level down...
    assert rn.loc[1, "CAGR mean"] < rg.loc[1, "CAGR mean"]
    # ...but the 1/N spread reduction is preserved net of costs.
    assert rn.loc[3, "CAGR range"] < rn.loc[1, "CAGR range"]
