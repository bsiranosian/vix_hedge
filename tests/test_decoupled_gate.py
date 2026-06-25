"""Decouple the exit from the entry gate: the ``ratchet`` roll + ``monetize_budget``.

The stock VXTH >50 shut-off does two jobs at once — an *entry* gate ("don't buy
expensive vol", regime weight 0 above forward-VIX 50) and an *exit* ("bank the spike":
the alive ladder is re-struck to 0 contracts at the next roll because the weight is 0).
This pins the two new knobs that split them:

* **``roll="ratchet"`` (hermetic):** funds *up* to the regime budget each roll but never
  trims a rung — so a ``target=0`` roll (forward-VIX >50) buys nothing (entry gate intact)
  yet holds the appreciated ladder instead of dumping it (no cliff). Contrast with
  ``rebalance``, where the same ``target=0`` roll liquidates the sleeve.
* **``monetize_budget`` (hermetic):** the explicit value-based exit that replaces the
  cliff — bank a fraction of the whole sleeve once value/cost-basis crosses each multiple;
  pure transfer, irreversible within a roll cycle, high-water reset at each roll.
* **finding (data-dependent):** keeping the gate and *holding* (ratchet, no forced exit)
  captures the COVID/GFC spikes with a *higher* Sharpe and lower drawdown than the cliff,
  and is robust to crash shape; an explicit *threshold* exit banks too early (the same
  banks-too-early effect).
"""

from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

from vix_hedge import config
from vix_hedge.data.load import OptionChain
from vix_hedge.vxth.engine import ALLOC_OFFICIAL, HedgeConfig
from vix_hedge.vxth.sleeves import _Base, make_sleeve

_DATE = pd.Timestamp("2015-06-01")
_PX = {"VIX": 18.0}
_TARGET = 3000.0
# A 30/60/90 ladder at one delta: one 30Δ call per tenor, premium rising with tenor.
_TENORS = [(30, "2015-07-01", 1.0), (60, "2015-07-31", 1.6), (90, "2015-08-30", 2.1)]
_GRID = [(20.0, 0.50), (21.0, 0.40), (22.0, 0.30), (23.0, 0.20), (24.0, 0.10)]  # 30Δ -> K22 each tenor


def _ladder_chain() -> OptionChain:
    rows = [{"date": "2015-06-01", "exdate": ex, "cp_flag": "C", "strike": 20.0,
             "mid": mid, "delta": 0.30, "dte": dte} for dte, ex, mid in _TENORS]
    df = pd.DataFrame(rows)
    df["forward"] = np.nan
    df["date"] = pd.to_datetime(df["date"])
    df["exdate"] = pd.to_datetime(df["exdate"])
    return OptionChain(df)


def _roll_chain(d1_mid: float) -> OptionChain:
    """d0 with 30/60/90 expiries; d1 (= the 30d expiry) re-lists the two survivors at
    ``d1_mid`` plus a fresh ~90d expiry. ``d1_mid`` > d0 mids = a spike at the roll."""
    rows = []
    for date, tenors in [
        ("2015-06-01", [("2015-07-01", 1.0), ("2015-07-31", 1.4), ("2015-08-30", 1.8)]),
        ("2015-07-01", [("2015-07-31", d1_mid), ("2015-08-30", d1_mid), ("2015-09-29", d1_mid)]),
    ]:
        for ex, mid in tenors:
            dte = (pd.Timestamp(ex) - pd.Timestamp(date)).days
            for k, dlt in _GRID:
                rows.append({"date": date, "exdate": ex, "cp_flag": "C", "strike": k,
                             "mid": mid, "delta": dlt, "dte": dte})
    df = pd.DataFrame(rows)
    df["forward"] = np.nan
    df["date"] = pd.to_datetime(df["date"])
    df["exdate"] = pd.to_datetime(df["exdate"])
    return OptionChain(df)


def _sleeve(chain, **params):
    cfg = HedgeConfig(0.30, (30, 60, 90), instrument="vix_call",
                      params={"sizing": "equal_dollars", **params})
    return make_sleeve("vix_call", chain, cfg)


# --- ratchet roll policy ----------------------------------------------------


def test_ratchet_requires_equal_dollars():
    cfg = HedgeConfig(0.30, (30, 60, 90), instrument="vix_call",
                      params={"sizing": "equal_contracts", "roll": "ratchet"})
    with pytest.raises(ValueError, match="ratchet.*equal_dollars"):
        make_sleeve("vix_call", _ladder_chain(), cfg)


def test_ratchet_cold_start_deploys_the_budget_like_rebalance():
    """With no position to carry, the ratchet funds the full ladder to ~target — the
    entry gate's *buy* job is unchanged (it is rebalance's max() against zero units)."""
    rat, _ = _sleeve(_ladder_chain(), roll="ratchet"), None
    dep = rat.roll_and_fund(_DATE, _PX, _TARGET)
    rat.mark(_DATE, _PX)
    assert dep == pytest.approx(_TARGET, rel=1e-9)
    assert rat.value() == pytest.approx(_TARGET, rel=1e-9)


def test_ratchet_holds_the_spiked_ladder_through_a_zero_target_roll():
    """The core decoupling: at a ``target=0`` roll (forward-VIX >50) the ratchet buys
    nothing (gate) but *keeps* the appreciated survivors — where rebalance dumps them."""
    px = {"VIX": 18.0}
    d0, d1 = pd.Timestamp("2015-06-01"), pd.Timestamp("2015-07-01")
    chain = _roll_chain(d1_mid=9.0)  # survivors mark 1.4/1.8 -> 9.0 at the roll (a spike)

    rat = _sleeve(chain, roll="ratchet")
    rat.roll_and_fund(d0, px, 3000.0)
    rat.mark(d1, px)                       # engine marks (to the spike) before rolling
    u_before = {(r.tenor, r.strike): rat._units_of(r) for r in rat.rungs if (r.tenor - d1).days > 0}
    held = rat.value()                     # spiked sleeve value entering the roll
    dep = rat.roll_and_fund(d1, px, 0.0)   # regime-3 cliff target

    survivors = [r for r in rat.rungs if (r.tenor, r.strike) in u_before]
    new = [r for r in rat.rungs if (r.tenor, r.strike) not in u_before]
    assert len(survivors) == 2 and pd.Timestamp("2015-07-01") not in {r.tenor for r in rat.rungs}
    for r in survivors:                    # survivors' contracts are untouched — not dumped
        assert rat._units_of(r) == pytest.approx(u_before[(r.tenor, r.strike)])
    assert all(rat._units_of(r) == 0.0 for r in new)  # nothing bought at the gate
    assert rat.value() == pytest.approx(held, rel=1e-9)  # full spiked value held
    assert dep == pytest.approx(held, rel=1e-9)          # frictionless: deployed == value held

    # the contrast: rebalance liquidates the same spiked ladder at the same zero-target roll
    reb = _sleeve(chain, roll="rebalance")
    reb.roll_and_fund(d0, px, 3000.0)
    reb.mark(d1, px)
    reb.roll_and_fund(d1, px, 0.0)
    assert reb.value() == pytest.approx(0.0)  # the cliff: everything banked into the base


def test_ratchet_never_reduces_a_rungs_contract_count():
    """A roll with a positive budget tops up decayers but never trims winners: every
    surviving rung's unit count is >= what it was (one-sided rebalance)."""
    px = {"VIX": 18.0}
    d0, d1 = pd.Timestamp("2015-06-01"), pd.Timestamp("2015-07-01")
    rat = _sleeve(_roll_chain(d1_mid=5.0), roll="ratchet")  # survivors appreciate 1.4/1.8 -> 5.0
    rat.roll_and_fund(d0, px, 3000.0)
    rat.mark(d1, px)
    u_before = {(r.tenor, r.strike): rat._units_of(r) for r in rat.rungs if (r.tenor - d1).days > 0}
    rat.roll_and_fund(d1, px, 3000.0)      # a normal, positive-budget roll
    for r in rat.rungs:
        key = (r.tenor, r.strike)
        if key in u_before:
            assert rat._units_of(r) >= u_before[key] - 1e-12  # winners not trimmed


# --- monetize_budget: the explicit sleeve-level exit ------------------------


def _funded_sleeve(**params):
    """A 3-rung sleeve funded to _TARGET (per-rung units), marked, _budget_hw reset."""
    s = _sleeve(_ladder_chain(), roll="ratchet", **params)
    s.roll_and_fund(_DATE, _PX, _TARGET)
    s.mark(_DATE, _PX)
    return s


def test_monetize_budget_resets_high_water_at_each_roll():
    s = _funded_sleeve(monetize_budget={3.0: 1.0})
    assert s._budget_hw == 0.0  # a roll re-bases the budget


def test_monetize_budget_is_a_pure_transfer_at_the_first_crossing():
    """At value = 3× cost basis with ``{3: 0.5}``, exactly half the sleeve is banked into
    the base and half stays held — wealth is conserved (a transfer, not a creation)."""
    s = _funded_sleeve(monetize_budget={3.0: 0.5})
    for r in s.rungs:
        r.value = r.entry * 3.0  # whole sleeve at 3× its cost basis
    held_before = s.value()
    base = _Base(weights={"SPX": 1.0})
    base.shares["SPX"] = 100.0
    px = {"SPX": 50.0, "VIX": 30.0}
    base_before = base.value(px)
    s.monetize(_DATE, px, base)
    banked = base.value(px) - base_before
    assert banked == pytest.approx(0.5 * held_before, rel=1e-12)        # sold half the sleeve
    assert banked + s.value() == pytest.approx(held_before, rel=1e-12)  # pure transfer


def test_monetize_budget_is_irreversible_within_a_cycle():
    """Once a threshold has banked, holding at the same multiple does not re-sell (the
    high-water makes it monotone) — only a *new* high in value/cost banks more."""
    s = _funded_sleeve(monetize_budget={3.0: 0.5})
    for r in s.rungs:
        r.value = r.entry * 3.0
    base = _Base(weights={"SPX": 1.0})
    base.shares["SPX"] = 100.0
    px = {"SPX": 50.0, "VIX": 30.0}
    s.monetize(_DATE, px, base)
    after_first = s.value()
    s.monetize(_DATE, px, base)            # same marks, same multiple -> no further sale
    assert s.value() == pytest.approx(after_first, rel=1e-12)
    # a fresh high (5×) crosses the next rung of the schedule and banks again
    s2 = _funded_sleeve(monetize_budget={3.0: 0.5, 5.0: 1.0})
    for r in s2.rungs:
        r.value = r.entry * 3.0
    s2.monetize(_DATE, px, base)
    assert s2.value() > 0.0
    for r in s2.rungs:
        r.value = r.entry * 5.0            # new high-water -> liquidate the remainder
    s2.monetize(_DATE, px, base)
    assert s2.value() == pytest.approx(0.0, abs=1e-9)


def test_monetize_budget_charges_the_spread_on_proceeds():
    from vix_hedge.vxth.costs import DEFAULT_RATES, ProportionalCost

    cfg = HedgeConfig(0.30, (30, 60, 90), instrument="vix_call",
                      params={"sizing": "equal_dollars", "roll": "ratchet", "monetize_budget": {3.0: 0.5}})
    s = make_sleeve("vix_call", _ladder_chain(), cfg, cost_model=ProportionalCost())
    s.roll_and_fund(_DATE, _PX, _TARGET)
    s.mark(_DATE, _PX)
    for r in s.rungs:
        r.value = r.entry * 3.0
    sold = 0.5 * s.value()
    base = _Base(weights={"SPX": 1.0})
    base.shares["SPX"] = 100.0
    px = {"SPX": 50.0, "VIX": 30.0}
    before = base.value(px)
    s.monetize(_DATE, px, base)
    banked = base.value(px) - before
    assert banked == pytest.approx(sold - DEFAULT_RATES["vix_call"] * sold, rel=1e-9)  # net of half-spread


# --- finding (data-dependent): banking survives by holding, not by a threshold ----

_HAS_DATA = config.VIX_OPTIONS_PARQUET.exists() and config.SPOT_PARQUET.exists()


@pytest.mark.skipif(not _HAS_DATA, reason="VIX option cache not built")
def test_holding_a_gated_hedge_beats_the_cliff_on_robustness():
    """End-to-end on the real VIX chain: keeping the entry gate and *holding* (ratchet,
    no forced exit) captures COVID/GFC with a higher Sharpe and a positive retention on
    BOTH crash shapes, while an explicit *threshold* exit banks too early. Pins the
    headline of ``scripts/run_decoupled_gate.py`` so it cannot silently regress."""
    from vix_hedge import data, metrics
    from vix_hedge.vxth import BASES
    from vix_hedge.vxth import episodes as ep
    from vix_hedge.vxth.backtest import forward_vix_signal
    from vix_hedge.vxth.engine import simulate

    spot = data.load_spot_prices()
    chain = data.load_vix_chain()
    sig = forward_vix_signal(chain, spot, source="hybrid_vx1")

    def run(**params):
        cfg = HedgeConfig(0.30, (30, 60, 90), ALLOC_OFFICIAL, params=params)
        return simulate(spot, chain, base=BASES["SPX"], cfg=cfg, start="2006-03-22", signal_series=sig)

    cliff = run()                                              # rebalance + alloc[3]=0 cliff
    hold = run(roll="ratchet")                                 # gate only, no forced exit
    thresh = run(roll="ratchet", monetize_budget={3.0: 0.5, 5.0: 0.5, 8.0: 1.0})

    def ret(c, win):
        return ep.window_return(c, *win)

    covid = ("2020-02-19", "2020-08-31")
    gfc = ("2008-09-02", "2009-09-30")

    # holding a gated hedge is the better-risk-adjusted, crash-shape-robust book
    assert metrics.annualized_sharpe(hold) > metrics.annualized_sharpe(cliff)
    assert metrics.max_drawdown(hold) < metrics.max_drawdown(cliff)  # smaller magnitude = shallower DD
    # the spike is captured by *holding* — strong positive retention on BOTH shapes
    assert ret(hold, covid) > 0.30
    assert ret(hold, gfc) > 0.10
    # the cliff over-fits the sharp V (huge COVID) but loses the prolonged rout to holding
    assert ret(cliff, covid) > ret(hold, covid)
    assert ret(hold, gfc) > ret(cliff, gfc)
    # an explicit threshold exit banks too early — far below simply holding (the same banks-too-early effect)
    assert ret(thresh, covid) < ret(hold, covid)
    assert ret(thresh, gfc) < ret(hold, gfc)
