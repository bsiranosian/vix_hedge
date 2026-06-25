"""Ladder sizing for the VIX-call sleeve: equal contracts vs equal dollars.

``params["sizing"]`` controls how a roll splits the regime's target dollars across
the ladder's rungs. The default ``"equal_contracts"`` gives every tenor the same
contract count (so its *dollar* weight is its premium — long-dated rungs carry
more); ``"equal_dollars"`` gives every priced tenor ``target/n`` dollars (so the
cheaper near-dated rungs buy more contracts). Both deploy ~target in total, and
the two coincide for a single-rung config. All hermetic — one synthetic chain.
"""

from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

from vix_hedge.data.load import OptionChain
from vix_hedge.vxth.engine import HedgeConfig
from vix_hedge.vxth.sleeves import make_sleeve

# A 30/60/90 ladder at one delta: one 30Δ call per tenor, premium rising with tenor.
_TENORS = [(30, "2015-07-01", 1.0), (60, "2015-07-31", 1.6), (90, "2015-08-30", 2.1)]
_DATE = pd.Timestamp("2015-06-01")
_PX = {"VIX": 18.0}
_TARGET = 3000.0


def _ladder_chain() -> OptionChain:
    rows = []
    for dte, ex, mid in _TENORS:
        rows.append({"date": "2015-06-01", "exdate": ex, "cp_flag": "C", "strike": 20.0,
                     "mid": mid, "delta": 0.30, "dte": dte})
    df = pd.DataFrame(rows)
    df["forward"] = np.nan
    df["date"] = pd.to_datetime(df["date"])
    df["exdate"] = pd.to_datetime(df["exdate"])
    return OptionChain(df)


def _sleeve(sizing: str | None, ladder=(30, 60, 90)):
    params = {"sizing": sizing} if sizing else {}
    cfg = HedgeConfig(0.30, ladder, instrument="vix_call", params=params)
    s = make_sleeve("vix_call", _ladder_chain(), cfg)
    deployed = s.roll_and_fund(_DATE, _PX, _TARGET)
    s.mark(_DATE, _PX)
    return s, deployed


def _rung_dollars(s) -> dict[float, float]:
    """Strike-keyed (here tenor-keyed) dollar weight of each rung after the roll."""
    return {r.tenor: s._units_of(r) * r.value for r in s.rungs}


def test_both_modes_deploy_the_target_frictionless():
    for sizing in ("equal_contracts", "equal_dollars"):
        s, deployed = _sleeve(sizing)
        assert deployed == pytest.approx(_TARGET, rel=1e-9), sizing
        assert s.value() == pytest.approx(_TARGET, rel=1e-9), sizing  # frictionless: deployed == value


def test_equal_contracts_weights_by_premium():
    """Default: a single shared unit count, so each tenor's dollars ∝ its mid."""
    s, _ = _sleeve("equal_contracts")
    mids = {pd.Timestamp(ex): m for _, ex, m in _TENORS}
    sum_mid = sum(m for *_, m in _TENORS)
    dollars = _rung_dollars(s)
    for tenor, premium in mids.items():
        assert dollars[tenor] == pytest.approx(_TARGET * premium / sum_mid, rel=1e-9)
    # every rung carries the SAME contract count.
    units = {r.tenor: s._units_of(r) for r in s.rungs}
    assert len(set(round(u, 6) for u in units.values())) == 1


def test_equal_dollars_splits_evenly_and_buys_more_of_the_cheap_rung():
    s, _ = _sleeve("equal_dollars")
    dollars = _rung_dollars(s)
    for v in dollars.values():  # each tenor gets target / n_rungs
        assert v == pytest.approx(_TARGET / len(_TENORS), rel=1e-9)
    by_tenor = {r.tenor: s._units_of(r) for r in s.rungs}
    cheap = pd.Timestamp("2015-07-01")   # 30d, mid 1.0
    rich = pd.Timestamp("2015-08-30")    # 90d, mid 2.1
    assert by_tenor[cheap] > by_tenor[rich]  # cheaper rung -> more contracts
    assert by_tenor[cheap] == pytest.approx((_TARGET / 3) / 1.0, rel=1e-9)
    assert by_tenor[rich] == pytest.approx((_TARGET / 3) / 2.1, rel=1e-9)


def test_single_rung_is_identical_under_both_modes():
    ec, dep_ec = _sleeve("equal_contracts", ladder=(30,))
    ed, dep_ed = _sleeve("equal_dollars", ladder=(30,))
    assert dep_ec == pytest.approx(dep_ed, rel=1e-12)
    assert ec.value() == pytest.approx(ed.value(), rel=1e-12)


def test_default_is_equal_dollars():
    """No sizing param -> equal-dollars (the engine default, the user's intended method)."""
    s_default, dep_default = _sleeve(None)
    s_explicit, dep_explicit = _sleeve("equal_dollars")
    assert dep_default == pytest.approx(dep_explicit, rel=1e-12)
    assert _rung_dollars(s_default) == pytest.approx(_rung_dollars(s_explicit))
    assert s_default._equal_dollars


def test_unknown_sizing_raises():
    with pytest.raises(ValueError, match="unknown sizing"):
        _sleeve("equal_dollarz")


def test_add_only_requires_equal_dollars():
    cfg = HedgeConfig(0.30, (30, 60, 90), instrument="vix_call",
                      params={"sizing": "equal_contracts", "roll": "add_only"})
    with pytest.raises(ValueError, match="add_only.*equal_dollars"):
        make_sleeve("vix_call", _ladder_chain(), cfg)


# --- add_only roll: a two-date chain so the front rung actually expires and rolls ---

_GRID = [(20.0, 0.50), (21.0, 0.40), (22.0, 0.30), (23.0, 0.20), (24.0, 0.10)]  # 30Δ -> K22 at every tenor


def _roll_chain() -> OptionChain:
    """d0 with 30/60/90 expiries; d1 (=the 30d expiry) with the two survivors re-listed
    plus a new ~90d expiry. Constant mid per (date, expiry) so K22 (the 30Δ pick) prices."""
    rows = []
    for date, tenors in [
        ("2015-06-01", [("2015-07-01", 1.0), ("2015-07-31", 1.4), ("2015-08-30", 1.8)]),
        ("2015-07-01", [("2015-07-31", 0.9), ("2015-08-30", 1.3), ("2015-09-29", 1.7)]),
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


def test_add_only_keeps_survivors_and_buys_only_the_new_rung():
    cfg = HedgeConfig(0.30, (30, 60, 90), instrument="vix_call",
                      params={"sizing": "equal_dollars", "roll": "add_only"})
    s = make_sleeve("vix_call", _roll_chain(), cfg)
    px = {"VIX": 18.0}
    d0, d1 = pd.Timestamp("2015-06-01"), pd.Timestamp("2015-07-01")

    s.roll_and_fund(d0, px, 3000.0)
    s.mark(d0, px)
    assert len(s.rungs) == 3 and s.value() == pytest.approx(3000.0, rel=1e-9)  # initial: 3 fresh thirds
    u0 = {(r.tenor, r.strike): s._units_of(r) for r in s.rungs}

    s.mark(d1, px)                       # engine marks before rolling
    dep = s.roll_and_fund(d1, px, 3000.0)

    assert pd.Timestamp("2015-07-01") not in {r.tenor for r in s.rungs}  # the 30d expired & dropped
    survivors = [r for r in s.rungs if (r.tenor, r.strike) in u0]
    new = [r for r in s.rungs if (r.tenor, r.strike) not in u0]
    assert len(survivors) == 2 and len(new) == 1
    for r in survivors:  # survivors keep their EXACT contracts — no re-strike, no extra turnover
        assert s._units_of(r) == pytest.approx(u0[(r.tenor, r.strike)])
    assert s._units_of(new[0]) * new[0].value == pytest.approx(3000.0 / 3, rel=1e-9)  # 1%/3 in the new rung
    assert dep == pytest.approx(s.value(), rel=1e-9)              # frictionless: deployed == value
    assert (pd.Timestamp("2015-07-01"), 22.0) not in s._units    # the expired rung's unit key is pruned


def test_add_only_charges_spread_only_on_the_new_rung():
    """Turnover: the half-spread at a roll is paid on the new rung's premium (~target/3),
    not on the carried survivors — the whole point of the low-turnover policy."""
    from vix_hedge.vxth.costs import DEFAULT_RATES, ProportionalCost

    cfg = HedgeConfig(0.30, (30, 60, 90), instrument="vix_call",
                      params={"sizing": "equal_dollars", "roll": "add_only"})
    s = make_sleeve("vix_call", _roll_chain(), cfg, cost_model=ProportionalCost())
    px = {"VIX": 18.0}
    s.roll_and_fund(pd.Timestamp("2015-06-01"), px, 3000.0)
    s.mark(pd.Timestamp("2015-07-01"), px)
    dep = s.roll_and_fund(pd.Timestamp("2015-07-01"), px, 3000.0)
    cost = dep - s.value()  # the spread that left the portfolio at this roll
    assert cost == pytest.approx(DEFAULT_RATES["vix_call"] * (3000.0 / 3), rel=1e-9)  # only the new third


def test_equal_dollars_monetize_schedule_conserves_wealth():
    """equal-dollars now composes with the partial-monetization schedule (per-rung
    units via ``apply_schedule(units_of=...)``). On the first crossing every rung still
    holds its full position, so the sold proceeds banked into the base plus the value
    still held must equal what the sleeve held entering the day — a pure transfer that
    also exercises the per-rung unit counts (the deposit is ``units_i·sold·value_i``)."""
    from vix_hedge.vxth.sleeves import _Base

    cfg = HedgeConfig(0.30, (30, 60, 90), instrument="vix_call",
                      params={"sizing": "equal_dollars", "monetize_schedule": {3.0: 0.5}})
    s = make_sleeve("vix_call", _ladder_chain(), cfg)
    s.roll_and_fund(_DATE, _PX, _TARGET)  # per-rung units; entries = mids 1.0/1.6/2.1 -> unequal counts
    s.mark(_DATE, _PX)
    base = _Base(weights={"SPX": 1.0})
    base.shares["SPX"] = 100.0
    px = {"SPX": 50.0, "VIX": 30.0}
    for r in s.rungs:
        r.value = r.entry * 3.0  # every rung crosses the 3× threshold this day
    held_before = s.value()  # full position, nothing sold yet
    base_before = base.value(px)
    s.monetize(_DATE, px, base)
    banked = base.value(px) - base_before
    assert banked + s.value() == pytest.approx(held_before, rel=1e-12)  # pure transfer
    assert banked == pytest.approx(0.5 * held_before, rel=1e-9)  # sold half of every rung
    assert all(r.value == pytest.approx(r.entry * 3.0 * 0.5) for r in s.rungs)  # remaining half held
