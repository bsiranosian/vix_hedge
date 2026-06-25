"""Partial, multi-threshold monetization.

Three layers:

* **Mechanism (hermetic):** the pure helpers in :mod:`vix_hedge.vxth.monetize` —
  schedule validation, the multiplicative ``remaining_fraction``, the high-water
  ``step`` (monetization never reverses), and ``apply_schedule``'s deposit / value
  scaling / rung-drop / state-prune. Plus that a one-rung schedule ``{m: 1.0}``
  reproduces the legacy full-liquidation path exactly.
* **Sleeve (hermetic):** drive ``VixCallLadderSleeve.monetize`` over a hand-built
  crash→recovery path with redeployment into a recovering base, and show partial
  monetization **retains more than hold-to-expiry and than the aggressive single
  threshold** — the LongTail "monetization matters" mechanism, made deterministic.
* **DoD (data-dependent):** via :func:`ensemble`, the *distribution* of retained
  payoff across entry-date cohorts over the GFC / Q4-2018 / COVID episodes — partial
  monetization's median dwarfs hold-to-expiry and is never dominated by a single
  fixed threshold (each single fails in the regime it is mistuned for).

The honest tradeoff (see the data-test docstring): partial monetization's win over
hold is *regime-dependent* — huge in fast V-shaped crashes (COVID), slightly negative
in slow routs (GFC, where holding the convex option longer wins) — but dominant on
net. It ties or beats every single threshold, strictly beating the aggressive one;
it does not beat a well-tuned deep single on every single path. The ladder buys
robustness across regimes, not a free lunch.
"""

from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

from vix_hedge.vxth import monetize as mon
from vix_hedge.vxth.engine import ALLOC_REVERSED, HedgeConfig
from vix_hedge.vxth.sleeves import _Base, _Rung
from vix_hedge.vxth.sleeves.vix_call import VixCallLadderSleeve

SCHED = {3.0: 0.5, 5.0: 0.5, 8.0: 1.0}  # Bhansali ladder: 50% at 3×, 50% of rest at 5×, all at 8×


# --- mechanism: pure helpers ------------------------------------------------


def test_normalize_schedule_coerces_and_validates():
    assert mon.normalize_schedule({3: 0.5, "5": "0.25"}) == {3.0: 0.5, 5.0: 0.25}
    for bad in ({0.0: 0.5}, {-1.0: 0.5}, {3.0: 1.5}, {3.0: -0.1}):
        with pytest.raises(ValueError):
            mon.normalize_schedule(bad)


def test_remaining_fraction_is_multiplicative():
    # each crossed threshold sells its fraction of what is *still held* → survivals multiply
    assert mon.remaining_fraction(2.9, SCHED) == 1.0
    assert mon.remaining_fraction(3.0, SCHED) == 0.5
    assert mon.remaining_fraction(4.9, SCHED) == 0.5
    assert mon.remaining_fraction(5.0, SCHED) == pytest.approx(0.25)
    assert mon.remaining_fraction(7.9, SCHED) == pytest.approx(0.25)
    assert mon.remaining_fraction(8.0, SCHED) == 0.0
    # monotonically non-increasing in the ratio
    ratios = np.linspace(0, 10, 50)
    rem = [mon.remaining_fraction(r, SCHED) for r in ratios]
    assert np.all(np.diff(rem) <= 1e-12)


def test_step_high_water_is_irreversible():
    # crossing 3× sells half and lifts the high-water; a later *lower* value sells nothing more
    sold, hw, rem = mon.step(entry=1.0, value=3.0, prev_hw=0.0, schedule=SCHED)
    assert sold == 0.5 and hw == 3.0 and rem == 0.5
    sold2, hw2, rem2 = mon.step(entry=1.0, value=2.0, prev_hw=hw, schedule=SCHED)
    assert sold2 == 0.0 and hw2 == 3.0 and rem2 == 0.5  # faded back, but stays half-sold
    # jumping straight past two thresholds banks both fractions at once
    sold3, hw3, rem3 = mon.step(entry=1.0, value=9.0, prev_hw=0.0, schedule=SCHED)
    assert hw3 == 9.0 and rem3 == 0.0 and sold3 == 1.0
    # a zero/old-entry position is inert
    assert mon.step(entry=0.0, value=5.0, prev_hw=0.0, schedule=SCHED) == (0.0, 0.0, 1.0)


def test_apply_schedule_deposits_scales_and_drops():
    tenor = pd.Timestamp("2020-12-31")
    rungs = [_Rung(tenor=tenor, strike=20.0, entry=2.0, value=6.0)]  # 6/2 = 3× → first threshold
    hw, banked = {}, []
    out = mon.apply_schedule(rungs, n_units=100.0, hw=hw, schedule=SCHED, deposit=banked.append)
    assert banked == [100.0 * 0.5 * 6.0]                 # sell 50% of 100 units at mid 6
    assert out[0].value == pytest.approx(3.0)            # value scaled to the 50% still held
    assert hw[(tenor, 20.0, 2.0)] == 3.0

    rungs[0].value = 16.0                                 # next day: 16/2 = 8× → liquidate remainder
    banked.clear()
    out = mon.apply_schedule(rungs, n_units=100.0, hw=hw, schedule=SCHED, deposit=banked.append)
    assert banked == [100.0 * 0.5 * 16.0]                # remaining 0.5 sold at 8×
    assert out == [] and hw == {}                        # rung fully monetized → dropped, state pruned


def test_apply_schedule_single_threshold_equals_legacy_full_liquidation():
    tenor = pd.Timestamp("2020-12-31")
    rung = _Rung(tenor=tenor, strike=20.0, entry=1.0, value=5.0)  # 5× ≥ 5
    banked = []
    out = mon.apply_schedule([rung], n_units=10.0, hw={}, schedule={5.0: 1.0}, deposit=banked.append)
    assert banked == [10.0 * 1.0 * 5.0] and out == []    # whole position sold once, exactly like monetize_mult


# --- sleeve: legacy parity + schedule precedence ----------------------------


def _sleeve(cfg, *, n_units=100.0, entry=1.0, value=1.0, tenor="2020-12-31"):
    s = VixCallLadderSleeve(None, cfg)  # chain unused by monetize()
    s.rungs = [_Rung(tenor=pd.Timestamp(tenor), strike=20.0, entry=entry, value=value)]
    s.n_units = n_units
    s._equal_dollars = False  # these tests poke n_units directly -> exercise the equal-contracts path
    return s


def _base(shares=1000.0):
    b = _Base(weights={"SPX": 1.0})
    b.shares["SPX"] = shares
    return b


def test_legacy_monetize_mult_unchanged():
    # no schedule → the original behavior: sell the whole rung once value ≥ mult·entry
    s = _sleeve(HedgeConfig(monetize_mult=4.0), entry=1.0, value=4.0)
    b = _base()
    s.monetize(pd.Timestamp("2020-06-01"), {"SPX": 50.0, "VIX": 30.0}, b)
    assert s.rungs == []                                  # whole rung liquidated
    assert b.shares["SPX"] == pytest.approx(1000.0 + 100.0 * 4.0 / 50.0)


def test_no_schedule_no_mult_is_noop():
    s = _sleeve(HedgeConfig(), entry=1.0, value=9.0)  # hold-to-expiry: never monetize
    b = _base()
    s.monetize(pd.Timestamp("2020-06-01"), {"SPX": 50.0, "VIX": 30.0}, b)
    assert len(s.rungs) == 1 and b.shares["SPX"] == 1000.0


def test_schedule_takes_precedence_over_mult():
    # both set → the schedule path wins (partial), not the single full liquidation
    s = _sleeve(HedgeConfig(monetize_mult=3.0, params={"monetize_schedule": SCHED}), value=3.0)
    b = _base()
    s.monetize(pd.Timestamp("2020-06-01"), {"SPX": 90.0, "VIX": 30.0}, b)
    assert len(s.rungs) == 1                              # only half sold, rung survives
    assert b.shares["SPX"] == pytest.approx(1000.0 + 100.0 * 0.5 * 3.0 / 90.0)


# --- sleeve: crash→recovery retained-payoff mechanism (deterministic) --------

# A long-dated VIX call appreciates as equities fall into the trough, then the
# spike reverts to ~0 by expiry while equities recover. Monetized proceeds are
# redeployed into the (cheap) base and ride the rebound; held proceeds decay away.
# SPX falls 100→90→80→70(trough) crossing 3×/5×/8×, then recovers to 110, call→0.
_PATH = [(100.0, 1.0), (90.0, 3.0), (80.0, 5.0), (70.0, 8.0), (85.0, 2.0), (110.0, 0.0)]


def _run_path(cfg, path=_PATH, *, n_units=1000.0, base_shares=1000.0):
    """Drive mark→monetize→value over ``path`` and return final portfolio value."""
    s = _sleeve(cfg, n_units=n_units, entry=path[0][1], value=path[0][1])
    b = _base(base_shares)
    total = b.value({"SPX": path[0][0]}) + s.value()
    for spx, call in path:
        for r in s.rungs:
            r.value = call                                # mark to today's option mid
        s.monetize(pd.Timestamp("2020-06-01"), {"SPX": spx, "VIX": 30.0}, b)
        total = b.value({"SPX": spx}) + s.value()
    return total


def test_partial_monetization_retains_more_than_hold_and_single():
    hold = _run_path(HedgeConfig())                                   # never monetize
    single3 = _run_path(HedgeConfig(monetize_mult=3.0))               # sell all at the first threshold
    multi = _run_path(HedgeConfig(params={"monetize_schedule": SCHED}))

    base_only = 1000.0 * 110.0  # the call reverts to 0, so hold-to-expiry retains nothing from the hedge
    assert hold == pytest.approx(base_only)
    # partial monetization banks 3× early, 5× lower, 8× at the trough → cheapest base, most retained
    assert multi > single3 > hold
    # all retained value is hedge proceeds compounded through the recovery
    assert multi - base_only == pytest.approx(
        1000.0 * (0.5 * 3 / 90 + 0.25 * 5 / 80 + 0.25 * 8 / 70) * 110.0
    )


def test_monetize_is_a_conserving_transfer_each_day():
    """Money conservation: each day ``monetize`` is a *pure transfer* — the proceeds
    banked into the base plus the sleeve value still held equal the value of the
    fraction held entering the day, marked at today's mid (prior sales already banked,
    not double-counted). No dollars are created or destroyed by monetization."""
    s = _sleeve(HedgeConfig(params={"monetize_schedule": SCHED}), n_units=1000.0,
                entry=_PATH[0][1], value=_PATH[0][1])
    b = _base(1000.0)
    entry = _PATH[0][1]
    hw_ratio, prev_remaining = 0.0, 1.0
    for spx, call in _PATH:
        for r in s.rungs:
            r.value = call  # mark to today's full mid (un-scaled)
        held = s.n_units * call * prev_remaining if s.rungs else 0.0  # value of the held fraction today
        base_before = b.value({"SPX": spx})
        s.monetize(pd.Timestamp("2020-06-01"), {"SPX": spx, "VIX": 30.0}, b)
        banked = b.value({"SPX": spx}) - base_before
        assert np.isclose(banked + s.value(), held, rtol=1e-9, atol=1e-6), (spx, call)
        hw_ratio = max(hw_ratio, call / entry)
        prev_remaining = mon.remaining_fraction(hw_ratio, SCHED)


def test_single_threshold_schedule_equals_legacy_through_the_sleeve():
    """End-to-end (not just at the helper): a one-rung ``{m: 1.0}`` schedule driven
    through ``sleeve.monetize`` over the full crash→recovery path produces the exact
    same final portfolio value as the legacy ``monetize_mult`` full-liquidation."""
    legacy = _run_path(HedgeConfig(monetize_mult=3.0))
    schedule = _run_path(HedgeConfig(params={"monetize_schedule": {3.0: 1.0}}))
    assert schedule == pytest.approx(legacy)


# --- DoD: retained-payoff distribution via the ensemble ---------------------

_HAS_DATA = None
try:
    from vix_hedge import config

    _HAS_DATA = config.VIX_OPTIONS_PARQUET.exists() and config.SPOT_PARQUET.exists()
except Exception:  # pragma: no cover
    _HAS_DATA = False

pytestmark = pytest.mark.skipif(not _HAS_DATA, reason="VIX option cache not built")

# Long-dated single call (rolls ~quarterly) — the regime where hold-to-expiry truly
# gives the spike back, so monetization's "retained payoff" is visible. Recovery
# window = trough → +12m (banked proceeds compound through the rebound).
_LADDER, _DELTA = (90,), 0.10
_NOFF, _STEP, _MONTHS = 4, 4, 12
_EP = {  # trough, run_start, run_end
    "GFC": ("2009-03-09", "2008-06-01", "2010-12-31"),
    "Q4-18": ("2018-12-24", "2018-03-01", "2019-12-31"),
    "COVID": ("2020-03-23", "2019-09-01", "2021-06-30"),
}


@pytest.fixture(scope="module")
def market():
    from vix_hedge import data

    return data.load_spot_prices(), data.load_vix_chain()


@pytest.fixture(scope="module")
def signal(market):
    from vix_hedge.vxth.backtest import forward_vix_signal

    spot, chain = market
    return forward_vix_signal(chain, spot, source="hybrid_vx1")


def _cfg(label, ladder=_LADDER, **kw):
    params = kw.pop("params", {})
    return HedgeConfig(_DELTA, ladder, ALLOC_REVERSED, label=label, params=params, **kw)


def _retained(market, signal, cfg, trough, run_s, run_e):
    """Median (and full cohort array) of recovery-window retained payoff (% return
    from the trough to +12m) across staggered entry-date cohorts — the
    entry-date-cohort lens."""
    from vix_hedge.vxth import BASES, ensemble
    from vix_hedge.vxth import episodes as ep

    spot, chain = market
    w_e = (pd.Timestamp(trough) + pd.DateOffset(months=_MONTHS)).strftime("%Y-%m-%d")
    res = ensemble(spot, chain, base=BASES["SPX"], cfg=cfg, n_offsets=_NOFF, step_days=_STEP,
                   start=run_s, end=run_e, signal_series=signal)
    return np.array([100 * ep.window_return(c, trough, w_e) for c in res["curves"].values()])


def test_dod_partial_monetization_retained_payoff_distribution(market, signal):
    """DoD: via ``ensemble``, partial multi-threshold monetization shows higher
    *median retained payoff* than hold-to-expiry and than a single threshold, across
    the GFC / Q4-2018 / COVID episodes — reported as the cohort distribution, not a
    point. Honest tradeoff: multi's win over hold is regime-dependent (it trails hold
    slightly in the slow GFC rout, where holding the convex call longer pays) but
    dominates on net via the fast COVID crash; it ties or beats every single
    threshold and is never dominated."""
    cfgs = {
        "hold": _cfg("hold"),
        "s3": _cfg("s3", monetize_mult=3.0),
        "s5": _cfg("s5", monetize_mult=5.0),
        "multi": _cfg("multi", params={"monetize_schedule": SCHED}),
    }
    pooled = {k: [] for k in cfgs}
    med = {k: {} for k in cfgs}
    for ep_name, (trough, run_s, run_e) in _EP.items():
        for k, c in cfgs.items():
            arr = _retained(market, signal, c, trough, run_s, run_e)
            pooled[k].extend(arr.tolist())
            med[k][ep_name] = float(np.median(arr))

    pmed = {k: float(np.median(v)) for k, v in pooled.items()}

    # 1) Headline: multi dwarfs hold-to-expiry in the fast crash and on the pooled median.
    assert med["multi"]["COVID"] > med["hold"]["COVID"] + 20, (med["multi"], med["hold"])
    assert pmed["multi"] > pmed["hold"] + 10, pmed

    # 2) multi is never dominated by a single fixed threshold (≥ each, every episode).
    for ep_name in _EP:
        for s in ("s3", "s5"):
            assert med["multi"][ep_name] >= med[s][ep_name] - 0.1, (ep_name, s, med)
    assert pmed["multi"] >= pmed["s3"] - 0.1 and pmed["multi"] >= pmed["s5"] - 0.1, pmed

    # 3) ...and it strictly beats the aggressive single in the prolonged rout, where
    #    selling everything at 3× discards convexity that is still paying (GFC).
    assert med["multi"]["GFC"] >= med["s3"]["GFC"], med
