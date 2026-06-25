"""Soft forward-VIX gates: hermetic unit checks (no data cache needed)."""

from __future__ import annotations

import numpy as np
import pytest

from vix_hedge.vxth.backtest import regime_of
from vix_hedge.vxth.engine import ALLOC_OFFICIAL, ALLOC_REVERSED
from vix_hedge.vxth.gate import HardGate, HysteresisGate, RampGate, make_gate


def test_hard_gate_matches_regime_schedule():
    # The default gate must be bit-for-bit alloc[regime_of(F)] (preserves all numbers).
    gate = make_gate(ALLOC_OFFICIAL)
    assert isinstance(gate, HardGate)
    for f in np.arange(0.0, 70.0, 0.25):
        assert gate.weight(float(f)) == ALLOC_OFFICIAL[regime_of(float(f))]
    # explicit {"mode": "hard"} is the same
    assert make_gate(ALLOC_OFFICIAL, {"mode": "hard"}).weight(20.0) == 0.010


def test_ramp_matches_proposal_formula():
    # w = full * clip((F-13)/4, 0, 1) on [0, 30]; full = alloc[1] = 1%.
    gate = make_gate(ALLOC_OFFICIAL, {"mode": "ramp"})
    assert isinstance(gate, RampGate)
    for f in np.arange(0.0, 30.01, 0.25):
        want = 0.010 * np.clip((f - 13.0) / 4.0, 0.0, 1.0)
        assert gate.weight(float(f)) == pytest.approx(want)
    # cliff is gone: a 14<->16 wobble no longer flips 0<->full
    assert 0.0 < gate.weight(14.0) < gate.weight(16.0) < 0.010
    # upper bands stay as steps (deliberate "vol too high" rolloff)
    assert gate.weight(31.0) == 0.005
    assert gate.weight(51.0) == 0.0


def test_ramp_is_monotone_nondecreasing_through_gate():
    gate = make_gate(ALLOC_OFFICIAL, {"mode": "ramp"})
    ws = [gate.weight(float(f)) for f in np.arange(10.0, 30.0, 0.5)]
    assert all(b >= a - 1e-12 for a, b in zip(ws, ws[1:], strict=False))


def test_hysteresis_schmitt_trigger():
    gate = make_gate(ALLOC_OFFICIAL, {"mode": "hysteresis"})
    assert isinstance(gate, HysteresisGate)
    # seeded OFF from a low first read; stays off across the dead band until >= 16
    assert gate.weight(10.0) == 0.0
    assert gate.weight(15.9) == 0.0  # within dead band, still off
    assert gate.weight(16.0) == 0.010  # crosses on threshold -> on
    # holds on through the dead band down to off_at; a 14<->16 wobble does NOT round-trip
    assert gate.weight(14.0) == 0.010
    assert gate.weight(13.6) == 0.010
    assert gate.weight(13.5) == 0.0  # off threshold (inclusive) -> off
    assert gate.weight(15.0) == 0.0  # needs >= 16 again, dead band keeps it off
    assert gate.weight(16.0) == 0.010


def test_hysteresis_seed_matches_hard_gate_on_first_read():
    # First read above 15 should start ON (so t0 matches the hard schedule).
    on = make_gate(ALLOC_OFFICIAL, {"mode": "hysteresis"})
    assert on.weight(20.0) == 0.010
    off = make_gate(ALLOC_OFFICIAL, {"mode": "hysteresis"})
    assert off.weight(12.0) == 0.0


def test_hysteresis_upper_bands_apply_when_on():
    gate = make_gate(ALLOC_OFFICIAL, {"mode": "hysteresis"})
    gate.weight(20.0)  # turn on
    assert gate.weight(40.0) == 0.005  # 30-50 half weight
    assert gate.weight(60.0) == 0.0  # >50 stand down even while "on"


def test_soft_gates_take_weights_from_alloc():
    # ALLOC_REVERSED swaps full/mid (0.5% in-gate, 1% in 30-50).
    ramp = make_gate(ALLOC_REVERSED, {"mode": "ramp"})
    assert ramp.weight(20.0) == pytest.approx(0.005)
    assert ramp.weight(40.0) == pytest.approx(0.010)
    hyst = make_gate(ALLOC_REVERSED, {"mode": "hysteresis"})
    hyst.weight(20.0)
    assert hyst.weight(20.0) == 0.005
    assert hyst.weight(40.0) == 0.010


def test_gate_param_overrides():
    ramp = make_gate(ALLOC_OFFICIAL, {"mode": "ramp", "lo": 12.0, "hi": 20.0})
    assert ramp.weight(12.0) == 0.0
    assert ramp.weight(16.0) == pytest.approx(0.010 * 0.5)
    hyst = make_gate(ALLOC_OFFICIAL, {"mode": "hysteresis", "on_at": 18.0, "off_at": 12.0})
    assert hyst.weight(11.0) == 0.0  # seed clearly off
    assert hyst.weight(17.0) == 0.0  # dead band [12, 18] holds off (below custom on_at)
    assert hyst.weight(18.0) == 0.010  # crosses custom on_at


def test_unknown_mode_raises():
    with pytest.raises(ValueError, match="unknown gate mode"):
        make_gate(ALLOC_OFFICIAL, {"mode": "sigmoid"})
