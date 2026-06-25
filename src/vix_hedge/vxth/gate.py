"""Soft gates for the forward-VIX hedge-weight schedule.

The VXTH engine sizes the hedge each roll by mapping the forward-VIX *level* ``F``
to a weight. The default is a hard 4-level step (:func:`~vix_hedge.vxth.backtest.regime_of`
+ ``alloc[]``): ``0%`` below 15, ``1%`` in 15-30, ``0.5%`` in 30-50, ``0%`` above
50. That ``0 ↔ 1%`` cliff at 15 is read on a single print of a contract that, near
expiry, sawtooths across the boundary: one vol-point of noise flips
the whole sleeve on or off, which (a) makes the COVID payoff ride a knife edge on
*which day* the gate is read and (b) churns full-sleeve round-trips whenever the
forward hovers near 15 (the Dec-19/Jan-20/Feb-20 deploy→unwind→deploy).

This module softens that gate two ways, both **stateful-friendly** (a gate is an
object with a ``weight(F)`` method, called once per roll in date order):

* :class:`HardGate` — the current step schedule, *bit-for-bit* the old behavior
  (``alloc[regime_of(F)]``). The default, so unconfigured runs are unchanged.
* :class:`RampGate` — replace the cliff with a linear ramp
  ``w = full · clip((F − lo) / (hi − lo), 0, 1)`` (defaults ``lo=13, hi=17`` →
  the proposal's ``1% · clip((F−13)/4, 0, 1)``). The 30/50 upper bands (the
  deliberate "vol too high, stand down" rule) stay as steps.
* :class:`HysteresisGate` — a Schmitt trigger on the gate: turn the sleeve **on**
  once ``F ≥ on_at`` (default 16), **off** once ``F ≤ off_at`` (default 13.5),
  and hold the prior state in the 13.5-16 dead band. Above the gate the normal
  30/50 bands apply.

Both soft gates take their weights from the run's ``alloc`` tuple (so ALLOC_OFFICIAL
vs ALLOC_REVERSED still flips full/mid), and the upper band edges from
``REGIME_BOUNDS``. Select one through ``HedgeConfig.params["gate"]`` — see
:func:`make_gate`.
"""

from __future__ import annotations

from dataclasses import dataclass

from vix_hedge.vxth.backtest import REGIME_BOUNDS, regime_of

# Upper-band edges shared by every gate (30 → half weight, 50 → stand down).
_MID_EDGE = REGIME_BOUNDS[2][0]  # 30: above this, half weight
_TOP_EDGE = REGIME_BOUNDS[3][0]  # 50: above this, stand down (blow-off top)


class HardGate:
    """The status-quo step schedule: ``alloc[regime_of(F)]`` (default, exact)."""

    def __init__(self, alloc: tuple[float, ...]):
        self.alloc = alloc

    def weight(self, fwd: float) -> float:
        return self.alloc[regime_of(fwd)]


@dataclass
class RampGate:
    """Linear gate ramp; upper (30/50) bands stay as steps.

    ``full`` / ``mid`` default to the run's ``alloc[1]`` / ``alloc[2]`` so the gate
    saturates to whatever the schedule's in-regime weight is.
    """

    full: float
    mid: float
    lo: float = 13.0
    hi: float = 17.0

    def weight(self, fwd: float) -> float:
        if fwd > _TOP_EDGE:  # > 50: stand down
            return 0.0
        if fwd > _MID_EDGE:  # 30-50: half weight
            return self.mid
        frac = (fwd - self.lo) / (self.hi - self.lo)
        frac = 0.0 if frac < 0.0 else 1.0 if frac > 1.0 else frac
        return self.full * frac


class HysteresisGate:
    """Schmitt-trigger gate (on at ``on_at``, off at ``off_at``); 30/50 bands above.

    Stateful: ``weight`` must be called in chronological order (the engine does).
    The initial state is seeded from the dead-band midpoint (``on`` iff ``F`` is
    above ``(on_at + off_at) / 2``) so the very first read is decided consistently
    with the gate's own thresholds (for the defaults that midpoint ≈ 14.75 ≈ the
    hard gate's 15 boundary).
    """

    def __init__(self, full: float, mid: float, on_at: float = 16.0, off_at: float = 13.5):
        self.full = full
        self.mid = mid
        self.on_at = on_at
        self.off_at = off_at
        self._on: bool | None = None  # seeded on first call

    def weight(self, fwd: float) -> float:
        if self._on is None:
            self._on = fwd > (self.on_at + self.off_at) / 2.0  # seed from dead-band midpoint
        if fwd >= self.on_at:
            self._on = True
        elif fwd <= self.off_at:
            self._on = False
        if not self._on:
            return 0.0
        if fwd > _TOP_EDGE:  # > 50: stand down even when "on"
            return 0.0
        if fwd > _MID_EDGE:  # 30-50: half weight
            return self.mid
        return self.full


def make_gate(alloc: tuple[float, ...], spec: dict | None = None):
    """Build a gate from ``alloc`` and a ``params["gate"]`` spec.

    ``spec=None`` (or ``{"mode": "hard"}``) → :class:`HardGate`, i.e. unchanged.
    ``{"mode": "ramp", "lo": .., "hi": ..}`` → :class:`RampGate`.
    ``{"mode": "hysteresis", "on_at": .., "off_at": ..}`` → :class:`HysteresisGate`.
    ``full``/``mid`` default to ``alloc[1]``/``alloc[2]`` but may be overridden.
    """
    if spec is None:
        return HardGate(alloc)
    spec = dict(spec)
    mode = spec.pop("mode", "hard")
    if mode == "hard":
        return HardGate(alloc)
    full = spec.pop("full", alloc[1])
    mid = spec.pop("mid", alloc[2])
    if mode == "ramp":
        return RampGate(full=full, mid=mid, **spec)
    if mode == "hysteresis":
        return HysteresisGate(full=full, mid=mid, **spec)
    raise ValueError(f"unknown gate mode {mode!r}; have hard/ramp/hysteresis")
