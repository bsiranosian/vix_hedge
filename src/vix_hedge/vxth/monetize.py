"""Bhansali-style partial, multi-threshold monetization.

The original VIX-call sleeve sold a rung **whole** the moment it reached
``monetize_mult``× its entry. This generalizes that to a *schedule*
``{multiple: fraction}`` that sells a fraction of the **remaining** position the
first time the position reaches each multiple, redeploying the proceeds into the
base. The canonical ``{3.0: 0.5, 5.0: 0.5, 8.0: 1.0}`` keeps half at 3×, half of
the rest at 5×, and liquidates the remainder at 8×.

Why partial beats hold-to-expiry: an appreciated option's time-decay penalty is
far larger *after* a volatility spike, so banking part of the gain and redeploying
it into the (depressed) base retains more than riding the option back down
([LongTail Monetization] literature). 8× captures prolonged routs, 3× short
corrections; the ladder spans both. *Single-path evidence — validate across
entry-date cohorts.*

State per position is a single **high-water ratio** (the max value/entry it has
reached). The fraction still held is a deterministic, monotonically *non-increasing*
function of that ratio, so monetization never reverses when the option later fades.
"""

from __future__ import annotations

from collections.abc import Callable, Mapping, Sequence


def normalize_schedule(schedule: Mapping) -> dict[float, float]:
    """Validate/coerce a ``{multiple: fraction}`` schedule to ``{float: float}``."""
    out: dict[float, float] = {}
    for mult, frac in dict(schedule).items():
        m, f = float(mult), float(frac)
        if m <= 0.0:
            raise ValueError(f"monetize multiple must be > 0, got {m}")
        if not 0.0 <= f <= 1.0:
            raise ValueError(f"monetize fraction must be in [0, 1], got {f}")
        out[m] = f
    return out


def remaining_fraction(hw_ratio: float, schedule: Mapping[float, float]) -> float:
    """Fraction of a position still held once its value has reached ``hw_ratio``×
    entry. Each crossed threshold sells its fraction of the *then-remaining* holding,
    so the survival fractions multiply (50% at 3× then 50% at 5× leaves 25%)."""
    remaining = 1.0
    for mult in sorted(schedule):
        if hw_ratio >= mult:
            remaining *= 1.0 - schedule[mult]
    return remaining


def step(entry: float, value: float, prev_hw: float, schedule: Mapping[float, float]) -> tuple[float, float, float]:
    """Advance one position by a day. Returns ``(sold, new_hw, remaining)`` where
    ``sold`` is the fraction of the *original* units to liquidate now (the drop in
    held fraction caused by any thresholds the new high-water ratio just crossed)."""
    if entry <= 0.0:
        return 0.0, prev_hw, remaining_fraction(prev_hw, schedule)
    new_hw = max(prev_hw, value / entry)
    before = remaining_fraction(prev_hw, schedule)
    after = remaining_fraction(new_hw, schedule)
    return before - after, new_hw, after


def apply_schedule(
    rungs: Sequence,
    n_units: float,
    hw: dict,
    schedule: Mapping,
    *,
    deposit: Callable[[float], None],
    units_of: Callable | None = None,
) -> list:
    """Run the schedule over ``rungs`` (already marked to today) for a sleeve holding
    ``n_units`` of each rung — or, when ``units_of`` is given, ``units_of(rung)`` of
    each rung (equal-dollar sizing, where the count differs by rung).

    ``hw`` is a mutable ``{(tenor, strike, entry): high-water ratio}`` dict carried
    across days (lazily owned by the sleeve). For each rung this sells the freshly
    crossed fraction via ``deposit($)``, scales the rung's ``value`` down to the
    fraction still held (so the sleeve's ``value()`` reflects the partial sale), and
    drops fully-monetized rungs. Returns the surviving rungs. Stale ``hw`` entries
    (rungs no longer present) are pruned, so identity is the position tuple — not a
    recycled object id.
    """
    sched = normalize_schedule(schedule)
    keep: list = []
    live: set = set()
    for r in rungs:
        key = (r.tenor, r.strike, r.entry)
        units = units_of(r) if units_of is not None else n_units
        sold, new_hw, remaining = step(r.entry, r.value, hw.get(key, 0.0), sched)
        if units > 0.0 and sold > 0.0:
            deposit(units * sold * r.value)  # cost hook: less cost.on_trade(...)
        hw[key] = new_hw
        r.value *= remaining  # scale this rung's contribution to value() to what's still held
        if remaining > 1e-12:
            keep.append(r)
            live.add(key)
    for k in [k for k in hw if k not in live]:
        del hw[k]
    return keep
