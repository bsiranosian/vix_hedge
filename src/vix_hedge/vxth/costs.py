"""Transaction-cost hook (bid/ask-spread fills, not midpoint).

Every existing result is a *frictionless, midpoint-fill* backtest. This makes the
friction explicit so every roadmap number is reportable **gross and net**: the
engine threads a ``cost_model`` to each sleeve, which charges it at every trade
(roll, monetize). The default :data:`NO_COST` is frictionless, so Wave-0 numbers
are unchanged until a real model is supplied.

The model answers one question — "what does trading this much notional cost?" —
because the hook is ``on_trade(notional, instrument=…)``. The realistic answer is
a **relative half-spread**: crossing a quoted bid/ask once costs ``(ask−bid)/2``,
i.e. a fraction of the premium traded. :class:`ProportionalCost` charges exactly
that, with a per-instrument rate **calibrated from the quoted bid/ask in our own
data** (medians over the strikes/tenors the sleeve actually trades, 2006–2020):

==============  =================================  ====================
instrument      legs traded                        median rel. half-spread
==============  =================================  ====================
``vix_call``    30Δ VIX calls, 30/60/90d ladder     ~5–8%  → rate **0.06**
==============  =================================  ====================

The median half-spread is roughly regime-stable, so a constant rate is a fair
first-order model; the *upper tail* widens in stress (mean VIX-call half-spread
8%→13% calm→crash), so monetizing a concentrated position into a spike can cost
more than the median implies — a documented caveat, not modeled here.

Contract rounding (integer contracts) is deliberately out of scope: on a book of
this size, per-contract premiums ($50–500) round to <0.05% of notional, far below
the half-spread, which is the dominant, first-order cost this models.
"""

from __future__ import annotations

from typing import Protocol, runtime_checkable


@runtime_checkable
class CostModel(Protocol):
    def on_trade(self, notional: float, *, instrument: str = "") -> float:
        """Dollar cost of trading ``notional`` of ``instrument`` (>= 0)."""
        ...


class _NoCost:
    def on_trade(self, notional: float, *, instrument: str = "") -> float:
        return 0.0


#: Default frictionless model — keeps Wave-0 numbers identical to today.
NO_COST: CostModel = _NoCost()

#: Per-instrument relative half-spreads (fraction of premium traded), calibrated
#: from quoted bid/ask in the data (see module docstring).
DEFAULT_RATES: dict[str, float] = {
    "vix_call": 0.06,
}


class ProportionalCost:
    """Half-spread transaction cost: ``cost = rate[instrument] · |notional|``.

    ``notional`` is the dollar premium (re)opened or sold; the sleeve charges this
    when it establishes a position or monetizes one, so a round-tripped position
    pays the half-spread each way it is traded. Rates default to the data-calibrated
    :data:`DEFAULT_RATES`; ``default`` covers any unlisted instrument. ``scale``
    multiplies every rate uniformly — a cheap way to stress higher/lower cost
    regimes (``scale=0`` recovers :data:`NO_COST`) without re-calibrating.
    """

    def __init__(self, rates: dict[str, float] | None = None, *, default: float = 0.05, scale: float = 1.0):
        self.rates = dict(DEFAULT_RATES if rates is None else rates)
        self.default = float(default)
        self.scale = float(scale)

    def on_trade(self, notional: float, *, instrument: str = "") -> float:
        rate = self.rates.get(instrument, self.default)
        return abs(float(notional)) * rate * self.scale


def bps(rate_bps: float) -> ProportionalCost:
    """A flat ``rate_bps``-of-notional cost across all instruments (e.g. ``bps(50)``
    for 50 bps). Convenience for a single uniform rate."""
    r = rate_bps / 1e4
    return ProportionalCost(rates={}, default=r)
