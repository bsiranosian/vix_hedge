"""Hedge-sleeve interface: pluggable instruments for the backtest engine.

A *sleeve* owns its option/asset positions and knows how to value, roll, and
(optionally) monetize them. ``engine.simulate`` drives **one sleeve plus a base
portfolio**. New instruments are added as **new modules** implementing
:class:`Sleeve` and registered with ``@register_sleeve("name")`` — so they do NOT
require editing ``engine.simulate``. Set ``HedgeConfig.instrument="name"`` to
select one. This focused repo ships the ``vix_call`` sleeve (the headline VIX-call
ladder); the protocol is kept pluggable so other instruments can be added.

Contract — all amounts are dollars; ``prices`` is a dict of base-asset closes
(SPX) plus ``"VIX"``:

* ``can_open(date, prices) -> bool`` — is a position openable here? (finds first day)
* ``roll_and_fund(date, prices, target_dollars) -> float`` — (re)build positions
  sized to ``~target_dollars``; return dollars actually deployed.
* ``mark(date, prices) -> None`` — update internal position values to ``date``.
* ``value() -> float`` — current marked dollar value (call after ``mark``).
* ``monetize(date, prices, base) -> None`` — move proceeds into ``base`` (default no-op).
* ``is_roll(date) -> bool`` — should the sleeve roll / portfolio rebalance now?

A sleeve owns whichever option ``chain`` its instrument needs (VIX chain for VIX
calls, SPX chain for put spreads); the caller passes the right chain to
``simulate``. Transaction costs arrive via ``cost_model`` (default frictionless;
see :mod:`vix_hedge.vxth.costs`).
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Protocol, runtime_checkable

#: Base-portfolio assets the engine can hold.
_BASE_ASSETS = ("SPX",)


@runtime_checkable
class Sleeve(Protocol):
    def can_open(self, date, prices: dict) -> bool: ...
    def roll_and_fund(self, date, prices: dict, target_dollars: float) -> float: ...
    def mark(self, date, prices: dict) -> None: ...
    def value(self) -> float: ...
    def monetize(self, date, prices: dict, base: _Base) -> None: ...
    def is_roll(self, date) -> bool: ...


@dataclass
class _Rung:
    """One option position in a ladder."""

    tenor: object  # pd.Timestamp expiration
    strike: float
    entry: float
    value: float = 0.0


@dataclass
class _Base:
    """Base sleeve = shares of the base assets at a fixed internal weight."""

    weights: dict
    shares: dict = field(default_factory=lambda: dict.fromkeys(_BASE_ASSETS, 0.0))

    def value(self, px: dict) -> float:
        return sum(self.shares[a] * px[a] for a in self.weights)

    def set_to(self, dollars: float, px: dict) -> None:
        for a, w in self.weights.items():
            self.shares[a] = w * dollars / px[a]

    def add(self, dollars: float, px: dict) -> None:
        for a, w in self.weights.items():
            self.shares[a] += w * dollars / px[a]


# --- registry --------------------------------------------------------------
_REGISTRY: dict[str, type] = {}


def register_sleeve(name: str):
    """Class decorator: register a sleeve implementation under ``name``."""

    def deco(cls):
        _REGISTRY[name] = cls
        return cls

    return deco


def make_sleeve(name: str, chain, cfg, *, cost_model=None) -> Sleeve:
    """Construct the registered sleeve ``name`` for ``chain``/``cfg``."""
    try:
        cls = _REGISTRY[name]
    except KeyError:
        raise ValueError(f"unknown sleeve {name!r}; registered: {sorted(_REGISTRY)}") from None
    return cls(chain, cfg, cost_model=cost_model)


def registered_sleeves() -> list[str]:
    return sorted(_REGISTRY)


# Import built-in sleeves so they self-register. A new sleeve adds one import line
# here -- e.g. ``from . import <name> as _<name>`` (suppress F401).
from vix_hedge.vxth.sleeves import vix_call as _vix_call  # noqa: E402,F401
