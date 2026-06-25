"""CBOE VXTH (VIX tail-hedge) replication."""

from vix_hedge.vxth.backtest import (
    REGIME_ALLOC,
    REGIME_BOUNDS,
    forward_vix_series,
    forward_vix_signal,
    regime_of,
    select_call,
)
from vix_hedge.vxth.benchmark import load_vxth_index, vxth_curve
from vix_hedge.vxth.engine import (
    ALLOC_OFFICIAL,
    ALLOC_REVERSED,
    BASES,
    HedgeConfig,
    simulate,
)
from vix_hedge.vxth.ensemble import ensemble, tranche_robustness, tranched_strategy
from vix_hedge.vxth.sleeves import Sleeve, make_sleeve, register_sleeve

__all__ = [
    "REGIME_ALLOC",
    "REGIME_BOUNDS",
    "forward_vix_series",
    "forward_vix_signal",
    "regime_of",
    "select_call",
    "load_vxth_index",
    "vxth_curve",
    "ALLOC_OFFICIAL",
    "ALLOC_REVERSED",
    "BASES",
    "HedgeConfig",
    "simulate",
    "ensemble",
    "tranche_robustness",
    "tranched_strategy",
    "Sleeve",
    "make_sleeve",
    "register_sleeve",
]
