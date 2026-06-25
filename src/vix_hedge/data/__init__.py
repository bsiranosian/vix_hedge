"""Data layer: build cleaned panels from raw option/price files, then load them.

The heavy raw -> parquet conversion lives in :mod:`vix_hedge.data.build` and is run
once (``python -m vix_hedge.data.build``). The lightweight loaders in
:mod:`vix_hedge.data.load` read those parquet caches and assemble the fast
lookup structures the backtests use.
"""

from vix_hedge.data.load import (
    DayChain,
    OptionChain,
    load_spot_prices,
    load_spx_chain,
    load_vix_chain,
)
from vix_hedge.data.vix_futures import load_panel as load_vix_futures

__all__ = [
    "DayChain",
    "OptionChain",
    "load_spot_prices",
    "load_spx_chain",
    "load_vix_chain",
    "load_vix_futures",
]
