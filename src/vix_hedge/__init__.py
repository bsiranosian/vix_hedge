"""vix_hedge: a focused reproduction of the VIX-call tail-hedge analysis.

* ``vix_hedge.vxth``         -- CBOE VXTH (VIX tail-hedge) replication + the
                                delta-dial / tenor-ladder / timing-luck study
* ``vix_hedge.vix_returns``  -- VIX regime transitions + single-option return stats

Shared infrastructure:

* ``vix_hedge.data``     -- raw -> cleaned panels (spot prices, VIX option chain)
* ``vix_hedge.metrics``  -- CAGR / Sharpe / drawdown performance stats
"""

__all__ = ["config", "data", "metrics", "vxth", "vix_returns"]
