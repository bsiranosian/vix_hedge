"""Performance metric calculations against closed-form cases."""

from __future__ import annotations

import numpy as np
import pandas as pd

from vix_hedge import metrics


def _curve(daily_return: float, days: int = 365 * 4):
    idx = pd.date_range("2000-01-01", periods=days, freq="D")
    return pd.Series(100.0 * (1 + daily_return) ** np.arange(days), index=idx)


def test_cagr_constant_growth():
    # exactly 10% per year compounded daily
    r = (1.10) ** (1 / 365) - 1
    c = _curve(r)
    assert np.isclose(metrics.cagr(c), 0.10, atol=1e-3)


def test_max_drawdown():
    idx = pd.date_range("2020-01-01", periods=5, freq="D")
    c = pd.Series([100, 120, 60, 90, 130], index=idx, dtype=float)
    assert np.isclose(metrics.max_drawdown(c), 0.5)  # 120 -> 60


def test_sharpe_matches_return_over_stddev():
    c = _curve(0.0005)
    sh = metrics.annualized_sharpe(c)
    expect = metrics.annualized_return(c) / metrics.annualized_stddev(c)
    assert np.isclose(sh, expect)


def test_monotonic_curve_has_zero_drawdown():
    assert metrics.max_drawdown(_curve(0.001)) == 0.0


def test_sortino_matches_return_over_downside_dev():
    rng = np.random.default_rng(1)
    idx = pd.date_range("2000-01-01", periods=365 * 6, freq="D")
    c = pd.Series(100.0 * np.cumprod(1 + rng.normal(0.0004, 0.01, len(idx))), index=idx)
    so = metrics.sortino(c)
    expect = metrics.annualized_return(c) / metrics.downside_deviation(c)
    assert np.isclose(so, expect)


def test_sortino_beats_sharpe_for_right_skew():
    """A right-skewed series (rare big up-months, small steady down-months) has more
    total vol than downside vol, so Sortino > Sharpe — the convex-hedge lens."""
    idx = pd.date_range("2000-01-31", periods=48, freq="ME")
    r = np.full(48, -0.005)   # steady small monthly losses ...
    r[::12] = 0.20            # ... punctuated by a rare big up-month each year
    curve = pd.Series(100.0 * np.cumprod(1 + r), index=idx)
    assert metrics.sortino(curve) > metrics.annualized_sharpe(curve)


def test_monotonic_curve_has_zero_downside_deviation():
    assert metrics.downside_deviation(_curve(0.001)) == 0.0
