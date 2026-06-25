"""Coverage for the geometric / CAGR-impact metrics.

Pins the arithmetic / geometric / volatility-tax algebra (the Spitznagel-vs-AQR
decomposition) and the ``cagr_impact`` benefit metric by hand.
"""

from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

from vix_hedge import metrics


def _curve(returns: list[float], start: str = "2000-01-03") -> pd.Series:
    """Business-day equity curve from a list of simple per-period returns."""
    idx = pd.bdate_range(start, periods=len(returns) + 1)
    return pd.Series(100.0 * np.cumprod([1.0, *[(1.0 + r) for r in returns]]), index=idx)


# --- hermetic: the geometric decomposition --------------------------------


def test_arithmetic_return_is_mean_times_periods():
    c = _curve([0.001] * 252)
    assert metrics.arithmetic_return(c) == pytest.approx(0.001 * 252, rel=1e-9)


def test_geometric_return_compounds():
    c = _curve([0.001] * 252)
    assert metrics.geometric_return(c) == pytest.approx(1.001**252 - 1.0, rel=1e-9)


def test_volatility_tax_zero_when_constant():
    """No dispersion -> AM == GM -> zero tax (AM-GM equality)."""
    c = _curve([0.0005] * 300)
    assert metrics.volatility_tax(c) == pytest.approx(0.0, abs=1e-9)


def test_volatility_tax_nonnegative_and_grows_with_vol():
    calm = _curve([0.005, -0.005] * 200)
    wild = _curve([0.05, -0.05] * 200)
    assert metrics.volatility_tax(calm) >= 0.0
    assert metrics.volatility_tax(wild) > metrics.volatility_tax(calm)


def test_volatility_tax_approximates_half_variance():
    """tax ≈ σ²/2 annualized -- the textbook volatility-drag identity."""
    r = [0.01, -0.01] * 300
    c = _curve(r)
    ann_var = float(np.var(r, ddof=0)) * 252
    assert metrics.volatility_tax(c) == pytest.approx(ann_var / 2.0, rel=0.05)


def test_cagr_impact_sign_and_zero():
    idx = pd.date_range("2010-01-01", "2020-01-01", freq="D")
    base = pd.Series(np.linspace(1.0, 2.0, len(idx)), index=idx)
    better = base * np.linspace(1.0, 1.2, len(idx))
    assert metrics.cagr_impact(better, base) > 0
    assert metrics.cagr_impact(base, base) == pytest.approx(0.0, abs=1e-9)


def test_decomposition_identity_arith_minus_tax():
    """Exact building-block identity: arithmetic_return − volatility_tax is the
    per-period geometric mean annualized linearly (tax := (AM − GM)·periods).
    This guards the two functions against drifting out of sync."""
    c = _curve([0.004, -0.012, 0.004, 0.004] * 250)
    r = c.pct_change().dropna()
    gm_linear = 252 * (float(np.prod(1.0 + r)) ** (1.0 / len(r)) - 1.0)
    assert metrics.arithmetic_return(c) - metrics.volatility_tax(c) == pytest.approx(gm_linear, rel=1e-9)


def test_convex_hedge_buys_tax_relief():
    """A hedge that clips the worst drop cuts variance -> lowers the volatility
    tax, the mechanism behind a positive cagr_impact (the Spitznagel channel)."""
    cycle = [0.004, -0.012, 0.004, 0.004]  # recurring small drawdown
    base = _curve(cycle * 250)
    hedged = _curve([(r * 0.5 if r < 0 else r) - 0.0003 for r in cycle * 250])  # halve losses, pay premium
    assert metrics.volatility_tax(hedged) < metrics.volatility_tax(base)  # tax relief
    assert metrics.cagr_impact(hedged, base) > 0  # net of drag and relief
