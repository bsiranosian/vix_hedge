"""Performance statistics for equity curves.

Mirrors the handful of ``PerformanceAnalytics`` measures the R scripts report:
CAGR, annualized Sharpe, max drawdown, Calmar, Sterling, annualized stddev.

Conventions match the R code:

* Sharpe / annualized stddev are computed on **monthly** returns (the R code did
  ``to.period(..., 'months')`` then ``Return.calculate``), annualized with
  ``sqrt(12)`` and a 0% risk-free rate.
* CAGR uses the calendar-day span of the curve: ``(end/start)**(365/days) - 1``.
* Calmar = CAGR / maxDrawdown. Sterling = CAGR / (maxDrawdown + 0.10), matching
  ``PerformanceAnalytics::SterlingRatio``'s default 10% excess.
"""

from __future__ import annotations

import numpy as np
import pandas as pd

MONTHS_PER_YEAR = 12


def to_monthly(values: pd.DataFrame | pd.Series) -> pd.DataFrame | pd.Series:
    """Last observation of each calendar month (``to.period(OHLC=F)`` analogue)."""
    return values.resample("ME").last()


def returns(values: pd.DataFrame | pd.Series) -> pd.DataFrame | pd.Series:
    """Simple period-over-period returns, dropping the leading NaN."""
    return values.pct_change().iloc[1:]


#: A normal holiday / long weekend spans <=4 calendar days; the 2001-09 closure
#: spanned 7. A daily-return calculation bridges any gap and treats the multi-day
#: jump as a single period, so we drop returns spanning a gap longer than this --
#: a standing safety net so a discontinuous series can't inject one spurious
#: "daily" return.
MAX_GAP_DAYS = 10


def daily_returns(values: pd.Series, max_gap_days: int = MAX_GAP_DAYS) -> pd.Series:
    """Daily simple returns with any return spanning a calendar gap longer than
    ``max_gap_days`` dropped -- a gap-aware :func:`returns` for *daily* series, so a
    missing-data hole cannot masquerade as one giant one-day move."""
    v = values.dropna()
    r = v.pct_change()
    gaps = v.index.to_series().diff().dt.days
    return r.mask(gaps > max_gap_days).dropna()


def cagr(values: pd.Series) -> float:
    """Compound annual growth rate over the curve's calendar-day span."""
    v = values.dropna()
    days = (v.index[-1] - v.index[0]).days
    if days <= 0 or v.iloc[0] <= 0:
        return np.nan
    return (v.iloc[-1] / v.iloc[0]) ** (365.0 / days) - 1.0


def annualized_return(values: pd.Series) -> float:
    """Geometric annualized return from monthly returns (``Return.annualized``)."""
    r = returns(to_monthly(values)).dropna()
    if len(r) < 1:
        return np.nan
    return float(np.prod(1.0 + r) ** (MONTHS_PER_YEAR / len(r)) - 1.0)


def annualized_sharpe(values: pd.Series, rf: float = 0.0) -> float:
    """Annualized Sharpe = ``Return.annualized / StdDev.annualized`` on monthly
    returns, matching ``PerformanceAnalytics::SharpeRatio(annualize=TRUE)``."""
    sd = annualized_stddev(values)
    if not sd or not np.isfinite(sd):
        return np.nan
    return (annualized_return(values) - rf) / sd


def annualized_stddev(values: pd.Series) -> float:
    """Annualized standard deviation of monthly returns."""
    r = returns(to_monthly(values)).dropna()
    return r.std(ddof=1) * np.sqrt(MONTHS_PER_YEAR)


def drawdown_curve(values: pd.Series) -> pd.Series:
    """Running drawdown (<= 0) of an equity curve."""
    v = values.dropna()
    return v / v.cummax() - 1.0


def max_drawdown(values: pd.Series) -> float:
    """Worst peak-to-trough drawdown as a positive fraction (e.g. 0.52)."""
    dd = drawdown_curve(values)
    return float(-dd.min()) if len(dd) else np.nan


def cagr_impact(hedged: pd.Series, base: pd.Series) -> float:
    """Hedged CAGR minus base CAGR over their common span -- the geometric /
    compounding benefit of the hedge (the Universa "Safe Haven" lens). A
    positive value means the hedge *raised* portfolio compound growth, which the
    standalone or risk-matched-drawdown metrics miss."""
    idx = hedged.dropna().index.intersection(base.dropna().index)
    if len(idx) < 2:
        return np.nan
    return cagr(hedged.loc[idx]) - cagr(base.loc[idx])


# --- geometric decomposition (the Spitznagel-vs-AQR lens) --------------------
# A tail hedge changes compound growth through two opposing channels. It lowers
# the *arithmetic* expected return (premium bleed -- AQR's "puts are pathetic"
# point) but it can also lower the *volatility tax* -- the gap between arithmetic
# and geometric return that compounding imposes on a volatile path (~σ²/2). The
# net, ``Δcagr = Δarithmetic − Δvolatility_tax``, is exactly what ``cagr_impact``
# measures; these helpers expose the two terms so a moneyness sweep can say *why*
# a strike band helps or hurts, not just whether it does.


def arithmetic_return(values: pd.Series, periods_per_year: int = 252) -> float:
    """Annualized arithmetic mean of daily simple returns (no compounding) -- the
    expected-return term AQR emphasizes. Sits above the geometric return by the
    volatility tax (below)."""
    r = daily_returns(values)
    if len(r) < 1:
        return np.nan
    return float(r.mean() * periods_per_year)


def geometric_return(values: pd.Series, periods_per_year: int = 252) -> float:
    """Annualized geometric (compound) return from daily simple returns -- the
    growth actually realized (the Universa/Spitznagel lens). Differs from
    :func:`cagr` only in counting trading days rather than calendar days, so the
    two agree to ~1bp; this form is the one the volatility-tax decomposition nets
    against :func:`arithmetic_return`."""
    r = daily_returns(values)
    n = len(r)
    if n < 1:
        return np.nan
    return float(np.prod(1.0 + r) ** (periods_per_year / n) - 1.0)


def volatility_tax(values: pd.Series, periods_per_year: int = 252) -> float:
    """The volatility drag: annualized (arithmetic mean − geometric mean) of the
    per-period returns, i.e. ``(mean(r) − [Π(1+r)]^(1/n) + 1) · periods``. Always
    ``>= 0`` by AM–GM and ``≈ σ²/2`` (annualized variance / 2) -- the compound-
    growth Spitznagel argues a tail hedge can *reclaim* by cutting variance.
    To leading order ``geometric_return ≈ arithmetic_return − volatility_tax``,
    so differencing a hedged and base curve's drag (``Δarithmetic``) and tax
    relief (``Δvolatility_tax``) decomposes :func:`cagr_impact` into *why* the
    hedge helped or hurt growth -- the AQR (drag) vs Spitznagel (tax) split."""
    r = daily_returns(values)
    n = len(r)
    if n < 1:
        return np.nan
    geo_mean = float(np.prod(1.0 + r) ** (1.0 / n) - 1.0)
    return float((r.mean() - geo_mean) * periods_per_year)


def calmar(values: pd.Series) -> float:
    mdd = max_drawdown(values)
    return cagr(values) / mdd if mdd else np.nan


def sterling(values: pd.Series, excess: float = 0.10) -> float:
    mdd = max_drawdown(values)
    return cagr(values) / (mdd + excess) if (mdd + excess) else np.nan


def downside_deviation(values: pd.Series, mar: float = 0.0) -> float:
    """Annualized downside deviation of monthly returns below ``mar`` -- the Sortino
    denominator. Upside months count as 0 (not excluded), the standard convention."""
    r = returns(to_monthly(values)).dropna()
    if len(r) < 1:
        return np.nan
    downside = np.minimum(r.to_numpy() - mar, 0.0)
    return float(np.sqrt(np.mean(downside**2)) * np.sqrt(MONTHS_PER_YEAR))


def sortino(values: pd.Series, rf: float = 0.0, mar: float = 0.0) -> float:
    """Annualized Sortino = ``(annualized return − rf) / downside deviation``. Like
    Sharpe but penalizing only *downside* volatility, so a convex hedge that adds
    right-skew (big up-moves in crashes) scores better here than on Sharpe -- the
    right lens for "smoother returns without giving up the crash payoff"."""
    dd = downside_deviation(values, mar)
    if not dd or not np.isfinite(dd):
        return np.nan
    return (annualized_return(values) - rf) / dd


def summary(curves: pd.DataFrame) -> pd.DataFrame:
    """Table of the standard measures, one row per column (portfolio).

    Drawdown/Sharpe are computed against each curve's own first valid point.
    """
    rows = {}
    for name in curves.columns:
        v = curves[name].dropna()
        rows[name] = {
            "CAGR": cagr(v),
            "Sharpe": annualized_sharpe(v),
            "MaxDrawdown": max_drawdown(v),
            "Calmar": calmar(v),
            "Sterling": sterling(v),
            "AnnStdDev": annualized_stddev(v),
        }
    return pd.DataFrame(rows).T
