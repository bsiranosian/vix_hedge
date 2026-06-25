"""Standard crash-episode windows for payoff comparisons.

Every roadmap item that reports crash payoff should use these same windows so the
report cells line up. Dates are inclusive close-to-close spans bracketing the
worst of each drawdown.
"""

from __future__ import annotations

import pandas as pd

#: name -> (start, end) inclusive.
CRASH_EPISODES: dict[str, tuple[str, str]] = {
    "DotCom 2000-02": ("2000-09-01", "2002-10-09"),  # predates the 2006 options sample -> NaN for option-driven curves
    "GFC 2008-09": ("2008-09-02", "2009-03-09"),
    "Q4 2018": ("2018-09-20", "2018-12-24"),
    "COVID 2020": ("2020-02-19", "2020-03-23"),
}


def window_return(curve: pd.Series, start: str, end: str) -> float:
    """Total return of ``curve`` over [start, end]; NaN if the window is empty."""
    seg = curve.loc[start:end].dropna()
    if len(seg) < 2:
        return float("nan")
    return float(seg.iloc[-1] / seg.iloc[0] - 1.0)


def episode_returns(curve: pd.Series, episodes: dict[str, tuple[str, str]] | None = None) -> dict[str, float]:
    """Total return of ``curve`` across each crash window (fraction, e.g. -0.30)."""
    episodes = episodes or CRASH_EPISODES
    return {name: window_return(curve, s, e) for name, (s, e) in episodes.items()}
