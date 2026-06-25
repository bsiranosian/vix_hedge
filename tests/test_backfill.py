"""Spot 2019-07..11 range (FRED/parity SPX + CBOE VIX) and the cross-gap return guard.

Hermetic checks on the put-call-parity forward recovery and the gap mask run
always; the end-to-end "the rebuilt panel is continuous" check skips unless the
data sources (CBOE VIX history + the full-2019 chain) are on disk.
"""

from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

from vix_hedge import config, metrics
from vix_hedge.data import build


def test_parity_forward_recovers_known_forward():
    """European parity is exactly linear in K, so the x-intercept of (C-P) vs K is
    the forward — recovered to machine precision from noiseless quotes."""
    fwd = 3000.0
    strikes = np.arange(2850.0, 3151.0, 10.0)
    g = pd.DataFrame({"exdate": pd.Timestamp("2019-08-16"), "dte": 30,
                      "strike": strikes, "cmp": fwd - strikes})
    assert abs(build._parity_forward(g) - fwd) < 1e-6


def test_parity_forward_picks_the_30dte_expiry():
    """With several expiries it uses the one nearest 30 DTE (ignores the others)."""
    s = np.arange(2900.0, 3101.0, 10.0)
    near = pd.DataFrame({"exdate": pd.Timestamp("2019-08-16"), "dte": 28, "strike": s, "cmp": 3000.0 - s})
    far = pd.DataFrame({"exdate": pd.Timestamp("2019-12-20"), "dte": 90, "strike": s, "cmp": 2500.0 - s})
    assert abs(build._parity_forward(pd.concat([near, far], ignore_index=True)) - 3000.0) < 1e-6


def test_daily_returns_drops_cross_gap():
    """A 157-day gap must not produce one ~6% 'daily' return."""
    idx = pd.to_datetime(["2019-06-26", "2019-06-27", "2019-06-28", "2019-12-02", "2019-12-03"])
    s = pd.Series([100.0, 101.0, 102.0, 108.0, 109.0], index=idx)
    r = metrics.daily_returns(s)
    assert len(r) == 3  # 4 raw returns minus the one spanning the gap
    assert (r.abs() < 0.05).all()  # the cross-gap jump is dropped
    # a normal multi-day weekend (<= max_gap_days) is retained
    assert len(metrics.daily_returns(s, max_gap_days=200)) == 4


# --- data-gated: the rebuilt panel is continuous across 2019 ------------------
_HAVE_SOURCES = config.VIX_HISTORY_CSV.exists() and config.SPOT_PARQUET.exists()


@pytest.mark.skipif(not _HAVE_SOURCES, reason="data sources / spot cache absent")
def test_rebuilt_spot_is_continuous_and_clean():
    from vix_hedge import data

    spot = data.load_spot_prices()
    d = spot.index
    big = [(d[i].date(), d[i + 1].date(), (d[i + 1] - d[i]).days)
           for i in range(len(d) - 1) if (d[i + 1] - d[i]).days > 5]
    # the only > 5-day gap left is the real 2001-09 market closure
    assert all(a.year != 2019 and b.year != 2019 for a, b, _ in big), big
    assert pd.Timestamp("2019-09-16") in d  # a 2019-H2 day is present
    assert (spot["VIX"] > 0).all()  # the VIX=0 tick was repaired
    # the 2019-06/07 seam is a normal day, not a cross-gap jump
    seam = spot["SPX"].pct_change().loc["2019-06-28":"2019-07-03"].abs().max()
    assert seam < 0.03, seam
