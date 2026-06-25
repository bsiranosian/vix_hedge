"""VX-futures sourcing: hermetic calendar checks + data-dependent panel/signal.

The data-dependent tests are skipped if the futures cache is not built
(``python -m vix_hedge.data.vix_futures``).
"""

from __future__ import annotations

import datetime as dt

import numpy as np
import pytest

from vix_hedge import config
from vix_hedge.data import vix_futures as vf


def test_standard_expiry_matches_known_contracts():
    # The CDN filename is the settlement date; these are real, verified contracts.
    assert vf.standard_expiry(2024, 1) == dt.date(2024, 1, 17)  # F (Jan 2024)
    assert vf.standard_expiry(2020, 3) == dt.date(2020, 3, 18)  # H (Mar 2020)
    assert vf.standard_expiry(2018, 2) == dt.date(2018, 2, 14)  # G (Feb 2018)


def test_standard_expiry_is_a_wednesday():
    # VX monthly settlement is a Wednesday (30 days before the following 3rd Friday).
    for y in range(2013, 2022):
        for m in range(1, 13):
            assert vf.standard_expiry(y, m).weekday() == 2


_HAS_FUT = config.VIX_FUTURES_PARQUET.exists()
futdata = pytest.mark.skipif(not _HAS_FUT, reason="VX-futures cache not built")


@futdata
def test_panel_shape_and_monotonic():
    panel = vf.load_panel()
    assert {"vx1", "vx2", "cmf30", "front_expiry"} <= set(panel.columns)
    assert panel.index.is_monotonic_increasing
    # Front-month always settles strictly in the future.
    assert (panel["front_expiry"].to_numpy() > panel.index.to_numpy()).all()
    # Real VIX-futures levels live in a sane band.
    assert panel["vx1"].between(8, 90).all()


@futdata
def test_cmf30_between_or_near_vx1_vx2():
    panel = vf.load_panel().dropna(subset=["vx2"])
    lo = np.minimum(panel["vx1"], panel["vx2"]) - 1e-6
    hi = np.maximum(panel["vx1"], panel["vx2"]) + 1e-6
    # CMF30 interpolates the two nearest contracts, so it sits within their span.
    assert ((panel["cmf30"] >= lo) & (panel["cmf30"] <= hi)).all()


@futdata
def test_forward_signal_sources():
    import pandas as pd

    from vix_hedge import data
    from vix_hedge.vxth.backtest import forward_vix_signal

    spot = data.load_spot_prices()
    chain = data.load_vix_chain()
    parity = forward_vix_signal(chain, spot, source="parity")
    hyb = forward_vix_signal(chain, spot, source="hybrid_vx1")
    assert isinstance(hyb, pd.Series)
    assert hyb.index.equals(spot.index)
    # Hybrid equals parity before futures coverage, diverges within it.
    pre = spot.index < pd.Timestamp("2013-01-02")
    assert np.allclose(hyb[pre].dropna().to_numpy(), parity[pre].dropna().to_numpy())
    assert not hyb.loc["2013-06-01":"2020-12-31"].equals(parity.loc["2013-06-01":"2020-12-31"])
