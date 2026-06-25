"""VXTH regime logic and VIX-returns regime transitions."""

from __future__ import annotations

import numpy as np
import pandas as pd

from vix_hedge import vix_returns as vr
from vix_hedge.vxth import backtest as vxth


def test_regime_of_bounds():
    assert vxth.regime_of(10) == 0  # <=15
    assert vxth.regime_of(20) == 1  # 15-30  -> 1% hedge
    assert vxth.regime_of(40) == 2  # 30-50  -> 0.5% hedge
    assert vxth.regime_of(60) == 3  # >50    -> 0%
    assert vxth.REGIME_ALLOC[1] == 0.010 and vxth.REGIME_ALLOC[2] == 0.005


def test_regime_series_counts_thresholds():
    vix = pd.Series([10, 16, 31, 51], index=pd.date_range("2020-01-01", periods=4))
    assert list(vr.regime_series(vix)) == [0, 1, 2, 3]


def test_transition_matrix_diagonal_and_offdiagonal():
    reg = pd.Series([0, 0, 1, 1, 0], index=pd.date_range("2020-01-01", periods=5))
    tm = vr.transition_matrix(reg)
    assert tm.loc[0, 0] == 1  # 0->0 once
    assert tm.loc[0, 1] == 1  # 0->1 once
    assert tm.loc[1, 1] == 1  # 1->1 once
    assert tm.loc[1, 0] == 1  # 1->0 once
    assert tm.to_numpy().sum() == 4  # n-1 transitions


def test_select_call_picks_target_delta():
    from tests.conftest import make_day

    strikes = np.array([15, 20, 25, 30, 40.0])
    deltas = np.array([0.5, 0.35, 0.2, 0.1, 0.03])  # call deltas decreasing with strike
    day = make_day("2010-06-01", "2010-07-16", strikes, put_mid=np.ones(5),
                   call_mid=np.array([5, 3, 2, 1, 0.4]), delta=None)
    # inject per-strike deltas via the block
    day._block.loc[day._block["cp_flag"] == "C", "delta"] = deltas
    pick = vxth.select_call(day, dte_target=45, delta_target=0.10)
    assert pick is not None
    _, strike = pick
    assert strike == 30.0  # the 0.10-delta call
