"""Shared test fixtures: synthetic option chains (no raw data needed)."""

from __future__ import annotations

import numpy as np
import pandas as pd
import pytest

from vix_hedge.data.load import DayChain


def pytest_addoption(parser):
    parser.addoption("--run-network", action="store_true", default=False,
                     help="run tests marked @pytest.mark.network (hit live feeds)")


def pytest_configure(config):
    config.addinivalue_line("markers", "network: hits a live network feed (skipped unless --run-network)")


def pytest_collection_modifyitems(config, items):
    if config.getoption("--run-network"):
        return
    skip = pytest.mark.skip(reason="needs --run-network")
    for item in items:
        if "network" in item.keywords:
            item.add_marker(skip)


def make_day(date, exdate, strikes, put_mid, call_mid, *, delta=None, forward=None) -> DayChain:
    """Build a one-expiration DayChain from parallel strike/mid arrays."""
    date, exdate = pd.Timestamp(date), pd.Timestamp(exdate)
    rows = []
    for cp, mids in (("P", put_mid), ("C", call_mid)):
        for k, m in zip(strikes, mids, strict=True):
            rows.append({
                "date": date, "exdate": exdate, "cp_flag": cp, "strike": float(k),
                "mid": m, "delta": np.nan if delta is None else delta,
                "forward": np.nan if forward is None else forward,
                "dte": (exdate - date).days,
            })
    block = pd.DataFrame(rows)
    block["cp_flag"] = block["cp_flag"].astype("category")
    return DayChain(date, block)


@pytest.fixture
def spx_day() -> DayChain:
    """SPX at ~1000 with strikes every 50; deep-ITM puts worth more than OTM."""
    strikes = np.arange(700, 1301, 50.0)
    spot = 1000.0
    put_mid = np.maximum(strikes - spot, 0) + 10.0  # intrinsic + 10 time value
    call_mid = np.maximum(spot - strikes, 0) + 10.0
    return make_day("2010-06-01", "2010-07-16", strikes, put_mid, call_mid)
