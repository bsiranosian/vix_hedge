"""Load the official CBOE VXTH index level as a benchmark series."""

from __future__ import annotations

import pandas as pd

from vix_hedge import config


def load_vxth_index() -> pd.Series:
    """Daily official VXTH index level, indexed by date.

    Prefers CBOE's full index history CSV (through present); falls back to the
    older ``VIXTH_daily_allocations.tsv`` (which ends mid-2019 and so misses the
    2020 spike that drove most of VXTH's return).
    """
    if config.VXTH_CBOE_CSV.exists():
        df = pd.read_csv(config.VXTH_CBOE_CSV)
        df["date"] = pd.to_datetime(df["DATE"], format="%m/%d/%Y")
        s = df.set_index("date")["VXTH"].sort_index()
    else:
        df = pd.read_csv(config.VXTH_ALLOCATIONS_TSV, sep="\t")
        df["date"] = pd.to_datetime(df["Date"], format="%m/%d/%Y")
        s = df.groupby("date")["VXTH"].last().sort_index()
    s.name = "VXTH"
    return s


def vxth_curve(index: pd.Series, dates: pd.DatetimeIndex, starting_balance: float) -> pd.Series:
    """Rescale the VXTH index to ``starting_balance`` at the first date in ``dates``."""
    s = index.reindex(dates).ffill()
    base = s.dropna().iloc[0]
    return s * (starting_balance / base)
