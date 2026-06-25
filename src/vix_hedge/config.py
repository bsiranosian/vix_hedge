"""Paths and shared constants for the vix_hedge analyses.

Raw data lives outside the repo in ``../option_data`` (see README). Everything
derived (cleaned panels, backtest outputs) is written under the repo's
``data/cache`` and ``results`` directories, which are git-ignored.
"""

from __future__ import annotations

import os
from pathlib import Path

# --- Locations -------------------------------------------------------------
REPO_ROOT = Path(__file__).resolve().parents[2]

# The raw OptionMetrics / purchased data directory. Override with $OPTION_DATA.
OPTION_DATA = Path(os.environ.get("OPTION_DATA", REPO_ROOT.parent / "option_data"))

CACHE_DIR = REPO_ROOT / "data" / "cache"
RESULTS_DIR = REPO_ROOT / "results"

# --- Raw input files (under OPTION_DATA) -----------------------------------
# SPX & VIX daily spot OHLC, 1996 -> 2019-06 (OptionMetrics secid panel).
SPX_VIX_SPOT_GZ = OPTION_DATA / "SPX_VIX_TLT_price.csv.gz"

# SPX end-of-day option chain, 1996-01 -> 2019-12 (OptionMetrics).
SPX_OPTIONS_CSV = OPTION_DATA / "SPX_filtered" / "SPX_option_prices_01Jan1996-31Dec2019.csv"

# Raw OptionMetrics SPX chain covering the full 2019. Used to reconstruct SPX spot
# via put-call parity for the 2019-07..2019-11 spot range (see build._backfill_spot_gap).
SPX_OPTIONS_2019_CSV_GZ = OPTION_DATA / "SPX_option_prices_01Jan2019-12Dec2019.csv.gz"

# Public CBOE VIX index daily history (1990 -> present). Authoritative VIX spot;
# matches the OptionMetrics VIX exactly on overlap. Source for the 2019-H2 spot
# range and for repairing stray bad ticks (e.g. a VIX=0 print). Refresh with:
#   curl -s -o ../option_data/VIX_History.csv $VIX_HISTORY_CDN
VIX_HISTORY_CSV = OPTION_DATA / "VIX_History.csv"
VIX_HISTORY_CDN = "https://cdn.cboe.com/api/global/us_indices/daily_prices/VIX_History.csv"

# Public FRED S&P 500 daily close (~last 10y). Real SPX spot for the 2019-07..2019-11
# range; exact where present. The put-call-parity reconstruction
# (build._reconstruct_spx_parity) is the no-network fallback (~5bps).
#   curl -s -o ../option_data/SP500_FRED.csv $SP500_HISTORY_CDN
SP500_HISTORY_CSV = OPTION_DATA / "SP500_FRED.csv"
SP500_HISTORY_CDN = "https://fred.stlouisfed.org/graph/fredgraph.csv?id=SP500"

# 2019/2020 SPX options from historicaloptiondata.com (different schema).
SPX_PURCHASED = {
    2019: [OPTION_DATA / "purchased_data" / "SPX_2019.csv", OPTION_DATA / "purchased_data" / "SPXW_2019.csv"],
    2020: [OPTION_DATA / "purchased_data" / "SPX_2020.csv", OPTION_DATA / "purchased_data" / "SPXW_2020.csv"],
}

# VIX options, 2006-02 -> 2019-12 (OptionMetrics) + 2019/2020 purchased.
VIX_OPTIONS_GZ = OPTION_DATA / "VIX_option_prices_2006-02-24_2019-12-31.csv.gz"
VIX_PURCHASED = {
    2019: [OPTION_DATA / "purchased_data" / "VIX_2019.csv"],
    2020: [OPTION_DATA / "purchased_data" / "VIX_2020.csv"],
}

# Official CBOE VXTH index. The full index level history (through present) comes
# from CBOE's CDN; the older tsv (daily allocation detail) ends mid-2019.
VXTH_CBOE_CSV = OPTION_DATA / "VXTH_History.csv"
VXTH_ALLOCATIONS_TSV = OPTION_DATA / "VIXTH_daily_allocations.tsv"

# Real VIX (VX) futures, downloaded per-contract from CBOE's CDN. The CDN only
# retains contracts from ~2013 on (older ones were purged); for the report's
# regime signal that window covers the entire 2020 event and every modern
# single-call experiment. Raw per-contract CSVs are cached under VIX_FUTURES_DIR;
# the daily front-month / constant-maturity panel is VIX_FUTURES_PARQUET.
VIX_FUTURES_DIR = OPTION_DATA / "vix_futures"
VIX_FUTURES_CDN = "https://cdn.cboe.com/data/us/futures/market_statistics/historical_data/VX/VX_{}.csv"

# --- Cleaned parquet caches (built by data.build) --------------------------
SPOT_PARQUET = CACHE_DIR / "spot_prices.parquet"
SPX_OPTIONS_PARQUET = CACHE_DIR / "spx_options.parquet"
VIX_OPTIONS_PARQUET = CACHE_DIR / "vix_options.parquet"
VIX_FUTURES_PARQUET = CACHE_DIR / "vix_futures.parquet"  # date, vx1, vx2, cmf30, front_expiry

# --- Modeling constants ----------------------------------------------------
MAX_DTE = 120  # keep only options expiring within this many days
STARTING_BALANCE = 100_000.0


def ensure_dirs() -> None:
    """Create the cache/results directories if they do not yet exist."""
    CACHE_DIR.mkdir(parents=True, exist_ok=True)
    RESULTS_DIR.mkdir(parents=True, exist_ok=True)
