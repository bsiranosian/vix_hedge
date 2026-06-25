"""Build extended VIX-option + spot panels from the 2025 OptionMetrics refresh.

The new pull (`VIX_option_prices_2006-2025.csv.gz`, root `gwd0l2x0mfrhmbvp.csv`)
is a *full* OptionMetrics export — more columns, ISO `YYYY-MM-DD` dates, and an
empty `forward_price` — so the committed `data.build` SQL (which expects the old
`YYYYMMDD` slim schema) can't read it directly. This standalone builder reads the
new schema and writes parallel `*_extended.parquet` caches, leaving the committed
pipeline and its caches untouched.

Outputs (under data/cache, git-ignored):
* ``vix_options_extended.parquet`` — VIX chain 2006-02 -> 2025-08 (same columns as
  ``vix_options.parquet``; ``forward`` is NaN — the engine reconstructs the forward
  VIX from put-call parity, so this is harmless, see backtest.forward_vix_series).
* ``spot_prices_extended.parquet`` — SPX/VIX daily closes 1996 -> 2025-08. Existing
  panel for dates <= 2020-12-31, then FRED SPX + CBOE VIX for 2021+.

Run: ``uv run python current_data_extension/build_extended.py``
"""

from __future__ import annotations

import duckdb
import pandas as pd

from vix_hedge import config
from vix_hedge.data import build as B

NEW_VIX_GZ = config.OPTION_DATA / "VIX_option_prices_2006-2025.csv.gz"
VIX_EXT_PARQUET = config.CACHE_DIR / "vix_options_extended.parquet"
SPOT_EXT_PARQUET = config.CACHE_DIR / "spot_prices_extended.parquet"

# New full-export schema: ISO dates, empty forward_price, ticker col to isolate VIX.
_NEW_VIX_SQL = """
SELECT strptime(date,   '%Y-%m-%d')::DATE AS date,
       strptime(exdate, '%Y-%m-%d')::DATE AS exdate,
       cp_flag,
       strike_price / 1000.0          AS strike,
       (best_bid + best_offer) / 2.0  AS mid,
       delta,
       CAST(NULL AS DOUBLE)           AS forward,   -- forward_price empty in this pull
       best_bid AS bid, best_offer AS ask
FROM read_csv(?, header = true, types = {{'date': 'VARCHAR', 'exdate': 'VARCHAR'}})
WHERE ticker = 'VIX'
  AND (best_bid + best_offer) >= 0
  AND datediff('day', strptime(date,   '%Y-%m-%d')::DATE,
                      strptime(exdate, '%Y-%m-%d')::DATE) BETWEEN 0 AND {max_dte}
"""


def build_vix_options_extended() -> pd.DataFrame:
    """Clean VIX chain from the 2025 OptionMetrics refresh (2006 -> 2025-08)."""
    if not NEW_VIX_GZ.exists():
        raise FileNotFoundError(f"{NEW_VIX_GZ} missing — extract the refresh zip first")
    con = duckdb.connect()
    df = con.execute(_NEW_VIX_SQL.format(max_dte=config.MAX_DTE), [str(NEW_VIX_GZ)]).df()
    con.close()
    for c in ("date", "exdate"):
        df[c] = pd.to_datetime(df[c])
    df = B._finalize_chain(df)  # reuse: weekend-exdate snap, (date,cp,exdate,strike) dedup, dtypes

    config.ensure_dirs()
    df.to_parquet(VIX_EXT_PARQUET, index=False)
    B._report("vix_options_extended", df, VIX_EXT_PARQUET)
    return df


def build_spot_prices_extended() -> pd.DataFrame:
    """Existing spot panel (<=2020) spliced with FRED SPX + CBOE VIX for 2021+."""
    if not config.SPOT_PARQUET.exists():
        raise FileNotFoundError("build the base spot panel first (python -m vix_hedge.data.build spot)")
    base = pd.read_parquet(config.SPOT_PARQUET)
    base["date"] = pd.to_datetime(base["date"])
    cut = pd.Timestamp("2020-12-31")
    base = base[base["date"] <= cut]

    cboe = B._load_cboe_vix()                 # CBOE VIX close, full history
    fred = B._load_fred_spx()                 # FRED S&P 500 close, 2016+
    ext = pd.concat({"SPX": fred, "VIX": cboe}, axis=1, sort=True).dropna()
    ext = ext[ext.index > cut]
    ext = ext.rename_axis("date").reset_index()
    ext["date"] = pd.to_datetime(ext["date"])

    out = pd.concat([base, ext[["date", "SPX", "VIX"]]], ignore_index=True)
    out = out.sort_values("date").reset_index(drop=True)

    config.ensure_dirs()
    out.to_parquet(SPOT_EXT_PARQUET, index=False)
    B._report("spot_prices_extended", out, SPOT_EXT_PARQUET)
    return out


if __name__ == "__main__":
    build_spot_prices_extended()
    build_vix_options_extended()
