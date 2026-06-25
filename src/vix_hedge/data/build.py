"""Build cleaned parquet panels from the raw ``../option_data`` files.

Run once after cloning::

    python -m vix_hedge.data.build            # build everything
    python -m vix_hedge.data.build spot spx   # build only some panels

Outputs (under ``data/cache``):

* ``spot_prices.parquet``  -- date, SPX, VIX  (1996 -> 2020)
* ``spx_options.parquet``  -- date, exdate, cp_flag, strike, mid, delta, dte
* ``vix_options.parquet``  -- date, exdate, cp_flag, strike, mid, bid, ask,
                              delta, forward, dte

The original R pipeline rebuilt a giant nested ``arr.list`` RDS for this; here we
keep tidy columnar parquet and do the per-date pivoting lazily at load time.
"""

from __future__ import annotations

import sys

import duckdb
import numpy as np
import pandas as pd

from vix_hedge import config

# ---------------------------------------------------------------------------
# 2019-H2 spot range (2019-07..2019-11). SPX is taken from FRED, or reconstructed
# from the full-2019 option chain via European put-call parity as a fallback; VIX
# is taken from the public CBOE history (which matches the OptionMetrics VIX
# exactly on overlap).
# ---------------------------------------------------------------------------
_SPX_PARITY_SQL = """
WITH base AS (
    SELECT strptime(date::VARCHAR,  '%Y%m%d')::DATE AS date,
           strptime(exdate::VARCHAR,'%Y%m%d')::DATE AS exdate,
           cp_flag,
           strike_price / 1000.0          AS strike,
           (best_bid + best_offer) / 2.0  AS mid
    FROM read_csv(?, header = true)
    WHERE (best_bid + best_offer) > 0
      AND datediff('day', strptime(date::VARCHAR,'%Y%m%d')::DATE,
                          strptime(exdate::VARCHAR,'%Y%m%d')::DATE) BETWEEN 15 AND 50
),
c AS (SELECT date, exdate, strike, mid FROM base WHERE cp_flag = 'C'),
p AS (SELECT date, exdate, strike, mid FROM base WHERE cp_flag = 'P')
SELECT c.date, c.exdate, c.strike, c.mid - p.mid AS cmp,
       datediff('day', c.date, c.exdate) AS dte
FROM c JOIN p USING (date, exdate, strike)
"""


def _parity_forward(g: pd.DataFrame) -> float:
    """SPX forward for one day: x-intercept of (C-P) vs K on the ~30-DTE expiry's
    12 nearest-ATM strikes (European parity ``C-P = e^{-rT}(F-K)`` is exactly
    linear in K). Validated to ~16bps median (7bps in calm 2019) vs true SPX."""
    ex = g.assign(d=(g["dte"] - 30).abs()).sort_values("d")["exdate"].iloc[0]
    e = g[g["exdate"] == ex]
    e = e.assign(a=e["cmp"].abs()).sort_values("a").head(12)
    if len(e) < 4:
        return float("nan")
    b1, b0 = np.polyfit(e["strike"].to_numpy(), e["cmp"].to_numpy(), 1)  # cmp = b1*K + b0
    return float("nan") if b1 >= 0 else -b0 / b1


def _reconstruct_spx_parity(gz_path) -> pd.Series:
    """Per-day SPX spot reconstructed from an OptionMetrics chain CSV via parity."""
    con = duckdb.connect()
    df = con.execute(_SPX_PARITY_SQL, [str(gz_path)]).df()
    con.close()
    df["date"] = pd.to_datetime(df["date"])
    return df.groupby("date").apply(_parity_forward, include_groups=False).dropna().rename("SPX")


def _load_cboe_vix() -> pd.Series:
    """Public CBOE VIX daily close history, indexed by date."""
    v = pd.read_csv(config.VIX_HISTORY_CSV)
    v.columns = [c.strip().upper() for c in v.columns]
    v["DATE"] = pd.to_datetime(v["DATE"])
    return v.set_index("DATE")["CLOSE"].sort_index().rename("VIX")


def _load_fred_spx() -> pd.Series:
    """Public FRED S&P 500 daily close, indexed by date ('.' = market holiday)."""
    s = pd.read_csv(config.SP500_HISTORY_CSV, na_values=".")
    s.columns = [c.strip().upper() for c in s.columns]
    val = "SP500" if "SP500" in s.columns else s.columns[1]
    s["DATE"] = pd.to_datetime(s[s.columns[0]] if s.columns[0] != "DATE" else s["DATE"])
    return s.dropna(subset=[val]).set_index("DATE")[val].astype(float).sort_index().rename("SPX")


def _backfill_spot_gap(out: pd.DataFrame) -> pd.DataFrame:
    """Fill any interior spot days the OptionMetrics CSV lacks (the 2019-07..11 range)
    and repair bad VIX ticks.

    SPX comes from **real FRED** where present, else a put-call-parity reconstruction
    off the full-2019 chain (~5bps, validated against FRED); VIX comes from **real
    CBOE** history. Adds a row for every day a VIX *and* an SPX value exist but ``out``
    is missing, and replaces any non-positive VIX print with the CBOE value. Existing
    OptionMetrics rows are never overwritten except a non-positive VIX repair."""
    if not config.VIX_HISTORY_CSV.exists():
        print("  (skip 2019-H2 backfill: CBOE VIX history not on disk)")
        return out
    cboe = _load_cboe_vix()
    if config.SP500_HISTORY_CSV.exists():
        spx, src = _load_fred_spx(), "FRED (real)"
    elif config.SPX_OPTIONS_2019_CSV_GZ.exists():
        spx, src = _reconstruct_spx_parity(config.SPX_OPTIONS_2019_CSV_GZ), "parity (fallback)"
    else:
        print("  (skip 2019-H2 backfill: no SPX source -- FRED CSV / 2019 chain absent)")
        return out
    out = out.copy()
    out["date"] = pd.to_datetime(out["date"])

    have = set(out["date"])
    fill = sorted((set(spx.index) & set(cboe.index)) - have)
    # restrict to the actual hole (don't append spurious dates outside the panel)
    lo, hi = out["date"].min(), out["date"].max()
    fill = [d for d in fill if lo < d < hi]
    add = pd.DataFrame({"date": fill, "SPX": [float(spx[d]) for d in fill],
                        "VIX": [float(cboe[d]) for d in fill]})

    cboe_map = cboe.reindex(out["date"]).to_numpy()
    bad = (out["VIX"] <= 0).to_numpy() & pd.notna(cboe_map)
    n_repair = int(bad.sum())
    out.loc[bad, "VIX"] = cboe_map[bad]

    out = pd.concat([out, add], ignore_index=True).sort_values("date").reset_index(drop=True)
    print(f"  backfilled {len(fill)} missing spot days (2019-H2, SPX={src}) + repaired {n_repair} bad VIX tick(s)")
    return out


# ---------------------------------------------------------------------------
# Spot prices
# ---------------------------------------------------------------------------
def build_spot_prices() -> pd.DataFrame:
    """Assemble daily SPX & VIX closes for 1996 -> 2020.

    1996 -> 2019-06 comes from the OptionMetrics secid panel; December-2019 and
    2020 are recovered from the ``underlying_last`` column of the purchased option
    files (where both exist we keep the OptionMetrics close). The 2019-07..2019-11
    range is supplied by :func:`_backfill_spot_gap` (SPX from FRED or put-call parity
    off the full-2019 chain, VIX from the CBOE history) -- so the panel is continuous
    1996 -> 2020.
    """
    con = duckdb.connect()

    gz = con.execute(
        """
        SELECT strptime(date::VARCHAR, '%Y%m%d')::DATE AS date,
               max(CASE WHEN ticker = 'SPX' THEN close END) AS SPX,
               max(CASE WHEN ticker = 'VIX' THEN close END) AS VIX
        FROM read_csv(?, header = true)
        WHERE ticker IN ('SPX', 'VIX')
        GROUP BY 1
        """,
        [str(config.SPX_VIX_SPOT_GZ)],
    ).df()

    def _underlying(files: list, col: str) -> pd.DataFrame:
        paths = [str(p) for p in files if p.exists()]
        if not paths:
            return pd.DataFrame({"date": pd.to_datetime([]), col: []})
        return con.execute(
            """
            SELECT strptime(quotedate, '%m/%d/%Y')::DATE AS date,
                   max(underlying_last) AS val
            FROM read_csv(?, header = true, ignore_errors = true,
                          types = {'quotedate': 'VARCHAR'})
            GROUP BY 1
            """,
            [paths],
        ).df().rename(columns={"val": col})

    spx_files = [f for yr in config.SPX_PURCHASED.values() for f in yr]
    vix_files = [f for yr in config.VIX_PURCHASED.values() for f in yr]
    spx_p = _underlying(spx_files, "SPX")
    vix_p = _underlying(vix_files, "VIX")
    con.close()

    purchased = spx_p.merge(vix_p, on="date", how="outer")
    out = gz.merge(purchased, on="date", how="outer", suffixes=("", "_p"))
    out["SPX"] = out["SPX"].fillna(out.pop("SPX_p"))
    out["VIX"] = out["VIX"].fillna(out.pop("VIX_p"))
    out = out.dropna(subset=["SPX", "VIX"]).sort_values("date").reset_index(drop=True)
    out["date"] = pd.to_datetime(out["date"])
    out = _backfill_spot_gap(out)  # supply the 2019-07..11 range (FRED/parity SPX + CBOE VIX)

    config.ensure_dirs()
    out.to_parquet(config.SPOT_PARQUET, index=False)
    _report("spot_prices", out, config.SPOT_PARQUET)
    return out


# ---------------------------------------------------------------------------
# Option chains
# ---------------------------------------------------------------------------
_OPTIONMETRICS_SPX_SQL = """
SELECT strptime(date::VARCHAR, '%Y%m%d')::DATE   AS date,
       strptime(exdate::VARCHAR, '%Y%m%d')::DATE AS exdate,
       cp_flag,
       strike_price / 1000.0          AS strike,
       (best_bid + best_offer) / 2.0  AS mid,
       delta,
       CAST(NULL AS DOUBLE)           AS forward,
       best_bid AS bid, best_offer AS ask
FROM read_csv(?, header = true)
WHERE datediff('day', strptime(date::VARCHAR, '%Y%m%d')::DATE,
                      strptime(exdate::VARCHAR, '%Y%m%d')::DATE) BETWEEN 0 AND {max_dte}
"""

# OptionMetrics VIX panel has the extra forward_price / bid / ask we want to keep.
_OPTIONMETRICS_VIX_SQL = """
SELECT strptime(date::VARCHAR, '%Y%m%d')::DATE   AS date,
       strptime(exdate::VARCHAR, '%Y%m%d')::DATE AS exdate,
       cp_flag,
       strike_price / 1000.0          AS strike,
       (best_bid + best_offer) / 2.0  AS mid,
       delta,
       forward_price                  AS forward,
       best_bid AS bid, best_offer AS ask
FROM read_csv(?, header = true)
WHERE datediff('day', strptime(date::VARCHAR, '%Y%m%d')::DATE,
                      strptime(exdate::VARCHAR, '%Y%m%d')::DATE) BETWEEN 0 AND {max_dte}
"""

# historicaloptiondata.com purchased schema (SPX/SPXW/VIX 2019-2020).
_PURCHASED_SQL = """
SELECT date, exdate, cp_flag, strike,
       avg(mid) AS mid, avg(delta) AS delta,
       CAST(NULL AS DOUBLE) AS forward,
       avg(bid) AS bid, avg(ask) AS ask
FROM (
    SELECT strptime(quotedate, '%m/%d/%Y')::DATE  AS date,
           strptime(expiration, '%m/%d/%Y')::DATE AS exdate,
           CASE WHEN lower(type) LIKE 'c%' THEN 'C' ELSE 'P' END AS cp_flag,
           strike::DOUBLE        AS strike,
           (bid + ask) / 2.0     AS mid,
           delta, bid, ask
    FROM read_csv(?, header = true, ignore_errors = true,
                  types = {{'quotedate': 'VARCHAR', 'expiration': 'VARCHAR'}})
    WHERE datediff('day', strptime(quotedate, '%m/%d/%Y')::DATE,
                          strptime(expiration, '%m/%d/%Y')::DATE) BETWEEN 0 AND {max_dte}
)
GROUP BY date, exdate, cp_flag, strike
"""


def _report(name: str, df: pd.DataFrame, path) -> None:
    span = f"{df['date'].min().date()} -> {df['date'].max().date()}"
    print(f"{name}: {len(df):,} rows  {span}  -> {path}")


def _fix_weekend_expirations(df: pd.DataFrame) -> pd.DataFrame:
    """Snap expirations that fall on a non-trading day back to a valid trade date.

    Pre-2015 SPX monthly options are listed expiring on a Saturday (AM-settled
    against Friday's open). We shift any exdate that is not itself a quote date
    back by 1 or 2 calendar days so expiration can be valued on a real close.
    Dates in 2021+ (post-sample leftovers) are left alone.
    """
    trade_dates = set(df["date"].unique())
    exdates = pd.Series(df["exdate"].unique())
    bad = exdates[~exdates.isin(trade_dates) & (exdates.dt.year < 2021)]
    remap: dict = {}
    for ex in bad:
        if (ex - pd.Timedelta(days=1)) in trade_dates:
            remap[ex] = ex - pd.Timedelta(days=1)
        elif (ex - pd.Timedelta(days=2)) in trade_dates:
            remap[ex] = ex - pd.Timedelta(days=2)
    if remap:
        df["exdate"] = df["exdate"].map(lambda d: remap.get(d, d))
    return df


def _finalize_chain(df: pd.DataFrame) -> pd.DataFrame:
    df = _fix_weekend_expirations(df)
    # The weekend shift can collide a weekly and a (shifted) monthly onto the
    # same (date, cp, exdate, strike); average them, as the R `acast` did.
    df = (
        duckdb.query(
            """
            SELECT date, exdate, cp_flag, strike,
                   avg(mid) AS mid, avg(delta) AS delta, avg(forward) AS forward,
                   avg(bid) AS bid, avg(ask) AS ask
            FROM df
            GROUP BY date, exdate, cp_flag, strike
            """
        ).df()
    )
    df["dte"] = (df["exdate"] - df["date"]).dt.days
    df = df[df["dte"].between(0, config.MAX_DTE)]
    df = df.astype(
        {
            "cp_flag": "category",
            "strike": "float64",
            "mid": "float32",
            "delta": "float32",
            "forward": "float32",
            "bid": "float32",
            "ask": "float32",
            "dte": "int16",
        }
    )
    return df.sort_values(["date", "cp_flag", "exdate", "strike"]).reset_index(drop=True)


def build_spx_options() -> pd.DataFrame:
    """Clean SPX chain: OptionMetrics 1996-2019 + purchased SPX/SPXW 2020."""
    con = duckdb.connect()
    parts = [con.execute(_OPTIONMETRICS_SPX_SQL.format(max_dte=config.MAX_DTE), [str(config.SPX_OPTIONS_CSV)]).df()]
    for f in config.SPX_PURCHASED[2020]:
        if f.exists():
            parts.append(con.execute(_PURCHASED_SQL.format(max_dte=config.MAX_DTE), [str(f)]).df())
    con.close()

    df = pd.concat(parts, ignore_index=True)
    for c in ("date", "exdate"):
        df[c] = pd.to_datetime(df[c])
    df = _finalize_chain(df)

    config.ensure_dirs()
    df.to_parquet(config.SPX_OPTIONS_PARQUET, index=False)
    _report("spx_options", df, config.SPX_OPTIONS_PARQUET)
    return df


def build_vix_options() -> pd.DataFrame:
    """Clean VIX chain: OptionMetrics 2006-2019 + purchased VIX 2020."""
    con = duckdb.connect()
    parts = [con.execute(_OPTIONMETRICS_VIX_SQL.format(max_dte=config.MAX_DTE), [str(config.VIX_OPTIONS_GZ)]).df()]
    for f in config.VIX_PURCHASED[2020]:
        if f.exists():
            parts.append(con.execute(_PURCHASED_SQL.format(max_dte=config.MAX_DTE), [str(f)]).df())
    con.close()

    df = pd.concat(parts, ignore_index=True)
    for c in ("date", "exdate"):
        df[c] = pd.to_datetime(df[c])
    df = _finalize_chain(df)

    config.ensure_dirs()
    df.to_parquet(config.VIX_OPTIONS_PARQUET, index=False)
    _report("vix_options", df, config.VIX_OPTIONS_PARQUET)
    return df


_BUILDERS = {
    "spot": build_spot_prices,
    "spx": build_spx_options,
    "vix": build_vix_options,
}


def main(argv: list[str] | None = None) -> None:
    argv = sys.argv[1:] if argv is None else argv
    targets = argv or list(_BUILDERS)
    for t in targets:
        if t not in _BUILDERS:
            raise SystemExit(f"unknown target {t!r}; choose from {list(_BUILDERS)}")
        _BUILDERS[t]()


if __name__ == "__main__":
    main()
