"""Validate the 2025 refresh against the committed caches on their overlap.

Four checks, all over the pre-2020 OptionMetrics window where the two pulls
should be the *same source*:

1. VIX option chain — inner-join old vs new on (date, exdate, cp_flag, strike),
   compare mid & delta leg-by-leg.
2. Spot SPX — committed panel vs FRED, 2016-2020.
3. Spot VIX — committed panel vs CBOE, full overlap.
4. Forward-VIX regime signal — parity forward old vs new (drives the hedge gate).

Run: ``uv run python current_data_extension/check_agreement.py``
"""

from __future__ import annotations

import pandas as pd
from build_extended import SPOT_EXT_PARQUET, VIX_EXT_PARQUET

from vix_hedge import config, data
from vix_hedge.data import build as B
from vix_hedge.vxth.backtest import forward_vix_series


def _pct(x: float) -> str:
    return f"{100 * x:.2f}%"


def check_vix_chain() -> None:
    print("\n" + "=" * 72 + "\n1. VIX OPTION CHAIN — old vs new, leg-by-leg (2006..2019)\n" + "=" * 72)
    old = pd.read_parquet(config.VIX_OPTIONS_PARQUET)
    new = pd.read_parquet(VIX_EXT_PARQUET)
    for d in (old, new):
        d["date"] = pd.to_datetime(d["date"])
        d["exdate"] = pd.to_datetime(d["exdate"])

    hi = pd.Timestamp("2019-12-31")  # old pull is OptionMetrics only through here
    old = old[old["date"] <= hi]
    new_ov = new[new["date"] <= hi]
    print(f"old rows (<=2019): {len(old):,}   new rows (<=2019): {len(new_ov):,}")

    keys = ["date", "exdate", "cp_flag", "strike"]
    j = old.merge(new_ov, on=keys, suffixes=("_o", "_n"))
    print(f"matched legs: {len(j):,}  "
          f"({_pct(len(j)/len(old))} of old, {_pct(len(j)/len(new_ov))} of new)")

    dmid = (j["mid_n"] - j["mid_o"]).abs()
    ddel = (j["delta_n"] - j["delta_o"]).abs()
    print("\nmid  (option price, $):")
    print(f"  median |Δ| = ${dmid.median():.4f}   mean |Δ| = ${dmid.mean():.4f}   "
          f"p99 |Δ| = ${dmid.quantile(0.99):.4f}")
    print(f"  exact (|Δ|<$0.005): {_pct((dmid < 0.005).mean())}   corr = {j['mid_o'].corr(j['mid_n']):.6f}")
    print("delta:")
    print(f"  median |Δ| = {ddel.median():.5f}   mean |Δ| = {ddel.mean():.5f}   "
          f"exact (<0.001): {_pct((ddel < 0.001).mean())}")

    # coverage per year (legs after DTE<=120 filter)
    print("\nleg coverage per year (old -> new):")
    co = old.assign(yr=old["date"].dt.year).groupby("yr").size()
    cn = new.assign(yr=new["date"].dt.year).groupby("yr").size()
    cov = pd.concat({"old": co, "new": cn}, axis=1).fillna(0).astype(int)
    print(cov.to_string())


def check_spot() -> None:
    print("\n" + "=" * 72 + "\n2/3. SPOT — committed panel vs public sources\n" + "=" * 72)
    panel = pd.read_parquet(config.SPOT_PARQUET)
    panel["date"] = pd.to_datetime(panel["date"])
    panel = panel.set_index("date")

    cboe = B._load_cboe_vix()
    fred = B._load_fred_spx()

    # VIX vs CBOE over the whole committed span
    vj = pd.concat({"panel": panel["VIX"], "cboe": cboe}, axis=1, sort=True).dropna()
    dv = (vj["panel"] - vj["cboe"]).abs()
    vc = vj["panel"].corr(vj["cboe"])
    print(f"VIX  panel vs CBOE  ({vj.index.min().date()}..{vj.index.max().date()}, n={len(vj):,}): "
          f"median |Δ| = {dv.median():.4f} vol pts, p99 = {dv.quantile(0.99):.4f}, corr = {vc:.6f}")

    # SPX vs FRED over the FRED overlap (2016+)
    sj = pd.concat({"panel": panel["SPX"], "fred": fred}, axis=1, sort=True).dropna()
    rel = ((sj["panel"] - sj["fred"]).abs() / sj["fred"])
    sc = sj["panel"].corr(sj["fred"])
    print(f"SPX  panel vs FRED  ({sj.index.min().date()}..{sj.index.max().date()}, n={len(sj):,}): "
          f"median |rel Δ| = {_pct(rel.median())}, p99 = {_pct(rel.quantile(0.99))}, corr = {sc:.6f}")

    # continuity at the 2020/2021 splice in the extended panel
    ext = pd.read_parquet(SPOT_EXT_PARQUET)
    ext["date"] = pd.to_datetime(ext["date"])
    span = ext[(ext["date"] >= "2020-12-20") & (ext["date"] <= "2021-01-10")]
    print("\nextended-panel splice (2020-12 -> 2021-01):")
    print(span.to_string(index=False))


def check_signal() -> None:
    print("\n" + "=" * 72 + "\n4. FORWARD-VIX PARITY SIGNAL — old vs new (drives the regime gate)\n" + "=" * 72)
    spot = data.load_spot_prices()
    old_chain = data.load_vix_chain()
    new_chain = data.OptionChain(pd.read_parquet(VIX_EXT_PARQUET, columns=data.load.__dict__["_COLS"]))
    fo = forward_vix_series(old_chain, spot)
    fn = forward_vix_series(new_chain, spot)
    j = pd.concat({"old": fo, "new": fn}, axis=1).dropna()
    j = j[j.index <= "2019-12-31"]
    d = (j["new"] - j["old"]).abs()
    print(f"parity forward VIX, overlap n={len(j):,}: median |Δ| = {d.median():.4f} vol pts, "
          f"p99 = {d.quantile(0.99):.4f}, corr = {j['old'].corr(j['new']):.6f}")


if __name__ == "__main__":
    check_vix_chain()
    check_spot()
    check_signal()
    print("\ndone.")
