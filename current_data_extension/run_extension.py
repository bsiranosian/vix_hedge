"""Re-run the headline VIX-call study (delta dial × 30/60/90 ladder) on the
2025-refreshed data, 2006 -> 2025-08.

Mirrors scripts/run_delta_ladder_report.py exactly — same engine, configs,
metrics, frictionless base, hybrid_vx1 forward-VIX regime signal — but on the
extended panels (current_data_extension/build_extended.py) and over three windows
plus the new out-of-sample crashes:

* 2006-03-22 .. 2020-12-31   — validation / in-sample (matches the original report, which ended 2020)
* 2006-03-22 .. 2025-08-29   — full extended sample
* 2021-01-01 .. 2025-08-29   — out-of-sample only (the years past the original report)

New crash windows the refresh unlocks: 2022 bear, Aug-2024 unwind, 2025 tariff.

Run: ``uv run python current_data_extension/run_extension.py``
  (writes JSON + markdown tables under results/current_data_extension/).
"""

from __future__ import annotations

import json
import time

import pandas as pd
from build_extended import SPOT_EXT_PARQUET, VIX_EXT_PARQUET

from vix_hedge import config, metrics
from vix_hedge.data.load import _COLS, OptionChain
from vix_hedge.vxth import episodes as ep
from vix_hedge.vxth import lag_jitter as lj
from vix_hedge.vxth import roll_phase as rp
from vix_hedge.vxth.backtest import forward_vix_signal
from vix_hedge.vxth.engine import ALLOC_OFFICIAL, BASES, HedgeConfig, simulate
from vix_hedge.vxth.ensemble import ensemble as run_ensemble

DELTAS = (0.50, 0.30, 0.20, 0.10, 0.05)
LADDER = (30, 60, 90)
START = "2006-03-22"

# crash windows: the four committed ones + the three the refresh unlocks.
NEW_EPISODES = {
    "2022 Bear": ("2022-01-03", "2022-10-12"),
    "Aug-2024 unwind": ("2024-07-16", "2024-08-05"),
    "2025 Tariff": ("2025-02-19", "2025-04-08"),
}
ALL_EPISODES = {**ep.CRASH_EPISODES, **NEW_EPISODES}

OUT = config.RESULTS_DIR / "current_data_extension"

# protected underlying -> (spot panel, base weights, output json).
BASE_SPEC = {
    "SPX": (SPOT_EXT_PARQUET, BASES["SPX"], "extension_metrics.json"),
}


def metrics_row(c: pd.Series, episodes: dict) -> dict:
    row = {
        "CAGR %": 100 * metrics.cagr(c),
        "Sharpe": metrics.annualized_sharpe(c),
        "Sortino": metrics.sortino(c),
        "Vol %": 100 * metrics.annualized_stddev(c),
        "MaxDD %": 100 * metrics.max_drawdown(c),
    }
    for name, (s, e) in episodes.items():
        row[name] = 100 * ep.window_return(c, s, e)
    return row


def run_window(spot, chain, sig, *, base, base_label, start, end, episodes) -> pd.DataFrame:
    def run(cfg):
        return simulate(spot, chain, base=base, cfg=cfg, start=start, end=end, signal_series=sig)

    curves = {f"{base_label} unhedged": run(None)}
    for d in DELTAS:  # single calls roll on the monthly VIX calendar (VXTH's roll), not weeklys
        curves[f"single 30d {int(d*100)}Δ"] = run(HedgeConfig(d, (30,), ALLOC_OFFICIAL, params={"expiry": "monthly"}))
    for d in DELTAS:
        curves[f"ladder 30/60/90 {int(d*100)}Δ"] = run(HedgeConfig(d, LADDER, ALLOC_OFFICIAL))
    return pd.DataFrame({k: metrics_row(v, episodes) for k, v in curves.items()}).T


def robustness(spot, chain, sig, *, base, start, end) -> pd.DataFrame:
    """Entry-date timing-luck spread for single 30Δ vs the 30/60/90 ladder."""
    rows = {}
    for label, cfg in {
        "single 30d 30Δ": HedgeConfig(0.30, (30,), ALLOC_OFFICIAL, params={"expiry": "monthly"}),
        "ladder 30/60/90 50Δ": HedgeConfig(0.50, LADDER, ALLOC_OFFICIAL),
        "ladder 30/60/90 30Δ": HedgeConfig(0.30, LADDER, ALLOC_OFFICIAL),
        "ladder 30/60/90 10Δ": HedgeConfig(0.10, LADDER, ALLOC_OFFICIAL),
        "ladder 30/60/90 5Δ": HedgeConfig(0.05, LADDER, ALLOC_OFFICIAL),
    }.items():
        e = run_ensemble(spot, chain, base=base, cfg=cfg, n_offsets=21,
                         start=start, end=end, signal_series=sig)
        tl = e["timing_luck"]
        tr = e["tranched"]
        rows[label] = {
            "CAGR range (pp)": tl["CAGR % range"],
            "Sharpe range": tl["Sharpe range"],
            "tranched CAGR %": 100 * metrics.cagr(tr),
            "tranched Sharpe": metrics.annualized_sharpe(tr),
        }
    return pd.DataFrame(rows).T


def roll_phase_robustness(spot, chain, sig, *, base) -> dict:
    """Roll-PHASE timing luck — the axis the entry-date ensemble above can't see. A single
    30Δ call by expiry *track* (offset days from the monthly VIX settlement; 0 = VXTH's
    monthly calendar) vs the phase-invariant 30/60/90 ladder under the same cadence shift,
    over 2017-2021 (weeklys populate every track and the window holds COVID). The single's
    COVID payoff swings ~50pp across one-week offsets; the ladder is flat."""
    s, e = "2017-01-01", "2021-12-31"
    single = rp.phase_sweep(spot, chain, base=base, signal_series=sig,
                            cfg=HedgeConfig(0.30, (30,), ALLOC_OFFICIAL), start=s, end=e)
    ladder = rp.cadence_sweep(spot, chain, base=base, signal_series=sig,
                              cfg=HedgeConfig(0.30, LADDER, ALLOC_OFFICIAL), start=s, end=e)
    return {"window": [s, e], "single": single.round(2).to_dict(orient="index"),
            "ladder": ladder.round(2).to_dict(orient="index")}


def gate_signal_robustness(spot, chain, *, base) -> dict:
    """Gate-READ timing luck — the third axis (entry-date ①, roll-phase ②, gate-read ③).
    The 4-level gate is read on the monthly settlement = the VX1 roll day, where the
    front-month future sawtooths across the 15 boundary. Reading VX1 one day stale flips
    the whole sleeve off going into COVID; a constant-maturity forward (CMF30) is smooth, so
    the same read is lag-robust. Reports the headline 50Δ ladder's metrics as the gate signal
    is shifted ±2 trading days, under VX1 vs CMF30 (full sample); the gate gates every
    instrument, so this applies to the single call too."""
    s, e = START, "2025-08-29"
    cfg = HedgeConfig(0.50, LADDER, ALLOC_OFFICIAL)
    lags = (-2, -1, 0, 1, 2)

    def jitter(source: str) -> dict:
        sig = forward_vix_signal(chain, spot, source=source)
        r = lj.lag_jitter(spot, chain, base=base, cfg=cfg, signal_series=sig, lags=lags, start=s, end=e)
        pl = r["per_lag"]
        t = pd.DataFrame({"CAGR %": 100 * pl["cagr"], "Sharpe": pl["sharpe"], "COVID 2020 %": 100 * pl["covid"]})
        t.loc["range"] = [100 * r["spread"]["cagr"], r["spread"]["sharpe"], 100 * r["spread"]["covid"]]
        t.index = [str(i) for i in t.index]
        return t.round(2).to_dict(orient="index")

    return {"window": [s, e], "config": "ladder 30/60/90 50Δ",
            "vx1": jitter("hybrid_vx1"), "cmf30": jitter("hybrid_cmf30")}


def main(base_name: str = "SPX") -> None:
    t0 = time.time()
    spot_parquet, base, out_json = BASE_SPEC[base_name]
    spot = pd.read_parquet(spot_parquet)
    spot["date"] = pd.to_datetime(spot["date"])
    spot = spot.set_index("date").sort_index()
    chain = OptionChain(pd.read_parquet(VIX_EXT_PARQUET, columns=_COLS))
    # hybrid_vx1 = real CBOE VX1 future where cached (2013..2021), parity elsewhere
    # (post-2021 the futures cache is absent, so the signal is parity there — which
    # check_agreement showed reproduces the old forward exactly on the overlap).
    # The signal depends only on VIX, so it is identical for the SPX and NDX books.
    sig = forward_vix_signal(chain, spot, source="hybrid_vx1")

    windows = {
        "validation_2006_2020": (START, "2020-12-31"),
        "full_2006_2025": (START, "2025-08-29"),
        "oos_2021_2025": ("2021-01-01", "2025-08-29"),  # OOS = the years past the original report (ended 2020)
    }
    out: dict[str, dict] = {}
    for name, (s, e) in windows.items():
        eps = ALL_EPISODES if name != "validation_2006_2020" else ep.CRASH_EPISODES
        tbl = run_window(spot, chain, sig, base=base, base_label=base_name, start=s, end=e, episodes=eps)
        out[name] = {"window": [s, e], "metrics": tbl.round(2).to_dict(orient="index")}
        print(f"\n{'='*100}\n{base_name}  {name}   {s} .. {e}\n{'='*100}")
        print(tbl.to_string(float_format=lambda x: f"{x:8.2f}"))

    print(f"\n{'='*100}\nROBUSTNESS — entry-date timing luck (full 2006-2025)\n{'='*100}")
    rob = robustness(spot, chain, sig, base=base, start=START, end="2025-08-29")
    print(rob.to_string(float_format=lambda x: f"{x:8.3f}"))
    out["robustness_full"] = rob.round(3).to_dict(orient="index")

    print(f"\n{'='*100}\nROLL-PHASE timing luck — single call by expiry track (2017-2021)\n{'='*100}")
    rpr = roll_phase_robustness(spot, chain, sig, base=base)
    out["roll_phase"] = rpr
    print("single 30Δ by offset (days from monthly):")
    print(pd.DataFrame(rpr["single"]).T.to_string(float_format=lambda x: f"{x:8.2f}"))
    print("\n30/60/90 ladder, same cadence shift (full chain):")
    print(pd.DataFrame(rpr["ladder"]).T.to_string(float_format=lambda x: f"{x:8.2f}"))

    print(f"\n{'='*100}\nGATE-READ timing luck — 50Δ ladder, signal lag jitter under VX1 vs CMF30\n{'='*100}")
    gsr = gate_signal_robustness(spot, chain, base=base)
    out["gate_robustness"] = gsr
    print("VX1 gate (front-month future):")
    print(pd.DataFrame(gsr["vx1"]).T.to_string(float_format=lambda x: f"{x:8.2f}"))
    print("\nCMF30 gate (constant-maturity):")
    print(pd.DataFrame(gsr["cmf30"]).T.to_string(float_format=lambda x: f"{x:8.2f}"))
    out["base"] = base_name

    OUT.mkdir(parents=True, exist_ok=True)
    (OUT / out_json).write_text(json.dumps(out, indent=2))
    print(f"\nran in {time.time()-t0:.0f}s -> {OUT/out_json}")


if __name__ == "__main__":
    main("SPX")
