"""Timing luck on two axes: entry date and **roll phase**.

Part 1 — entry-date ensemble. For each config, run it across staggered
(one-per-trading-day) entry dates spanning a monthly roll cycle, then report:

* **Measure:** the *distribution* of CAGR / Sharpe / max-drawdown / crash-window
  payoff across cohorts -- median, 10/90 pct, IQR, range -- not a point estimate.
* **Fix:** the residual spread of the N-tranche strategy (equal-weight mean of
  N de-phased cohorts) vs N -- it shrinks ~1/N toward zero at ~no cost to mean CAGR.

The headline of Part 1 is that entry-date luck is *small*: this hedge resizes to the
regime allocation at every roll, so cohorts re-sync and the full-window spread is
~0.1pp for the single call *and* the ladder. Where you start barely matters.

Part 2 — roll phase (:mod:`vix_hedge.vxth.roll_phase`). The axis that actually breaks a
single call isn't the entry date, it's **which expiry it holds into a spike**. On a
weekly-inclusive chain, rolling a single call one week off VXTH's monthly settlement is
the difference between catching COVID (+29%) and missing it (−18%): a ~50pp swing from a
one-week offset. The 30/60/90 ladder spans tenors on the full chain, so the same cadence
shift moves it ~0. That contrast is the robustness case for the ladder.

Usage::

    uv run python scripts/run_ensemble_analysis.py
    uv run python scripts/run_ensemble_analysis.py --start 2018-01-01 --end 2020-12-31 --n 21
"""

from __future__ import annotations

import argparse
import time

import pandas as pd

from vix_hedge import config, data
from vix_hedge.vxth import BASES, HedgeConfig, roll_phase
from vix_hedge.vxth.backtest import forward_vix_signal
from vix_hedge.vxth.engine import ALLOC_OFFICIAL
from vix_hedge.vxth.ensemble import per_offset_metrics, run_cohorts_td, timing_luck, tranche_robustness

# The two configs the roadmap names: a single 30d 30Δ call (timing-luck-dominated)
# vs the 30/60/90 ladder (already partly diversified across tenor).
CONFIGS = [
    HedgeConfig(0.30, (30,), label="single 30d 30Δ", params={"expiry": "monthly"}),  # VXTH monthly roll
    HedgeConfig(0.30, (30, 60, 90), label="ladder 30/60/90 30Δ"),
    HedgeConfig(0.10, (30, 60, 90), label="ladder 30/60/90 10Δ"),  # the report's headline delta
]
N_VALUES = (1, 3, 7, 21)


def distribution_table(per: pd.DataFrame) -> pd.DataFrame:
    """Per-metric distribution across cohorts: median, 10/90 pct, IQR, range."""
    d = per.describe(percentiles=[0.1, 0.25, 0.5, 0.75, 0.9]).T
    d["IQR"] = d["75%"] - d["25%"]
    d["range"] = d["max"] - d["min"]
    return d[["50%", "10%", "90%", "IQR", "range", "mean", "std"]].rename(columns={"50%": "median"})


def main() -> None:
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--start", default="2006-03-22")
    p.add_argument("--end", default=None)
    p.add_argument("--n", type=int, default=21, help="cohorts (trading-day offsets across the roll cycle)")
    p.add_argument("--signal", default="hybrid_vx1", choices=["hybrid_vx1", "hybrid_cmf30", "parity"])
    p.add_argument("--base", default="SPX", choices=list(BASES))
    args = p.parse_args()

    spot = data.load_spot_prices()
    chain = data.load_vix_chain()
    sig = forward_vix_signal(chain, spot, source=args.signal)
    base = BASES[args.base]

    outdir = config.RESULTS_DIR / "ensemble"
    outdir.mkdir(parents=True, exist_ok=True)

    print(f"base={args.base}  signal={args.signal}  cohorts={args.n}  window {args.start}..{args.end or 'end'}\n")
    for cfg in CONFIGS:
        t = time.time()
        pool = run_cohorts_td(spot, chain, base=base, cfg=cfg, n=args.n,
                              start=args.start, end=args.end, signal_series=sig)
        per = per_offset_metrics(pool)
        dist = distribution_table(per)
        sweep = tranche_robustness(pool, N_VALUES)
        tl = timing_luck(per)
        dt = time.time() - t

        slug = cfg.label.replace(" ", "_").replace("/", "-").replace("Δ", "d")
        per.to_csv(outdir / f"{slug}_per_offset.csv")
        dist.to_csv(outdir / f"{slug}_distribution.csv")
        sweep.to_csv(outdir / f"{slug}_tranche_sweep.csv")

        print(f"=== {cfg.label}  ({len(pool)} cohorts, {dt:.1f}s) ===")
        print("Measure — per-cohort distribution (entry-date timing luck):")
        print(distribution_table(per).to_string(float_format=lambda x: f"{x:8.3f}"))
        print(f"\n  single-cohort spread:  CAGR range {tl['CAGR % range']:.2f}pp "
              f"(IQR {tl['CAGR % IQR']:.2f}pp) · Sharpe range {tl['Sharpe range']:.3f}")
        print("\nFix — N-tranche strategy: residual spread vs N (the 1/N fix):")
        print(sweep.to_string(float_format=lambda x: f"{x:8.4f}"))
        red = sweep["CAGR range"].iloc[0] / max(sweep["CAGR range"].iloc[-2], 1e-9)
        print(f"\n  CAGR-range reduction 1→{N_VALUES[-2]} tranches: {red:.1f}x   "
              f"mean-CAGR drift across N: {sweep['CAGR mean'].max() - sweep['CAGR mean'].min():.2f}pp\n")

    # ----- Part 2: roll-phase timing luck (the axis that actually breaks a single call) ----- #
    rp_start, rp_end = "2017-01-01", "2021-12-31"  # weeklys populate every track here, window holds COVID
    print(f"\n{'='*78}\nPART 2 — ROLL-PHASE TIMING LUCK   (window {rp_start}..{rp_end})\n{'='*78}")
    print("A single 30Δ call by expiry track — offset_days from the monthly VIX settlement\n"
          "(0 = the VXTH monthly track; ±7 = the weekly ~1 week off it):")
    single = roll_phase.phase_sweep(spot, chain, base=base, signal_series=sig,
                                    cfg=HedgeConfig(0.30, (30,), ALLOC_OFFICIAL, label="single"),
                                    start=rp_start, end=rp_end)
    print(single.to_string(float_format=lambda x: f"{x:8.1f}"))
    print("\nThe 30/60/90 ladder under the SAME cadence shift, kept on the full chain "
          "(so it still spans tenors):")
    ladder = roll_phase.cadence_sweep(spot, chain, base=base, signal_series=sig,
                                      cfg=HedgeConfig(0.30, (30, 60, 90), ALLOC_OFFICIAL, label="ladder"),
                                      start=rp_start, end=rp_end)
    print(ladder.to_string(float_format=lambda x: f"{x:8.1f}"))
    sc, lc = single.loc["range", "COVID 2020 %"], ladder.loc["range", "COVID 2020 %"]
    print(f"\n  COVID-payoff range across a one-week roll offset:  single {sc:.0f}pp  vs  ladder {lc:.0f}pp")
    print("  -> entry date barely matters (~0.1pp, Part 1); ROLL PHASE is the lever — a single call's "
          "COVID\n     outcome flips on a one-week offset, while the tenor ladder is ~invariant to it.")
    single.to_csv(outdir / "roll_phase_single.csv")
    ladder.to_csv(outdir / "roll_phase_ladder_cadence.csv")

    print(f"\nwrote per-config CSVs -> {outdir}")


if __name__ == "__main__":
    main()
