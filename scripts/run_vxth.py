"""Run the VXTH (VIX tail-hedge) replication and compare to the CBOE index.

A thin standalone driver over :func:`vix_hedge.vxth.engine.simulate` (the one
sleeve-driven engine): it runs the unhedged SPX base and a 30/60/90 VIX-call
ladder from the same start, then overlays the official CBOE VXTH index.

Usage::

    uv run python scripts/run_vxth.py
    uv run python scripts/run_vxth.py --signal spot --delta 0.10 --start 2006-03-22

Outputs (under ``results/vxth/``): metrics table, equity-curve & drawdown PNGs,
and the portfolio curves (incl. the official VXTH benchmark).
"""

from __future__ import annotations

import argparse

import pandas as pd

from vix_hedge import config, data, metrics, plotting
from vix_hedge.vxth import benchmark
from vix_hedge.vxth.engine import ALLOC_OFFICIAL, BASES, HedgeConfig, simulate

LADDER = (35, 65, 95)  # 30/60/90-day tenor ladder (nearest-DTE picks)


def main() -> None:
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--signal", choices=["forward", "spot"], default="forward",
                   help="regime signal: option forward VIX (default) or spot VIX")
    p.add_argument("--delta", type=float, default=0.10, help="target call delta")
    p.add_argument("--start", default=None)
    p.add_argument("--end", default=None)
    args = p.parse_args()

    spot = data.load_spot_prices()
    chain = data.load_vix_chain()
    base = BASES["SPX"]

    cfg = HedgeConfig(delta=args.delta, ladder_dtes=LADDER, alloc=ALLOC_OFFICIAL,
                      signal=args.signal, label="SPX/VIXOPT")
    hedged = simulate(spot, chain, base=base, cfg=cfg, start=args.start, end=args.end)
    # Run the benchmark from the hedge's first tradeable date so both curves start
    # at the same dollar balance (an apples-to-apples comparison).
    start0 = str(hedged.index[0].date())
    spx = simulate(spot, chain, base=base, cfg=None, start=start0, end=args.end).rename("SPX")

    curves = pd.concat([spx, hedged], axis=1)
    # attach the official VXTH index, rescaled to the same start
    curves["VXTH (official)"] = benchmark.vxth_curve(
        benchmark.load_vxth_index(), curves.index, config.STARTING_BALANCE)

    stats = metrics.summary(curves)
    print(f"\nVXTH replication  (ladder={LADDER}, delta={args.delta}, signal={args.signal})")
    print(f"  span : {curves.index.min().date()} -> {curves.index.max().date()}  ({len(curves)} days)")
    print(f"  final $ per ${config.STARTING_BALANCE:,.0f}: "
          + ", ".join(f"{c}={curves[c].iloc[-1]:,.0f}" for c in curves.columns))
    print("\n" + stats.round(4).to_string())

    outdir = config.RESULTS_DIR / "vxth"
    outdir.mkdir(parents=True, exist_ok=True)
    tag = f"{args.signal}_d{args.delta}"
    stats.round(6).to_csv(outdir / f"metrics_{tag}.csv")
    curves.to_csv(outdir / f"curves_{tag}.csv")
    plotting.equity_curves(curves, f"VXTH replication vs official index ({tag})", outdir / f"equity_{tag}.png")
    plotting.drawdowns(curves, f"Drawdowns ({tag})", outdir / f"drawdown_{tag}.png")
    print(f"\nwrote metrics/curves + plots -> {outdir}")


if __name__ == "__main__":
    main()
