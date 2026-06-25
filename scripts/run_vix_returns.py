"""VIX regime transitions + VIX-call return distribution.

Usage::

    uv run python scripts/run_vix_returns.py
    uv run python scripts/run_vix_returns.py --dte 30 --delta 0.1

Outputs (under ``results/vix_returns/``): the transition matrix, a histogram of
held-to-expiry call max returns, and the per-contract return table.
"""

from __future__ import annotations

import argparse

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402

from vix_hedge import config, data  # noqa: E402
from vix_hedge import vix_returns as vr  # noqa: E402


def main() -> None:
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--dte", type=int, default=120, help="target days-to-expiry for the call")
    p.add_argument("--delta", type=float, default=0.10, help="target call delta")
    args = p.parse_args()

    spot = data.load_spot_prices()

    # --- 1. regime transitions (full SPX/VIX history) ---------------------
    regime = vr.regime_series(spot["VIX"])
    tm = vr.transition_matrix(regime)
    print("VIX regime transition counts (thresholds 15/30/50):")
    print(tm.to_string())
    print("\nregime day counts:", regime.value_counts().sort_index().to_dict())

    # --- 2. VIX-call return distribution ----------------------------------
    chain = data.load_vix_chain()
    stats = vr.option_return_stats(chain, spot, dte=args.dte, delta=args.delta)
    mr = stats["max_returns"]
    print(f"\nHeld-to-expiry VIX calls  (dte={args.dte}, delta={args.delta}):")
    print(f"  contracts: {stats['n_contracts']}   worthless: {stats['n_worthless']} "
          f"({100 * stats['frac_worthless']:.1f}%)")
    print(f"  max-return median: {mr['max_return'].median():.2f}x   mean: {mr['max_return'].mean():.2f}x")
    print("  exceedance:", {f"{m}x": f"{c} ({stats['exceed_pct'][m]}%)" for m, c in stats["exceed_counts"].items()})

    outdir = config.RESULTS_DIR / "vix_returns"
    outdir.mkdir(parents=True, exist_ok=True)
    tm.to_csv(outdir / "transition_matrix.csv")
    mr.to_csv(outdir / f"max_returns_dte{args.dte}_d{args.delta}.csv", index=False)

    fig, ax = plt.subplots(figsize=(9, 5))
    ax.hist(mr["max_return"].clip(upper=50), bins=60, color="steelblue", edgecolor="white")
    ax.set_title(f"Max return of {args.dte}-DTE {args.delta}-delta VIX calls (clipped at 50x)")
    ax.set_xlabel("max return (x entry)")
    ax.set_ylabel("count")
    ax.grid(True, alpha=0.3)
    fig.tight_layout()
    fig.savefig(outdir / f"hist_dte{args.dte}_d{args.delta}.png", dpi=120)
    plt.close(fig)
    print(f"\nwrote transition matrix, return table, histogram -> {outdir}")


if __name__ == "__main__":
    main()
