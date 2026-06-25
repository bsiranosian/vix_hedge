"""Equity-curve + drawdown chart for the extended (2006->2025) headline configs."""

from __future__ import annotations

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import pandas as pd
from build_extended import SPOT_EXT_PARQUET, VIX_EXT_PARQUET

from vix_hedge import config, metrics
from vix_hedge.data.load import _COLS, OptionChain
from vix_hedge.vxth.backtest import forward_vix_signal
from vix_hedge.vxth.engine import ALLOC_OFFICIAL, BASES, HedgeConfig, simulate

OUT = config.RESULTS_DIR / "current_data_extension"


def main() -> None:
    spot = pd.read_parquet(SPOT_EXT_PARQUET)
    spot["date"] = pd.to_datetime(spot["date"])
    spot = spot.set_index("date").sort_index()
    chain = OptionChain(pd.read_parquet(VIX_EXT_PARQUET, columns=_COLS))
    sig = forward_vix_signal(chain, spot, source="hybrid_vx1")

    def run(cfg):
        return simulate(spot, chain, base=BASES["SPX"], cfg=cfg, start="2006-03-22",
                        end="2025-08-29", signal_series=sig)

    series = {
        "SPX unhedged": run(None),
        "single 30d 30Δ": run(HedgeConfig(0.30, (30,), ALLOC_OFFICIAL, params={"expiry": "monthly"})),
        "ladder 30/60/90 50Δ": run(HedgeConfig(0.50, (30, 60, 90), ALLOC_OFFICIAL)),
        "ladder 30/60/90 30Δ": run(HedgeConfig(0.30, (30, 60, 90), ALLOC_OFFICIAL)),
        "ladder 30/60/90 5Δ": run(HedgeConfig(0.05, (30, 60, 90), ALLOC_OFFICIAL)),
    }
    colors = {"SPX unhedged": "#444", "single 30d 30Δ": "#1f77b4",
              "ladder 30/60/90 50Δ": "#2ca02c", "ladder 30/60/90 30Δ": "#ff7f0e",
              "ladder 30/60/90 5Δ": "#d62728"}

    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(12, 8), height_ratios=[3, 1.4], sharex=True)
    for name, c in series.items():
        ax1.plot(c.index, c / c.iloc[0], label=name, color=colors[name],
                 lw=2 if "unhedged" in name else 1.4)
        ax2.plot(c.index, 100 * metrics.drawdown_curve(c), color=colors[name],
                 lw=2 if "unhedged" in name else 1.2)
    ax1.set_yscale("log")
    ax1.set_ylabel("growth of $1 (log)")
    ax1.legend(loc="upper left", fontsize=9)
    ax1.set_title("VIX-call tail hedge on the 2025-refreshed data — SPX base, frictionless, 2006-03 → 2025-08")
    ax1.grid(alpha=0.3, which="both")
    # shade the OOS extension period
    oos_end = next(iter(series.values())).index[-1]
    for ax in (ax1, ax2):
        ax.axvspan(pd.Timestamp("2021-01-01"), oos_end, color="gold", alpha=0.08)
    ax2.set_ylabel("drawdown %")
    ax2.grid(alpha=0.3)
    ax2.annotate("OOS extension\n(beyond old cache)", xy=(pd.Timestamp("2023-01-01"), ax2.get_ylim()[0] * 0.8),
                 fontsize=8, color="#777")
    fig.tight_layout()
    OUT.mkdir(parents=True, exist_ok=True)
    fig.savefig(OUT / "equity_curves.png", dpi=110)
    print(f"wrote {OUT/'equity_curves.png'}")


if __name__ == "__main__":
    main()
