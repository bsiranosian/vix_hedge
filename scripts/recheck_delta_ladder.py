"""Delta / laddering conclusions, with one consolidated table per structure.

Three questions:

1. **Option delta (§4.3).** Sweep the call delta 50 → 5 on a *single* 30-day call and
   on the *30/60/90 ladder*. The report's framing is "lower delta = cheaper + more
   convex"; this re-check asks the complementary question the user raised — does the
   **high (≈50Δ) call raise Sharpe and *smooth* returns**? A 50Δ call is far less of a
   lottery ticket (it is usually worth something), so it should cut volatility and
   entry-date dispersion at the cost of CAGR.
2. **Laddering (§4.5).** Single 30d call vs the 30/60/90 ladder at each delta.
3. **Robustness / timing luck.** Every cell is *also* run across 21 staggered
   entry-date cohorts (one per trading day over a roll cycle), so we report the
   *distribution* (mean ± spread of Sharpe / CAGR / COVID payoff), not one lucky draw.
   "Smoother" = lower vol, lower MaxDD, **and** a tighter cohort spread.

Frictionless, SPX base, hybrid_vx1 forward-VIX signal (the README-validated default).

Usage::

    uv run python scripts/recheck_delta_ladder.py            # full grid, 21 cohorts
    uv run python scripts/recheck_delta_ladder.py --cohorts 11
"""

from __future__ import annotations

import argparse
import time

import pandas as pd

from vix_hedge import config, data, metrics
from vix_hedge.vxth import BASES
from vix_hedge.vxth import episodes as ep
from vix_hedge.vxth.backtest import forward_vix_signal
from vix_hedge.vxth.engine import ALLOC_OFFICIAL, HedgeConfig, simulate
from vix_hedge.vxth.ensemble import per_offset_metrics, run_cohorts_td

DELTAS = (0.50, 0.30, 0.20, 0.10, 0.05)
STRUCTURES = {"single 30d": (30,), "ladder 30/60/90": (30, 60, 90)}


def _point_row(c: pd.Series) -> dict:
    """Headline metrics for one curve (a single entry-date draw)."""
    return {
        "CAGR %": 100 * metrics.cagr(c),
        "Sharpe": metrics.annualized_sharpe(c),
        "Sortino": metrics.sortino(c),
        "AnnVol %": 100 * metrics.annualized_stddev(c),
        "MaxDD %": 100 * metrics.max_drawdown(c),
        "GFC %": 100 * ep.window_return(c, *ep.CRASH_EPISODES["GFC 2008-09"]),
        "Q4-18 %": 100 * ep.window_return(c, *ep.CRASH_EPISODES["Q4 2018"]),
        "COVID %": 100 * ep.window_return(c, *ep.CRASH_EPISODES["COVID 2020"]),
    }


def _ensemble_row(per: pd.DataFrame) -> dict:
    """Cohort distribution: mean and spread across the 21 entry dates."""
    cov = per["ep:COVID 2020"]
    return {
        "Sharpe mean": per["Sharpe"].mean(),
        "Sharpe std": per["Sharpe"].std(ddof=1),
        "Sharpe rng": per["Sharpe"].max() - per["Sharpe"].min(),
        "CAGR mean %": per["CAGR %"].mean(),
        "CAGR rng pp": per["CAGR %"].max() - per["CAGR %"].min(),
        "COVID mean %": cov.mean(),
        "COVID std pp": cov.std(ddof=1),
        "% caught COVID": 100 * float((cov > 5).mean()),  # cohorts whose option actually paid
    }


def main() -> None:
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--start", default="2006-03-22")
    p.add_argument("--end", default=None)
    p.add_argument("--cohorts", type=int, default=21, help="entry-date cohorts for the robustness read")
    p.add_argument("--signal", default="hybrid_vx1", choices=["hybrid_vx1", "hybrid_cmf30", "parity"])
    args = p.parse_args()

    spot = data.load_spot_prices()
    chain = data.load_vix_chain()
    sig = forward_vix_signal(chain, spot, source=args.signal)
    base = BASES["SPX"]
    outdir = config.RESULTS_DIR / "recheck_delta_ladder"
    outdir.mkdir(parents=True, exist_ok=True)

    spx = simulate(spot, chain, base=base, cfg=None, start=args.start, end=args.end, signal_series=sig)
    print(f"Delta / ladder re-check — base=SPX  signal={args.signal}  "
          f"{args.start}..{args.end or 'end'}  cohorts={args.cohorts}")
    print(f"reference SPX (unhedged): CAGR {100 * metrics.cagr(spx):.2f}%  "
          f"Sharpe {metrics.annualized_sharpe(spx):.2f}  MaxDD {100 * metrics.max_drawdown(spx):.0f}%  "
          f"COVID {100 * ep.window_return(spx, *ep.CRASH_EPISODES['COVID 2020']):.1f}%\n")

    point: dict[str, dict] = {}
    ens: dict[str, dict] = {}
    t0 = time.time()
    for sname, dtes in STRUCTURES.items():
        for d in DELTAS:
            key = f"{sname}  {int(d * 100)}Δ"
            # single calls roll on the monthly VIX calendar (VXTH's roll); ladders use the full chain
            params = {"expiry": "monthly"} if len(dtes) == 1 else {}
            cfg = HedgeConfig(d, dtes, ALLOC_OFFICIAL, label=key, params=params)
            c = simulate(spot, chain, base=base, cfg=cfg, start=args.start, end=args.end, signal_series=sig)
            point[key] = _point_row(c)
            pool = run_cohorts_td(spot, chain, base=base, cfg=cfg, n=args.cohorts,
                                  start=args.start, end=args.end, signal_series=sig)
            ens[key] = _ensemble_row(per_offset_metrics(pool))
            r = point[key]
            print(f"  {key:24s} CAGR {r['CAGR %']:5.2f}  Sharpe {r['Sharpe']:4.2f}  "
                  f"vol {r['AnnVol %']:4.1f}  MaxDD {r['MaxDD %']:4.1f}  COVID {r['COVID %']:6.1f}   "
                  f"[{time.time() - t0:4.0f}s]")

    ptab = pd.DataFrame(point).T
    etab = pd.DataFrame(ens).T
    ptab.to_csv(outdir / "point_metrics.csv")
    etab.to_csv(outdir / "ensemble_metrics.csv")

    def show(df: pd.DataFrame, title: str, fmt: str = "8.2f") -> None:
        print(f"\n=== {title} ===")
        print(df.to_string(float_format=lambda x: format(x, fmt)))

    for sname in STRUCTURES:
        rows = [k for k in ptab.index if k.startswith(sname)]
        show(ptab.loc[rows], f"Point estimates — {sname} (one entry-date draw)")
    for sname in STRUCTURES:
        rows = [k for k in etab.index if k.startswith(sname)]
        show(etab.loc[rows], f"Robustness across {args.cohorts} entry-date cohorts — {sname}")

    print(f"\nwrote point_metrics.csv + ensemble_metrics.csv -> {outdir}")
    print(f"(total {time.time() - t0:.0f}s)")


if __name__ == "__main__":
    main()
