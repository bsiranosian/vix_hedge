"""Decouple the exit decision from the entry gate (the >50-VIX cliff).

The stock VXTH ">50 shut-off" does two jobs at once on the forward-VIX regime:

* an **entry gate** — weight 0 above forward-VIX 50, i.e. "don't buy expensive vol";
* an **exit / bank-the-spike** — because the regime weight is 0, the *alive* ladder
  is re-struck to 0 contracts at the next roll, folding the spiked sleeve into the
  base (see ``run_vix_monetization``'s note: the 4-level resize already monetizes).

The second job nailed the COVID and GFC tops in-sample — but that is luck: it banks
on the VIX *level* crossing a tuned threshold (50) at whatever monthly-expiry roll
happens to fall there, not on the hedge's realized payoff.

This study **keeps the entry gate and removes the cliff's exit** (``roll="ratchet"``:
fund up to the regime budget each roll but never trim a rung), then re-introduces an
**explicit, value-based exit** in its place:

* ``monetize_budget`` — a *sleeve-level* trim: bank a fraction of the whole sleeve
  once its value reaches k× its cost basis (the direct analogue of the cliff);
* ``monetize_schedule`` — the *per-rung* schedule: sell a rung at k× its entry.

The question: **does the COVID/GFC banking survive an explicit exit?** Two payoff
lenses per crash (as in ``run_vix_monetization``): the **trough** (window to the
bottom = peak hedge value) and the **retention** through the recovery — what banking
actually changes. Reported as point estimates *and* as a distribution across a grid
of reasonable hedge specs (delta × ladder), with a cliff-threshold sweep that shows
the COVID banking is *insensitive* to the exact level (VIX overshot any threshold) —
so the cliff's edge is the monthly-expiry-vs-bottom **alignment**, not a tunable
level. That alignment is neither tunable nor tranche-able: entry-date staggering is
inert here (every cohort snaps to the same VIX-expiry roll grid), so the cliff's luck
is *not* an RTL problem you can diversify away (``--show-cohort-degeneracy``).

Usage::

    uv run python scripts/run_decoupled_gate.py
    uv run python scripts/run_decoupled_gate.py --net
    uv run python scripts/run_decoupled_gate.py --delta 0.10 --base NDX
"""

from __future__ import annotations

import argparse
import time

import pandas as pd

from vix_hedge import config, data, metrics
from vix_hedge.vxth import BASES
from vix_hedge.vxth import episodes as ep
from vix_hedge.vxth.backtest import forward_vix_signal
from vix_hedge.vxth.costs import ProportionalCost
from vix_hedge.vxth.engine import ALLOC_OFFICIAL, HedgeConfig, simulate

SCHED = {3.0: 0.5, 5.0: 0.5, 8.0: 1.0}
# (label, params) — same delta/ladder/alloc; only the exit machinery changes.
VARIANTS = [
    ("cliff (stock VXTH)", {}),                                          # rebalance + alloc[3]=0 cliff
    ("entry_only (hold)", {"roll": "ratchet"}),                          # gate only, no forced exit
    ("entry_only + budget 3/5/8", {"roll": "ratchet", "monetize_budget": SCHED}),
    ("entry_only + perrung 3/5/8", {"roll": "ratchet", "monetize_schedule": SCHED}),
]
# Robustness grid — does the ranking survive across reasonable hedge specs?
SPEC_DELTAS = (0.10, 0.20, 0.30)
SPEC_LADDERS = ((30,), (30, 60, 90), (30, 90, 180))
# Trough = standard window to the crash bottom (= peak hedge value).
TROUGH = {"GFC": "GFC 2008-09", "COVID": "COVID 2020"}
# Retention = through the recovery — the prolonged-rout vs sharp-V contrast.
RETENTION = {"GFC": ("2008-09-02", "2009-09-30"), "COVID": ("2020-02-19", "2020-08-31")}


def row_of(c: pd.Series) -> dict:
    """Full-sample risk/return + the four crash payoff cells for one curve."""
    eps = ep.episode_returns(c)
    row = {
        "CAGR %": 100 * metrics.cagr(c),
        "Sharpe": metrics.annualized_sharpe(c),
        "MaxDD %": 100 * metrics.max_drawdown(c),
    }
    for k, e in TROUGH.items():
        row[f"{k}↓"] = 100 * eps[e]
    for k, (s, e) in RETENTION.items():
        row[f"{k}→"] = 100 * ep.window_return(c, s, e)
    return row


def run_one(spot, chain, sig, params, *, delta, ladder, base, start, end, cost) -> pd.Series:
    cfg = HedgeConfig(delta, ladder, ALLOC_OFFICIAL, params=params)
    cm = ProportionalCost() if cost else None
    return simulate(spot, chain, base=base, cfg=cfg, start=start, end=end,
                    signal_series=sig, cost_model=cm)


def cliff_at(sig: pd.Series, threshold: float) -> pd.Series:
    """Remap the forward-VIX signal so the *top* regime boundary sits at ``threshold``
    instead of 50 (the 15/30 boundaries are untouched), to sweep the cliff level
    without editing the global regime bounds. Lowering it forces ``(T, 50]`` into the
    shut-off (regime 3); raising it lets ``(50, T]`` keep buying at 0.5% (regime 2)."""
    s = sig.copy()
    if threshold < 50.0:
        return s.where(s <= threshold, 51.0)           # >T -> regime 3
    if threshold > 50.0:
        return s.mask((s > 50.0) & (s <= threshold), 49.0)  # (50,T] -> regime 2
    return s


def _show(tbl: pd.DataFrame, caption: str, outdir, tag: str) -> None:
    cols = ["CAGR %", "Sharpe", "MaxDD %", "GFC↓", "COVID↓", "GFC→", "COVID→"]
    tbl = tbl[cols]
    tbl.to_csv(outdir / f"{tag}_mean.csv")
    print(f"\n  [{tag}] {caption}:")
    print("  " + tbl.to_string(float_format=lambda x: f"{x:7.2f}").replace("\n", "\n  "))


def main() -> None:
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--start", default="2006-03-22")
    p.add_argument("--end", default=None)
    p.add_argument("--delta", type=float, default=0.30)
    p.add_argument("--ladder", default="30,60,90")
    p.add_argument("--base", default="SPX", choices=list(BASES))
    p.add_argument("--signal", default="hybrid_vx1")
    p.add_argument("--net", action="store_true", help="also report net of the default VIX-call spread")
    p.add_argument("--show-cohort-degeneracy", action="store_true",
                   help="demonstrate that entry-date tranching does not move the crash banking")
    args = p.parse_args()
    ladder = tuple(int(x) for x in args.ladder.split(","))
    base = BASES[args.base]

    spot = data.load_spot_prices()
    chain = data.load_vix_chain()
    sig = forward_vix_signal(chain, spot, source=args.signal)

    outdir = config.RESULTS_DIR / "decoupled_gate"
    outdir.mkdir(parents=True, exist_ok=True)
    print(f"Decoupled-gate study — base={args.base}  {int(args.delta * 100)}Δ ladder {ladder}  "
          f"signal={args.signal}  window {args.start}..{args.end or 'end'}\n")

    base_curve = simulate(spot, chain, base=base, cfg=None, start=args.start, end=args.end, signal_series=sig)

    # --- 1) the core comparison (point estimates) ----------------------------
    def table(cost: bool) -> pd.DataFrame:
        rows = {"SPX base" if args.base == "SPX" else f"{args.base} base": row_of(base_curve)}
        for label, params in VARIANTS:
            t = time.time()
            c = run_one(spot, chain, sig, params, delta=args.delta, ladder=ladder,
                        base=base, start=args.start, end=args.end, cost=cost)
            rows[label] = row_of(c)
            print(f"    {label:30s} CAGR {rows[label]['CAGR %']:5.2f}%  Sharpe {rows[label]['Sharpe']:4.2f}  "
                  f"COVID→ {rows[label]['COVID→']:+6.1f}%  GFC→ {rows[label]['GFC→']:+6.1f}%  ({time.time() - t:.0f}s)")
        return pd.DataFrame(rows).T

    print("  === core comparison (gross) ===")
    gross = table(cost=False)
    _show(gross, "gross — window↓ = to trough/peak hedge; window→ = retention through recovery", outdir, "gross")
    if args.net:
        print("\n  === core comparison (net of default VIX-call spread) ===")
        net = table(cost=True)
        _show(net, "net of cost", outdir, "net")

    # --- 2) robustness distribution across hedge specs -----------------------
    print(f"\n  === robustness across {len(SPEC_DELTAS) * len(SPEC_LADDERS)} specs "
          f"(δ∈{SPEC_DELTAS} × ladder∈{[len(x) for x in SPEC_LADDERS]}-rung) ===")
    dist = _robustness(spot, chain, sig, base, args.start, args.end)
    dist.to_csv(outdir / "robustness.csv")
    print("  " + dist.to_string(float_format=lambda x: f"{x:7.1f}").replace("\n", "\n  "))

    # --- 3) cliff threshold sensitivity --------------------------------------
    print("\n  === cliff threshold sweep (COVID overshoots any level -> banking is expiry-vs-bottom "
          "alignment, not a tunable knob; the value-exit reference has no level to tune) ===")
    thr = _threshold_sweep(spot, chain, sig, base, args.delta, ladder, args.start, args.end)
    thr.to_csv(outdir / "threshold.csv")
    print("  " + thr.to_string(float_format=lambda x: f"{x:+7.1f}").replace("\n", "\n  "))

    if args.show_cohort_degeneracy:
        _cohort_degeneracy(spot, chain, sig, base, args.delta, ladder, args.start, args.end)

    _verdict(gross)
    _plots(gross, dist, outdir, args.base)
    print(f"\nwrote *_mean.csv, robustness.csv, threshold.csv + *.png -> {outdir}")


def _robustness(spot, chain, sig, base, start, end) -> pd.DataFrame:
    """Per-variant mean and [min,max] of the key metrics across the spec grid."""
    out: dict[str, dict] = {}
    for label, params in VARIANTS:
        rows = []
        for d in SPEC_DELTAS:
            for lad in SPEC_LADDERS:
                c = run_one(spot, chain, sig, params, delta=d, ladder=lad, base=base,
                            start=start, end=end, cost=False)
                rows.append(row_of(c))
        df = pd.DataFrame(rows)
        rec = {}
        for m in ("Sharpe", "COVID→", "GFC→"):
            rec[f"{m} mean"] = df[m].mean()
            rec[f"{m} min"] = df[m].min()
            rec[f"{m} max"] = df[m].max()
        out[label] = rec
    return pd.DataFrame(out).T


def _threshold_sweep(spot, chain, sig, base, delta, ladder, start, end) -> pd.DataFrame:
    """COVID→ / GFC→ retention of the cliff as the >50 boundary is moved (and, as a
    reference line, the value-based decoupled exit which ignores the VIX level)."""
    out: dict[str, dict] = {}
    for T in (45, 48, 50, 52, 55):
        c = run_one(spot, chain, cliff_at(sig, T), {}, delta=delta, ladder=ladder,
                    base=base, start=start, end=end, cost=False)
        r = row_of(c)
        out[f"cliff @ {T}"] = {"CAGR %": r["CAGR %"], "Sharpe": r["Sharpe"], "COVID→": r["COVID→"], "GFC→": r["GFC→"]}
    ref = run_one(spot, chain, sig, {"roll": "ratchet", "monetize_budget": SCHED},
                  delta=delta, ladder=ladder, base=base, start=start, end=end, cost=False)
    rr = row_of(ref)
    out["entry_only+budget (no knob)"] = {"CAGR %": rr["CAGR %"], "Sharpe": rr["Sharpe"],
                                          "COVID→": rr["COVID→"], "GFC→": rr["GFC→"]}
    return pd.DataFrame(out).T


def _cohort_degeneracy(spot, chain, sig, base, delta, ladder, start, end) -> None:
    """Show that staggering the entry date leaves the crash banking unchanged — all
    cohorts converge onto the same monthly VIX-expiry roll grid."""
    from vix_hedge.vxth.ensemble import common_trade_dates
    cfg = HedgeConfig(delta, ladder, ALLOC_OFFICIAL, params={})
    starts = [d.strftime("%Y-%m-%d") for d in common_trade_dates(spot, chain, start, end)[:21]]
    cov = [100 * ep.window_return(simulate(spot, chain, base=base, cfg=cfg, start=s, end=end, signal_series=sig),
                                   *RETENTION["COVID"]) for s in starts]
    print(f"\n  [cohort degeneracy] cliff COVID→ across 21 entry-date cohorts: "
          f"min {min(cov):.2f}  max {max(cov):.2f}  spread {max(cov) - min(cov):.3f}pp "
          f"-> entry-date tranching cannot diversify the cliff's alignment luck.")


def _verdict(gross: pd.DataFrame) -> None:
    cliff, hold = gross.loc["cliff (stock VXTH)"], gross.loc["entry_only (hold)"]
    bud = gross.loc["entry_only + budget 3/5/8"]
    print("\n  === verdict: does COVID/GFC banking survive without the cliff? ===")
    print(f"    cliff      : COVID→ {cliff['COVID→']:+5.0f}%  GFC→ {cliff['GFC→']:+5.0f}%  "
          f"Sharpe {cliff['Sharpe']:.2f}  (great on the sharp V, weak on the prolonged rout)")
    print(f"    hold (gate): COVID→ {hold['COVID→']:+5.0f}%  GFC→ {hold['GFC→']:+5.0f}%  "
          f"Sharpe {hold['Sharpe']:.2f}  (robust on BOTH crash shapes, best risk-adjusted)")
    print(f"    +explicit  : COVID→ {bud['COVID→']:+5.0f}%  GFC→ {bud['GFC→']:+5.0f}%  "
          f"Sharpe {bud['Sharpe']:.2f}  (an explicit threshold exit banks too early — the same banks-too-early effect)")
    print("    => The cliff's COVID banking does NOT transfer to a principled value-exit (banks too early).")
    print("       But you don't need it: keeping the gate and HOLDING (no forced exit) is the robust win.")


def _plots(gross: pd.DataFrame, dist: pd.DataFrame, outdir, base_name: str) -> None:
    try:
        import matplotlib

        matplotlib.use("Agg")
        import matplotlib.pyplot as plt
    except Exception as e:  # noqa: BLE001 - plotting is optional
        print(f"(skipping plots: {e})")
        return
    variants = [v[0] for v in VARIANTS]
    colors = ["#444", "#2ca02c", "#d62728", "#9467bd"]

    # 1) Retention by crash (trough vs through-recovery), per variant.
    fig, axes = plt.subplots(1, 2, figsize=(11, 4.6))
    for ax, crash in zip(axes, ["COVID", "GFC"], strict=True):
        x = range(len(variants))
        ax.bar([i - 0.2 for i in x], [gross.at[v, f"{crash}↓"] for v in variants], 0.4,
               color=colors, alpha=0.45, label="trough (peak hedge)")
        ax.bar([i + 0.2 for i in x], [gross.at[v, f"{crash}→"] for v in variants], 0.4,
               color=colors, label="retention (→ recovery)")
        ax.axhline(0, color="k", lw=0.7)
        ax.set_xticks(list(x))
        ax.set_xticklabels([v.replace(" + ", "\n+").replace(" (", "\n(") for v in variants], fontsize=7.5)
        ax.set_title(f"{crash}: trough payoff vs retention", fontsize=10)
        ax.set_ylabel("portfolio return over window (%)")
        ax.grid(True, axis="y", alpha=0.15)
    fig.suptitle("Bank-the-spike: the cliff wins COVID's V but loses GFC's rout to plain holding", fontsize=11)
    fig.tight_layout()
    fig.savefig(outdir / "retention.png", dpi=130)
    plt.close(fig)

    # 2) Robustness across specs — COVID→ retention mean with [min,max] whiskers.
    fig, ax = plt.subplots(figsize=(8.5, 4.6))
    for crash, dx, mk in (("COVID→", -0.12, "o"), ("GFC→", 0.12, "s")):
        means = [dist.at[v, f"{crash} mean"] for v in variants]
        los = [dist.at[v, f"{crash} mean"] - dist.at[v, f"{crash} min"] for v in variants]
        his = [dist.at[v, f"{crash} max"] - dist.at[v, f"{crash} mean"] for v in variants]
        ax.errorbar([i + dx for i in range(len(variants))], means, yerr=[los, his], fmt=mk,
                    capsize=4, label=crash[:-1], lw=1.5)
    ax.axhline(0, color="k", lw=0.7)
    ax.set_xticks(range(len(variants)))
    ax.set_xticklabels([v.replace(" + ", "\n+").replace(" (", "\n(") for v in variants], fontsize=7.5)
    ax.set_ylabel("retention through recovery (%)")
    ax.set_title("Robustness across delta×ladder specs (point = mean, whisker = min..max)")
    ax.legend()
    ax.grid(True, axis="y", alpha=0.15)
    fig.tight_layout()
    fig.savefig(outdir / "robustness.png", dpi=130)
    plt.close(fig)


if __name__ == "__main__":
    main()
