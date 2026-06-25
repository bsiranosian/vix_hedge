"""Headline interactive report: option delta × structure.

The project's headline finding: **call delta is the Sharpe ↔ convexity dial, and
the 30/60/90 *tenor ladder* — not entry-date averaging — is what makes the hedge
robust** to roll-phase luck. This renders that result as a self-contained,
interactive Plotly report (hover / zoom / legend-toggle) with one equity-curve
chart + a full metrics table per section.

Sections:

1. Headline — the marquee curves (SPX, single 30Δ, 50Δ/30Δ/5Δ ladders).
2. Option delta, single 30-day call — Sharpe rises & vol falls into 50Δ, but the
   lone call misses COVID at every delta.
3. Option delta, 30/60/90 ladder — same dial; 50Δ ladder = smoothest/max-Sharpe,
   deep-OTM = max convexity; the ladder catches COVID at every delta.
4. Laddering vs a single call — the tenor ladder is the robustness lever.

Frictionless, SPX base, hybrid_cmf30 forward-VIX signal (the lag-robust default;
identical headline numbers to VX1 but no roll-day expiry sawtooth).

Usage::

    uv run python scripts/run_delta_ladder_report.py
    uv run python scripts/run_delta_ladder_report.py --signal parity --out /tmp/r.html
"""

from __future__ import annotations

import argparse
import time
from pathlib import Path

import pandas as pd

from vix_hedge import config, data, metrics
from vix_hedge.vxth import BASES, roll_phase
from vix_hedge.vxth import episodes as ep
from vix_hedge.vxth.backtest import forward_vix_signal
from vix_hedge.vxth.engine import ALLOC_OFFICIAL, HedgeConfig, simulate
from vix_hedge.vxth.experiments import Section
from vix_hedge.vxth.report import build_report

DELTAS = (0.50, 0.30, 0.20, 0.10, 0.05)
LADDER = (30, 60, 90)
START = "2006-03-22"

# Columns of the headline metrics table (the "table of different values").
_EPISODES = {"GFC %": "GFC 2008-09", "Q4-18 %": "Q4 2018", "COVID %": "COVID 2020"}


def _metrics_row(c: pd.Series) -> dict:
    row = {
        "CAGR %": 100 * metrics.cagr(c),
        "Sharpe": metrics.annualized_sharpe(c),
        "Sortino": metrics.sortino(c),
        "Vol %": 100 * metrics.annualized_stddev(c),
        "MaxDD %": 100 * metrics.max_drawdown(c),
    }
    for col, name in _EPISODES.items():
        row[col] = 100 * ep.window_return(c, *ep.CRASH_EPISODES[name])
    return row


def _metrics_table(curves: dict[str, pd.Series]) -> pd.DataFrame:
    return pd.DataFrame({k: _metrics_row(v) for k, v in curves.items()}).T


def _section(key: str, title: str, blurb: str, curves: dict[str, pd.Series]) -> Section:
    return Section(key, title, blurb, pd.DataFrame(curves), _metrics_table(curves), report=None)


def main() -> None:
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--start", default=START)
    p.add_argument("--end", default=None)
    p.add_argument("--signal", default="hybrid_cmf30", choices=["hybrid_cmf30", "hybrid_vx1", "parity"])
    p.add_argument("--out", default=None)
    args = p.parse_args()

    t0 = time.time()
    spot = data.load_spot_prices()
    chain = data.load_vix_chain()
    sig = forward_vix_signal(chain, spot, source=args.signal)

    def run(cfg: HedgeConfig | None) -> pd.Series:
        return simulate(spot, chain, base=BASES["SPX"], cfg=cfg, start=args.start, end=args.end, signal_series=sig)

    spx = run(None).rename("SPX (unhedged)")
    # Single calls roll on the monthly VIX settlement calendar (VXTH's roll); the
    # nearest-DTE default would land on weeklys phased off that cycle. The ladder keeps
    # the full chain — its 60/90-day rungs span any spike regardless of front-rung phase.
    single = {d: run(HedgeConfig(d, (30,), ALLOC_OFFICIAL, params={"expiry": "monthly"}))
              .rename(f"single 30d {int(d*100)}Δ") for d in DELTAS}
    ladder = {d: run(HedgeConfig(d, LADDER, ALLOC_OFFICIAL)).rename(f"ladder 30/60/90 {int(d*100)}Δ") for d in DELTAS}
    print(f"ran {1 + 2 * len(DELTAS)} backtests in {time.time() - t0:.0f}s  (signal: {args.signal})")

    # Roll-phase evidence (2017–2021, where VIX weeklys populate every expiry track and the
    # window contains COVID): a single 30Δ call held on three roll phases — the VXTH monthly
    # track and the weeklys ±1 week off it — vs the phase-invariant 30/60/90 ladder.
    rp_s, rp_e = "2017-01-01", "2021-12-31"
    single_cfg = HedgeConfig(0.30, (30,), ALLOC_OFFICIAL)  # plain single; phase comes from the chain
    rp_phase = {off: simulate(spot, roll_phase.phase_filtered_chain(chain, off), base=BASES["SPX"],
                              cfg=single_cfg, start=rp_s, end=rp_e, signal_series=sig) for off in (-7, 0, 7)}
    rp_spx = simulate(spot, chain, base=BASES["SPX"], cfg=None, start=rp_s, end=rp_e, signal_series=sig)
    rp_ladder = simulate(spot, chain, base=BASES["SPX"], cfg=HedgeConfig(0.30, LADDER, ALLOC_OFFICIAL),
                         start=rp_s, end=rp_e, signal_series=sig)
    rp_curves = {
        "SPX (unhedged)": rp_spx.rename("SPX (unhedged)"),
        "single 30Δ · monthly track (VXTH)": rp_phase[0].rename("single 30Δ · monthly track (VXTH)"),
        "single 30Δ · weekly −1 week": rp_phase[-7].rename("single 30Δ · weekly −1 week"),
        "single 30Δ · weekly +1 week": rp_phase[7].rename("single 30Δ · weekly +1 week"),
        "ladder 30/60/90 30Δ": rp_ladder.rename("ladder 30/60/90 30Δ"),
    }

    sections = [
        _section(
            "headline",
            "Headline — delta sets the Sharpe ↔ convexity dial; the tenor ladder makes it robust",
            "Three clean results define this hedge. <b>(1) A higher call delta raises Sharpe and smooths "
            "the path</b>: the 50Δ ladder is the smoothest, highest-Sharpe configuration in the study "
            "(Sharpe 0.66, 14.0% vol), beating unhedged SPX on Sharpe, CAGR <i>and</i> drawdown while "
            "cushioning COVID. <b>(2) Deeper-OTM trades that smoothness for crash "
            "convexity</b> — the 5Δ ladder turns COVID into +232% but its Sharpe falls to 0.23; the "
            "<b>10Δ ladder — the delta the original report builds its 'all modifications' config on</b> — "
            "sits mid-dial (+130% through COVID, 14.8% CAGR, 0.29 Sharpe), the convexity-vs-Sharpe "
            "compromise. <b>(3) Robustness comes from the tenor ladder — robustness to <i>roll timing</i></b>. "
            "The entry date barely matters (across 21 staggered cohorts the full-window spread is ≈0; the monthly "
            "roll re-syncs them). What decides a single call is its roll <i>phase</i> — which expiry it holds into "
            "the spike. A single 30Δ call on VXTH's <b>monthly</b> settlement catches COVID (+29%) and replicates "
            "the index; shift its roll one week onto an adjacent <b>weekly</b> and its lone option expires before "
            "the peak, flipping COVID to −18% (see the roll-phase section below). Holding 30/60/90 simultaneously "
            "removes that dependence — the ladder catches COVID at every delta and on every phase. On the right "
            "calendar the single call ties the ladder in-sample; the ladder's edge is robustness, not raw return.",
            {spx.name: spx, single[0.30].name: single[0.30],
             ladder[0.50].name: ladder[0.50], ladder[0.30].name: ladder[0.30],
             ladder[0.10].name: ladder[0.10], ladder[0.05].name: ladder[0.05]},
        ),
        _section(
            "delta_single",
            "Option delta — single 30-day call",
            "Sweeping the call delta on a single 30-day call, rolled on VXTH's <b>monthly</b> VIX settlement "
            "calendar (the right roll — a nearest-DTE roll can grab a weekly and miss the spike; see the roll-phase "
            "section). Sharpe rises and volatility falls toward 50Δ — a near-the-money call is reliably worth "
            "something rather than a lottery ticket — and on the monthly calendar the single call catches COVID at "
            "every delta (50Δ ≈ flat, deeper-OTM increasingly convex), so it beats unhedged SPX on CAGR throughout "
            "and on Sharpe near the money. The catch is fragility: this whole curve depends on holding the monthly "
            "contract, and a one-week roll offset breaks it (next sections).",
            {spx.name: spx, **{single[d].name: single[d] for d in DELTAS}},
        ),
        _section(
            "delta_ladder",
            "Option delta — 30/60/90 ladder",
            "The same delta dial on the tenor ladder. <b>50Δ = the smoothest, max-Sharpe end</b> (0.66 Sharpe, "
            "14.0% vol, cushions COVID to −1.9%); <b>deep-OTM = the max-convexity end</b> (5Δ: +232% COVID, "
            "19.1% CAGR, but 85% vol and 0.23 Sharpe). Sortino <i>inverts</i> the ranking (2.22 at 5Δ vs 1.13 at "
            "50Δ) because it ignores the big crash-time <i>up</i>-moves — so 'smoother' means 50Δ on total vol, "
            "deep-OTM on downside-only risk. Every laddered delta catches COVID (the ladder always holds a live "
            "60–90-day option through the spike).",
            {spx.name: spx, **{ladder[d].name: ladder[d] for d in DELTAS}},
        ),
        _section(
            "ladder_vs_single",
            "Laddering vs a single call — the robustness lever",
            "Holding the delta fixed and comparing structures. On the <b>monthly</b> calendar the single call and "
            "the ladder are close — at 30Δ both catch COVID (single +29%, ladder +26%) with near-identical Sharpe, "
            "and at 50Δ both cushion it (≈−2%) at ~0.65 Sharpe. So in-sample, once the single call is rolled "
            "correctly, structure barely changes the point estimate. The ladder's advantage is <i>robustness</i>, "
            "not this in-sample gap: it doesn't depend on getting the roll calendar exactly right (next section), "
            "and it beats the single call out-of-sample at every delta (2021–2025). Same strike, similar in-sample "
            "outcome — but the ladder is the one that survives a different roll phase or a differently-timed spike.",
            {spx.name: spx, single[0.50].name: single[0.50], ladder[0.50].name: ladder[0.50],
             single[0.30].name: single[0.30], ladder[0.30].name: ladder[0.30]},
        ),
        _section(
            "roll_phase",
            "Roll-phase timing luck — a one-week offset breaks the single call",
            "The robustness case for the ladder, isolated. The <b>entry date</b> barely matters — this hedge "
            "resizes to the regime weight every roll, so staggered start dates re-sync and the full-window spread "
            "is ~0.1pp for the single call <i>and</i> the ladder (see <code>run_ensemble_analysis.py</code>). The "
            "axis that actually decides a single call's crash is the <b>roll phase</b>: which expiry it holds into "
            "the spike. Here the <i>same</i> single 30Δ call is held on three roll tracks over 2017–2021 — the "
            "standard <b>monthly VIX settlement</b> (VXTH's calendar) and the <b>weeklys ±1 week</b> off it. The "
            "monthly track catches COVID (it holds the contract that spans the mid-March peak); shift the roll by a "
            "single week in <i>either</i> direction and the lone option expires before the peak, so COVID flips "
            "negative — a ~50pp swing from a one-week offset. The <b>30/60/90 ladder</b> keeps the full chain, so a "
            "60/90-day rung is always alive through the spike: it catches COVID on <i>every</i> phase. That is what "
            "the tenor ladder buys — not higher return (on the right calendar a single call ties it), but immunity "
            "to a roll-phase choice that otherwise makes or breaks the hedge.",
            rp_curves,
        ),
    ]

    for s in sections:
        print(f"\n{s.title}")
        print(s.metrics.to_string(float_format=lambda x: f"{x:8.2f}"))

    outdir = config.RESULTS_DIR / "delta_ladder"
    outdir.mkdir(parents=True, exist_ok=True)
    out = outdir / "report.html" if args.out is None else Path(args.out)
    span = f"{spx.index.min().date()} – {spx.index.max().date()}"
    signal_desc = {
        "hybrid_vx1": "real CBOE front-month VIX future (VX1) where available (2013+), put-call-parity forward before",
        "hybrid_cmf30": "real 30-day constant-maturity VIX forward (2013+), put-call-parity forward before",
        "parity": "put-call-parity forward from the VIX options",
    }[args.signal]
    build_report(
        sections, out,
        title="Tail hedging with VIX calls — delta &amp; laddering (headline results)",
        subtitle="How option delta trades smoothness for convexity, and why the tenor ladder is the robust choice",
        meta=f"SPX base · frictionless · backtest {span} · regime signal: {signal_desc} · "
             f"ladder rungs sized equal-dollar per tenor · "
             f"crash-window returns total; Sharpe/Sortino/vol annualized on monthly returns",
    )
    print(f"\nwrote interactive report -> {out}")


if __name__ == "__main__":
    main()
