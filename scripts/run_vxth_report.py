"""Reproduce the MSE448 VIX-call-hedge report as an interactive HTML.

Runs every experiment (§4.1-4.7) and writes a self-contained, interactive
sectioned report to ``results/vxth/report.html``.

Usage::

    uv run python scripts/run_vxth_report.py
    uv run python scripts/run_vxth_report.py --start 2006-03-22 --end 2020-12-31
"""

from __future__ import annotations

import argparse
import time
from pathlib import Path

from vix_hedge import config, data
from vix_hedge.vxth import experiments, report


def main() -> None:
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--start", default=experiments.START)
    p.add_argument("--end", default=None)
    p.add_argument("--out", default=None)
    p.add_argument("--signal", default="hybrid_vx1", choices=["hybrid_vx1", "hybrid_cmf30", "parity"],
                   help="forward-VIX regime signal: real front-month VX1 (default), 30-day CMF, or parity proxy")
    args = p.parse_args()

    t = time.time()
    spot = data.load_spot_prices()
    chain = data.load_vix_chain()
    lib = experiments.CurveLibrary(spot, chain, start=args.start, end=args.end, signal_source=args.signal)
    sections = experiments.build_sections(lib)
    print(f"ran {len(lib._cache)} backtests in {time.time() - t:.1f}s  (signal: {lib.signal_source})")

    for sec in sections:
        if sec.metrics.empty:
            continue
        print(f"\n{sec.title}")
        print(sec.metrics.to_string())

    outdir = config.RESULTS_DIR / "vxth"
    outdir.mkdir(parents=True, exist_ok=True)
    out = outdir / "report.html" if args.out is None else Path(args.out)
    span = f"{sections[0].curves.index.min().date()} – {sections[0].curves.index.max().date()}"
    signal_desc = {
        "hybrid_vx1": "real CBOE front-month VIX future (VX1) where available (2013+), put-call-parity forward before",
        "hybrid_cmf30": "real 30-day constant-maturity VIX forward (2013+), put-call-parity forward before",
        "parity": "put-call-parity forward from the VIX options",
    }[lib.signal_source]
    report.build_report(
        sections, out,
        title="Tail-risk hedging with VIX calls",
        subtitle="Interactive reproduction of the MSE448 report — portfolios &amp; settings",
        meta=f"Backtest window {span} · regime signal: {signal_desc} · "
             f"midpoint fills, no transaction costs · reproduced numbers shown vs. report",
    )
    print(f"\nwrote interactive report -> {out}")


if __name__ == "__main__":
    main()
