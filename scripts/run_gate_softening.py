"""Soft-gate study: does softening the forward-VIX 0↔1% cliff help?

The hedge weight is gated on the forward VIX by a hard 4-level step
(``alloc[regime_of(F)]``): the ``0↔1%`` jump at ``F=15`` is read on a single print
of a contract that, near a VIX-future expiry, sawtooths across 15.
This script tests three softenings of that cliff (:mod:`vix_hedge.vxth.gate`):

* ``ramp``  — ``w = 1% · clip((F−13)/4, 0, 1)`` (linear gate, steps above 30/50).
* ``hyst``  — Schmitt trigger, on at 16 / off at 13.5 (the proposal's defaults).
* ``hyst-wide`` — Schmitt trigger, on at 16 / off at **12** (a wider dead band).

against the ``hard`` baseline, on both forward-VIX signals (the lag-fragile
``hybrid_vx1`` and the lag-robust ``hybrid_cmf30``), through three lenses:

1. **Performance** (gross CAGR/Sharpe/Sortino/MaxDD/COVID) and **cost drag**
   (frictionless − net with the VIX-call half-spread).
2. **Lag-jitter** — the *right* robustness channel: re-run with the gate
   signal shifted ±k trading days and report the metric spread. The entry-date
   ensemble is blind to this; the lag harness is not.
3. **Turnover** — gate-driven trading off the weight path (deploy events and
   full-sleeve-equivalents per year), isolating the gate from the monthly roll.

Headline config: 50Δ 30/60/90 ladder, equal-dollar, SPX base, ALLOC_OFFICIAL.

Usage::

    uv run python scripts/run_gate_softening.py
    uv run python scripts/run_gate_softening.py --lags -3 -2 -1 0 1 2 3
"""

from __future__ import annotations

import argparse
import time

import pandas as pd

from vix_hedge import data, metrics
from vix_hedge.vxth import episodes as ep
from vix_hedge.vxth import lag_jitter as LJ
from vix_hedge.vxth.backtest import forward_vix_signal
from vix_hedge.vxth.costs import ProportionalCost
from vix_hedge.vxth.engine import ALLOC_OFFICIAL, BASES, HedgeConfig, simulate

LADDER = (30, 60, 90)
DELTA = 0.50
GATES: dict[str, dict | None] = {
    "hard": None,
    "ramp": {"mode": "ramp"},
    "hyst": {"mode": "hysteresis"},  # proposal default: on 16 / off 13.5
    "hyst-wide": {"mode": "hysteresis", "on_at": 16.0, "off_at": 12.0},
}


def _cfg(spec: dict | None) -> HedgeConfig:
    params = {"sizing": "equal_dollars"}
    if spec is not None:
        params["gate"] = spec
    return HedgeConfig(DELTA, LADDER, ALLOC_OFFICIAL, params=params)


def _perf_row(gross: pd.Series, net: pd.Series) -> dict:
    return {
        "CAGR %": 100 * metrics.cagr(gross),
        "Sharpe": metrics.annualized_sharpe(gross),
        "Sortino": metrics.sortino(gross),
        "MaxDD %": 100 * metrics.max_drawdown(gross),
        "COVID %": 100 * ep.window_return(gross, *ep.CRASH_EPISODES["COVID 2020"]),
        "drag pp": 100 * (metrics.cagr(gross) - metrics.cagr(net)),
        "net Shrp": metrics.annualized_sharpe(net),
    }


def _show(df: pd.DataFrame, title: str, fmt: str = "8.3f") -> None:
    print(f"\n=== {title} ===")
    print(df.to_string(float_format=lambda x: format(x, fmt)))


def main() -> None:
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("--start", default="2006-03-22")
    p.add_argument("--end", default=None)
    p.add_argument("--lags", type=int, nargs="+", default=[-2, -1, 0, 1, 2])
    p.add_argument("--cost-scale", type=float, default=1.0)
    args = p.parse_args()
    lags = tuple(args.lags)

    spot = data.load_spot_prices()
    chain = data.load_vix_chain()
    base = BASES["SPX"]
    cost = ProportionalCost(scale=args.cost_scale)
    spx = simulate(spot, chain, base=base, cfg=None, start=args.start, end=args.end)
    span = f"{spx.index.min().date()} .. {spx.index.max().date()}"
    print(f"Soft-gate study — {int(DELTA * 100)}Δ {LADDER} ladder · equal-dollar · SPX · {span}")
    print(f"reference SPX (unhedged): CAGR {100 * metrics.cagr(spx):.2f}%  Sharpe {metrics.annualized_sharpe(spx):.3f}")

    t0 = time.time()
    for src in ("hybrid_vx1", "hybrid_cmf30"):
        sig = forward_vix_signal(chain, spot, source=src)
        kw = dict(base=base, start=args.start, end=args.end, signal_series=sig)
        print(f"\n{'#' * 12} forward-VIX signal = {src} {'#' * 12}")

        perf, turn, covid_by_lag, spreads = {}, {}, {}, {}
        for name, spec in GATES.items():
            cfg = _cfg(spec)
            gross = simulate(spot, chain, cfg=cfg, cost_model=None, **kw)
            net = simulate(spot, chain, cfg=cfg, cost_model=cost, **kw)
            perf[name] = _perf_row(gross, net)
            to = LJ.gate_turnover(sig, alloc=ALLOC_OFFICIAL, gate_spec=spec, dates=gross.index)
            turn[name] = {"deploys/yr": to["deploys_per_yr"], "sleeves/yr": to["sleeves_traded_per_yr"],
                          "frac_on": to["frac_on"]}
            res = LJ.lag_jitter(spot, chain, cfg=cfg, signal_series=sig, lags=lags, base=base,
                                start=args.start, end=args.end)
            covid_by_lag[name] = res["per_lag"]["covid"] * 100
            spreads[name] = res["spread"][["covid", "sharpe", "cagr", "maxdd"]]

        _show(pd.DataFrame(perf).T, f"Performance + cost drag [{src}]")
        _show(pd.DataFrame(turn).T, f"Gate-driven turnover (per year) [{src}]", fmt="8.2f")
        _show(pd.DataFrame(covid_by_lag), f"COVID portfolio return % by signal lag (days) [{src}]", fmt="8.2f")
        _show(pd.DataFrame(spreads).T, f"Lag-jitter spread = max−min over lags {lags} [{src}]")

    print(f"\nran in {time.time() - t0:.0f}s")
    print(
        "\nRead: on vx1 the hard gate has the audit's COVID knife edge (−1.9%→−33.9% at +1 lag);\n"
        "  the proposal's hyst (off 13.5) tracks it exactly, ramp only half-dampens it at a steep\n"
        "  level cost. Only a wider dead band (hyst-wide, off 12) holds the on-state through the\n"
        "  2019-20 sawtooth — i.e. lag-robustness is a *signal* lever (cmf30 fixes even the hard\n"
        "  gate for free), not a gate-*shape* lever. Softening buys fewer deploy events but no\n"
        "  cost saving (roll-driven) — hyst sits 'on' more, so its drag is slightly higher."
    )


if __name__ == "__main__":
    main()
