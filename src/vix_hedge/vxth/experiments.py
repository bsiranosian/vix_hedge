"""Reproduce the MSE448 report's experiments (§4.1-4.7) as report sections.

Each :class:`Section` bundles the equity curves, a metrics table, and the
report's published numbers for side-by-side comparison. A :class:`CurveLibrary`
memoizes the engine runs so shared variants (e.g. the SPX benchmark, the
all-modifications hedge) are computed once.

Caveat reproduced honestly: single-call variants (§4.3 delta, §4.4 allocation)
are highly timing-luck-sensitive -- the report flags this throughout -- so their
numbers diverge more than the robust ladder / all-modifications configs, which
match closely.
"""

from __future__ import annotations

from dataclasses import dataclass, field

import pandas as pd

from vix_hedge import metrics
from vix_hedge.config import STARTING_BALANCE
from vix_hedge.data.load import OptionChain
from vix_hedge.vxth import benchmark
from vix_hedge.vxth.backtest import forward_vix_signal
from vix_hedge.vxth.engine import (
    ALLOC_OFFICIAL,
    ALLOC_REVERSED,
    BASES,
    HedgeConfig,
    simulate,
)

START = "2006-03-22"  # VXTH index inception; common start for all backtests


@dataclass
class Section:
    key: str
    title: str
    blurb: str
    curves: pd.DataFrame
    metrics: pd.DataFrame
    report: pd.DataFrame | None = None  # published numbers, indexed like metrics
    extra: dict = field(default_factory=dict)  # transition matrix, return stats, ...


def metrics_table(curves: pd.DataFrame) -> pd.DataFrame:
    """CAGR (%), annualized Sharpe, max drawdown (%) per portfolio."""
    rows = {}
    for c in curves.columns:
        v = curves[c].dropna()
        rows[c] = {
            "CAGR %": round(100 * metrics.cagr(v), 2),
            "Sharpe": round(metrics.annualized_sharpe(v), 2),
            "MaxDD %": round(100 * metrics.max_drawdown(v), 1),
        }
    return pd.DataFrame(rows).T


class CurveLibrary:
    """Lazily computes and caches engine runs keyed by (base, config)."""

    def __init__(self, spot: pd.DataFrame, chain: OptionChain, start: str = START,
                 end: str | None = None, signal_source: str = "hybrid_vx1"):
        self.spot, self.chain, self.start, self.end = spot, chain, start, end
        self.signal_source = signal_source
        self.signal = forward_vix_signal(chain, spot, source=signal_source)
        self._cache: dict[tuple, pd.Series] = {}

    def base(self, base_name: str) -> pd.Series:
        return self._run(base_name, None)

    def hedge(self, base_name: str, cfg: HedgeConfig) -> pd.Series:
        return self._run(base_name, cfg)

    def _run(self, base_name: str, cfg: HedgeConfig | None) -> pd.Series:
        key = (base_name, _cfg_key(cfg))
        if key not in self._cache:
            s = simulate(
                self.spot, self.chain, base=BASES[base_name], cfg=cfg,
                start=self.start, end=self.end, signal_series=self.signal,
            )
            self._cache[key] = s
        return self._cache[key].copy()

    def vxth_index(self, dates: pd.DatetimeIndex) -> pd.Series:
        return benchmark.vxth_curve(benchmark.load_vxth_index(), dates, STARTING_BALANCE)


def _cfg_key(cfg: HedgeConfig | None):
    if cfg is None:
        return None
    return (cfg.instrument, cfg.delta, cfg.ladder_dtes, cfg.alloc, cfg.monetize_mult, cfg.signal,
            cfg.n_tranches, tuple(sorted(cfg.params.items())))


# Named hedge configs used across sections -------------------------------------
# Single calls use the monthly VIX settlement calendar (params expiry="monthly") so they
# replicate VXTH's roll; the nearest-DTE default would grab weeklys (see vix_call sleeve).
SINGLE = {"expiry": "monthly"}
BASELINE = HedgeConfig(0.30, (30,), ALLOC_OFFICIAL, label="hedge (30d, 30Δ)", params=SINGLE)
ALL_MODS = HedgeConfig(0.10, (30, 60, 90), ALLOC_REVERSED, monetize_mult=100, label="all modifications")


def _df(named: dict[str, pd.Series]) -> pd.DataFrame:
    return pd.DataFrame(named)


def build_sections(lib: CurveLibrary) -> list[Section]:
    from vix_hedge import vix_returns as vr  # local: only the report's §4.2 section needs it

    spx = lib.base("SPX")
    base_hedge = lib.hedge("SPX", BASELINE)
    sections: list[Section] = []

    # -- §4.1 VXTH replication -------------------------------------------------
    vxth = lib.vxth_index(spx.index)
    curves = _df({"SPX": spx, "VXTH (official)": vxth, "VXTH (replicated)": base_hedge})
    rep = pd.DataFrame(
        {"CAGR %": [7.49, 12.2, 8.27], "Sharpe": [0.49, 0.67, 0.62], "MaxDD %": [52.5, 37.4, 35.1]},
        index=["SPX", "VXTH (official)", "VXTH (replicated)"],
    )
    sections.append(Section(
        "vxth", "§4.1 VXTH replication",
        "Replicate the CBOE VXTH index: a single ~30-day, 30-delta VIX call bought monthly "
        "with the official allocation schedule (1% in 15-30, 0.5% in 30-50).",
        curves, metrics_table(curves), rep,
    ))

    # -- §4.2 trade signals ----------------------------------------------------
    vix = lib.spot.loc[lib.start:, "VIX"]
    regime = vr.regime_series(vix)
    sig_note = {
        "hybrid_vx1": "The regime is read off the <b>real CBOE front-month VIX future (VX1)</b> from 2013 on "
                      "(CBOE's free per-contract history), and a put-call-parity forward before that; "
                      "the real future hedges a little less in calm markets, nudging every variant toward "
                      "the report's published numbers.",
        "hybrid_cmf30": "The regime is read off the real 30-day constant-maturity VIX forward from 2013 on, "
                        "and a put-call-parity forward before.",
        "parity": "The regime is read off a put-call-parity forward derived from the VIX options "
                  "(F = K + C − P at the ATM strike) — a faithful stand-in for the 30-day VIX forward.",
    }.get(lib.signal_source, "")
    sections.append(Section(
        "signals", "§4.2 Trade signals: VIX regimes & transitions",
        "VIX rarely jumps more than one threshold band per day -- it never reaches the top band "
        "without first passing through the middle -- so skipping the hedge at the lowest band costs little. "
        + sig_note,
        _df({"VIX": vix}), pd.DataFrame(),
        extra={"transition_matrix": vr.transition_matrix(regime), "thresholds": (15, 30, 50)},
    ))

    # -- §4.3 option delta -----------------------------------------------------
    deltas = [0.50, 0.30, 0.20, 0.10, 0.05]
    named = {"SPX": spx}
    for d in deltas:
        cfg = HedgeConfig(d, (30,), ALLOC_OFFICIAL, label=f"{int(d * 100)}Δ", params=SINGLE)
        named[f"{int(d * 100)}-delta"] = lib.hedge("SPX", cfg)
    curves = _df(named)
    stats = vr.option_return_stats(lib.chain, lib.spot, dte=90, delta=0.10)
    sections.append(Section(
        "delta", "§4.3 Option delta",
        "Lower-delta calls are cheaper and more convex: they expire worthless more often but pay "
        "off far larger in a spike. A 90-day 10-delta call expires worthless ~97% of the time, yet a "
        "few exceed 50x. These single calls roll on the standard <b>monthly</b> VIX settlement (VXTH's "
        "calendar); their crash payoff is roll-phase-sensitive — a one-week offset flips it — which is what "
        "the §4.5 tenor ladder fixes.",
        curves, metrics_table(curves), _report_row({"30-delta": (8.27, 0.62), "10-delta": (8.90, 0.58)}),
        extra={"return_stats": stats},
    ))

    # -- §4.4 hedge allocation -------------------------------------------------
    curves = _df({
        "SPX": spx,
        "official (1%/0.5%)": base_hedge,
        "reversed (0.5%/1%)": lib.hedge(
            "SPX", HedgeConfig(0.30, (30,), ALLOC_REVERSED, label="reversed", params=SINGLE)),
    })
    sections.append(Section(
        "allocation", "§4.4 Hedge allocation",
        "Reversing the VXTH schedule (0.5% in 15-30, 1% in 30-50) spends less in mild vol and more "
        "as a spike builds. (Single calls here roll on the monthly VIX settlement; the tenor ladder, §4.5, "
        "is the roll-phase-robust structure.)",
        curves, metrics_table(curves), _report_row({"reversed (0.5%/1%)": (9.81, 0.58)}),
    ))

    # -- §4.5 DTE ladder -------------------------------------------------------
    ladder = HedgeConfig(0.30, (30, 60, 90), ALLOC_OFFICIAL, label="ladder 30/60/90")
    curves = _df({"SPX": spx, "single 30d call": base_hedge, "ladder 30/60/90": lib.hedge("SPX", ladder)})
    sections.append(Section(
        "ladder", "§4.5 DTE ladder",
        "Holding a ladder of 30/60/90-day calls (rolling the front into a new 90-day) keeps a hedge "
        "in place at all horizons, cutting timing luck.",
        curves, metrics_table(curves), _report_row({"ladder 30/60/90": (9.89, 0.64)}),
    ))

    # -- §4.6 monetization -----------------------------------------------------
    mon_base = HedgeConfig(0.10, (30, 60, 90), ALLOC_REVERSED, label="hold to expiry")
    mon_100 = HedgeConfig(0.10, (30, 60, 90), ALLOC_REVERSED, monetize_mult=100, label="sell at 100x")
    curves = _df({
        "SPX": spx,
        "hold to expiry": lib.hedge("SPX", mon_base),
        "sell at 100x": lib.hedge("SPX", mon_100),
    })
    sections.append(Section(
        "monetization", "§4.6 Monetization rule",
        "Selling a call once it reaches 100x its entry (reinvesting in the base) locks in spike gains "
        "before they can round-trip to worthless -- trading some absolute return for a higher Sharpe.",
        curves, metrics_table(curves), _report_row({"sell at 100x": (8.14, 0.75)}),
    ))

    # -- §4.7 all modifications ------------------------------------------------
    all_spx = lib.hedge("SPX", ALL_MODS)
    curves = _df({"SPX": spx, "VXTH baseline": base_hedge, "all modifications": all_spx})
    sections.append(Section(
        "allmods", "§4.7 All modifications",
        "Combining 10-delta calls, the reversed allocation, the 30/60/90 ladder, and 100x monetization.",
        curves, metrics_table(curves), _report_row({"all modifications": (11.41, 0.65)}),
    ))

    return sections


def _report_row(values: dict[str, tuple[float, float]]) -> pd.DataFrame:
    """Build a small report-targets frame from {name: (cagr, sharpe)}."""
    return pd.DataFrame(
        {"CAGR %": {k: v[0] for k, v in values.items()}, "Sharpe": {k: v[1] for k, v in values.items()}}
    )
