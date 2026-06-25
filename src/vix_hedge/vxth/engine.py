"""Flexible tail-hedge engine (the MSE448 report's strategy).

Simulates one portfolio = a *base* asset mix (SPX) plus an optional hedge
**sleeve**, with every knob the report sweeps:

* **delta**          -- target call delta (lower = cheaper, more convex; §4.3)
* **alloc**          -- regime -> hedge-weight schedule on forward VIX (§4.4)
* **ladder_dtes**    -- a single ~30d call, or a 30/60/90 ladder (§4.5)
* **monetize_mult**  -- sell a rung once it reaches Nx its entry (§4.6)
* **instrument**     -- which hedge sleeve to use (default ``"vix_call"``; new
                        instruments register in :mod:`vix_hedge.vxth.sleeves`)

Accounting is in dollars. Each day the sleeve is marked and (if hedged)
monetized; at each roll the whole portfolio is rebalanced to the regime's target
weight. A run with an all-zero ``alloc`` (``cfg=None``) is just the rebalanced
base benchmark, so the same function produces hedged and unhedged curves.

The hedge logic itself lives behind the :class:`~vix_hedge.vxth.sleeves.Sleeve`
interface, so adding an instrument means adding a sleeve module — not editing
this loop.
"""

from __future__ import annotations

from dataclasses import dataclass, field

import numpy as np
import pandas as pd

from vix_hedge.config import STARTING_BALANCE
from vix_hedge.data.load import OptionChain
from vix_hedge.vxth.backtest import forward_vix_signal
from vix_hedge.vxth.gate import make_gate
from vix_hedge.vxth.sleeves import _BASE_ASSETS, _Base, make_sleeve

ALLOC_OFFICIAL = (0.0, 0.010, 0.005, 0.0)  # CBOE VXTH: 1% in 15-30, 0.5% in 30-50
ALLOC_REVERSED = (0.0, 0.005, 0.010, 0.0)  # report §4.4: reversed -> better

# Named base portfolios (weights over the base assets in ``_BASE_ASSETS``).
BASES = {
    "SPX": {"SPX": 1.0},
}


@dataclass
class HedgeConfig:
    """All hedge knobs for one strategy variant."""

    delta: float = 0.30
    ladder_dtes: tuple[int, ...] = (30,)
    alloc: tuple[float, ...] = ALLOC_OFFICIAL
    monetize_mult: float | None = None
    signal: str = "forward"
    label: str = "hedge"
    instrument: str = "vix_call"
    #: Number of overlapping entry-date tranches the *deployed* strategy is the
    #: equal-weight mean of (1 = a single cohort). ``simulate`` itself always runs
    #: one cohort; the tranching is assembled by :mod:`vix_hedge.vxth.ensemble`
    #: (``tranched_strategy``), which reads this field. Higher N cuts entry-date
    #: "timing luck" ~1/N at no cost to mean return.
    n_tranches: int = 1
    #: Instrument-specific knobs (put-spread strikes, monetize schedule, …) so new
    #: sleeves add config here instead of editing HedgeConfig's fields.
    params: dict = field(default_factory=dict)


def simulate(
    spot: pd.DataFrame,
    chain: OptionChain,
    *,
    base: dict,
    cfg: HedgeConfig | None = None,
    start: str | None = "2006-03-22",
    end: str | None = None,
    starting_balance: float = STARTING_BALANCE,
    signal_series: pd.Series | None = None,
    cost_model=None,
) -> pd.Series:
    """Equity curve for one portfolio. ``cfg=None`` -> unhedged base benchmark."""
    hedged = cfg is not None
    cfg = cfg or HedgeConfig(alloc=(0, 0, 0, 0))
    alloc = cfg.alloc
    if signal_series is None:
        signal_series = forward_vix_signal(chain, spot) if cfg.signal == "forward" else spot["VIX"]

    dates = pd.DatetimeIndex(np.intersect1d(chain.trade_dates, spot.index.to_numpy()))
    if start:
        dates = dates[dates >= pd.Timestamp(start)]
    if end:
        dates = dates[dates <= pd.Timestamp(end)]

    sleeve = make_sleeve(cfg.instrument, chain, cfg, cost_model=cost_model)
    # Forward-VIX -> hedge-weight gate. Default (params["gate"] absent) is the hard
    # step schedule, so unconfigured runs are unchanged; "ramp"/"hysteresis" soften
    # the 0<->1% cliff (see vix_hedge.vxth.gate). Stateful: weight() is called once
    # per roll in date order below.
    gate = make_gate(alloc, cfg.params.get("gate"))

    def prices(date) -> dict:
        # any spot column named in `base` is a valid base asset (e.g. a precomputed
        # blended index), on top of the standard set — existing bases unchanged.
        p = {a: float(spot.at[date, a]) for a in {*_BASE_ASSETS, *base} if a in spot.columns}
        p["VIX"] = float(spot.at[date, "VIX"])
        return p

    # --- locate first tradeable date ---
    first = None
    for d in dates:
        if not hedged or sleeve.can_open(d, prices(d)):
            first = d
            break
    if first is None:
        raise RuntimeError("no hedge positions available in range")
    dates = dates[dates >= first]

    base_sleeve = _Base(weights=base)
    total0 = starting_balance
    p0 = prices(first)
    v0 = gate.weight(signal_series.at[first]) if hedged else 0.0
    deployed = sleeve.roll_and_fund(first, p0, v0 * total0)
    base_sleeve.set_to(total0 - deployed, p0)

    out = np.empty(len(dates))
    for t, date in enumerate(dates):
        p = prices(date)
        sleeve.mark(date, p)
        if hedged:
            sleeve.monetize(date, p, base_sleeve)
        total = base_sleeve.value(p) + sleeve.value()
        out[t] = total

        if sleeve.is_roll(date):  # roll + rebalance to the regime weight
            v = gate.weight(signal_series.at[date]) if hedged else 0.0
            deployed = sleeve.roll_and_fund(date, p, v * total)
            base_sleeve.set_to(total - deployed, p)

    return pd.Series(out, index=dates, name=cfg.label if hedged else "base")
