"""VIX-call ladder sleeve — the original VXTH-style hedge, as a pluggable sleeve.

Holds ``ladder_dtes`` OTM VIX calls (one rung per tenor) at the target ``delta``,
rolling the front rung at expiry. Optional ``monetize_mult`` sells a rung once it
reaches Nx its entry, moving proceeds to the base. This is a behavior-preserving
extraction of the logic that used to live inline in ``engine.simulate``.

**Ladder sizing** (``params["sizing"]``) — how the regime's target dollars are
split across the rungs:

* ``"equal_dollars"`` (**default**) — each priced rung gets ``target / n_rungs``
  dollars (its own ``units = (target/n) / mid``), so the budget is split evenly
  across tenors and the cheaper near-dated rungs buy proportionally more contracts.
* ``"equal_contracts"`` — one shared ``n_units`` for every rung, so the value is
  ``n_units * sum(rung mids)`` and each tenor's *dollar* weight is its premium (the
  long-dated rungs carry more of the budget). This is the original VXTH/MSE448
  behavior; the published-figure reproduction tests pin it explicitly so those
  numbers stay reproducible.

The two coincide for a single-rung (non-ladder) config.

**Expiry universe** (``params["expiry"]``) — which listed expirations are selectable:

* ``"all"`` (**default**) — pick the expiration nearest the target DTE from every listed
  expiry, including VIX **weeklys** (present in the chain from 2016 on).
* ``"monthly"`` — restrict to standard monthly VIX settlements (the VXTH roll calendar;
  see :func:`~vix_hedge.vxth.backtest.is_monthly_vix_expiry`). Use this to **replicate
  VXTH**: with weeklys in the chain, "nearest 30 DTE" can grab a weekly phased a week off
  the monthly cycle, which flips a single call's COVID payoff (it expires just before the
  spike peak). A single 30Δ ``expiry="monthly"`` call tracks the official index; the tenor
  ladder doesn't need it (its 60/90-day rungs span any spike regardless of phase).

**Roll policy** (``params["roll"]``) — what a roll does to the surviving rungs:

* ``"rebalance"`` (default) — re-strike the *whole* ladder to the budget each roll
  (every rung's size is reset). Faithful to VXTH's monthly re-strike, but it trims
  winners / tops up decayers and trades all rungs each period.
* ``"add_only"`` — leave the surviving rungs untouched and only buy the freshly
  added long-end rung, sized to one budget slice. Minimises turnover and only pays
  the spread on the new rung. Requires ``sizing="equal_dollars"`` (it carries each
  rung's own unit count across rolls).
* ``"ratchet"`` — **decouple the entry gate from the exit.** Like ``rebalance`` it
  funds every rung *up* to its budget share each roll, but a rung's contract count can
  only RISE — winners are never trimmed back to the (small or zero) regime budget. So
  the regime gate keeps its *entry* job (buy toward target; buy nothing when the regime
  weight is 0, i.e. forward VIX >50 or <15 — "don't buy expensive vol") but loses its
  *exit* job of dumping the whole alive ladder at the >50 cliff. The >50 shut-off in
  stock VXTH does both jobs at once and so banks the COVID/GFC spikes for free — luck
  that depends on the monthly roll landing near the top. With ``ratchet`` the spike is
  instead banked by an **explicit** value-based monetization (below). Requires
  ``sizing="equal_dollars"``.

**Monetization** — how an appreciated hedge is harvested (independent of the roll):

* ``monetize_mult`` (a scalar) / ``params["monetize_schedule"]`` (``{value/entry: frac}``)
  — *per-rung* rules: sell a rung when its own value reaches Nx its entry premium.
* ``params["monetize_budget"]`` (``{value/cost: frac}``) — a *sleeve-level* trim: bank a
  fraction of the whole sleeve once its value reaches k× its cost basis (Σ units·entry).
  This is the explicit, payoff-gated exit that replaces the VIX-level >50 cliff; pair it
  with ``roll="ratchet"`` to separate the bank-the-spike decision from the entry gate.
"""

from __future__ import annotations

import numpy as np

from vix_hedge.vxth.backtest import select_calls
from vix_hedge.vxth.sleeves import _Rung, register_sleeve


def _rung_value(chain_day, vix_close: float, r: _Rung) -> float:
    """Per-rung value: intrinsic ``max(0, VIX - K)`` at/after expiry, else mid."""
    if (r.tenor - chain_day.date).days <= 0:
        return max(0.0, vix_close - r.strike)
    m = chain_day.mid("C", r.tenor, r.strike)
    return m if np.isfinite(m) else r.value


@register_sleeve("vix_call")
class VixCallLadderSleeve:
    def __init__(self, chain, cfg, *, cost_model=None):
        self.chain = chain
        self.cfg = cfg
        self.cost = cost_model
        self.rungs: list[_Rung] = []
        self.n_units = 0.0  # shared contract count (equal-contracts sizing)
        sizing = (self.cfg.params or {}).get("sizing", "equal_dollars")  # equal_dollars is the default
        if sizing not in ("equal_dollars", "equal_contracts"):
            raise ValueError(f"unknown sizing {sizing!r}; expected 'equal_dollars' or 'equal_contracts'")
        self._equal_dollars = sizing == "equal_dollars"
        self._units: dict[tuple, float] = {}  # per-(tenor,strike) contracts (equal-dollars sizing)
        # Roll policy: "rebalance" (default) re-strikes the whole ladder to the budget
        # each roll; "add_only" leaves the surviving rungs untouched and only buys the
        # freshly added long-end rung (minimal turnover / transaction cost); "ratchet"
        # funds *up* to the budget but never trims a rung (decoupled entry gate — see
        # the module docstring).
        roll = (self.cfg.params or {}).get("roll", "rebalance")
        if roll not in ("rebalance", "add_only", "ratchet"):
            raise ValueError(f"unknown roll {roll!r}; expected 'rebalance', 'add_only', or 'ratchet'")
        self._add_only = roll == "add_only"
        self._ratchet = roll == "ratchet"
        if (self._add_only or self._ratchet) and not self._equal_dollars:
            raise ValueError(f"roll={roll!r} requires sizing='equal_dollars' (it carries per-rung units)")
        # Expiry universe: "all" (default) picks the nearest-DTE listed expiration;
        # "monthly" restricts to standard monthly VIX settlements (the VXTH roll
        # calendar) so a single call doesn't land on a weekly phased off the monthly
        # cycle — required to replicate VXTH (see backtest.is_monthly_vix_expiry).
        expiry = (self.cfg.params or {}).get("expiry", "all")
        if expiry not in ("all", "monthly"):
            raise ValueError(f"unknown expiry {expiry!r}; expected 'all' or 'monthly'")
        self._monthly = expiry == "monthly"

    def _charge(self, notional: float) -> float:
        """Half-spread cost of trading ``notional`` of premium (0 when frictionless)."""
        return self.cost.on_trade(notional, instrument="vix_call") if self.cost is not None else 0.0

    def _deltas(self) -> list[float]:
        """The strike ladder — one rung per delta target. ``params["deltas"]``
        (a list) diversifies across strikes at each tenor; absent it, the single
        ``cfg.delta`` (unchanged VXTH behavior, byte-identical to the old ladder)."""
        ds = (self.cfg.params or {}).get("deltas")
        return [float(x) for x in ds] if ds else [self.cfg.delta]

    def _new_ladder(self, day, keep: list[_Rung]) -> list[_Rung]:
        rungs = list(keep)
        have = {r.tenor for r in rungs}  # tenors already on the ladder (1+ strikes each)
        need = len(self.cfg.ladder_dtes) - len(have)
        deltas = self._deltas()
        for t_open in sorted(self.cfg.ladder_dtes, reverse=True)[:need]:
            pick = select_calls(day, t_open, tuple(deltas), monthly_only=self._monthly)
            if pick is None or pick[0] in have:
                continue
            ex, strikes = pick
            for k in dict.fromkeys(strikes):  # de-dup strikes when two deltas collide
                rungs.append(_Rung(tenor=ex, strike=k, entry=0.0))
            have.add(ex)
        return sorted(rungs, key=lambda r: (r.tenor, r.strike))

    def _units_of(self, r: _Rung) -> float:
        """Contracts held of rung ``r`` — the shared ``n_units`` (equal-contracts) or
        the per-rung count fixed at the last roll (equal-dollars)."""
        return self._units.get((r.tenor, r.strike), 0.0) if self._equal_dollars else self.n_units

    def _has_position(self) -> bool:
        """Whether the sleeve holds anything (gates monetization in either sizing)."""
        return any(self._units.values()) if self._equal_dollars else self.n_units > 0

    def can_open(self, date, prices: dict) -> bool:
        return bool(self._new_ladder(self.chain.day(date), []))

    def is_roll(self, date) -> bool:
        return (not self.rungs) or min((r.tenor - date).days for r in self.rungs) <= 0

    def mark(self, date, prices: dict) -> None:
        day = self.chain.day(date)
        vix = prices["VIX"]
        for r in self.rungs:
            r.value = _rung_value(day, vix, r)

    def value(self) -> float:
        if self._equal_dollars:
            return sum(self._units_of(r) * r.value for r in self.rungs)
        return self.n_units * sum(r.value for r in self.rungs)

    def monetize(self, date, prices: dict, base) -> None:
        # Sleeve-level value-vs-budget trim (``params["monetize_budget"]``): bank a
        # fraction of the *whole* sleeve once its value reaches k× its cost basis. This
        # is the explicit exit that replaces the >50-VIX cliff — it banks on realized
        # payoff rather than on the VIX level. Takes precedence over the per-rung rules.
        budget_sched = (self.cfg.params or {}).get("monetize_budget")
        if budget_sched is not None:
            self._monetize_budget(prices, base, budget_sched)
            return
        # A partial, multi-threshold schedule (``{multiple: fraction}``) takes
        # precedence; falls back to the single full-liquidation ``monetize_mult`` when
        # no schedule is given (unchanged behavior).
        schedule = (self.cfg.params or {}).get("monetize_schedule")
        if schedule:
            if not (self.rungs and self._has_position()):
                return
            from vix_hedge.vxth.monetize import apply_schedule

            hw = getattr(self, "_mon_hw", None)
            if hw is None:
                hw = self._mon_hw = {}  # lazily owned high-water state ({pos -> max value/entry})
            # The spread is paid on the proceeds sold, so deposit the net. Under
            # equal-dollars each rung has its own unit count, so pass ``units_of``
            # (the scalar ``n_units`` is unused then).
            self.rungs = apply_schedule(
                self.rungs, self.n_units, hw, schedule,
                deposit=lambda d: base.add(d - self._charge(d), prices),
                units_of=self._units_of if self._equal_dollars else None,
            )
            return
        if not self.cfg.monetize_mult:  # per-rung ``u > 0`` below gates an empty position
            return
        keep = []
        for r in self.rungs:
            u = self._units_of(r)
            if u > 0 and r.entry > 0 and r.value >= self.cfg.monetize_mult * r.entry:
                proceeds = u * r.value
                base.add(proceeds - self._charge(proceeds), prices)  # less the half-spread on the sale
            else:
                keep.append(r)
        self.rungs = keep

    def _monetize_budget(self, prices: dict, base, schedule) -> None:
        """Bank a fraction of the whole sleeve once value/cost-basis crosses each
        multiple in ``schedule`` (``{k: fraction}``). The "budget" is the cost basis
        of the *current* holdings (``Σ units·entry`` = premium paid), so the trigger is
        the aggregate multiple-on-cost — the sleeve-level analogue of the per-rung
        ``monetize_schedule``, and a direct, value-based replacement for the VIX cliff.

        The high-water (``_budget_hw``) is on the value/cost ratio and is reset at each
        roll (``roll_and_fund``), so it tracks the multiple reached *within the current
        roll cycle* — fresh each ~month and never stale-blocked by a prior crash's peak.
        Selling scales every rung's units by the retained fraction, which scales value
        and cost equally (ratio unchanged), so a sale never re-triggers the same level.
        """
        from vix_hedge.vxth.monetize import normalize_schedule, remaining_fraction

        if not (self.rungs and self._has_position()):
            return
        cost = sum(self._units_of(r) * r.entry for r in self.rungs)  # premium paid for the holdings
        val = self.value()
        if cost <= 0.0 or val <= 0.0:
            return
        sched = normalize_schedule(schedule)
        prev_hw = getattr(self, "_budget_hw", 0.0)
        new_hw = max(prev_hw, val / cost)
        self._budget_hw = new_hw
        before = remaining_fraction(prev_hw, sched)
        after = remaining_fraction(new_hw, sched)
        if before <= 0.0 or after >= before:
            return  # no threshold freshly crossed this day
        sell_frac = 1.0 - after / before  # fraction of the *current* sleeve to bank now
        proceeds = sell_frac * val
        base.add(proceeds - self._charge(proceeds), prices)  # less the half-spread on the sale
        keep = 1.0 - sell_frac
        if self._equal_dollars:
            self._units = {k: u * keep for k, u in self._units.items()}
        else:
            self.n_units *= keep

    def _prune_units(self) -> None:
        """Drop per-rung unit entries for rungs no longer on the ladder."""
        keys = {(r.tenor, r.strike) for r in self.rungs}
        self._units = {k: u for k, u in self._units.items() if k in keys}

    def roll_and_fund(self, date, prices: dict, target_dollars: float) -> float:
        day = self.chain.day(date)
        survivors = [r for r in self.rungs if (r.tenor - date).days > 0]
        self.rungs = self._new_ladder(day, survivors)
        new = [r for r in self.rungs if r.entry == 0.0]  # the freshly opened rung(s)
        for r in new:
            m = day.mid("C", r.tenor, r.strike)
            r.entry = r.value = m if np.isfinite(m) else 0.0
        self._budget_hw = 0.0  # a roll re-bases the budget -> reset the monetize_budget high-water

        if self._ratchet:
            # Decoupled entry gate: fund *up* to the budget but never trim a rung. Each
            # rung's contract count can only RISE — winners are carried at full size, never
            # re-struck down to the (small/zero) regime budget. So the gate still does its
            # *entry* job — it buys toward ``target_dollars`` and, when the regime weight is
            # 0 (forward VIX >50 or <15), the per-rung target is 0 and nothing new is bought
            # ("don't buy expensive vol") — but it no longer does its *exit* job of dumping
            # the alive ladder at the >50 cliff. Banking the spike is delegated to an
            # explicit value-based monetization (``monetize_budget`` / ``monetize_schedule``).
            # The half-spread is paid only on the freshly bought contracts (no sale to pay on).
            live = [r for r in self.rungs if r.value > 0]
            per = (target_dollars / len(live)) if live else 0.0
            traded = 0.0
            units: dict[tuple, float] = {}
            for r in live:
                key = (r.tenor, r.strike)
                cur = self._units.get(key, 0.0)
                u = max(cur, per / r.value)  # ratchet: top up to the budget share, never sell
                units[key] = u
                traded += (u - cur) * r.value  # premium of the freshly bought contracts only
            self._units = units
            return self.value() + self._charge(traded)

        if self._add_only:
            # Minimal-turnover roll: the survivors keep their contracts untouched; only
            # the new long-end rung is bought, sized to one budget slice (target / ladder
            # size, i.e. "1%/3 of the new call"). The expired rung's value is returned to
            # the base by the engine (it drops out of value()), and the half-spread is
            # charged on the *new* premium only — not on the carried rungs.
            per = target_dollars / len(self.cfg.ladder_dtes)
            for r in new:
                self._units[(r.tenor, r.strike)] = (per / r.value) if r.value > 0 else 0.0
            self._prune_units()
            traded = sum(self._units_of(r) * r.value for r in new)
            return self.value() + self._charge(traded)

        if self._equal_dollars:
            # Split the budget evenly across rungs with a (positive) mark, so each
            # tenor carries the same dollars and the cheaper rungs buy more contracts.
            # Premium deployed still totals ~target (the sum of the equal slices).
            live = [r for r in self.rungs if r.value > 0]
            per = (target_dollars / len(live)) if live else 0.0
            self._units = {(r.tenor, r.strike): per / r.value for r in live}
            premium = per * len(live)
        else:
            sum_val = sum(r.value for r in self.rungs)
            self.n_units = (target_dollars / sum_val) if sum_val > 0 else 0.0
            premium = self.n_units * sum_val
        # Pay the half-spread on the premium established. Charging the full
        # position each roll treats every roll as a round-trip — exact for the
        # single-call config (100% turnover/month), a conservative upper bound for
        # the ladder where ~2/3 of the rungs carry over (the "add_only" roll above is
        # the honest low-turnover counterpart). Returns premium + cost so the engine
        # debits both from the base (the cost leaves the portfolio).
        return premium + self._charge(premium)
