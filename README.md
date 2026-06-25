# vix_hedge

Hedging an equity portfolio against market crashes with **VIX call options** — a
clean, tested Python reproduction of the core results from the MSE448 report
*Tail-risk hedging with VIX calls*, run on the original 2006–2020 sample **and**
extended to 2025.

This repo is deliberately scoped to the **headline strategy and its robustness
story**. Other research directions explored separately are not included here.

## The three headline results

1. **Call delta is the Sharpe ↔ convexity dial.** A near-the-money **50Δ** ladder
   is the smoothest, highest-Sharpe hedge in the study (Sharpe ≈ 0.66, ~14% vol),
   beating unhedged SPX on Sharpe, CAGR *and* drawdown while still cushioning
   COVID. Going deeper out-of-the-money trades that
   smoothness for crash convexity (a **5Δ** ladder turns COVID into **+232%** but
   its Sharpe falls to ~0.23). The **10Δ** ladder — the delta the original report
   builds its "all modifications" config on — sits mid-dial (+130% through COVID,
   ~14.8% CAGR, Sharpe ≈ 0.29), the convexity-vs-Sharpe compromise.

2. **The 30/60/90 *tenor ladder* is what makes the hedge robust to roll timing.**
   Where you *enter* barely matters — the hedge resizes to the regime weight every
   roll, so staggered start dates re-sync and the full-window spread is ~0.1pp for a
   single call and the ladder alike. What matters is the *roll phase*: which
   expiry a single call holds into a spike. On VXTH's **monthly** settlement calendar
   a single 30Δ call catches COVID and replicates the index — but shift its roll by one
   week onto an adjacent VIX **weekly** and its lone option expires just before the
   peak, flipping COVID from **+29% to −18%** (a ~50pp swing from a one-week offset).
   Holding 30/60/90-day calls simultaneously removes that dependence — a 60/90-day rung
   is always alive through the spike, so the ladder catches COVID on *every* phase and
   beats the single call out-of-sample at every delta. On the right calendar a single
   call ~ties the ladder in-sample; **the ladder's edge is robustness, not raw return.**

3. **The decision threshold can be made boundary-luck-robust.** The hedge weight
   is gated on the forward VIX by a hard `0 ↔ 1%` step at 15. Read on a single
   print of a contract that sawtooths across that boundary near expiry, the COVID
   payoff can ride a knife edge. Using the **30-day constant-maturity forward
   (CMF30)** as the gate signal — and/or softening the gate (ramp / hysteresis) —
   removes that fragility, for the same headline numbers (see
   `run_gate_softening.py`, `run_decoupled_gate.py`).

## Quickstart

```bash
uv sync                                            # Python 3.12 env
uv run python -m vix_hedge.data.build              # raw CSV/gz -> cleaned parquet (~30 s)
uv run python -m vix_hedge.data.vix_futures        # real VX futures from CBOE's CDN -> CMF30 gate (~30 s, online)
uv run python scripts/run_delta_ladder_report.py   # HEADLINE -> results/delta_ladder/report.html
uv run python scripts/run_vxth_report.py           # full §4.1-§4.7 reproduction -> results/vxth/report.html
```

(The futures fetch is optional: if the cache is absent the regime signal falls
back to the put-call-parity forward automatically — it tracks CMF30 to ~0.3 vol
points.)

## Interactive HTML reports

Each is a **self-contained** Plotly page (hover / zoom / legend-toggle, no server,
no external JS) written under `results/`:

| script | report | what it shows |
|---|---|---|
| `scripts/run_delta_ladder_report.py` | `results/delta_ladder/report.html` | **Headline.** Delta dial × single-call-vs-ladder + a *roll-phase timing-luck* section, with a metrics + crash-payoff table per section. |
| `scripts/run_vxth_report.py` | `results/vxth/report.html` | Faithful §4.1–§4.7 reproduction of the report (VXTH replication, signals, delta, allocation, ladder, monetization, all-mods), reproduced numbers vs. published. |
| `current_data_extension/make_report.py` | `results/current_data_extension/report.html` | The **2025 extension explorer**: equity/drawdown, the delta dial across windows, the risk/return frontier, crash payoffs (incl. 2022 / Aug-2024 / 2025), and the regime gate. |

The other scripts print tables / write CSVs + PNGs:

```bash
uv run python scripts/recheck_delta_ladder.py    # delta/ladder metrics + entry-date robustness tables
uv run python scripts/run_ensemble_analysis.py   # entry-date timing-luck distribution + 1/N tranching
uv run python scripts/run_gate_softening.py       # boundary luck: hard vs ramp vs hysteresis gate, + lag-jitter
uv run python scripts/run_decoupled_gate.py       # boundary luck: split the >50 cliff's entry-gate vs bank-the-spike jobs
uv run python scripts/run_vxth.py                 # standalone VXTH ladder run + PNGs
uv run python scripts/run_vix_returns.py          # VIX regime transitions + single-call return distribution
```

## The 2025 data extension

`current_data_extension/` re-runs the headline study on a 2025 OptionMetrics
refresh, 2006 → 2025-08, over three windows (in-sample 2006–2020, full 2006–2025,
out-of-sample 2021–2025) and the new crashes the refresh unlocks (2022 bear,
Aug-2024 unwind, 2025 tariff):

```bash
uv run python current_data_extension/build_extended.py   # build *_extended.parquet caches (needs the 2025 refresh CSV)
uv run python current_data_extension/run_extension.py     # -> results/current_data_extension/extension_metrics.json
uv run python current_data_extension/make_report.py       # -> results/current_data_extension/report.html
```

The headline survives out of sample: the 50Δ 30/60/90 ladder still beats unhedged
SPX on Sharpe, CAGR and vol over the full cycle. The delta dial *flattens and
reverses* OOS — with no systemic crash, near-money wins on return too; deep-OTM
convexity only pays when a big spike actually arrives.

## Validation

Reproduced vs. the report (2006–2020, CAGR % / annualized Sharpe; real-VX1 signal,
`run_vxth_report.py`):

| | reproduced | report |
|---|---|---|
| SPX | 7.4 / 0.49 | 7.49 / 0.49 |
| VXTH (official) | 12.2 / 0.67 | 12.2 / 0.67 |
| VXTH replication (30d, 30Δ)¹ | 10.6 / 0.56 | 8.27 / 0.62 |
| DTE ladder | 10.3 / 0.54 | 9.89 / 0.64 |
| all modifications | 11.2 / 0.47 | 11.41 / 0.65 |

¹ **The single-call replication is on VXTH's monthly settlement calendar**
(`params={"expiry": "monthly"}`), which is what makes it track the index: CAGR 10.6 /
Sharpe 0.56 vs official 12.2 / 0.67, and it catches COVID (+29%). Earlier this row read
**5.3 / 0.40 and *missed* COVID** — a roll-phase artifact: the chain lists VIX *weeklys*
(2016 on), and the nearest-DTE pick grabbed one phased ~a week off the monthly cycle, so
the lone call expired just before the March-2020 peak. That one-week sensitivity is the
timing-luck thesis itself (see *Roll-phase timing luck* in the headline report and
`run_ensemble_analysis.py`): a single call's COVID payoff swings ~50pp across roll phases,
while the **DTE ladder is phase-invariant** (its 60/90-day rungs span any spike), the
robust anchor at ~10.3% CAGR. The residual gap to the official index is methodology
(signal source, strike rounding), not roll phase. `tests/test_vxth_engine.py` pins these.

## The regime signal

The hedge weight follows the one-month-**forward** VIX. Three sources, selected
with `--signal`:

* `hybrid_cmf30` — the real CBOE **30-day constant-maturity VIX forward** where
  available (2013→present), put-call-parity proxy before. **The lag-robust default
  for the headline report.** A constant-maturity point has no front-month roll-day
  sawtooth, so the regime read on a VIX-expiry Wednesday doesn't flip on *which*
  contract you read — same headline numbers as VX1 without hanging the COVID hedge
  on a knife edge.
* `hybrid_vx1` — the real CBOE **front-month VX1** where available, parity before.
  What the published report used, so it stays the signal for the faithful
  reproduction (`run_vxth_report.py`) and the 2025 extension (which holds the gate
  fixed to isolate the *data* refresh).
* `parity` — the forward derived from the VIX options themselves by put-call
  parity (`F = K + C − P` at the ATM strike), self-consistent across the whole
  sample.

## Data

Raw OptionMetrics / purchased EOD data is expected in `../option_data` (override
with `$OPTION_DATA`) and is **not redistributed here** (licensed). The cleaned
parquet caches it produces live under `data/cache/` and are git-ignored. `data.build`
writes:

* `spot_prices.parquet` — daily SPX & VIX closes, 1996 → 2020.
* `vix_options.parquet` — cleaned VIX option rows (mid, delta, bid/ask).
* `vix_futures.parquet` — real front-month / 30-day-CMF VIX futures, 2013 → 2021
  (built by `data.vix_futures` from CBOE's free CDN; the optional CMF30 gate).

Because the underlying chain is licensed, a fresh clone reproduces results only
with access to that raw data; the committed **interactive HTML reports** are the
public artifact.

## Architecture

The engine is built so a new hedge instrument is a new **sleeve module**, not an
edit to the simulator:

```
src/vix_hedge/
  config.py             # data paths ($OPTION_DATA) and constants
  metrics.py            # CAGR / Sharpe / Sortino / drawdown + cagr_impact and the
                        #   arithmetic↔geometric (volatility-tax) split
  plotting.py           # shared matplotlib/Plotly helpers
  data/
    build.py, load.py   # raw -> parquet; OptionChain (fast lazy per-day views)
    vix_futures.py      # real VX futures from CBOE's CDN -> VX1/CMF30 panel
  vxth/
    engine.py           # the flexible hedge simulator (drives a Sleeve; all knobs)
    sleeves/vix_call.py # the VIX-call ladder sleeve (the worked reference)
    backtest.py         # forward-VIX signal, select_call(s), regime
    gate.py             # hard / ramp / hysteresis gates on the forward-VIX threshold
    ensemble.py         # entry-date ensemble + tranching (timing-luck harness)
    lag_jitter.py       # signal-lag robustness harness for the gate (boundary luck)
    costs.py            # transaction-cost model (gross vs net)
    episodes.py         # standard crash windows (DotCom / GFC / Q4-18 / COVID)
    monetize.py         # the §4.6 monetization rule (sell at k× entry)
    experiments.py      # assemble report §4.1-4.7 as Sections
    report.py           # interactive Plotly sectioned HTML; benchmark.py # VXTH index
  vix_returns/          # VIX regime transitions + single-option return distribution
scripts/                # the headline reports + the robustness studies
tests/                  # hermetic unit tests + data-dependent smoke tests
current_data_extension/ # the 2025 refresh study + its interactive explorer
```

`OptionChain` is the performance core: the panel is sorted by date and each trade
day is sliced via binary search and grouped lazily, so the sequential backtest
values legs at ~1 ms/day with flat memory.

## Notes & limitations

* Single-call results hinge on the **roll phase** — which expiry you hold into a spike.
  Single calls here roll on the standard *monthly* VIX settlement calendar
  (`params={"expiry": "monthly"}`, VXTH's roll); on the nearest-DTE default a weekly-inclusive
  chain can land them a week off that cycle and flip the crash payoff. The tenor ladder is
  phase-invariant by construction, which is the robustness it exists to provide.
* Backtests default to **frictionless midpoint** fills; an optional transaction-cost
  model (`vxth/costs.py`) reports **gross vs net** when supplied (per-instrument
  half-spread calibrated from the quoted bid/ask). Integer-contract rounding is out
  of scope (negligible at this book size).
* A real VX1/CMF30 series back to 2006 needs a paid feed; CBOE's free CDN only
  retains VX contracts from ~2013 on (which still covers COVID and every modern
  experiment), and the parity proxy covers the earlier sample.
