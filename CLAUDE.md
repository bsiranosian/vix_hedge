# CLAUDE.md — working agreement for the vix_hedge (public) repo

A focused Python reproduction of the VIX-call tail-hedge analysis (MSE448): the
delta dial, the 30/60/90 tenor ladder, and the timing-/boundary-luck robustness
story, on the 2006–2020 sample and extended to 2025. Raw data lives **outside**
the repo in `../option_data`; cleaned parquet caches are in `data/cache/`
(git-ignored). See `README.md`.

This repo intentionally excludes other research directions explored separately.
Don't add them back here.

## Conventions

- **Env / tooling:** `uv`. Run code with `uv run python …`, tests with
  `uv run python -m pytest -q`, lint with
  `uv run ruff check src/ tests/ scripts/ current_data_extension/`.
- **Style:** ruff (line length 120; E/F/W/I/B/UP). Match the surrounding code:
  terse docstrings that say *why*, type hints, `from __future__ import annotations`.
- **Tests:** hermetic where possible; data-dependent tests `skipif` the parquet
  cache is absent. Every new module gets a test.
- **Numbers are validated.** `tests/test_vxth_engine.py` reproduces published VXTH
  figures — do not change engine behavior without keeping these green.
- **Don't commit data.** Nothing under `data/cache/` or `../option_data`. The
  committed deliverables are the interactive HTML reports + PNGs under `results/`.

## The sleeve interface — `vix_hedge.vxth.sleeves`

`engine.simulate` drives **one `Sleeve` plus a base portfolio**. A new instrument
is a new module implementing the `Sleeve` protocol, registered with
`@register_sleeve("name")` and selected via `HedgeConfig(instrument="name")` — so
it does NOT require editing `engine.simulate`. `sleeves/vix_call.py` is the worked
reference (the VIX-call ladder). Instrument-specific knobs go in
`HedgeConfig.params` (a dict). `prices` is a dict of base-asset closes plus
`"VIX"`; all amounts are dollars.

## Report every result as a distribution

Evaluate ideas through the entry-date ensemble + tranching harness
(`vxth/ensemble.py`, which both *measures* and *reduces* timing luck) and the
lag-jitter harness (`vxth/lag_jitter.py`, for gate/boundary luck). Report
distributions, not point estimates, with the honest cost / payoff / robustness
tradeoff. Use the standard crash windows in `vxth/episodes.py` for payoff cells.
