"""Standalone interactive explorer for the 2025-refresh extension results.

Self-contained Plotly HTML — kept deliberately separate from the original
scripts/run_delta_ladder_report.py. Where that report tells the in-sample
headline story, this page is for *exploring* the extended (2006->2025) results
and the key hedge-design decisions:

* equity curves & drawdowns (full sample, OOS shaded, **plus an out-of-sample
  2021-01→ re-based view**) — the path;
* the delta dial across windows — *which call delta?* (and how the answer
  flattens/reverses out of sample);
* a risk/return frontier with a window switch — *single call vs tenor ladder?*;
* crash-payoff bars — *where does each structure help?* (incl. the 2022/Aug-24/
  2025 events the refresh unlocked);
* the VIX regime history that gates the hedge.

Numbers come verbatim from extension_metrics.json (run_extension.py) so the page
can never drift from the tables. Curves are recomputed for the figures.

Run: ``uv run python current_data_extension/make_report.py``
  -> results/current_data_extension/report.html
"""

from __future__ import annotations

import json

import pandas as pd
import plotly.graph_objects as go
from build_extended import SPOT_EXT_PARQUET, VIX_EXT_PARQUET
from plotly.subplots import make_subplots

from vix_hedge import config, metrics
from vix_hedge.data.load import _COLS, OptionChain
from vix_hedge.vxth.backtest import forward_vix_signal, regime_of
from vix_hedge.vxth.engine import ALLOC_OFFICIAL, BASES, HedgeConfig, simulate

OUT = config.RESULTS_DIR / "current_data_extension"

# protected underlying -> (spot panel, base weights, metrics json, html).
BASE_SPEC = {
    "SPX": (SPOT_EXT_PARQUET, BASES["SPX"], "extension_metrics.json", "report.html"),
}

# hedged-config labels are base-agnostic; only the unhedged row carries the base name.
_HEDGED_CONFIGS = {
    # single call on the monthly VIX settlement calendar (VXTH's roll); ladders use the
    # full chain (their 60/90-day rungs span any spike regardless of front-rung phase).
    "single 30d 30Δ": HedgeConfig(0.30, (30,), ALLOC_OFFICIAL, params={"expiry": "monthly"}),
    "ladder 30/60/90 50Δ": HedgeConfig(0.50, (30, 60, 90), ALLOC_OFFICIAL),
    "ladder 30/60/90 30Δ": HedgeConfig(0.30, (30, 60, 90), ALLOC_OFFICIAL),
    "ladder 30/60/90 10Δ": HedgeConfig(0.10, (30, 60, 90), ALLOC_OFFICIAL),  # the report's headline delta
    "ladder 30/60/90 5Δ": HedgeConfig(0.05, (30, 60, 90), ALLOC_OFFICIAL),
}
_HEDGED_COLORS = {
    "single 30d 30Δ": "#1f77b4",
    "ladder 30/60/90 50Δ": "#2ca02c",
    "ladder 30/60/90 30Δ": "#ff7f0e",
    "ladder 30/60/90 10Δ": "#9467bd",
    "ladder 30/60/90 5Δ": "#d62728",
}
WINDOWS = {  # json key -> (friendly label, color)
    "validation_2006_2020": ("in-sample 2006–2020", "#1f77b4"),
    "full_2006_2025": ("full 2006–2025", "#2ca02c"),
    "oos_2021_2025": ("out-of-sample 2021–2025", "#d62728"),
}
LADDER_DELTAS = ["50Δ", "30Δ", "20Δ", "10Δ", "5Δ"]  # near-money -> deep-OTM
EPISODES_BARS = ["GFC 2008-09", "Q4 2018", "COVID 2020", "2022 Bear", "Aug-2024 unwind", "2025 Tariff"]
OOS_SHADE_START = "2021-01-01"
POST_COVID_START = "2021-01-01"  # out-of-sample (past the original report), re-based to $1 view


def curve_configs(base_label: str) -> dict:
    return {f"{base_label} unhedged": None, **_HEDGED_CONFIGS}


def colors(base_label: str) -> dict:
    return {f"{base_label} unhedged": "#444444", **_HEDGED_COLORS}


# --------------------------------------------------------------------------- #
# data
# --------------------------------------------------------------------------- #
def _load(spec) -> tuple[pd.DataFrame, OptionChain, pd.Series, dict]:
    spot_parquet, _base, json_name, _html = spec
    spot = pd.read_parquet(spot_parquet)
    spot["date"] = pd.to_datetime(spot["date"])
    spot = spot.set_index("date").sort_index()
    chain = OptionChain(pd.read_parquet(VIX_EXT_PARQUET, columns=_COLS))
    sig = forward_vix_signal(chain, spot, source="hybrid_vx1")
    jpath = OUT / json_name
    if not jpath.exists():
        raise FileNotFoundError(f"{jpath} missing — run run_extension.py for this base first")
    j = json.loads(jpath.read_text())
    return spot, chain, sig, j


def _curves(spot, chain, sig, base, base_label) -> dict[str, pd.Series]:
    out = {}
    for name, cfg in curve_configs(base_label).items():
        out[name] = simulate(spot, chain, base=base, cfg=cfg,
                             start="2006-03-22", end="2025-08-29", signal_series=sig)
    return out


def _levered_curve(curve: pd.Series, k: float) -> pd.Series:
    """Equity curve from levering ``curve``'s daily returns by ``k`` (gross of
    financing), re-based to 1.0 at the window start."""
    r = metrics.daily_returns(curve)
    return pd.concat([pd.Series([1.0], index=[curve.index[0]]), (1.0 + k * r).cumprod()])


def _lever_to_vol(curve: pd.Series, target_vol: float, iters: int = 48) -> tuple[float, pd.Series]:
    """Solve for the leverage ``k`` whose levered curve has annualized vol
    ``target_vol``. Annualized (monthly) vol rises monotonically with ``k``, so a
    bisection converges; a single scalar can't linearly vol-match a fat-tailed
    convex payoff (the COVID tail dominates a deep-OTM ladder's vol), which is
    exactly why this solves rather than using the naive vol ratio."""
    lo, hi = 1e-3, 25.0
    if metrics.annualized_stddev(_levered_curve(curve, hi)) <= target_vol:
        k = hi
    elif metrics.annualized_stddev(_levered_curve(curve, lo)) >= target_vol:
        k = lo
    else:
        for _ in range(iters):
            mid = 0.5 * (lo + hi)
            if metrics.annualized_stddev(_levered_curve(curve, mid)) < target_vol:
                lo = mid
            else:
                hi = mid
        k = 0.5 * (lo + hi)
    return k, _levered_curve(curve, k)


def levered_frontier(spot, chain, sig, base, j: dict) -> dict:
    """Per window, the **50Δ** 30/60/90 ladder *scaled to exactly the unhedged base's
    annualized vol* (gross of financing; leverage solved by :func:`_lever_to_vol`).
    One point per window on the base's risk line: its CAGR vs the base reads the
    ladder's Sharpe edge straight off the chart. Geometric CAGR is recomputed on the
    levered path, so the vol drag that leverage adds is paid honestly.

    Returns ``{window_key: {"Vol %","CAGR %","Sharpe","k"}}``.
    """
    sim = lambda cfg: simulate(spot, chain, base=base, cfg=cfg,  # noqa: E731
                               start="2006-03-22", end="2025-08-29", signal_series=sig)
    base_curve = sim(None)
    ladder = sim(HedgeConfig(0.50, (30, 60, 90), ALLOC_OFFICIAL))
    out: dict = {}
    for win in WINDOWS:
        s, e = j[win]["window"]
        base_vol = metrics.annualized_stddev(base_curve.loc[s:e])
        cs = ladder.loc[s:e]
        sv = metrics.annualized_stddev(cs)
        if pd.isna(sv) or sv <= 0:
            continue
        k, lc = _lever_to_vol(cs, base_vol)
        out[win] = {"Vol %": 100 * metrics.annualized_stddev(lc),
                    "CAGR %": 100 * metrics.cagr(lc),
                    "Sharpe": metrics.annualized_sharpe(lc), "k": float(k)}
    return out


# --------------------------------------------------------------------------- #
# figures
# --------------------------------------------------------------------------- #
def fig_equity(curves: dict[str, pd.Series], cmap: dict) -> go.Figure:
    fig = go.Figure()
    for name, c in curves.items():
        n = c / c.dropna().iloc[0]
        fig.add_trace(go.Scatter(
            x=n.index, y=n.to_numpy(), name=name, mode="lines",
            line=dict(width=2.2 if "unhedged" in name else 1.6, color=cmap[name]),
            hovertemplate=f"<b>{name}</b><br>%{{x|%Y-%m-%d}}<br>%{{y:.2f}}x<extra></extra>",
        ))
    end = str(next(iter(curves.values())).index[-1].date())
    fig.add_vrect(x0=OOS_SHADE_START, x1=end,
                  fillcolor="gold", opacity=0.10, line_width=0,
                  annotation_text="out-of-sample extension", annotation_position="top left",
                  annotation_font_size=11)
    fig.update_layout(template="plotly_white", height=540, margin=dict(l=60, r=20, t=10, b=40),
                      yaxis=dict(title="growth of $1 (log)", type="log"), xaxis_title="date",
                      legend=dict(orientation="h", yanchor="bottom", y=1.0, x=0), hovermode="x unified")
    return fig


def fig_drawdown(curves: dict[str, pd.Series], cmap: dict) -> go.Figure:
    fig = go.Figure()
    for name, c in curves.items():
        dd = 100 * metrics.drawdown_curve(c)
        fig.add_trace(go.Scatter(
            x=dd.index, y=dd.to_numpy(), name=name, mode="lines",
            line=dict(width=2.0 if "unhedged" in name else 1.3, color=cmap[name]),
            hovertemplate=f"<b>{name}</b><br>%{{x|%Y-%m-%d}}<br>%{{y:.1f}}%<extra></extra>",
        ))
    end = str(next(iter(curves.values())).index[-1].date())
    fig.add_vrect(x0=OOS_SHADE_START, x1=end, fillcolor="gold", opacity=0.10, line_width=0)
    fig.update_layout(template="plotly_white", height=380, margin=dict(l=60, r=20, t=10, b=40),
                      yaxis_title="drawdown %", xaxis_title="date",
                      legend=dict(orientation="h", yanchor="bottom", y=1.0, x=0), hovermode="x unified")
    return fig


def _ladder_metric(j: dict, win: str, field: str) -> list[float]:
    m = j[win]["metrics"]
    return [m[f"ladder 30/60/90 {d}"][field] for d in LADDER_DELTAS]


def fig_dial(j: dict) -> go.Figure:
    """Sharpe & CAGR of the 30/60/90 ladder vs call delta, per window."""
    fig = make_subplots(rows=1, cols=2, subplot_titles=("Sharpe vs delta", "CAGR % vs delta"),
                        horizontal_spacing=0.10)
    for win, (label, color) in WINDOWS.items():
        fig.add_trace(go.Scatter(x=LADDER_DELTAS, y=_ladder_metric(j, win, "Sharpe"),
                                 name=label, legendgroup=label, line=dict(color=color, width=2.4),
                                 mode="lines+markers", marker=dict(size=8)), row=1, col=1)
        fig.add_trace(go.Scatter(x=LADDER_DELTAS, y=_ladder_metric(j, win, "CAGR %"),
                                 name=label, legendgroup=label, showlegend=False,
                                 line=dict(color=color, width=2.4), mode="lines+markers",
                                 marker=dict(size=8)), row=1, col=2)
    fig.update_xaxes(title_text="call delta  (near-money → deep-OTM)", row=1, col=1)
    fig.update_xaxes(title_text="call delta  (near-money → deep-OTM)", row=1, col=2)
    fig.update_yaxes(title_text="Sharpe", row=1, col=1)
    fig.update_yaxes(title_text="CAGR %", row=1, col=2)
    fig.update_layout(template="plotly_white", height=440, margin=dict(l=60, r=20, t=40, b=50),
                      legend=dict(orientation="h", yanchor="bottom", y=1.08, x=0))
    return fig


def fig_frontier(j: dict, base_label: str, levered: dict | None = None) -> go.Figure:
    """Vol vs CAGR for every config, with a window switcher (updatemenus).

    If ``levered`` is supplied (see :func:`levered_frontier`), each window also gets
    a series of 5 points = the 30/60/90 ladders rescaled to the unhedged base's vol,
    so the ladder's risk-adjusted edge is read directly as CAGR at the base's risk.
    """
    unhedged = f"{base_label} unhedged"
    fig = go.Figure()
    win_keys = list(WINDOWS)
    vis_blocks = []  # (start, count) of traces per window
    hov = "<br>vol %{x:.1f}% · CAGR %{y:.1f}% · Sharpe %{customdata:.2f}<extra></extra>"
    for win in win_keys:
        m = j[win]["metrics"]
        start = len(fig.data)
        for struct, color, symbol in (("single 30d", "#1f77b4", "circle"),
                                      ("ladder 30/60/90", "#2ca02c", "diamond")):
            labels = [k for k in m if k.startswith(struct)]
            fig.add_trace(go.Scatter(
                x=[m[k]["Vol %"] for k in labels], y=[m[k]["CAGR %"] for k in labels],
                text=[k.split()[-1] for k in labels], mode="markers+text", textposition="top center",
                name=struct, marker=dict(size=12, color=color, symbol=symbol),
                customdata=[m[k]["Sharpe"] for k in labels],
                hovertemplate="%{text} " + struct + hov,
                visible=(win == win_keys[0]),
            ))
        if levered is not None and win in levered:
            lp = levered[win]
            fig.add_trace(go.Scatter(
                x=[lp["Vol %"]], y=[lp["CAGR %"]], text=[f"50Δ ladder @ {base_label} vol"],
                mode="markers+text", textposition="middle right",
                name="50Δ ladder · vol-matched to " + base_label,
                marker=dict(size=13, color="#9467bd", symbol="square", line=dict(width=1)),
                customdata=[[lp["Sharpe"], lp["k"]]],
                hovertemplate="50Δ ladder, levered to " + base_label + " vol"
                              "<br>vol %{x:.1f}% · CAGR %{y:.1f}% · Sharpe %{customdata[0]:.2f} · "
                              "leverage %{customdata[1]:.2f}×<extra></extra>",
                visible=(win == win_keys[0]),
            ))
        base_row = m[unhedged]
        fig.add_trace(go.Scatter(x=[base_row["Vol %"]], y=[base_row["CAGR %"]], text=[base_label],
                                 mode="markers+text", textposition="bottom center", name=unhedged,
                                 marker=dict(size=15, color="#444", symbol="star"),
                                 customdata=[base_row["Sharpe"]],
                                 hovertemplate=unhedged + hov,
                                 visible=(win == win_keys[0])))
        vis_blocks.append((start, len(fig.data) - start))

    buttons = []
    for i, win in enumerate(win_keys):
        vis = [False] * len(fig.data)
        s, c = vis_blocks[i]
        for k in range(s, s + c):
            vis[k] = True
        buttons.append(dict(label=WINDOWS[win][0], method="update", args=[{"visible": vis}]))
    fig.update_layout(
        template="plotly_white", height=480, margin=dict(l=60, r=20, t=60, b=50),
        xaxis_title="annualized vol %  (← less risk)", yaxis_title="CAGR %  (↑ more return)",
        legend=dict(orientation="h", yanchor="bottom", y=1.02, x=0),
        updatemenus=[dict(type="buttons", direction="right", x=0, y=1.16, showactive=True,
                          buttons=buttons, pad=dict(b=4))],
    )
    return fig


def fig_crash_bars(j: dict, base_label: str, cmap: dict) -> go.Figure:
    m = j["full_2006_2025"]["metrics"]
    configs = [f"{base_label} unhedged", "single 30d 30Δ", "ladder 30/60/90 50Δ",
               "ladder 30/60/90 30Δ", "ladder 30/60/90 10Δ", "ladder 30/60/90 5Δ"]
    fig = go.Figure()
    for cfg in configs:
        fig.add_trace(go.Bar(name=cfg, x=EPISODES_BARS,
                             y=[m[cfg].get(e) for e in EPISODES_BARS],
                             marker_color=cmap.get(cfg)))
    fig.add_hline(y=0, line_color="#999", line_width=1)
    fig.update_layout(template="plotly_white", height=460, barmode="group",
                      margin=dict(l=60, r=20, t=10, b=40), yaxis_title="crash-window total return %",
                      legend=dict(orientation="h", yanchor="bottom", y=1.0, x=0))
    return fig


def fig_vix_regime(spot: pd.DataFrame) -> go.Figure:
    vix = spot["VIX"].loc["2006-03-22":"2025-08-29"]
    fig = go.Figure()
    fig.add_trace(go.Scatter(x=vix.index, y=vix.to_numpy(), name="VIX",
                             line=dict(color="#333", width=0.8),
                             hovertemplate="%{x|%Y-%m-%d}<br>VIX %{y:.1f}<extra></extra>"))
    bands = [(0, 15, "#4575b4", "0% hedge (VIX<15)"), (15, 30, "#fee090", "1% hedge (15–30)"),
             (30, 50, "#fc8d59", "0.5% hedge (30–50)"), (50, max(90, float(vix.max()) + 5), "#d73027", "0% (VIX>50)")]
    for lo, hi, col, lbl in bands:
        fig.add_hrect(y0=lo, y1=hi, fillcolor=col, opacity=0.12, line_width=0,
                      annotation_text=lbl, annotation_position="top left", annotation_font_size=10)
    fig.add_vrect(x0=OOS_SHADE_START, x1=str(vix.index[-1].date()), fillcolor="gold", opacity=0.08, line_width=0)
    fig.update_layout(template="plotly_white", height=360, margin=dict(l=60, r=20, t=10, b=40),
                      yaxis_title="VIX", xaxis_title="date", showlegend=False)
    return fig


def fig_gate_signal(spot: pd.DataFrame) -> go.Figure | None:
    """Why the gate reads a constant-maturity forward, not the raw front-month future.

    Winter-2019/20 run-up into COVID. The 4-level schedule is driven by the *forward*
    value of VIX; VX1 (the front-month future) decays toward spot as it nears its own
    expiry then jumps back up at each monthly roll — a sawtooth — and the gate is read on
    the settlement Wednesday, i.e. the roll day, so it sees the freshly-rolled (higher)
    contract while one trading day earlier it would read the dying contract ≈ spot. CMF30
    (constant-30-day-maturity forward) equals VX1 *on* the roll day but is smooth between
    rolls, so the same gate decision no longer hangs on which day it is read.

    Top: spot VIX, VX1 (sawtooth), CMF30 (smooth) with the 15 gate line and the monthly
    gate-read days marked. Bottom: the hedge weight set at each gate read under VX1 vs
    CMF30, and how a 1-day-earlier read changes it (VX1 flips off; CMF30 doesn't).
    Returns ``None`` if the VX-futures cache is absent (the section is then skipped).
    """
    from vix_hedge.data import vix_futures as vf
    if not config.VIX_FUTURES_PARQUET.exists():
        return None
    panel = vf.load_panel()
    s, e = "2019-10-01", "2020-02-26"
    win = panel.loc[s:e]
    if win.empty or "cmf30" not in win.columns:
        return None
    vix = spot["VIX"].loc[s:e]
    idx = win.index

    def wpct(f: float) -> float:  # hard-gate hedge weight % for a forward level
        return ALLOC_OFFICIAL[regime_of(float(f))] * 100.0

    # gate-read days = monthly VX settlements in the window (= the roll days: the engine
    # reads the regime on exactly these days for a monthly-rolled single call).
    gate_days, w_vx1, w_vx1_lag, w_cmf, w_cmf_lag = [], [], [], [], []
    for p in pd.period_range(idx.min(), idx.max(), freq="M"):
        d = pd.Timestamp(vf.standard_expiry(p.year, p.month))
        pos = idx.get_indexer([d], method="nearest")[0]
        gd, pv = idx[pos], idx[max(pos - 1, 0)]
        gate_days.append(gd)
        w_vx1.append(wpct(win.at[gd, "vx1"]))
        w_vx1_lag.append(wpct(win.at[pv, "vx1"]))
        w_cmf.append(wpct(win.at[gd, "cmf30"]))
        w_cmf_lag.append(wpct(win.at[pv, "cmf30"]))

    titles = ("Which forward does the gate read? — VX1 sawtooths across the 15 line; CMF30 stays smooth",
              "Resulting hedge weight at each monthly gate read — and how a 1-day-earlier read changes it")
    fig = make_subplots(rows=2, cols=1, shared_xaxes=True, vertical_spacing=0.10,
                        row_heights=[0.62, 0.38], subplot_titles=titles)
    fig.add_trace(go.Scatter(x=vix.index, y=vix.to_numpy(), name="spot VIX",
                             line=dict(color="#bbbbbb", width=1.2),
                             hovertemplate="%{x|%Y-%m-%d}<br>spot VIX %{y:.2f}<extra></extra>"), row=1, col=1)
    fig.add_trace(go.Scatter(x=idx, y=win["cmf30"].to_numpy(), name="CMF30 (constant 30-day forward)",
                             line=dict(color="#1f77b4", width=2.6),
                             hovertemplate="%{x|%Y-%m-%d}<br>CMF30 %{y:.2f}<extra></extra>"), row=1, col=1)
    fig.add_trace(go.Scatter(x=idx, y=win["vx1"].to_numpy(), name="VX1 (front-month future)",
                             line=dict(color="#ff7f0e", width=1.8),
                             hovertemplate="%{x|%Y-%m-%d}<br>VX1 %{y:.2f}<extra></extra>"), row=1, col=1)
    fig.add_hline(y=15, line=dict(color="#d62728", width=1.4, dash="dash"), row=1, col=1,
                  annotation_text="gate boundary: 0% ↔ 1% at forward VIX = 15",
                  annotation_position="bottom right", annotation_font=dict(size=11, color="#d62728"))
    for gd in gate_days:  # the monthly gate-read / roll days
        fig.add_vline(x=gd, line=dict(color="#aaaaaa", width=0.8, dash="dot"), row=1, col=1)
    feb = [g for g in gate_days if (g.year, g.month) == (2020, 2)]
    if feb:
        g = feb[0]
        fig.add_annotation(x=g, y=float(win.at[g, "vx1"]), row=1, col=1,
                           text="Feb-19 gate read — VX1 15.4 → <b>on</b>;<br>one day earlier 14.9 → <b>off</b>",
                           showarrow=True, arrowhead=2, ax=-72, ay=-58, align="left",
                           font=dict(size=10.5), bgcolor="rgba(255,255,255,0.88)", bordercolor="#d62728")

    # bottom: weight set at each gate read. CMF30 solid+dashed overlap (read-day robust);
    # VX1 solid is on but its 1-day-early read (dashed) collapses to 0 — the knife edge.
    fig.add_trace(go.Scatter(x=gate_days, y=w_cmf, name="CMF30 gate, on the roll day",
                             mode="lines+markers", line=dict(color="#1f77b4", width=4.5, shape="hv"),
                             marker=dict(size=9, color="#1f77b4"),
                             hovertemplate="%{x|%Y-%m-%d}<br>CMF30 → %{y:.1f}% hedge<extra></extra>"), row=2, col=1)
    _hov = "%{x|%Y-%m-%d}<br>%{y:.1f}% hedge<extra></extra>"
    fig.add_trace(go.Scatter(x=gate_days, y=w_cmf_lag, name="CMF30 gate, read 1 day earlier",
                             mode="lines", line=dict(color="#1f77b4", width=1.6, shape="hv", dash="dash"),
                             hovertemplate=_hov), row=2, col=1)
    fig.add_trace(go.Scatter(x=gate_days, y=w_vx1, name="VX1 gate, on the roll day",
                             mode="lines+markers", line=dict(color="#ff7f0e", width=2, shape="hv"),
                             marker=dict(size=8, color="#ff7f0e", symbol="square"), hovertemplate=_hov), row=2, col=1)
    fig.add_trace(go.Scatter(x=gate_days, y=w_vx1_lag, name="VX1 gate, read 1 day earlier",
                             mode="lines+markers", line=dict(color="#ff7f0e", width=2, shape="hv", dash="dash"),
                             marker=dict(size=8, color="#ff7f0e", symbol="x"), hovertemplate=_hov), row=2, col=1)
    fig.update_yaxes(title_text="forward VIX", row=1, col=1)
    fig.update_yaxes(title_text="hedge weight %", range=[-0.15, 1.25], row=2, col=1)
    fig.update_xaxes(title_text="date  (winter 2019–20, into the COVID spike)", row=2, col=1)
    fig.update_layout(template="plotly_white", height=640, margin=dict(l=60, r=20, t=48, b=44),
                      legend=dict(orientation="h", yanchor="bottom", y=1.04, x=0), hovermode="x unified")
    return fig


# --------------------------------------------------------------------------- #
# tables + html
# --------------------------------------------------------------------------- #
_TABLE_COLS = ["CAGR %", "Sharpe", "Sortino", "Vol %", "MaxDD %", "GFC 2008-09",
               "COVID 2020", "2022 Bear", "2025 Tariff"]
_TABLE_COLS_OOS = ["CAGR %", "Sharpe", "Sortino", "Vol %", "MaxDD %", "2022 Bear",
                   "Aug-2024 unwind", "2025 Tariff"]


def _metrics_table_html(j: dict, win: str, cols: list[str]) -> str:
    m = j[win]["metrics"]
    df = pd.DataFrame(m).T[[c for c in cols if c in next(iter(m.values()))]]
    return df.to_html(classes="metrics", border=0, na_rep="—", float_format=lambda x: f"{x:.2f}")


def _robustness_table_html(j: dict) -> str:
    df = pd.DataFrame(j["robustness_full"]).T
    return df.to_html(classes="metrics", border=0, na_rep="—", float_format=lambda x: f"{x:.3f}")


def _roll_phase_table_html(j: dict) -> str:
    """Roll-PHASE timing luck: the single call by expiry track (big COVID swing) vs the
    phase-invariant ladder — the axis the entry-date table above cannot see."""
    rp = j.get("roll_phase")
    if not rp:
        return ""

    def _lbl(i: str) -> str:
        if i == "0":
            return "0 (monthly · VXTH)"
        if i == "range":
            return "range (max−min)"
        return f"{int(i):+d}d"

    def _tbl(d: dict, idx_name: str) -> str:
        df = pd.DataFrame(d).T
        cols = [c for c in ["CAGR %", "Sharpe", "COVID 2020 %"] if c in df.columns]
        df = df[cols]
        df.index = [_lbl(str(i)) for i in df.index]
        df.index.name = idx_name
        return df.to_html(classes="metrics", border=0, float_format=lambda x: f"{x:.2f}")

    s0, s1 = rp["window"]
    return (
        f'<p class="blurb" style="margin-bottom:4px"><b>single 30Δ call</b> by expiry track, {s0}→{s1} '
        '(0 = the standard monthly VIX settlement, VXTH&rsquo;s calendar; ±7/±14 = VIX weeklys that many '
        'days off it):</p>' + _tbl(rp["single"], "offset from monthly") +
        '<p class="blurb" style="margin:10px 0 4px"><b>30Δ 30/60/90 ladder</b> (the same 30Δ strike target as '
        'the single above — only the tenor structure differs), the same roll-cadence shift on the full chain '
        '(spans tenors → phase-invariant):</p>' + _tbl(rp["ladder"], "cadence shift"))


def _gate_robustness_table_html(j: dict) -> str:
    """Gate-READ timing luck (axis ③): the headline 50Δ ladder under a ±2-day jitter of the
    gate signal, VX1 (front-month future) vs CMF30 (constant-maturity). VX1 swings ~34pp at
    COVID (a 1-day-stale read turns the hedge off into the spike); CMF30 is lag-flat."""
    g = j.get("gate_robustness")
    if not g:
        return ""

    def _lbl(i: str) -> str:
        if i == "0":
            return "0 (read on the roll day)"
        if i == "range":
            return "range (max−min)"
        return f"{int(i):+d}d"

    def _tbl(d: dict, idx_name: str) -> str:
        df = pd.DataFrame(d).T
        cols = [c for c in ["CAGR %", "Sharpe", "COVID 2020 %"] if c in df.columns]
        df = df[cols]
        df.index = [_lbl(str(i)) for i in df.index]
        df.index.name = idx_name
        return df.to_html(classes="metrics", border=0, float_format=lambda x: f"{x:.2f}")

    s0, s1 = g["window"]
    return (
        f'<p class="blurb" style="margin-bottom:4px">Headline <b>50Δ 30/60/90 ladder</b>, {s0}→{s1}, as the gate '
        'signal is read <b>k trading days stale</b> (0 = read on the settlement/roll day). Off the <b>VX1</b> '
        'front-month future the gate sawtooths across 15, so a 1-day-stale read turns the hedge <b>off</b> going '
        'into COVID:</p>' + _tbl(g["vx1"], "VX1 read lag") +
        '<p class="blurb" style="margin:10px 0 4px">The same jitter off <b>CMF30</b> (constant-30-day forward) — '
        'smooth across rolls, so the gate decision barely moves:</p>' + _tbl(g["cmf30"], "CMF30 read lag"))


# --------------------------------------------------------------------------- #
# narrative — the committed SPX headline prose.
# --------------------------------------------------------------------------- #
def _postcovid_blurb(last: str) -> str:
    return (
        f'<h3 style="margin-top:34px">Out-of-sample only — 2021-01 → {last} '
        f'(each curve re-based to $1 at 2021-01)</h3>'
        f'<p class="blurb">The same configs and figures restricted to the out-of-sample window — the years past '
        f'the original report (which ended in 2020) — with every curve re-based to $1 at 2021-01 and drawdowns '
        f'measured from that start. It isolates how each structure behaved <i>after</i> the in-sample period: '
        f'the COVID vol spike (Feb–Mar 2020) is now in-sample, so it no longer dominates the log scale and '
        f'compresses everything that follows it.</p>'
    )


def _spx_prose() -> dict:
    return {
        "path_blurb": (
            '<p class="blurb">The marquee configs over the full sample; the gold band is the 4.7-year '
            'out-of-sample window (2021→, past the original report). The <b>50Δ ladder</b> (green) rides above '
            'unhedged SPX with shallower drawdowns; the <b>5Δ ladder</b> (red) is the convexity bet — it '
            'spikes at COVID then bleeds. Toggle traces in the legend; zoom into 2020–2025.</p>'),
        "path_footer": (
            '<p class="key"><b>Decision — survival:</b> across the full 2006→2025 cycle the 50Δ ladder still '
            'beats unhedged SPX on Sharpe, CAGR <i>and</i> vol, even after absorbing 4.7 bull-market years.</p>'),
        "dial_blurb": (
            '<p class="blurb">Sharpe and CAGR of the 30/60/90 ladder as you sweep the call delta from '
            'near-money (50Δ) to deep-OTM (5Δ), drawn separately for each window. In-sample, deeper-OTM '
            '<i>raised</i> CAGR (the COVID jackpot paid for the bleed). Out of sample — no systemic crash — '
            'the CAGR line <b>flattens and reverses</b>: near-money wins on return too.</p>'),
        "dial_footer": (
            '<p class="key"><b>Decision — delta:</b> 50Δ is the robust default. Deep-OTM convexity only earns '
            'its keep when a COVID/GFC-scale spike actually arrives; absent one it is the most expensive rung.</p>'),
        "frontier_blurb": (
            '<p class="blurb">Every config as a point: annualized vol (x) vs CAGR (y), hover for Sharpe. '
            'Diamonds are 30/60/90 ladders, circles single 30-day calls, the star is unhedged SPX. '
            'The <b>purple square</b> is the <b>50Δ ladder levered to SPX&rsquo;s exact volatility</b> '
            '(its returns scaled by a leverage k solved so its annualized vol equals SPX&rsquo;s — gross of '
            'financing; CAGR recomputed geometrically so leverage pays its own vol drag). It sits on '
            'SPX&rsquo;s risk line, so its height above the SPX star reads the ladder&rsquo;s Sharpe edge '
            'straight off as <i>extra CAGR at equal risk</i>. Use the buttons to switch window. '
            'In-sample the ladder and single-call families <b>nearly overlap</b> — a single call rolled on '
            'the monthly calendar is competitive — but <b>out-of-sample the ladder family sits up-and-left</b> '
            '(more return per unit risk at every delta); the 50Δ ladder is the efficient corner.</p>'),
        "frontier_footer": (
            '<p class="key"><b>Decision — structure:</b> the tenor ladder <i>matches</i> a correctly-rolled single '
            'call in-sample and <i>dominates</i> it out-of-sample — the robustness edge showed up on new data.</p>'),
        "crashes_blurb": (
            '<p class="blurb">Total return through each crash window (full-sample curves). The three '
            'right-hand groups are new: the <b>2022 slow bear</b>, the <b>Aug-2024</b> one-day unwind, and '
            'the <b>2025 tariff</b> selloff. Note the ladder catches COVID at every delta but gives almost '
            'nothing in the 2022 grind, and helps in the sharp 2025 selloff — match the instrument to the '
            'crash <i>shape</i>.</p>'),
        "crashes_footer": (
            '<p class="key"><b>Caveat:</b> Aug-2024 was a 1-day round-trip — single-cohort cells there are '
            'roll-phase-luck noise, not signal (see the robustness section). The 2022 grind is the known '
            'slow-crash blind spot of a VIX-call hedge — a months-long bleed never spikes VIX enough to '
            'pay, so the convexity sits idle.</p>'),
        "regime_blurb": (
            '<p class="blurb">The hedge weight follows a 4-level schedule on the forward VIX. This is the '
            'context for the crash cells above: e.g. the gate kept the hedge <i>on</i> all through 2022 '
            '(VIX 20–32) yet it still didn’t pay, and VIX was in the lowest band going into the Aug-2024 spike.</p>'),
        "tldr": (
            '<div class="tldr"><b>What to take away</b><ul>'
            '<li><b>The headline survives</b> 4.7 years out of sample (2021→) — the 50Δ 30/60/90 ladder still '
            'beats unhedged SPX on Sharpe, CAGR and vol over the full cycle.</li>'
            '<li><b>Delta dial flattens & reverses OOS</b> — with no systemic crash, near-money (50Δ) wins on '
            'return too; deep-OTM convexity only pays when a big spike arrives.</li>'
            '<li><b>The tenor ladder matches a correctly-rolled single call in-sample and beats it out-of-sample</b> '
            '— and, unlike the single call, it doesn\'t depend on getting the monthly roll phase right.</li>'
            '<li><b>Instrument ↔ crash shape:</b> ladder owns the fast vol spike (COVID, 2025 tariff), misses '
            'the 2022 slow grind; Aug-2024 is luck-dominated noise.</li>'
            '</ul></div>'),
    }


_CSS = """
body{font-family:-apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif;color:#222;
 max-width:1040px;margin:0 auto;padding:24px 18px 90px;line-height:1.55}
h1{font-size:27px;margin-bottom:2px} h2{font-size:21px;margin-top:10px;border-bottom:2px solid #eee;padding-bottom:6px}
.sub{color:#666;margin-top:0;font-size:15px} .meta{color:#888;font-size:13px}
.blurb{color:#3a3a3a;font-size:15px} section{margin:40px 0}
.key{background:#f4f8f4;border-left:3px solid #2ca02c;padding:10px 14px;border-radius:3px;font-size:14.5px}
.tldr{background:#fbfbfb;border:1px solid #eee;border-radius:6px;padding:6px 18px;margin:16px 0}
.tldr li{margin:7px 0}
table.metrics{border-collapse:collapse;margin-top:14px;font-size:13.5px;width:100%}
table.metrics th,table.metrics td{padding:6px 10px;text-align:right;border-bottom:1px solid #eee}
table.metrics th:first-child,table.metrics td:first-child{text-align:left;font-weight:600}
table.metrics th{background:#fafafa;border-bottom:2px solid #ddd;position:sticky;top:0}
nav{position:sticky;top:0;background:#fff;padding:8px 0;border-bottom:1px solid #eee;z-index:5}
nav a{margin-right:14px;font-size:13.5px;text-decoration:none;color:#1f77b4}
"""


def main(base_name: str = "SPX") -> None:
    spec = BASE_SPEC[base_name]
    _spot_parquet, base, json_name, html_name = spec
    B = base_name
    spot, chain, sig, j = _load(spec)
    curves = _curves(spot, chain, sig, base, B)
    levered = levered_frontier(spot, chain, sig, base, j)
    cmap = colors(B)
    post = {name: c.loc[POST_COVID_START:] for name, c in curves.items()}  # re-based out-of-sample (2021+)
    last = str(next(iter(curves.values())).index[-1].date())
    span = f"{curves[f'{B} unhedged'].index.min().date()} → {last}"
    prose = _spx_prose()

    figs = [
        ("path", "Equity curves & drawdowns — does the hedge survive the extension?",
         prose["path_blurb"],
         [fig_equity(curves, cmap), fig_drawdown(curves, cmap),
          _postcovid_blurb(last), fig_equity(post, cmap), fig_drawdown(post, cmap)],
         prose["path_footer"]),
        ("dial", "The delta dial — which call delta? (and how the answer moves out of sample)",
         prose["dial_blurb"], [fig_dial(j)], prose["dial_footer"]),
        ("frontier", "Risk/return frontier — single call vs tenor ladder?",
         prose["frontier_blurb"], [fig_frontier(j, B, levered)], prose["frontier_footer"]),
        ("crashes", "Crash payoffs — where does each structure help?",
         prose["crashes_blurb"], [fig_crash_bars(j, B, cmap)], prose["crashes_footer"]),
        ("regime", "The VIX regime gate — when is the hedge on?",
         prose["regime_blurb"], [fig_vix_regime(spot)], ""),
    ]

    # The gate-signal figure + Robustness ③ explain why the gate reads a constant-maturity
    # forward (CMF30) rather than the raw front-month future (VX1). Skipped if the VX-futures
    # cache is absent (the figure builds straight off it).
    gate_fig = fig_gate_signal(spot)
    if gate_fig is not None:
        gate_blurb = (
            '<p class="blurb">The 4-level schedule above is driven by the <i>forward</i> value of VIX — but '
            '<b>which</b> forward? This study reads it off <b>VX1</b>, the front-month VIX future (the report&rsquo;s '
            'apparent basis). The catch: a single futures contract decays toward spot VIX as it nears its own '
            'expiry, then jumps back up when it rolls to the next contract — a <b>sawtooth</b> (top panel, orange). '
            'And the gate is read on the monthly settlement Wednesday, which is <i>exactly the roll day</i>: the gate '
            'sees the freshly-rolled (higher) contract, while one trading day earlier it would read the dying '
            'contract ≈ spot. In the calm run-up to COVID, VX1 crosses the 15 gate at every roll. <b>CMF30</b> '
            '(blue) — CBOE&rsquo;s constant-30-day-maturity forward, interpolating VX1/VX2 — <i>equals</i> VX1 on '
            'the roll day but stays smooth between rolls (it is always a 30-day-out forward), so it doesn&rsquo;t '
            'sawtooth across the line.</p>')
        gate_footer = (
            '<p class="key"><b>Why CMF30 is necessary:</b> the bottom panel is the consequence. At four of the five '
            'monthly gate reads in winter 2019–20 the hedge was switched <b>on</b> — but reading VX1 just <b>one '
            'trading day earlier</b> (dashed orange) flips every one of them <b>off</b>, so the book would have '
            'entered COVID <i>unhedged</i>. CMF30 returns the <b>same</b> on/off decision yet is read-day-robust (its '
            '1-day-early read, dashed blue, is unchanged). And it is free: over 2006→2025 swapping the gate to CMF30 '
            'leaves the headline untouched (single 30Δ CAGR 9.2 / Sharpe 0.53 / COVID +29% either way; 50Δ ladder '
            'identical) — it removes a knife-edge without moving the numbers. This is the <i>gate-read</i> '
            'timing-luck axis, quantified for the headline ladder in <b>Robustness ③</b> below (the call '
            '<i>roll-phase</i> axis is ②).</p>')
        figs.append((
            "gate_signal",
            "The gate signal — VX1 sawtooth vs CMF30 (why the gate reads a constant-maturity forward)",
            gate_blurb, [gate_fig], gate_footer))

    blocks, first = [], True
    for key, title, blurb, fig_list, footer in figs:
        fig_html = ""
        for f in fig_list:
            if isinstance(f, str):  # interleaved HTML (e.g. the post-COVID sub-heading)
                fig_html += f
                continue
            fig_html += f.to_html(full_html=False, include_plotlyjs="inline" if first else False)
            first = False
        blocks.append(f'<section id="{key}"><h2>{title}</h2>{blurb}{fig_html}{footer}</section>')

    # Robustness ③ (gate read) is present only when the JSON carries it (VX-futures cache built).
    has_gate = bool(j.get("gate_robustness"))
    axes_txt = (
        'then the three timing-luck axes (① entry date — small; ② roll phase — breaks the single call; '
        '③ gate read — breaks every instrument under VX1, fixed by CMF30).' if has_gate else
        'then the two timing-luck axes (entry date — small; roll phase — the one that matters).')
    gate3_html = (
        '<h3>Robustness ③ — gate-read timing luck (which day, and which forward, the gate is read on)</h3>'
        '<p class="blurb">The third axis is the <b>gate</b> itself, and it is orthogonal to ① and ②: the 4-level '
        'schedule is read on the monthly settlement — the VX1 roll day — where the front-month future sawtooths '
        'across the 15 boundary (see the <a href="#gate_signal">Gate signal</a> figure). Reading it just <b>one day '
        'stale</b> flips the whole hedge <b>off</b> going into COVID: the headline 50Δ ladder&rsquo;s COVID return '
        'swings <b>~34pp</b> (+0.1% → −33.9%) and Sharpe drops 0.64→0.52 — and this hits the <i>roll-phase-robust '
        'ladder</i>, because the gate gates every instrument. Switching the gate to the constant-maturity '
        '<b>CMF30</b> forward collapses that swing to ~0 — the robust fix, at no cost to the headline.</p>'
        + _gate_robustness_table_html(j)) if has_gate else ''

    tables = (
        '<section id="tables"><h2>Metrics tables</h2>'
        f'<p class="blurb">Verbatim from <code>{json_name}</code>. '
        f'Full sample 2006→2025, then out-of-sample 2021→2025 only, {axes_txt}</p>'
        '<h3>Full sample — 2006→2025</h3>' + _metrics_table_html(j, "full_2006_2025", _TABLE_COLS) +
        '<h3>Out-of-sample only — 2021-01→2025-08</h3>' + _metrics_table_html(j, "oos_2021_2025", _TABLE_COLS_OOS) +
        '<h3>Robustness ① — entry-date timing luck (21 cohorts, full sample)</h3>'
        '<p class="blurb">This is <b>deliberately tiny</b> and is <i>not</i> evidence the single call is '
        'robust: the hedge resizes to the regime weight every roll, so staggered start dates re-sync at the '
        'next monthly roll and the full-window CAGR range is ~0.1pp for the single call <i>and</i> the ladder. '
        'Entry date is simply the wrong axis — see ② below.</p>' + _robustness_table_html(j) +
        '<h3>Robustness ② — roll-phase timing luck (the axis that actually breaks the single call)</h3>'
        '<p class="blurb">The fragility that matters is <b>roll phase</b>: <i>which</i> expiry the single 30Δ '
        'call holds into a spike. On VXTH&rsquo;s <b>monthly</b> settlement it catches COVID (+29%); shift its '
        'roll by a single week onto an adjacent VIX <b>weekly</b> and its lone option expires before the peak, '
        'so COVID flips negative — a <b>~50pp swing from a one-week change to the very dial that turns a single '
        'call into a VXTH replication</b>. The <b>30Δ</b> 30/60/90 ladder — the same 30Δ as the single, just '
        'spread across three tenors — is invariant to the same shift (a 60/90-day rung always spans the spike). '
        'That is the real robustness result; the entry-date table above can&rsquo;t see '
        'it because it never varies the expiry the call holds.</p>' + _roll_phase_table_html(j) +
        gate3_html +
        '</section>'
    )
    blocks.append(tables)

    nav_items = [("path", "Path"), ("dial", "Delta dial"), ("frontier", "Frontier"),
                 ("crashes", "Crashes"), ("regime", "Regime")]
    if gate_fig is not None:
        nav_items.append(("gate_signal", "Gate signal"))
    nav_items.append(("tables", "Tables"))
    nav = " ".join(f'<a href="#{k}">{label}</a>' for k, label in nav_items)

    title = "VIX-call hedge — 2025 data-extension explorer"
    h1 = "VIX-call tail hedge — 2025 data-extension explorer"
    sub = ("Exploring the refreshed results &amp; key hedge-design decisions · "
           "SPX base · frictionless · hybrid_vx1 signal")
    meta = (f"Backtest {span} · gold band = out-of-sample extension beyond the old cache · "
            "crash-window returns total; Sharpe/Sortino/vol annualized on monthly returns · "
            f"numbers from {json_name}")

    html = f"""<!doctype html><html><head><meta charset="utf-8">
<title>{title}</title><style>{_CSS}</style></head><body>
<h1>{h1}</h1>
<p class="sub">{sub}</p>
<p class="meta">{meta}</p>
{prose["tldr"]}
<nav>{nav}</nav>
{''.join(blocks)}
</body></html>"""
    OUT.mkdir(parents=True, exist_ok=True)
    out = OUT / html_name
    out.write_text(html)
    print(f"wrote interactive explorer ({B}) -> {out}  ({len(html)//1024} KB)")


if __name__ == "__main__":
    main("SPX")
