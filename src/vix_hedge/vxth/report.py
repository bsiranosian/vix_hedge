"""Render the VIX-call-hedge experiments as a self-contained interactive HTML.

One section per paper experiment: an interactive Plotly equity-curve chart
(hover, zoom, legend-toggle) plus a metrics table that shows the reproduced
numbers next to the report's published figures.
"""

from __future__ import annotations

from pathlib import Path

import pandas as pd
import plotly.graph_objects as go

from vix_hedge import metrics as _metrics
from vix_hedge.vxth.experiments import Section

_PALETTE = ["#1f77b4", "#d62728", "#2ca02c", "#9467bd", "#ff7f0e", "#17becf", "#8c564b", "#7f7f7f"]


def _normalize(curves: pd.DataFrame) -> pd.DataFrame:
    return curves / curves.apply(lambda c: c.dropna().iloc[0])


def equity_figure(curves: pd.DataFrame, *, logy: bool = True) -> go.Figure:
    data = _normalize(curves)
    fig = go.Figure()
    for i, col in enumerate(data.columns):
        s = data[col].dropna()
        fig.add_trace(go.Scatter(
            x=s.index, y=s.to_numpy(), name=col, mode="lines",
            line=dict(width=1.6, color=_PALETTE[i % len(_PALETTE)]),
            hovertemplate=f"<b>{col}</b><br>%{{x|%Y-%m-%d}}<br>%{{y:.2f}}x<extra></extra>",
        ))
    fig.update_layout(
        template="plotly_white", height=520, margin=dict(l=60, r=20, t=10, b=40),
        yaxis=dict(title="Growth of $1 (log)" if logy else "Growth of $1", type="log" if logy else "linear"),
        xaxis=dict(title="Date"), legend=dict(orientation="h", yanchor="bottom", y=1.0, x=0),
        hovermode="x unified",
    )
    return fig


def vix_history_figure(vix: pd.Series, thresholds=(15, 30, 50)) -> go.Figure:
    fig = go.Figure()
    fig.add_trace(go.Scatter(x=vix.index, y=vix.to_numpy(), name="VIX", line=dict(color="#333", width=1)))
    colors = ["#4575b4", "#fee090", "#fc8d59", "#d73027"]
    labels = ["0% hedge (VIX<15)", "1% hedge (15-30)", "0.5% hedge (30-50)", "0% hedge (VIX>50)"]
    edges = [0, *thresholds, max(100, float(vix.max()) + 5)]
    for i in range(len(edges) - 1):
        fig.add_hrect(y0=edges[i], y1=edges[i + 1], fillcolor=colors[i], opacity=0.12, line_width=0,
                      annotation_text=labels[i], annotation_position="top left", annotation_font_size=10)
    for t in thresholds:
        fig.add_hline(y=t, line_dash="dot", line_color="#888")
    fig.update_layout(template="plotly_white", height=420, margin=dict(l=60, r=20, t=10, b=40),
                      yaxis_title="VIX", xaxis_title="Date", showlegend=False)
    return fig


def transition_heatmap(tm: pd.DataFrame) -> go.Figure:
    z = tm.to_numpy()
    fig = go.Figure(go.Heatmap(
        z=z, x=[f"to {c}" for c in tm.columns], y=[f"from {r}" for r in tm.index],
        colorscale="Blues", showscale=False,
        text=z, texttemplate="%{text}", textfont_size=13,
    ))
    fig.update_layout(template="plotly_white", height=360, margin=dict(l=70, r=20, t=10, b=40),
                      yaxis=dict(autorange="reversed"))
    return fig


def _metrics_table_html(section: Section) -> str:
    m = section.metrics.copy()
    if section.report is not None:
        for col in ("CAGR %", "Sharpe"):
            if col in section.report.columns:
                m[f"{col} (report)"] = section.report[col]
        m = m[[c for c in ("CAGR %", "CAGR % (report)", "Sharpe", "Sharpe (report)", "MaxDD %") if c in m.columns]]
    return m.to_html(classes="metrics", border=0, na_rep="—", float_format=lambda x: f"{x:.2f}")


def _return_stats_html(stats: dict) -> str:
    ex = " · ".join(f"{m}x: {p}%" for m, p in stats["exceed_pct"].items())
    return (
        f'<p class="note"><b>90-day, 10-delta VIX call return distribution</b> '
        f'({stats["n_contracts"]} contracts): '
        f'<b>{100 * stats["frac_worthless"]:.1f}%</b> expire worthless; '
        f'max-return exceedance — {ex}.</p>'
    )


def _section_html(section: Section, fig_html: str) -> str:
    parts = [f'<section><h2>{section.title}</h2><p class="blurb">{section.blurb}</p>', fig_html]
    if "return_stats" in section.extra:
        parts.append(_return_stats_html(section.extra["return_stats"]))
    if not section.metrics.empty:
        parts.append(_metrics_table_html(section))
    parts.append("</section>")
    return "\n".join(parts)


_CSS = """
body{font-family:-apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif;color:#222;
 max-width:980px;margin:0 auto;padding:24px 18px 80px;line-height:1.5}
h1{font-size:26px;margin-bottom:4px} h2{font-size:20px;margin-top:8px;border-bottom:2px solid #eee;padding-bottom:6px}
.sub{color:#666;margin-top:0} .blurb{color:#444}
section{margin:34px 0}
table.metrics{border-collapse:collapse;margin-top:14px;font-size:14px}
table.metrics th,table.metrics td{padding:6px 14px;text-align:right;border-bottom:1px solid #eee}
table.metrics th:first-child,table.metrics td:first-child{text-align:left;font-weight:600}
table.metrics th{background:#fafafa;border-bottom:2px solid #ddd}
.note{background:#f7f9fb;border-left:3px solid #1f77b4;padding:8px 12px;font-size:14px;border-radius:3px}
nav a{margin-right:12px;font-size:14px} .meta{color:#888;font-size:13px}
"""


def build_report(sections: list[Section], path: Path, *, title: str, subtitle: str = "", meta: str = "") -> Path:
    blocks, first = [], True
    for sec in sections:
        if sec.key == "signals":
            fig = vix_history_figure(sec.curves["VIX"], sec.extra["thresholds"])
            fig_html = fig.to_html(full_html=False, include_plotlyjs="inline" if first else False)
            first = False
            tm_fig = transition_heatmap(sec.extra["transition_matrix"])
            tm_html = tm_fig.to_html(full_html=False, include_plotlyjs=False)
            body = (f'<section><h2>{sec.title}</h2><p class="blurb">{sec.blurb}</p>{fig_html}'
                    f'<p class="note">Transition matrix of daily VIX regime (2006-2020): VIX never skips a band.</p>'
                    f'{tm_html}</section>')
            blocks.append(body)
            continue
        fig = equity_figure(sec.curves, logy=True)
        fig_html = fig.to_html(full_html=False, include_plotlyjs="inline" if first else False)
        first = False
        blocks.append(_section_html(sec, fig_html))

    nav = " ".join(f'<a href="#{s.key}">{s.title.split(":")[0]}</a>' for s in sections)
    sections_html = "\n".join(
        b.replace("<section>", f'<section id="{s.key}">', 1) for b, s in zip(blocks, sections, strict=True)
    )
    html = f"""<!doctype html><html><head><meta charset="utf-8">
<title>{title}</title><style>{_CSS}</style></head><body>
<h1>{title}</h1><p class="sub">{subtitle}</p><p class="meta">{meta}</p>
<nav>{nav}</nav>
{sections_html}
</body></html>"""
    path.write_text(html)
    return path


def curve_metrics(curves: pd.DataFrame) -> pd.DataFrame:
    """Convenience: CAGR/Sharpe/MaxDD/AnnStdDev for an arbitrary curve set."""
    return _metrics.summary(curves)
