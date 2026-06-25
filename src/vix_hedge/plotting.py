"""Small matplotlib helpers for equity-curve and drawdown charts."""

from __future__ import annotations

from pathlib import Path

import matplotlib

matplotlib.use("Agg")  # headless: save figures, never open a window
import matplotlib.pyplot as plt  # noqa: E402

from vix_hedge import metrics  # noqa: E402


def equity_curves(curves, title: str, path: Path, *, logy: bool = True, normalize: bool = True) -> Path:
    """Plot portfolio value curves (optionally log-y, normalized to start=1)."""
    fig, ax = plt.subplots(figsize=(10, 6))
    # normalize each column by its own first valid value (curves may start on
    # different dates, e.g. an external benchmark joined in later)
    data = curves / curves.apply(lambda c: c.dropna().iloc[0]) if normalize else curves
    for col in data.columns:
        ax.plot(data.index, data[col], label=col, lw=1.2)
    if logy:
        ax.set_yscale("log")
    ax.set_title(title)
    ax.set_xlabel("Date")
    ax.set_ylabel("Value (normalized)" if normalize else "Value")
    ax.legend(loc="upper left", fontsize=9)
    ax.grid(True, alpha=0.3)
    fig.tight_layout()
    fig.savefig(path, dpi=120)
    plt.close(fig)
    return path


def drawdowns(curves, title: str, path: Path) -> Path:
    """Plot running drawdown of each portfolio."""
    fig, ax = plt.subplots(figsize=(10, 5))
    for col in curves.columns:
        dd = metrics.drawdown_curve(curves[col])
        ax.plot(dd.index, 100 * dd, label=col, lw=1.0)
    ax.set_title(title)
    ax.set_xlabel("Date")
    ax.set_ylabel("Drawdown (%)")
    ax.legend(loc="lower left", fontsize=9)
    ax.grid(True, alpha=0.3)
    fig.tight_layout()
    fig.savefig(path, dpi=120)
    plt.close(fig)
    return path
