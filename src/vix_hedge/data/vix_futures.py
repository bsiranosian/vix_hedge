"""Real CBOE VIX (VX) futures -> a daily forward-VIX regime signal.

The MSE448 report's VXTH allocation follows the *forward* value of VIX, which the
report read off the front-month VIX future (VX1). The original R repo expected a
``vix_futures/`` directory that no longer exists, so the rest of the port proxies
the forward by put-call parity on the VIX options (see
``vxth.backtest.forward_vix_series``). This module sources the *real* thing.

CBOE publishes every VX contract's daily history as a free, unauthenticated CSV
on its CDN::

    https://cdn.cboe.com/.../historical_data/VX/VX_<settlement-date>.csv

The filename is the contract's final-settlement date, and VX monthly settlement
is the Wednesday 30 days before the third Friday of the *following* month (shifted
off exchange holidays). We enumerate contracts from that calendar, probe the CDN
(only real contracts return a CSV), cache each raw file, and stitch a daily panel:

* ``vx1``     -- front-month settle (nearest contract not yet expired)
* ``vx2``     -- second-month settle
* ``cmf30``   -- constant-30-day-maturity forward, linearly interpolating vx1/vx2
                 on days-to-settlement (CBOE's "forward value of VIX" construction)

**Coverage:** the CDN only retains contracts from ~2013 on (older ones were
purged, and Nasdaq Data Link's continuous series is discontinued + bot-walled),
but it stays current — a no-arg ``build_panel`` / ``download_contracts`` now
extends to the present (``end`` defaults to ~45 days out). 2013-present covers
the 2020 spike and every modern single-call experiment, which is where the
regime signal actually bites. For a real forward
back to 2006 you need a paid feed (CBOE DataShop, or a vendor like FirstRate /
Portara); ``vxth`` falls back to the parity proxy outside the covered window.
"""

from __future__ import annotations

import datetime as dt
import urllib.request

import pandas as pd

from vix_hedge import config

_UA = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120 Safari/537.36"
# Offsets tried around the calendar Wednesday to absorb holiday shifts.
_SHIFTS = (0, -1, 1, -2, 2, -3, 3)


def _third_friday(year: int, month: int) -> dt.date:
    first = dt.date(year, month, 1)
    return first + dt.timedelta(days=(4 - first.weekday()) % 7 + 14)


def standard_expiry(year: int, month: int) -> dt.date:
    """Standard VX final-settlement date for the ``(year, month)`` contract:
    the Wednesday 30 days before the third Friday of the following month."""
    ny, nm = (year + 1, 1) if month == 12 else (year, month + 1)
    return _third_friday(ny, nm) - dt.timedelta(days=30)


def _default_end() -> str:
    """Today + ~45 days, so the documented no-arg refresh always reaches the
    nearest unexpired contract (the CDN serves contracts as they list)."""
    return (dt.date.today() + dt.timedelta(days=45)).isoformat()


def _contract_months(start: dt.date, end: dt.date):
    """Yield (year, month) for every monthly contract whose settlement plausibly
    falls in ``[start, end]`` (a small buffer is added by the caller)."""
    y, m = start.year, start.month
    while (y, m) <= (end.year, end.month):
        yield y, m
        y, m = (y + 1, 1) if m == 12 else (y, m + 1)


def _fetch_csv(date: dt.date, *, timeout: int = 25) -> str | None:
    """Return the raw CSV text for the VX contract settling on ``date``, or None."""
    req = urllib.request.Request(config.VIX_FUTURES_CDN.format(date.isoformat()), headers={"User-Agent": _UA})
    try:
        with urllib.request.urlopen(req, timeout=timeout) as r:  # noqa: S310 (trusted host)
            if r.status != 200:
                return None
            text = r.read().decode("utf-8", "replace")
    except Exception:
        return None
    return text if text.startswith("Trade Date") else None


def download_contracts(start: str = "2012-06-01", end: str | None = None, *, refresh: bool = False) -> list[dt.date]:
    """Download (cache-first) every available VX contract settling in the range.

    ``end`` defaults to ~45 days out (the present), so a no-arg call extends the
    cache to today. Returns the list of settlement dates found. Each raw CSV is
    cached under ``VIX_FUTURES_DIR/raw`` so subsequent builds are fully offline.
    """
    end = end or _default_end()
    raw_dir = config.VIX_FUTURES_DIR / "raw"
    raw_dir.mkdir(parents=True, exist_ok=True)
    s, e = dt.date.fromisoformat(start), dt.date.fromisoformat(end)
    found: list[dt.date] = []
    for year, month in _contract_months(s, e):
        base = standard_expiry(year, month)
        # If any cached file sits within the shift window, reuse it.
        cached = next((base + dt.timedelta(days=o) for o in _SHIFTS
                       if (raw_dir / f"VX_{(base + dt.timedelta(days=o)).isoformat()}.csv").exists()), None)
        if cached is not None and not refresh:
            found.append(cached)
            continue
        for off in _SHIFTS:
            cand = base + dt.timedelta(days=off)
            if not (s <= cand <= e):
                continue
            text = _fetch_csv(cand)
            if text is not None:
                (raw_dir / f"VX_{cand.isoformat()}.csv").write_text(text)
                found.append(cand)
                break
    return sorted(found)


def _load_raw() -> pd.DataFrame:
    """Concatenate every cached contract CSV into a tidy (date, expiry, settle) frame."""
    raw_dir = config.VIX_FUTURES_DIR / "raw"
    files = sorted(raw_dir.glob("VX_*.csv")) if raw_dir.exists() else []
    if not files:
        raise FileNotFoundError(
            f"no cached VX contracts under {raw_dir}; run `python -m vix_hedge.data.vix_futures`"
        )
    frames = []
    for f in files:
        expiry = pd.Timestamp(f.stem.removeprefix("VX_"))
        df = pd.read_csv(f, usecols=["Trade Date", "Close", "Settle"])
        df = df.rename(columns={"Trade Date": "date", "Settle": "settle", "Close": "close"})
        df["date"] = pd.to_datetime(df["date"])
        df["expiry"] = expiry
        # CBOE only backfilled the Settle column from ~mid-2013; before that
        # Settle is 0 but Close carries the price. Coalesce so early calm-period
        # history is usable. Rows with neither (e.g. an empty final SOQ row) drop.
        df["settle"] = df["settle"].where(df["settle"] > 0, df["close"])
        df = df[(df["settle"] > 0) & df["settle"].notna()]
        frames.append(df[["date", "expiry", "settle"]])
    return pd.concat(frames, ignore_index=True)


def build_panel(start: str = "2012-06-01", end: str | None = None, *, save: bool = True) -> pd.DataFrame:
    """Build the daily VX1/VX2/CMF30 panel from cached contracts (downloads if absent)."""
    end = end or _default_end()
    if not (config.VIX_FUTURES_DIR / "raw").exists():
        download_contracts(start, end)
    raw = _load_raw()

    rows: list[dict] = []
    for date, grp in raw.groupby("date", sort=True):
        # Live contracts = those settling strictly after today, nearest first.
        live = grp[grp["expiry"] > date].sort_values("expiry")
        if live.empty:
            continue
        dte = (live["expiry"] - date).dt.days.to_numpy()
        settle = live["settle"].to_numpy()
        exp = live["expiry"].to_numpy()
        rec = {"date": date, "vx1": float(settle[0]), "front_expiry": pd.Timestamp(exp[0]),
               "vx2": float(settle[1]) if len(settle) > 1 else float("nan")}
        # Constant-30-day-maturity forward: interpolate the two nearest contracts
        # on days-to-settlement (CBOE's forward-VIX construction).
        if len(settle) > 1 and dte[1] != dte[0]:
            w = (30 - dte[0]) / (dte[1] - dte[0])
            w = min(max(w, 0.0), 1.0)  # don't extrapolate past the bracketing pair
            rec["cmf30"] = float((1 - w) * settle[0] + w * settle[1])
        else:
            rec["cmf30"] = float(settle[0])
        rows.append(rec)

    panel = pd.DataFrame(rows).set_index("date").sort_index()
    if save:
        config.CACHE_DIR.mkdir(parents=True, exist_ok=True)
        panel.reset_index().to_parquet(config.VIX_FUTURES_PARQUET, index=False)
    return panel


def load_panel() -> pd.DataFrame:
    """Load the cached VX futures panel (date-indexed)."""
    if not config.VIX_FUTURES_PARQUET.exists():
        return build_panel()
    df = pd.read_parquet(config.VIX_FUTURES_PARQUET)
    df["date"] = pd.to_datetime(df["date"])
    return df.set_index("date").sort_index()


def forward_vix_futures(measure: str = "vx1") -> pd.Series:
    """Real daily forward-VIX from VX futures (``vx1`` front-month, or ``cmf30``)."""
    panel = load_panel()
    s = panel[measure].copy()
    s.name = f"forward_vix_{measure}"
    return s


if __name__ == "__main__":
    import sys

    rng = sys.argv[1:3] if len(sys.argv) >= 3 else ("2012-06-01", _default_end())
    dates = download_contracts(*rng)
    panel = build_panel(*rng)
    print(f"contracts: {len(dates)}  ({dates[0]} .. {dates[-1]})")
    print(f"panel rows: {len(panel)}  ({panel.index[0].date()} .. {panel.index[-1].date()})")
    print(panel.tail())
