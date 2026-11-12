"""Draw the README figures (pip install matplotlib numpy scipy; python figures/make_figures.py).

indicators.png: the five-number summaries across regions that wellbeing_analysis.R
printed for each indicator (copied from the project report, since the data
spreadsheet is not part of this repository).
rayleigh.png: a Python port of rayleigh_simulation.R (inverse transform sample).
"""
import os

import matplotlib.pyplot as plt
import numpy as np

HERE = os.path.dirname(os.path.abspath(__file__))
INK, MUTED, GRID = "#0b0b0b", "#52514e", "#e4e3df"
BLUE, BLUE_LIGHT, ORANGE = "#2a78d6", "#a9c8ef", "#eb6834"
plt.rcParams.update({"font.family": "DejaVu Sans", "font.size": 11, "text.color": INK,
                     "axes.labelcolor": MUTED, "xtick.color": MUTED, "ytick.color": MUTED})

# indicator: (min, Q1, median, Q3, max, unit format)
SUMMARIES = [
    ("Life satisfaction (0\u201310)",          (4.5, 6.0, 6.5, 7.0, 7.8), "{:.1f}"),
    ("Life expectancy (years)",           (72.2, 79.2, 81.55, 82.725, 85.2), "{:.1f}"),
    ("Mortality (per 1,000, age-adj.)",   (5.7, 6.9, 7.6, 8.975, 13.3), "{:.1f}"),
    ("Air pollution (PM2.5, µg/m³)", (1.7, 9.95, 13.3, 18.2, 38.2), "{:.1f}"),
    ("Homicides (per 100,000)",           (0.0, 0.575, 0.9, 1.2, 10.0), "{:.1f}"),
    ("Income (USD per person)",           (5709, 13618.75, 17783, 21063.5, 29279), "{:,.0f}"),
    ("Employment rate (%)",               (32.2, 61.575, 66.95, 72.65, 88.9), "{:.1f}"),
    ("Unemployment rate (%)",             (1.8, 4.6, 6.95, 10.925, 29.7), "{:.1f}"),
    ("Education (%)",                     (40.7, 72.55, 84.1, 89.25, 97.5), "{:.1f}"),
    ("Broadband internet (% homes)",      (59.0, 76.0, 82.0, 89.0, 100.0), "{:.0f}"),
    ("Voter turnout (%)",                 (40.5, 57.0, 66.7, 76.4, 91.1), "{:.1f}"),
    ("Social support (%)",                (68.8, 87.4, 91.85, 93.9, 100.0), "{:.1f}"),
    ("Rooms per person",                  (0.9, 1.5, 1.7, 1.9, 2.3), "{:.1f}"),
]


def indicators():
    n = len(SUMMARIES)
    fig, ax = plt.subplots(figsize=(10, 0.52 * n + 1.4), facecolor="white")
    for i, (name, (lo, q1, med, q3, hi), fmt) in enumerate(SUMMARIES):
        y = n - 1 - i
        scale = lambda v: (v - lo) / (hi - lo)  # each indicator on its own 0-1 range
        ax.plot([0, 1], [y, y], color=GRID, lw=2, solid_capstyle="round", zorder=1)
        ax.barh(y, scale(q3) - scale(q1), left=scale(q1), height=0.42, color=BLUE_LIGHT, zorder=2)
        ax.plot([scale(med)] * 2, [y - 0.24, y + 0.24], color=BLUE, lw=3, zorder=3)
        ax.text(-0.03, y, fmt.format(lo), ha="right", va="center", fontsize=9, color=MUTED)
        ax.text(1.03, y, fmt.format(hi), ha="left", va="center", fontsize=9, color=MUTED)
        ax.text(-0.17, y, name, ha="right", va="center", fontsize=10.5)
    ax.set_xlim(-0.75, 1.15)
    ax.set_ylim(-0.8, n - 0.3)
    ax.axis("off")
    ax.text(-0.75, n + 0.25, "How European regions differ on 13 well-being indicators",
            fontsize=13, va="bottom")
    ax.text(-0.75, n - 0.35, "Each row: lowest region to highest region; shaded box = middle 50% of regions, "
            "dark line = median", fontsize=9.5, color=MUTED, va="bottom")
    fig.tight_layout()
    fig.savefig(os.path.join(HERE, "indicators.png"), dpi=150, facecolor="white")
    plt.close(fig)


def rayleigh():
    rng = np.random.default_rng(2025)
    beta, n = 2, 1000
    sample = np.sqrt(-2 * beta ** 2 * np.log(1 - rng.random(n)))  # inverse transform
    x = np.linspace(0, sample.max(), 400)
    density = x / beta ** 2 * np.exp(-x ** 2 / (2 * beta ** 2))
    fig, ax = plt.subplots(figsize=(8, 3.8), facecolor="white")
    ax.hist(sample, bins=30, density=True, color=BLUE_LIGHT, edgecolor="white", linewidth=1.5,
            label="1,000 simulated values (inverse transform)")
    ax.plot(x, density, color=ORANGE, lw=2.5, label=r"Rayleigh($\beta = 2$) density")
    ax.set_xlabel("Value")
    ax.set_ylabel("Density")
    ax.legend(frameon=False)
    ax.set_title("Simulated sample against the theoretical density", loc="left", fontsize=13)
    ax.grid(axis="y", color=GRID)
    ax.set_axisbelow(True)
    for side in ("top", "right", "left"):
        ax.spines[side].set_visible(False)
    ax.spines["bottom"].set_color(GRID)
    ax.tick_params(length=0)
    fig.tight_layout()
    fig.savefig(os.path.join(HERE, "rayleigh.png"), dpi=150, facecolor="white")
    plt.close(fig)


if __name__ == "__main__":
    indicators()
    rayleigh()
