#!/usr/local/bin/python3
from matplotlib import pyplot as plt
from sklearn.linear_model import LinearRegression
import numpy as np
import json
import sys

if len(sys.argv) < 2:
    print("Usage: %s path" % sys.argv[0])
    exit()

path = sys.argv[1]
with open(path) as file:
    data = {}
    for [kind, measures] in json.load(file):
        sub_data = data.setdefault(kind, {})
        for [m, n, v] in measures:
            series = sub_data.setdefault(m, [])
            series.append((n, v))
    for kind in data:
        for k in data[kind]:
            data[kind][k].sort()

def scatter(ax, data, key, label):
    x, y = zip(*data[key])
    ax.scatter(x, y, label=label, s=6)

def plot(ax, data):
    scatter(ax, data, "diet/add_interval", "Adding an interval")
    scatter(ax, data, "diet/remove_interval", "Deleting an interval")
    scatter(ax, data, "diet/take_interval", "Finding a sub-interval")
    ax.legend()
    ax.set_xlabel("Number of intervals in the structure", fontstyle="italic", labelpad=10)

plt.style.use("seaborn")
plt.rcParams["font.family"] = "sans-serif"
plt.rcParams["font.sans-serif"] = "SF Pro Display"
plt.rcParams["font.size"] = 13
plt.rcParams["xtick.labelsize"] = 9
plt.rcParams["ytick.labelsize"] = 9

fig, axs = plt.subplots(ncols=3)
fig.suptitle("Time and memory benchmark of DIET operations", weight="bold")

plot(axs[0], data["monotonic-clock"])
axs[0].set_ylabel("Duration (ns)", fontstyle="italic", labelpad=10)

plot(axs[1], data["major-allocated"])
axs[1].set_ylabel("Major-heap allocations (mw/run)", fontstyle="italic", labelpad=10)

plot(axs[2], data["minor-allocated"])
axs[2].set_ylabel("Minor-heap allocations (w/run)", fontstyle="italic", labelpad=10)

plt.tight_layout()
plt.show()