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
    for [m, n, v] in json.load(file):
        series = data.setdefault(m, [])
        series.append((n, v * 1000))
    for k in data:
        data[k].sort()

def scatter(ax, key, label):
    x, y = zip(*data[key])
    ax.scatter(x, y, label=label, s=10)

    # Display a linear regression of the same color.
    model = LinearRegression()
    x = np.reshape(x, (-1, 1))
    model.fit(x, y)
    ax.plot(x, model.predict(x), linewidth=1)

plt.style.use("seaborn")
plt.rcParams["font.family"] = "sans-serif"
plt.rcParams["font.sans-serif"] = "SF Pro Display"
plt.rcParams["font.size"] = 13
plt.rcParams["xtick.labelsize"] = 9
plt.rcParams["ytick.labelsize"] = 9

fig, axs = plt.subplots(2)
fig.suptitle("Read and write performance using different disk access patterns", weight="bold")
scatter(axs[0], "sequential.read", "Sequential read")
scatter(axs[0], "append.read", "Sequential read with O_APPEND")
scatter(axs[0], "random.read", "Random-offset read")
axs[0].legend()
axs[0].set_xlabel("Number of 128-byte blocks read", fontstyle="italic", labelpad=10)
axs[0].set_ylabel("Duration (ms)", fontstyle="italic", labelpad=20)

scatter(axs[1], "sequential.write", "Sequential write")
scatter(axs[1], "append.write", "Sequential write with O_APPEND")
scatter(axs[1], "random.write", "Random-offset write")
axs[1].legend()
axs[1].set_xlabel("Number of 128-byte blocks written", fontstyle="italic", labelpad=10)
axs[1].set_ylabel("Duration (ms)", fontstyle="italic", labelpad=25)

plt.tight_layout()
plt.show()