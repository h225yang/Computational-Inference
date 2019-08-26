#!/usr/bin/env python

from scipy.stats import norm
import numpy as np
import matplotlib.pyplot as plt

def L(xn, theta, n):
    return n * norm.pdf(xn, theta) * norm.cdf(xn, theta)**(n-1)

def L2(y_bar, theta, n):
    return norm.pdf(y_bar, theta, 1/n)

x = np.linspace(0, 6, 200)

y = [L(3.5, i, 5) for i in x]

y_2 = [L(4, i, 4) for i in x]

y_3 = np.array(y) * np.array(y_2)

plt.plot(x, y, x, y_2, x, y_3)
plt.show()

