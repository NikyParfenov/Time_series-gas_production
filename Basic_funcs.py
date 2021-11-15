import numpy as np


# Basic functions for trend approximation (lin, log, hyp) and Fourier fitting

def func_hyp(x, y0, a, b):
    return y0 * 1 / (1 + a * x) ** b


def func_log(x, y0, a, b):
    return y0 * np.log(a * x + 1) + b


def func_lin(x, y0, a):
    return y0 + a * x


# Fourier approximation of k-order.
# The 1st func is only for simultaneous trend correction during Fourier matching
# The 2nd func is the main func of k-order Fourier

def func_general_fourier(x, a, theta, trend_func, *args):
    return trend_func(x, *args) * (1 + a * np.cos(2 * np.pi / 365.25 * x + theta))


def func_general_fourier_k(x, a, theta, k, trend_func, *args):
    return trend_func(x, *args) * a * np.cos(2 * np.pi * k / 365.25 * x + theta)
