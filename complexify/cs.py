"""
Collection of complex-step safe functions to replace standard numpy operations.
Parts are adapted from https://github.com/OpenMDAO/OpenMDAO/blob/master/openmdao/utils/cs_safe.py
"""

import numpy as np


def abs(x):  # noqa: A001
    """
    complex-step safe version of numpy.abs function.

    Parameters
    ----------
    x : ndarray
        array value to be computed on

    Returns
    -------
    ndarray
    """
    if isinstance(x, np.ndarray):
        return x * np.sign(x)
    elif x.real < 0.0:
        return -x
    return x


def norm(x, axis=None):
    """
    complex-step safe version of numpy.linalg.norm function.

    Parameters
    ----------
    x : ndarray
        array value to be computed on
    axis : None or int
        axis to perform the norm up to

    Returns
    -------
    ndarray
    """
    return np.sqrt(np.sum(x**2, axis=axis))


def arctan2(y, x):
    """
    Numpy-compatible, complex-compatible arctan2 function for use with complex-step.

    Parameters
    ----------
    y : float or complex
        The length of the side opposite the angle being determined.
    x : float or complex
        The length of the side adjacent to the angle being determined.

    Returns
    -------
    ndarray
        The angle whose opposite side has length y and whose adjacent side has length x.
    """
    if np.iscomplexobj(x) or np.iscomplexobj(y):
        a = np.real(y)
        b = np.imag(y)
        c = np.real(x)
        d = np.imag(x)
        return np.arctan2(a, c) + 1j * (c * b - a * d) / (a**2 + c**2)
    else:
        return np.arctan2(y, x)


def mean_std(x):
    """
    A function to compute mean and standard deviation of a numpy array.

    Parameters
    ----------
    x : ndarray
        The array of sampled values

    Returns
    -------
    mean
        The mean value of the input array
    stdev
        The standard deviation of the input array
    """
    mean = np.mean(x)
    std = np.sqrt(np.mean((x - mean) ** 2))
    return mean, std


def std(x):
    """
    A function to compute the standard deviation of a numpy array.

    Parameters
    ----------
    x : ndarray
        The array of sampled values

    Returns
    -------
    stdev
        The standard deviation of the input array
    """
    std = np.sqrt(np.mean((x - np.mean(x)) ** 2))
    return std
