import unittest
import numpy as np

from complexify import cs
from parameterized import parameterized


class TestComplexSafe(unittest.TestCase):
    def setUp(self):
        # Generate random arrays with a fixed seed
        np.random.seed(3)
        self.a = np.random.rand(5)
        self.b = np.random.rand(5)

        self.hFD = 1e-6
        self.hCS = 1e-40

        self.aFD = self.a.copy()
        self.aFD[0] += self.hFD
        self.aCS = np.zeros_like(self.a, dtype=np.complex128)
        self.aCS[:] = self.a.copy()
        self.aCS[0] += 1.0j * self.hCS

    def test_mean_std(self):
        # test mean_std
        m, s = cs.mean_std(self.a)
        np.testing.assert_allclose(m, np.mean(self.a), rtol=1e-6)
        np.testing.assert_allclose(s, np.std(self.a), rtol=1e-6)

    @parameterized.expand(
        [
            (cs.norm, np.linalg.norm, 1e-6),
            (cs.arctan2, np.arctan2, 1e-6),
            (cs.abs, np.abs, 1e-6),
            (cs.std, np.std, 1e-5),
        ]
    )
    def test_functions(self, cs_func, np_func, rtol):
        if cs_func == cs.arctan2:
            args = ((self.b, self.aFD), (self.b, self.a), (self.b, self.aCS))
        else:
            args = ((self.aFD,), (self.a,), (self.aCS,))

        # Compute the FD reference
        value_FD = (np_func(*args[0]) - np_func(*args[1])) / self.hFD

        # Compute CS
        value_CS = np.imag(cs_func(*args[2])) / self.hCS

        np.testing.assert_allclose(value_CS, value_FD, rtol=rtol)


if __name__ == "__main__":
    unittest.main()
