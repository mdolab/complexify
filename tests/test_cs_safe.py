import unittest
import numpy as np

from complexify import cs_safe


class TestComplexSafe(unittest.TestCase):
    def setUp(self):

        # Generate random arrays with a fixed seed
        np.random.seed(3)
        self.a = np.random.rand(5)
        self.b = np.random.rand(5)

        self.hFD = 1e-6
        self.hCS = 1e-40

    def test_norm(self):
        # Test a computation of a unit vector  u = a / | a |

        # Compute the FD reference
        aFD = self.a.copy()
        aFD[0] += self.hFD
        unitFD = (aFD / np.linalg.norm(aFD) - self.a / np.linalg.norm(self.a)) / self.hFD

        # Compute CS
        aCS = np.zeros_like(self.a, dtype=np.complex128)
        aCS[:] = self.a.copy()
        aCS[0] += 1.0j * self.hCS
        unitCS = np.imag(aCS / cs_safe.norm(aCS)) / self.hCS

        np.testing.assert_allclose(unitCS, unitFD, rtol=1e-6)

    def test_arctan2(self):
        # Compute the FD reference
        aFD = self.a.copy()
        aFD[0] += self.hFD
        arctan2FD = (np.arctan2(self.b, aFD) - np.arctan2(self.b, self.a)) / self.hFD

        # Compute CS
        aCS = np.zeros_like(self.a, dtype=np.complex128)
        aCS[:] = self.a.copy()
        aCS[0] += 1.0j * self.hCS
        arctan2CS = np.imag(cs_safe.arctan2(self.b, aCS)) / self.hCS

        np.testing.assert_allclose(arctan2CS, arctan2FD, rtol=1e-6)

    def test_abs(self):
        # Compute the FD reference
        aFD = self.a.copy()
        aFD[0] += self.hFD
        absFD = (np.abs(aFD) - np.abs(self.a)) / self.hFD

        # Compute CS
        aCS = np.zeros_like(self.a, dtype=np.complex128)
        aCS[:] = self.a.copy()
        aCS[0] += 1.0j * self.hCS
        absCS = np.imag(cs_safe.abs(aCS)) / self.hCS

        np.testing.assert_allclose(absCS, absFD, rtol=1e-6)

    def test_mean_std(self):
        # Compute the FD reference
        aFD = self.a.copy()
        aFD[0] += self.hFD
        absFD = (np.std(aFD) - np.std(self.a)) / self.hFD

        # Compute CS
        aCS = np.zeros_like(self.a, dtype=np.complex128)
        aCS[:] = self.a.copy()
        aCS[0] += 1.0j * self.hCS
        absCS = np.imag(cs_safe.std(aCS)) / self.hCS

        np.testing.assert_allclose(absCS, absFD, rtol=1e-5)

        # test mean_std
        m, s = cs_safe.mean_std(self.a)
        np.testing.assert_allclose(m, np.mean(self.a), rtol=1e-6)
        np.testing.assert_allclose(s, np.std(self.a), rtol=1e-6)


if __name__ == "__main__":
    unittest.main()
