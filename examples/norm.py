import numpy as np
from complexify.cs_safe import norm

# Compute the FD reference
a = np.array([3.0, -10.0, 2.0], dtype=np.float)
b = np.array([1.0, 4.0, -5.0], dtype=np.float)

a_copy = a.copy()
h = 1.0e-4
a_copy[0] += h

# EX1 - Normalize vec b = a / | a |
unit_fd = (a_copy / np.linalg.norm(a_copy) - a / np.linalg.norm(a)) / h

# EX2 - obj = norm(a) * norm(b)
obj_fd = (np.linalg.norm(a_copy) * np.linalg.norm(b) - np.linalg.norm(a) * np.linalg.norm(b)) / h

# Compute CS
h = 1.e-20
a = np.array([3.+h*1j, -10.0, 2.0], dtype=np.complex128)
b = np.array([1.0, 4.0, -5.0], dtype=np.complex128)

# EX1
unit_cs_wrong = a / np.linalg.norm(a)
unit_cs = a / norm(a)
print("FD unit:", unit_fd)
print("CS unit wrong:", np.imag(unit_cs_wrong / h))
print("CS unit right:", np.imag(unit_cs / h))

# EX2
obj_cs_wrong = np.linalg.norm(a) * np.linalg.norm(b)
obj_cs = norm(a) * norm(b)
print("FD obj:", obj_fd)
print("CS obj wrong:", np.imag(obj_cs_wrong / h))
print("CS obj right:", np.imag(obj_cs / h))

