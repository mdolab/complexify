from setuptools import setup
import re

__version__ = re.findall(r"""__version__ = ["']+([0-9\.]*)["']+""", open("complexify/__init__.py").read())[0]

setup(
    name="complexify",
    version=__version__,
    description="complexify is a package providing tools to run complex step in python or Fortran codes.",
    keywords="complexify",
    author="",
    author_email="",
    url="https://github.com/mdolab/complexify",
    license="Apache 2.0",
    packages=["complexify"],
    install_requires=["numpy>=1.16"],
    classifiers=["Operating System :: Linux", "Programming Language :: Python, Fortran"],
    entry_points={"console_scripts": ["complexify = complexify.complexify:main"]},
    extras_require={
        "testing": ["parameterized", "testflo"],
    },
)
