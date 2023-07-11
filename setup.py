from setuptools import setup
import re
import os

__version__ = re.findall(r"""__version__ = ["']+([0-9\.]*)["']+""", open("complexify/__init__.py").read())[0]

this_directory = os.path.abspath(os.path.dirname(__file__))
with open(os.path.join(this_directory, "README.md"), encoding="utf-8") as f:
    long_description = f.read()

with open("doc/requirements.txt") as f:
    docs_require = f.read().splitlines()

setup(
    name="complexify",
    version=__version__,
    description="complexify is a package providing tools to run complex step in python or Fortran codes.",
    long_description=long_description,
    long_description_content_type="text/markdown",
    keywords="complexify",
    author="",
    author_email="",
    url="https://github.com/mdolab/complexify",
    license="Apache License Version 2.0",
    packages=["complexify"],
    install_requires=["numpy>=1.16"],
    classifiers=["Operating System :: Linux", "Programming Language :: Python, Fortran"],
    entry_points={"console_scripts": ["complexify = complexify.complexify:main"]},
    extras_require={
        "docs": docs_require,
        "testing": ["parameterized", "testflo"],
    },
)
