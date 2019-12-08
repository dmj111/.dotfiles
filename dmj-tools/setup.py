from setuptools import setup

setup(
    name="dmj-tools",
    version="0.1",
    py_modules=["clean_git_gone"],
    install_requires=["Click", "gitpython"],
    entry_points={"console_scripts": ["clean_git_gone = clean_git_gone:cli"]},
)
