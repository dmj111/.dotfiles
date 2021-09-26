from setuptools import setup

setup(
    name="dmj-tools",
    version="0.6",
    py_modules=["clean_git_gone"],
    install_requires=["Click", "gitpython"],
    entry_points={"console_scripts": ["git-clean-gone = clean_git_gone:cli"]},
    scripts=[
        "git-clean-stash",
        "git-rsh",
        "git-rsu",
        "git-smg",
        "git-trash",
    ],
)
