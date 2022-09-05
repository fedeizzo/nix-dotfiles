from setuptools import setup, find_packages

with open("requirements.txt") as f:
    req = f.read().splitlines()
    requirements = []
    extra_deps = []
    for i in range(len(req)):
        if req[i].startswith("https"):
            extra_deps.append(req[i])
        elif req[i] != "":
            requirements.append(req[i])

setup(
    name="name",
    version="0.1.0",
    author="author",
    author_email="email",
    packages=find_packages(exclude=("tests",)),
    description="desc",
    classifiers=[
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
        "License :: OSI Approved :: Apache Software License",
        "Operating System :: OS Independent",
    ],
    python_requires=">=3.9",
    setup_requires=[],
    entry_points={"console_scripts": ["name = name:main"]},
    install_requires=requirements,
    dependency_links=extra_deps,
)
