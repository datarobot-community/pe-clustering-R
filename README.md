**Please note:** The code in these repos is sourced from the DataRobot user community and is not owned or maintained by DataRobot, Inc. You may need to make edits or updates for this code to function properly in your environment.

# Prediction Explanation Clustering (R package)

This repository hosts the R package `datarobot.pe.clustering`. This package allows users of the DataRobot platform to perform dimensionality reduction and clustering on DataRobot's prediction explanations.

## Usage

You can install this package using ``devtools::install_github()`` (see below) and then incorporate it into your R scripts, together with the `datarobot` R package and a DataRobot installation.

Once installed, view the vignette for further documentation and examples.

## Repository Contents

This repository contains the package metadata, package code, unit tests, and documentation. 

## Setup/Installation

#### Installing from GitHub

The development version here can be downloaded straight from GitHub. Add in the `build_vignettes` flag to ensure the vignettes are built. 

```R
if (!require("devtools")) { install.packages("devtools") }
devtools::install_github("datarobot-community/pe-clustering-R", build_vignettes=TRUE)
```

You will need to [set up a GitHub PAT token](https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/) and then `export GITHUB_PAT=<token>` in your shell before running `install_github`.

To install a particular version from GitHub (or a particular branch), use `@` notation in `install_github`, e.g. `devtools::install_github("datarobot-community/pe-clustering-R@v1.0")` or `devtools::install_github("datarobot-community/pe-clustering-R@myAwesomeBranch")`.


## Development and Contributing

If you'd like to report an issue or bug, suggest improvements, or contribute code to this project, please refer to [CONTRIBUTING.md](CONTRIBUTING.md).


# Code of Conduct

This project has adopted the Contributor Covenant for its Code of Conduct. 
See [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md) to read it in full.

# License

Licensed under the Apache License 2.0. 
See [LICENSE](LICENSE) to read it in full.


