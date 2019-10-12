# Contributing Guidelines

Guidelines for developing and contributing to this project.

## List of project maintainers

- [Andrew Mathis](https://github.com/ammathis)

## Opening new issues

- Before opening a new issue check if there are any existing issues or pull requests that match your case
- Open an issue, and make sure to label the issue accordingly - bug, improvement, feature request, etc...
- Be as specific and detailed as possible


## Setting up the development environment

- Install R (and ideally RStudio for convenience)
- Install dependencies as listed in DESCRIPTION
- Install the `devtools` package
- Set up your DataRobot API credentials in a YAML file as described in the `datarobot::ConnectToDataRobot` documentation 
- Checkout the project
- (Optional) If you will be running full integration tests with a live DataRobot installation:
  - Create a DataRobot project for the unit test data (data specified in [/tests/testthat/helper_init_test_resources.R](/tests/testthat/helper_init_test_resources.R))
  - Specify the project and model IDs in [/tests/testthat/test_project_model_ids.yaml](/tests/testthat/test_project_model_ids.yaml)
- (Optional) If you plan to refresh the DataRobot data used for the vignette:
  - Create a DataRobot project for the vignette data (data specified in [/vignettes/PredictionExplanationClustering.Rmd](/vignettes/PredictionExplanationClustering.Rmd))
  - Specify the project and model IDs in [/vignettes/vignette_project_model_ids.yaml](/vignettes/vignette_project_model_ids.yaml)
- Run `devtools::check()` to run checks and unit tests
- To run full integration tests, alter `TEST_WITH_LIVE_DATAROBOT_CONNECTION` flag in [/tests/testthat/helper_init_test_resources.R](/tests/testthat/helper_init_test_resources.R)

## Project structure

- Main package code is in [/R](/R). `roxygen2` documentation integrated with code.
- Test structure in [/tests/testthat](/tests/testthat)
- Vignette in [/vignettes](/vignettes)
- Auto-generated documentation in [/man](/man) (generated automatically by roxygen2, but committed to repository for compatibility with `devtools::install_github`)

## Making a pull request

- Before commiting, make sure to run `devtools::check()` to ensure documentation is refreshed and checks are run.
- Have a branch with a descriptive name
- Squash / rebase your commits before opening your pull request
- Pull the latest changes from `master`
- Provide sufficient description of the pull request. Include whether it relates to an existing issue, and specify what the pull request does - (bug fix, documentation correction, dependency update, new functionality, etc...). When in doubt, overcommunicate

## Responding to issues and pull requests

This project's maintainers will make every effort to respond to any open issues as soon as possible.

If you don't get a response within 7 days of creating your issue or pull request, please send us an email at community@datarobot.com.
