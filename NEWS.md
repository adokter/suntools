# suntools 1.0.0

We're excited to introduce the first release of `suntools`. This package, derived from the deprecated `maptools` package, offers a set of functions to accurately calculate sun position, sunrise, sunset, solar noon and twilight. The underlying algorithms are based on research by the National Oceanic & Atmospheric Administration (NOAA).

## Features

* Core methods from `maptools/R/sun-methods.R` have been retained in their original form. These methods have been split into separate scripts for better readability.
* The package now includes unit tests to ensure reliability of functions.
* CI/CD has been introduced via GitHub Actions for a more efficient workflow. This includes automated checks that run across different operating systems (macOS, Windows, Ubuntu) and R versions.
* Added a `NEWS.md` file to track changes to the package.


## Bug Fixes

* Resolved an issue where functions in the package would return a dataframe with a row name "newlon" when inputting a single-row matrix. This was unexpected and appeared in R versions < 4.3.0.

