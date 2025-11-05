# suntools 1.1.0

Corrects the output date (given when `POSIXct.out = TRUE`) on days that shift between daylight savings time and standard time. On these days only the output date was one hour off (#19).

# suntools 1.0.1

Minor change to unit test to maintain compatibility with CRAN Debian system

* changed timezone EET to Europe/Helsinki in tests

# suntools 1.0.0

First release of `suntools` package on CRAN. Provides a set of functions to calculate sun position, sunrise, sunset, solar noon and twilight, which were formerly made available by the [maptools]((https://CRAN.R-project.org/package=maptools) package.

## New features

* core functionality from former `maptools/R/sun-methods.R` has been retained in the original form.
* new method for spatial objects of class `sf`
* each method / function moved into a separate file for better readability.
* The package now includes unit tests to ensure reliability of functions.
* CI/CD has been introduced via GitHub Actions for a more efficient workflow. This includes automated checks that run across different operating systems (macOS, Windows, Ubuntu) and R versions.
* Added a `NEWS.md` file to track changes to the package.

## Bug Fixes
* Resolved an issue where functions in the package would return a dataframe with a row name "newlon" when inputting a single-row matrix. This was unexpected and appeared in R versions < 4.3.0.
* Resolved an issue where functions in the package would return numeric values instead of dataframes. 
