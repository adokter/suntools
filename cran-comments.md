# suntools 1.0.0
New package submissions.

This package takes over several sun functions of R package maptools, which is to be deprecated.

* made methods compatible with sp evolution and deprecation
* new method for sf class
* Core methods from `maptools/R/sun-methods.R` are retained in their original form and split into separate scripts in for easier maintenance and readability.
* Generated detailed documentation for each method including examples
* Unit tests introduced to ensure reliability of functions.
* CI/CD introduced via GitHub Actions for a more efficient workflow. This includes automated checks that run across different operating systems (macOS, Windows, Ubuntu) and R versions.
* Carefully reviewed all files of the package and attributed authorship and references in DESCRIPTION and README
