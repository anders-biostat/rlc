## Resubmission

This is a new version of the package that follows the update of `jrc` package. `rlc` can now utilize new functionality of
`jrc` and produce server app that can be used by multiple clients simultaneously. The package has also been restrunctured
so that the entire app is now contained within a single `R6` object, which inherits from `jrc::App`.

## Reverse dependencies

There are none.

## Test environments
* local ubuntu 18.04 LST, R 3.6.2
* win-builder: R-devel
* ubuntu 16.04.6 LTS, R-devel, R 3.6.2, R 3.5.3 (travis ci)

## R CMD check results

There were no ERRORS, WARNINGS or NOTES

## Test environments
* local ubuntu 18.04 LST, R 3.6.1
* win-builder: R-devel
* ubuntu 14.04.5 LTS, R-devel, R 3.5.2, R 3.4.4 (travis ci)

## R CMD check results

There were no ERRORS, WARNINGS or NOTES

## Examples in \donttest{}
To test any function in this package one needs to open a web page and
establish a websocket connection. While this works locally,
devtools::check_rhub() always produces errors about not being able to
open a web browser. Therefore all the examples are wrapped in 
`donttest{}`. Yet, all examples were tested locally both manually in 
RStudio and with R CMD check --run-donttest. 
