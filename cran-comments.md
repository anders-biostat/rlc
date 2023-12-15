## Resubmission

This version of the package now relies on the updated version of D3 (v7.8.5 instead of v4.10.0). Following the update of the underlying `jrc` package, the `listen` function has been added to make the use of rlc apps from Jupyter Notebooks more convenient. Also, some bugs were fixed (see NEWS.md).

## Test environments

* local Ubuntu 23.10, R 4.3.1
* win-builder: R-devel
* Ubuntu Linux 20.04.1 LTS, R-release; Windows Server 2022, R-devel, 64 bit; Fedora Linux, R-devel (R-hub)

There is a NOTE, since I've updated my email address.
```
* checking CRAN incoming feasibility ... [5s/28s] NOTE
Maintainer: ‘Svetlana Ovchinnikova <svetlana.ovchinnikova@bioquant.uni-heidelberg.de>’

New maintainer:
  Svetlana Ovchinnikova
    <svetlana.ovchinnikova@bioquant.uni-heidelberg.de>
Old maintainer(s):
  Svetlana Ovchinnikova <s.ovchinnikova@zmbh.uni-heidelberg.de>
```

# Previous cran-comments

## Resubmission

This is a new version of the package. It fixes some bugs, updates styling, adjusts some default parameters and also adds some minor functionality to customise user input blocks.

## Test environments

* local Ubuntu 21.10, R 4.0.4
* win-builder: R-devel
* Ubuntu Linux 20.04.1 LTS, R-release; Windows Server 2022, R-devel, 64 bit; Fedora Linux, R-devel (R-hub)

There is a NOTE when testing for Windows Server (R-hub): 
```
* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
```
But this note does not appear neither locally (Windows 10, 64 bit, R 4.0.2), nor in the win-builder checks.

## Resubmission

This is a new version of the package. It fixes multiple bugs, adds a way to control legend's size and location and to specify the tooltip content with custom HTML.

## Test environments

* local ubuntu 20.04 LST, R 4.1.1
* win-builder: R-devel
* Ubuntu 20.04.1 LTS, R-release, Windows Server 2008 R2 SP1, R-devel, Fedora Linux, R-devel (R-hub)

## R CMD check results

There were no ERRORS, WARNINGS or NOTES

## Resubmission

> Please change http --> https, add trailing slashes, or follow moved content as appropriate.

> Please fix and resubmit.

Done.

## Test environments

* local ubuntu 20.04 LST, R 4.0.2
* win-builder: R-devel
* ubuntu 16.04.6 LTS, R-devel, R 4.0.2, R 3.6.3 (travis ci)

## R CMD check results

There were no ERRORS, WARNINGS or NOTES

## Resubmission

This is a new version of the package. It introduces a new function `lc_image`, and some addional arguments (`clear` in `mark`, 
`on_positionClick` in all charts with axes), fixes styling and some known issues. For more details, please, check `NEWS.md`.

All the examples were checked locally by both R CMD check --run-donttest and running them manually in RStudio. 
However, all other checks were performed with _R_CHECK_DONTTEST_EXAMPLES_=FALSE.
Examples, wrapped in donttest{}, are functional and don't produce errors, but require some browser to be installed. 
I would rather not change `donttest` to `dontrun`, since it can confuse users.

## Test environments
* local ubuntu 20.04 LST, R 4.0.2
* win-builder: R-devel

produces the following NOTE

Found the following (possibly) invalid URLs:
  URL: http://www.r-pkg.org/pkg/rlc (moved to https://www.r-pkg.org:443/pkg/rlc)
    From: README.md
    Status: 200
    Message: OK

However, the URL seems to be valid. 

* ubuntu 16.04.6 LTS, R-devel, R 4.0.2, R 3.6.3 (travis ci)

## R CMD check results

There were no ERRORS, WARNINGS or NOTES

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
