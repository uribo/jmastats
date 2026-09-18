## Submission summary

This is a patch release. It updates the bundled station datasets to the September 2026 revision of the Japan Meteorological Agency (JMA) sources and fixes two bugs in `jma_collect()`: a failure when the cache directory does not exist, and a `block_no` validation that rejected stations added in the new data.

## Test environments

* local macOS 26.7, R 4.6.1
* GitHub Actions: macOS, Windows and Ubuntu (R devel, release and oldrel-1), and Ubuntu 22.04 with R 4.1
* win-builder: R-devel (2026-09-16 r90549 ucrt)

## R CMD check results

0 errors | 0 warnings | 0 notes

* The checks above were run on the development version 0.3.0.9000. The only NOTEs were "Version contains large components" for that version number (local and win-builder) and, locally, an outdated HTML Tidy that skipped HTML validation of the manual. Neither applies to the submitted version.
* Examples wrapped in `\donttest{}` download data from the JMA website, so they are not run on CRAN.
* "JMA" in DESCRIPTION is the abbreviation of the Japan Meteorological Agency, not a misspelling.

## Reverse dependencies

There are currently no reverse dependencies.
