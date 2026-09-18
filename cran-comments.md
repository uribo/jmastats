## Submission summary

This is a patch release. It updates the bundled station datasets to the September 2026 revision of the Japan Meteorological Agency (JMA) sources and fixes two bugs in `jma_collect()`: a failure when the cache directory does not exist, and a `block_no` validation that rejected stations added in the new data.

## Test environments

* local macOS 26.7, R 4.6.1
* GitHub Actions: macOS, Windows and Ubuntu (R devel, release and oldrel-1), and Ubuntu 22.04 with R 4.1

## R CMD check results

0 errors | 0 warnings | 0 notes

* Examples wrapped in `\donttest{}` download data from the JMA website, so they are not run on CRAN.
* "JMA" in DESCRIPTION is the abbreviation of the Japan Meteorological Agency, not a misspelling.

## Reverse dependencies

There are currently no reverse dependencies.
