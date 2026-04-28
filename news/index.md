# Changelog

## jmastats (development version)

## jmastats 0.3.0

CRAN release: 2025-01-28

### New features

- Retrieve climatological normals based on past data using
  [`jma_collect()`](https://uribo.github.io/jmastats/reference/jma_collect.md)
  ([\#20](https://github.com/uribo/jmastats/issues/20)).

### Datasets

- Various datasets handled by the package have been updated to the
  latest version in January 2025.

## jmastats 0.2.2

CRAN release: 2024-08-01

### Datasets

- Various datasets handled by the package have been updated to the
  latest version in July 2024.

### Fixes

- The coordinates and prefecture code of the station dataset have been
  corrected.

## jmastats 0.2.1

CRAN release: 2024-03-02

### Datasets

- Various datasets handled by the package have been updated to the
  latest version in March 2024.

## jmastats 0.2.0

CRAN release: 2023-09-11

- Initial release for CRAN.

### Features

- Added an interval when executing
  [`jma_collect()`](https://uribo.github.io/jmastats/reference/jma_collect.md)
  for reducing the load on the server.
- A message is displayed when the data obtained by
  [`jma_collect()`](https://uribo.github.io/jmastats/reference/jma_collect.md)
  contains values such as missing values.

### Fixes

- Fixed an issue with parameters when acquiring data with
  [`jma_collect()`](https://uribo.github.io/jmastats/reference/jma_collect.md).

### Datasets

- Various datasets handled by the package have been updated to the
  latest version in March 2023.

## jmastats 0.1.0

- Data is now downloaded and saved in a local folder using rappdir
  package. In acquiring data under the same conditions, locally stored
  files are loaded.
- Added a `NEWS.md` file to track changes to the package.
