# Read RSMC Tokyo-Typhoon Center's best track data

**\[stable\]**

Tidy formatting best track data and combine each point to line.

## Usage

``` r
read_rsmc_besttrack(path)

track_combine(
  data,
  group_vars = c("international_number", "storm_name"),
  keep_vars = NULL,
  geometry = geometry
)
```

## Arguments

- path:

  path to best track data (`.txt`). Give the path as a directory in the
  user's computer or the URL.

- data:

  Import data using read_rsmc_besttrack

- group_vars:

  To combine track variables.

- keep_vars:

  Keep variables.

- geometry:

  geometry column name (default `geometry`).

## Value

a `tbl` object

## Details

- `read_rsmc_besttrack()`: Read single best track data into
  [sf](https://r-spatial.github.io/sf/reference/sf.html) contains
  observation record as point.

- `track_combine()`: Combine track data to line by id (such as
  international_number and storm_name).

## See also

<https://www.jma.go.jp/jma/jma-eng/jma-center/rsmc-hp-pub-eg/RSMC_HP.htm>

## Examples

``` r
read_rsmc_besttrack(path = system.file("dummy/bst.txt", package = "jmastats"))
#> Simple feature collection with 2 features and 21 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 129.3 ymin: 32.5 xmax: 129.6 ymax: 33
#> Geodetic CRS:  WGS 84
#> # A tibble: 2 × 22
#>   datetime            indicator_002 grade `central_pressure(hPa)`
#>   <dttm>              <chr>         <chr>                   <dbl>
#> 1 1991-09-27 06:00:00 002           5                         935
#> 2 1991-09-27 12:00:00 002           5                         994
#> # ℹ 18 more variables: `maximum_sustained_wind_speed(knot)` <dbl>,
#> #   `_direction_of_the_longest_radius_of_50kt_winds_or_greater` <chr>,
#> #   `_the_longest_radius_of_50kt_winds_or_greater(nm)` <chr>,
#> #   `_the_shortest_radius_of_50kt_winds_or_greater(nm)` <chr>,
#> #   `_direction_of_the_longest_radius_of_30kt_winds_or_greater` <chr>,
#> #   `_the_longest_radius_of_30kt_winds_or_greater(nm)` <chr>,
#> #   `_the_shortest_radius_of_30kt_winds_or_greater(nm)` <chr>, …

read_rsmc_besttrack(path = system.file("dummy/bst.txt", package = "jmastats")) |>
  track_combine()
#> Simple feature collection with 1 feature and 2 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: 129.3 ymin: 32.5 xmax: 129.6 ymax: 33
#> Geodetic CRS:  WGS 84
#> # A tibble: 1 × 3
#>   international_number storm_name               geometry
#> * <chr>                <fct>            <LINESTRING [°]>
#> 1 9119                 MIRREILE   (129.3 32.5, 129.6 33)
```
