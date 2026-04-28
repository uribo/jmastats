# Convert and split tidal level data

**\[stable\]**

## Usage

``` r
pivot_tide_level(data)
```

## Arguments

- data:

  tidal level data

## Value

List to store two datasets containing hourly and tide level data.

## See also

[`read_tide_level()`](https://uribo.github.io/jmastats/reference/read_tide_level.md)

## Examples

``` r
read_tide_level(system.file("dummy/tide.txt", package = "jmastats")) |>
  pivot_tide_level()
#> New names:
#> • `hm` -> `hm...1`
#> • `hm` -> `hm...2`
#> • `hm` -> `hm...3`
#> • `hm` -> `hm...4`
#> New names:
#> • `tidal_level_cm` -> `tidal_level_cm...1`
#> • `tidal_level_cm` -> `tidal_level_cm...2`
#> • `tidal_level_cm` -> `tidal_level_cm...3`
#> • `tidal_level_cm` -> `tidal_level_cm...4`
#> New names:
#> • `hm` -> `hm...1`
#> • `hm` -> `hm...2`
#> • `hm` -> `hm...3`
#> • `hm` -> `hm...4`
#> New names:
#> • `tidal_level_cm` -> `tidal_level_cm...1`
#> • `tidal_level_cm` -> `tidal_level_cm...2`
#> • `tidal_level_cm` -> `tidal_level_cm...3`
#> • `tidal_level_cm` -> `tidal_level_cm...4`
#> $hourly
#> # A tibble: 24 × 3
#>    datetime            stn   tide_value
#>    <dttm>              <chr>       [cm]
#>  1 2023-01-01 00:00:00 TK           128
#>  2 2023-01-01 01:00:00 TK           127
#>  3 2023-01-01 02:00:00 TK           122
#>  4 2023-01-01 03:00:00 TK           115
#>  5 2023-01-01 04:00:00 TK           107
#>  6 2023-01-01 05:00:00 TK           102
#>  7 2023-01-01 06:00:00 TK           101
#>  8 2023-01-01 07:00:00 TK           106
#>  9 2023-01-01 08:00:00 TK           117
#> 10 2023-01-01 09:00:00 TK           132
#> # ℹ 14 more rows
#> 
#> $tide
#> # A tibble: 4 × 6
#>   date       stn   tide_level count time   tide_value
#>   <date>     <chr> <chr>      <chr> <time>       [cm]
#> 1 2023-01-01 TK    low        1     00:18         128
#> 2 2023-01-01 TK    high       1     05:42         101
#> 3 2023-01-01 TK    low        2     11:58         160
#> 4 2023-01-01 TK    high       2     19:29          60
#> 
```
