# Parse data variable units

**\[stable\]**

## Usage

``` r
parse_unit(data, rename = TRUE)
```

## Arguments

- data:

  data

- rename:

  *logical*

## Value

a `tbl` object

## Examples

``` r
# For data retrieved with jma_collect(), here is a minimal example.
d <-
tibble::tibble(date = as.Date(c(17742, 17742, 17742, 17742, 17742, 17742), origin = "1970-01-01"),
               time = c(1, 2, 3, 4, 5, 6),
               `precipitation(mm)` = c(0, 0, 0, 0, 0, 0),
               `temperature(℃)` = c(22.4, 22.1, 21, 20.2, 20.4, 23.5))
d |> parse_unit(rename = TRUE)
#> New names:
#> • `` -> `...1`
#> • `` -> `...2`
#> # A tibble: 6 × 4
#>   date        time precipitation temperature
#>   <date>     <dbl>          [mm]        [°C]
#> 1 2018-07-30     1             0        22.4
#> 2 2018-07-30     2             0        22.1
#> 3 2018-07-30     3             0        21  
#> 4 2018-07-30     4             0        20.2
#> 5 2018-07-30     5             0        20.4
#> 6 2018-07-30     6             0        23.5
```
