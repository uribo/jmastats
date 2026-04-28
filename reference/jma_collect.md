# Collect JMA Historical Weather Data

**\[stable\]**

Refer to the data available in the JMA Historical Weather Data Search.
Executed by specifying the target location and date. Currently, not all
types of data acquisition are supported.

## Usage

``` r
jma_collect(
  item = NULL,
  block_no,
  year,
  month,
  day,
  cache = TRUE,
  pack = TRUE,
  quiet = FALSE
)
```

## Arguments

- item:

  Type of weather data to be acquired. Mainly specifies the interval
  between records (e.g. `daily` or `hourly`). See NOTE for details.

- block_no:

  Block number of the location to be observed. It is assumed that
  block_no is input as a string consisting of a 4- or 5-digit number. If
  a numeric value is specified, it is processed as a string.

- year:

  select year

- month:

  select month

- day:

  select date (default `NULL`)

- cache:

  use cash and save to cache. (`TRUE`, the default)

- pack:

  Whether to packing common variables or not. (`TRUE`, the default)

- quiet:

  Whether to output information on variable and row combinations that
  were treated as missing values for some reason. (`TRUE`, the default)

## Value

a `tbl` object

## Note

The parameter `item` chooses one from these:

- annually: Annual value. Please specify a location by `block_no`.

- monthly: Monthly value. Please specify location and year.

- 3monthly: Value every 3 months. Please specify location and year.

- 10daily: Seasonal value. Please specify location and year.

- mb5daily: Semi-seasonal value. Please specify location and year.

- daily: Daily value. Please specify location, year and month.

- hourly: Hourly value. Please specify location, year, month and day.

- rank: Values of the largest in the history of observations.

- nml_ym: Climatological normals for each year and month.

- nml_3m: Climatological normals for each 3 months.

- nml_10d: Climatological normals for each season (almost 10 days).

- nml_mb5d: Climatological normals for each semi-season (almost 5 days).

- nml_daily: Daily climatological normals for specific month. for each
  location.

## Examples

``` r
# \donttest{
# Annually
jma_collect(item = "annually", "1284", year = 2017, month = 11, cache = FALSE)
#> Data from: https://www.data.jma.go.jp/stats/etrn/view/annually_a.php?prec_no=11&block_no=1284&year=2017&month=11&day=&view=Treated as missing: lines 1, 32, 42 at precipitation_sum(mm)
#> Treated as missing: lines 1, 32, 42, 43 at precipitation_max_per_day(mm)
#> Treated as missing: lines 1, 18, 21, 29, 32, 42 at precipitation_max_1hour(mm)
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 42 at precipitation_max_10minutes(mm)
#> Treated as missing: lines 1, 10, 17, 21, 32 at temperature_average(℃)
#> Treated as missing: lines 1, 7, 10, 17, 21, 32 at temperature_average_max(℃)
#> Treated as missing: lines 1, 7, 10, 17, 21, 32 at temperature_average_min(℃)
#> Treated as missing: lines 1, 3 at temperature_max(℃)
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48 at temperature_min(℃)
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44 at humidity_average(%)
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44 at humidity_min(%)
#> Treated as missing: lines 1, 32 at wind_average_speed(m/s)
#> Treated as missing: lines 1, 6, 32, 46 at wind_max_speed(m/s)
#> Treated as missing: lines 3, 7, 9, 13 at wind_max_speed_direction
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32 at wind_max_instantaneous_speed(m/s)
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 at wind_max_instantaneous_direction
#> Treated as missing: lines 1, 10, 32, 33, 44 at daylight_(h)
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48 at snow_fall(cm)
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48 at snow_max_fall_day(cm)
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48 at snow_depth(cm)
#> 
#> 
#> ── Column specification ────────────────────────────────────────────────────────
#> cols(
#>   .default = col_double(),
#>   wind_max_speed_direction = col_character(),
#>   wind_max_instantaneous_direction = col_character(),
#>   `snow_fall(cm)` = col_logical(),
#>   `snow_max_fall_day(cm)` = col_logical(),
#>   `snow_depth(cm)` = col_logical()
#> )
#> ℹ Use `spec()` for the full column specifications.
#> # A tibble: 48 × 7
#>     year precipitation$`sum(mm)` temperature$`average(℃)` humidity$`average(%)`
#>    <dbl>                   <dbl>                    <dbl>                 <dbl>
#>  1  1978                     107                      0.4                    NA
#>  2  1979                     687                      5.8                    NA
#>  3  1980                     675                      5.2                    NA
#>  4  1981                     904                      5.1                    NA
#>  5  1982                     711                      6                      NA
#>  6  1983                     799                      4.9                    NA
#>  7  1984                     686                      5.5                    NA
#>  8  1985                     913                      5.2                    NA
#>  9  1986                     622                      4.9                    NA
#> 10  1987                     917                      5.7                    NA
#> # ℹ 38 more rows
#> # ℹ 11 more variables: precipitation$`max_per_day(mm)` <dbl>,
#> #   $`max_1hour(mm)` <dbl>, $`max_10minutes(mm)` <dbl>,
#> #   temperature$`average_max(℃)` <dbl>, $`average_min(℃)` <dbl>,
#> #   $`max(℃)` <dbl>, $`min(℃)` <dbl>, humidity$`min(%)` <dbl>,
#> #   wind <tibble[,5]>, daylight <tibble[,1]>, snow <tibble[,3]>
# Daily
jma_collect(item = "daily", block_no = "0010", year = 2017, month = 11, cache = FALSE)
#> Retrying in 7 seconds.
#> Data from: https://www.data.jma.go.jp/stats/etrn/view/daily_a1.php?prec_no=12&block_no=0010&year=2017&month=11&day=&view=Treated as missing: lines 7 at precipitation_sum(mm)
#> Treated as missing: lines 7 at precipitation_max_1hour(mm)
#> Treated as missing: lines 7 at precipitation_max_10minutes(mm)
#> Treated as missing: lines 7, 16, 17, 18, 19, 20, 21, 24, 25, 26, 27, 30 at temperature_average(℃)
#> Treated as missing: lines 7, 19, 20, 21, 24, 25, 26, 27, 30 at temperature_max(℃)
#> Treated as missing: lines 2, 3, 4, 5, 7, 10, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 at temperature_min(℃)
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 at humidity_average(%)
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 at humidity_min(%)
#> Treated as missing: lines 4, 7, 15, 16, 23, 24 at wind_average_speed(m/s)
#> Treated as missing: lines 4, 7, 15, 16, 23, 24 at wind_max_speed(m/s)
#> Treated as missing: lines 4, 7, 15, 16, 23, 24 at wind_max_speed_direction(m/s)
#> Treated as missing: lines 4, 7, 15, 16, 23, 24 at wind_max_instantaneous_speed(m/s)
#> Treated as missing: lines 4, 7, 15, 16, 23, 24 at wind_max_instantaneous_direction(m/s)
#> Treated as missing: lines 4, 7, 15, 16, 23, 24 at wind_direction_frequency(m/s)
#> Treated as missing: lines 7 at sunshine_duration_(h)
#> Treated as missing: lines 7 at snow_fall(cm)
#> Treated as missing: lines 7 at snow_depth(cm)
#> 
#> 
#> ── Column specification ────────────────────────────────────────────────────────
#> cols(
#>   date = col_double(),
#>   `precipitation_sum(mm)` = col_double(),
#>   `precipitation_max_1hour(mm)` = col_double(),
#>   `precipitation_max_10minutes(mm)` = col_double(),
#>   `temperature_average(℃)` = col_double(),
#>   `temperature_max(℃)` = col_double(),
#>   `temperature_min(℃)` = col_double(),
#>   `humidity_average(%)` = col_logical(),
#>   `humidity_min(%)` = col_logical(),
#>   `wind_average_speed(m/s)` = col_double(),
#>   `wind_max_speed(m/s)` = col_double(),
#>   `wind_max_speed_direction(m/s)` = col_character(),
#>   `wind_max_instantaneous_speed(m/s)` = col_double(),
#>   `wind_max_instantaneous_direction(m/s)` = col_character(),
#>   `wind_direction_frequency(m/s)` = col_character(),
#>   `sunshine_duration_(h)` = col_double(),
#>   `snow_fall(cm)` = col_double(),
#>   `snow_depth(cm)` = col_double()
#> )
#> # A tibble: 30 × 7
#>    date       precipitation$`sum(mm)` $`max_1hour(mm)` temperature$`average(℃)`
#>    <date>                       <dbl>            <dbl>                    <dbl>
#>  1 2017-11-01                    14                5.5                      9.1
#>  2 2017-11-02                     1                2                        5.8
#>  3 2017-11-03                     3.5              1                        3.1
#>  4 2017-11-04                     7.5              2.5                      1  
#>  5 2017-11-05                     1.5              1                        2  
#>  6 2017-11-06                     0                0                       10  
#>  7 2017-11-07                     0                0                        8.7
#>  8 2017-11-08                     8                7                        9.5
#>  9 2017-11-09                     2                1.5                      3.3
#> 10 2017-11-10                     2.5              1                        2.9
#> # ℹ 20 more rows
#> # ℹ 7 more variables: precipitation$`max_10minutes(mm)` <dbl>,
#> #   temperature$`max(℃)` <dbl>, $`min(℃)` <dbl>, humidity <tibble[,2]>,
#> #   wind <tibble[,6]>, sunshine <tibble[,1]>, snow <tibble[,2]>
jma_collect(item = "daily", "0422", year = 2017, month = 11, cache = FALSE)
#> Retrying in 7 seconds.
#> Data from: https://www.data.jma.go.jp/stats/etrn/view/daily_a1.php?prec_no=48&block_no=0422&year=2017&month=11&day=&view=Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 at temperature_average(℃)
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 at temperature_max(℃)
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 at temperature_min(℃)
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 at humidity_average(%)
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 at humidity_min(%)
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 at wind_average_speed(m/s)
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 at wind_max_speed(m/s)
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 at wind_max_speed_direction(m/s)
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 at wind_max_instantaneous_speed(m/s)
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 at wind_max_instantaneous_direction(m/s)
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 at wind_direction_frequency(m/s)
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 at sunshine_duration_(h)
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 at snow_fall(cm)
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30 at snow_depth(cm)
#> 
#> 
#> ── Column specification ────────────────────────────────────────────────────────
#> cols(
#>   date = col_double(),
#>   `precipitation_sum(mm)` = col_double(),
#>   `precipitation_max_1hour(mm)` = col_double(),
#>   `precipitation_max_10minutes(mm)` = col_double(),
#>   `temperature_average(℃)` = col_logical(),
#>   `temperature_max(℃)` = col_logical(),
#>   `temperature_min(℃)` = col_logical(),
#>   `humidity_average(%)` = col_logical(),
#>   `humidity_min(%)` = col_logical(),
#>   `wind_average_speed(m/s)` = col_logical(),
#>   `wind_max_speed(m/s)` = col_logical(),
#>   `wind_max_speed_direction(m/s)` = col_logical(),
#>   `wind_max_instantaneous_speed(m/s)` = col_logical(),
#>   `wind_max_instantaneous_direction(m/s)` = col_logical(),
#>   `wind_direction_frequency(m/s)` = col_logical(),
#>   `sunshine_duration_(h)` = col_logical(),
#>   `snow_fall(cm)` = col_logical(),
#>   `snow_depth(cm)` = col_logical()
#> )
#> # A tibble: 30 × 7
#>    date       precipitation$`sum(mm)` $`max_1hour(mm)` temperature$`average(℃)`
#>    <date>                       <dbl>            <dbl> <lgl>                   
#>  1 2017-11-01                     0                0   NA                      
#>  2 2017-11-02                     0                0   NA                      
#>  3 2017-11-03                     0                0   NA                      
#>  4 2017-11-04                     0.5              0.5 NA                      
#>  5 2017-11-05                     0                0   NA                      
#>  6 2017-11-06                     0                0   NA                      
#>  7 2017-11-07                     0                0   NA                      
#>  8 2017-11-08                     4                1.5 NA                      
#>  9 2017-11-09                     0                0   NA                      
#> 10 2017-11-10                     0                0   NA                      
#> # ℹ 20 more rows
#> # ℹ 7 more variables: precipitation$`max_10minutes(mm)` <dbl>,
#> #   temperature$`max(℃)` <lgl>, $`min(℃)` <lgl>, humidity <tibble[,2]>,
#> #   wind <tibble[,6]>, sunshine <tibble[,1]>, snow <tibble[,2]>
# Hourly
jma_collect("hourly", "0010", 2018, 7, 30, cache = FALSE)
#> Retrying in 7 seconds.
#> Data from: https://www.data.jma.go.jp/stats/etrn/view/hourly_a1.php?prec_no=12&block_no=0010&year=2018&month=7&day=30&view=Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24 at dew_point(℃)
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24 at vapor(hPa)
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24 at humidity(%)
#> Treated as missing: lines 1, 2, 3, 4, 20, 21, 22, 23, 24 at daylight_(h)
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24 at snow_fall_moment(cm)
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24 at snow_fall_period(cm)
#> 
#> 
#> ── Column specification ────────────────────────────────────────────────────────
#> cols(
#>   time = col_double(),
#>   `precipitation(mm)` = col_double(),
#>   `temperature(℃)` = col_double(),
#>   `dew_point(℃)` = col_logical(),
#>   `vapor(hPa)` = col_logical(),
#>   `humidity(%)` = col_logical(),
#>   `wind_speed(m/s)` = col_double(),
#>   wind_direction = col_character(),
#>   `daylight_(h)` = col_double(),
#>   `snow_fall_moment(cm)` = col_logical(),
#>   `snow_fall_period(cm)` = col_logical()
#> )
#> # A tibble: 24 × 12
#>    date        time `precipitation(mm)` `temperature(℃)` `dew_point(℃)`
#>    <date>     <dbl>               <dbl>            <dbl> <lgl>         
#>  1 2018-07-30     1                   0             22.4 NA            
#>  2 2018-07-30     2                   0             22.1 NA            
#>  3 2018-07-30     3                   0             21   NA            
#>  4 2018-07-30     4                   0             20.2 NA            
#>  5 2018-07-30     5                   0             20.4 NA            
#>  6 2018-07-30     6                   0             23.5 NA            
#>  7 2018-07-30     7                   0             27.3 NA            
#>  8 2018-07-30     8                   0             28.7 NA            
#>  9 2018-07-30     9                   0             30   NA            
#> 10 2018-07-30    10                   0             30.8 NA            
#> # ℹ 14 more rows
#> # ℹ 7 more variables: `vapor(hPa)` <lgl>, `humidity(%)` <lgl>,
#> #   `wind_speed(m/s)` <dbl>, wind_direction <chr>, `daylight_(h)` <dbl>,
#> #   `snow_fall_moment(cm)` <lgl>, `snow_fall_period(cm)` <lgl>
# Historical Ranking
jma_collect("rank", block_no = "47646", year = 2020, cache = FALSE)
#> Retrying in 7 seconds.
#> Data from: https://www.data.jma.go.jp/stats/etrn/view/rank_s.php?prec_no=40&block_no=47646&year=2020&month=&day=&view=
#> 
#> ── Column specification ────────────────────────────────────────────────────────
#> cols(
#>   element = col_character(),
#>   period = col_character(),
#>   rank = col_double(),
#>   value = col_character(),
#>   date = col_character()
#> )
#> # A tibble: 370 × 5
#>    element             period            rank value date      
#>    <chr>               <chr>            <dbl> <chr> <chr>     
#>  1 日最低海面気圧(hPa) 1921/1から1921/1     1 965.3 1922/8/24 
#>  2 日最低海面気圧(hPa) 1921/1から1921/1     2 966.6 2002/10/1 
#>  3 日最低海面気圧(hPa) 1921/1から1921/1     3 968.9 2017/10/23
#>  4 日最低海面気圧(hPa) 1921/1から1921/1     4 969.0 1928/10/8 
#>  5 日最低海面気圧(hPa) 1921/1から1921/1     5 969.3 1932/11/15
#>  6 日最低海面気圧(hPa) 1921/1から1921/1     6 970.0 2019/10/12
#>  7 日最低海面気圧(hPa) 1921/1から1921/1     7 970.2 1943/10/3 
#>  8 日最低海面気圧(hPa) 1921/1から1921/1     8 970.5 1936/10/3 
#>  9 日最低海面気圧(hPa) 1921/1から1921/1     9 971.2 1998/9/16 
#> 10 日最低海面気圧(hPa) 1921/1から1921/1    10 972.7 1994/2/21 
#> # ℹ 360 more rows
# Climatological normals
jma_collect("nml_ym", block_no = "47646", cache = FALSE, pack = FALSE)
#> Retrying in 7 seconds.
#> Data from: https://www.data.jma.go.jp/stats/etrn/view/nml_sfc_ym.php?prec_no=40&block_no=47646&year=&month=&view=
#> The record is based on the statistical period from 1991 to 2020 (30 years of data).
#> Treated as missing: lines 1, 2, 12 at temperature_min(℃)
#> Treated as missing: lines 5, 6, 7, 8, 9, 10 at snow_fall(cm)
#> Treated as missing: lines 5, 6, 7, 8, 9, 10 at snow_max_fall_day(cm)
#> Treated as missing: lines 4, 5, 6, 7, 8, 9, 10 at snow_depth(cm)
#> 
#> 
#> ── Column specification ────────────────────────────────────────────────────────
#> cols(
#>   .default = col_double(),
#>   element = col_character(),
#>   wind_most_frequent_direction = col_character(),
#>   `snow_fall(cm)` = col_character(),
#>   `snow_max_fall_day(cm)` = col_character(),
#>   `snow_depth(cm)` = col_character(),
#>   cloud_covering_mean = col_character(),
#>   condition_snow_days = col_character(),
#>   condition_fog_days = col_character(),
#>   condition_thunder_days = col_character()
#> )
#> ℹ Use `spec()` for the full column specifications.
#> # A tibble: 13 × 20
#>    element `atmosphere_land(hPa)` atmosphere_surface(hP…¹ precipitation_sum(mm…²
#>    <chr>                    <dbl>                   <dbl>                  <dbl>
#>  1 1月                      1012.                   1016.                   50.6
#>  2 2月                      1013.                   1016.                   47.1
#>  3 3月                      1012.                   1015.                   95.5
#>  4 4月                      1011.                   1014                   110. 
#>  5 5月                      1009.                   1012                   130. 
#>  6 6月                      1006                    1009.                  132. 
#>  7 7月                      1006.                   1009.                  135. 
#>  8 8月                      1007.                   1010.                  118. 
#>  9 9月                      1010                    1013.                  188. 
#> 10 10月                     1014.                   1017.                  194. 
#> 11 11月                     1015.                   1018.                   79.1
#> 12 12月                     1014.                   1017                    48.5
#> 13 年                       1011.                   1014.                 1326  
#> # ℹ abbreviated names: ¹​`atmosphere_surface(hPa)`, ²​`precipitation_sum(mm)`
#> # ℹ 16 more variables: `temperature_average(℃)` <dbl>,
#> #   `temperature_max(℃)` <dbl>, `temperature_min(℃)` <dbl>, `vapor(hPa)` <dbl>,
#> #   `relative_humidity(%)` <dbl>, `wind_average_speed(m/s)` <dbl>,
#> #   wind_most_frequent_direction <chr>, `daylight_(h)` <dbl>,
#> #   `solar_irradiance_average(MJ/m^2)` <dbl>, `snow_fall(cm)` <chr>,
#> #   `snow_max_fall_day(cm)` <chr>, `snow_depth(cm)` <chr>, …
jma_collect("nml_3m", "47646", cache = FALSE, pack = FALSE, quiet = TRUE)
#> Retrying in 7 seconds.
#> Data from: https://www.data.jma.go.jp/stats/etrn/view/nml_sfc_3m.php?prec_no=40&block_no=47646&year=&month=&view=
#> The record is based on the statistical period from 1991 to .2020 (30 years of data).
#> 
#> 
#> ── Column specification ────────────────────────────────────────────────────────
#> cols(
#>   element = col_character(),
#>   `precipitation(mm)` = col_double(),
#>   `temperature_average(℃)` = col_double(),
#>   `temperature_min_num_days_lt_0.0(℃)` = col_double(),
#>   `temperature_min_num_days_geq_35.0(℃)` = col_double(),
#>   `temperature_max_num_days_lt_0.0(℃)` = col_double(),
#>   `temperature_max_num_days_geq_25.0(℃)` = col_double(),
#>   `temperature_max_num_days_geq_30.0(℃)` = col_double(),
#>   `temperature_max_num_days_geq_35.0(℃)` = col_double(),
#>   `daylight_(h)` = col_double(),
#>   `snow_fall(cm)` = col_character(),
#>   `snow_depth(cm)` = col_character()
#> )
#> # A tibble: 12 × 12
#>    element    `precipitation(mm)` temperature_average(℃…¹ temperature_min_num_…²
#>    <chr>                    <dbl>                   <dbl>                  <dbl>
#>  1 11月～1月                 184.                     6.3                   48.1
#>  2 12月～2月                 147.                     4.2                   65.4
#>  3 1月～3月                  193.                     5                     57.1
#>  4 2月～4月                  252.                     8.2                   32.4
#>  5 3月～5月                  335                     12.7                   12  
#>  6 4月～6月                  371.                    17                      1.2
#>  7 5月～7月                  396.                    20.9                    0  
#>  8 6月～8月                  385.                    23.8                    0  
#>  9 7月～9月                  440.                    24.3                    0  
#> 10 8月～10月                 499.                    21.6                    0  
#> 11 9月～11月                 460.                    16.5                    3  
#> 12 10月～12月                321.                    10.8                   22  
#> # ℹ abbreviated names: ¹​`temperature_average(℃)`,
#> #   ²​`temperature_min_num_days_lt_0.0(℃)`
#> # ℹ 8 more variables: `temperature_min_num_days_geq_35.0(℃)` <dbl>,
#> #   `temperature_max_num_days_lt_0.0(℃)` <dbl>,
#> #   `temperature_max_num_days_geq_25.0(℃)` <dbl>,
#> #   `temperature_max_num_days_geq_30.0(℃)` <dbl>,
#> #   `temperature_max_num_days_geq_35.0(℃)` <dbl>, `daylight_(h)` <dbl>, …
jma_collect("nml_10d", "0228", cache = FALSE, pack = FALSE, quiet = TRUE)
#> Retrying in 7 seconds.
#> Data from: https://www.data.jma.go.jp/stats/etrn/view/nml_amd_10d.php?prec_no=33&block_no=0228&year=&month=&view=
#> The record is based on the statistical period from 1991 to 2020 (30 years of data).
#> 
#> 
#> ── Column specification ────────────────────────────────────────────────────────
#> cols(
#>   element = col_character(),
#>   element2 = col_character(),
#>   `precipitation(mm)` = col_double(),
#>   `temperature_average(℃)` = col_double(),
#>   `temperature_max(℃)` = col_double(),
#>   `temperature_min(℃)` = col_double(),
#>   `wind_average_speed(m/s)` = col_double(),
#>   `daylight_(h)` = col_double(),
#>   `snow_fall(cm)` = col_logical(),
#>   `snow_depth(cm)` = col_logical()
#> )
#> # A tibble: 36 × 9
#>    element `precipitation(mm)` `temperature_average(℃)` `temperature_max(℃)`
#>    <chr>                 <dbl>                    <dbl>                <dbl>
#>  1 1月上旬                19.4                     -1.5                  2.5
#>  2 1月中旬                12.3                     -2.4                  1.9
#>  3 1月下旬                14.8                     -2.3                  2  
#>  4 2月上旬                12                       -2.1                  2.3
#>  5 2月中旬                16.7                     -1.4                  3.1
#>  6 2月下旬                11.1                     -0.3                  4.7
#>  7 3月上旬                26.9                      0.8                  5.7
#>  8 3月中旬                22.5                      2.3                  7.8
#>  9 3月下旬                27.6                      3.7                  9.5
#> 10 4月上旬                24.2                      6.3                 12.3
#> # ℹ 26 more rows
#> # ℹ 5 more variables: `temperature_min(℃)` <dbl>,
#> #   `wind_average_speed(m/s)` <dbl>, `daylight_(h)` <dbl>,
#> #   `snow_fall(cm)` <lgl>, `snow_depth(cm)` <lgl>
jma_collect("nml_mb5d", "0228", cache = FALSE, pack = FALSE, quiet = FALSE)
#> Retrying in 7 seconds.
#> Data from: https://www.data.jma.go.jp/stats/etrn/view/nml_amd_mb5d.php?prec_no=33&block_no=0228&year=&month=&view=
#> The record is based on the statistical period from 1991 to 2020 (30 years of data).
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 72, 73 at temperature_average(℃)
#> Treated as missing: lines 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 66, 67, 68, 69, 70, 71, 72, 73 at temperature_min(℃)
#> 
#> 
#> ── Column specification ────────────────────────────────────────────────────────
#> cols(
#>   element = col_character(),
#>   element2 = col_character(),
#>   element3 = col_character(),
#>   `precipitation(mm)` = col_double(),
#>   `temperature_average(℃)` = col_double(),
#>   `temperature_max(℃)` = col_double(),
#>   `temperature_min(℃)` = col_double(),
#>   `daylight_(h)` = col_double()
#> )
#> # A tibble: 73 × 6
#>    element       `precipitation(mm)` temperature_average(…¹ `temperature_max(℃)`
#>    <chr>                       <dbl>                  <dbl>                <dbl>
#>  1 1月第1半旬1～5日……                 9.8                   -1.4                  2.6
#>  2 1月第2半旬6～10日……                 8.6                   -1.8                  2.2
#>  3 1月第3半旬11～15日…                 7.4                   -2.2                  2  
#>  4 1月第4半旬16～20日…                 6.6                   -2.3                  2  
#>  5 1月第5半旬21～25日…                 6.5                   -2.3                  2.1
#>  6 1月第6半旬26～31日…                 7.8                   -2.3                  2.1
#>  7 2月第1半旬1～5日……                 6.5                   -2.2                  2.2
#>  8 2月第2半旬6～10日……                 6.6                   -1.9                  2.5
#>  9 2月第3半旬11～15日…                 7.6                   -1.5                  2.9
#> 10 2月第4半旬16～20日…                 8                     -1.1                  3.5
#> # ℹ 63 more rows
#> # ℹ abbreviated name: ¹​`temperature_average(℃)`
#> # ℹ 2 more variables: `temperature_min(℃)` <dbl>, `daylight_(h)` <dbl>
# }
```
