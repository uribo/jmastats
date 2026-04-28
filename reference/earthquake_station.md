# Japan Meteorological Agency's earthquake observe stations

This data corresponds to the July 18, 2024 update.

## Usage

``` r
earthquake_station
```

## Format

A simple feature data frame with 671 rows 7 variables

## Examples

``` r
head(earthquake_station)
#> Simple feature collection with 6 features and 6 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 141.315 ymin: 42.78333 xmax: 141.6783 ymax: 43.275
#> Geodetic CRS:  WGS 84
#> # A tibble: 6 × 7
#>   prefecture area         station_name address observation_begin observation_end
#>   <chr>      <chr>        <chr>        <chr>   <chr>             <chr>          
#> 1 北海道     石狩地方北部 石狩市花川   石狩市花川北… 199604011200      NA             
#> 2 北海道     石狩地方北部 石狩市聚富   石狩市厚田区… 201210021200      NA             
#> 3 北海道     石狩地方中部 札幌中央区北２条…… 札幌市中央区… 1876              NA             
#> 4 北海道     石狩地方中部 江別市高砂町 江別市高砂町… 199604011200      NA             
#> 5 北海道     石狩地方南部 千歳市北栄   千歳市北栄1… 199604011200      NA             
#> 6 北海道     石狩地方南部 新千歳空港   千歳市美々新… 200707021200      NA             
#> # ℹ 1 more variable: geometry <POINT [°]>
```
