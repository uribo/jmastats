# Tidal observation stations of Japan Meteorological Agency

Observation stations from 1997 to 2024. This data corresponds to the
January 1, 2024 update.

## Usage

``` r
tide_station
```

## Format

A data frame with 1949 rows 7 variables

## Examples

``` r
head(tide_station)
#> Simple feature collection with 6 features and 6 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 140.7333 ymin: 41.78333 xmax: 145.5667 ymax: 45.4
#> Geodetic CRS:  WGS 84
#> # A tibble: 6 × 7
#>   year  id    stn   station_name address         type             geometry
#>   <chr> <chr> <chr> <chr>        <chr>           <chr>         <POINT [°]>
#> 1 1997  1     WN    稚内         北海道 稚内市 新港町…… フロート…     (141.6833 45.4)
#> 2 1997  2     AS    網走         北海道 網走市 港町…… フロート… (144.2833 44.01667)
#> 3 1997  3     HN    花咲         北海道 根室市 花咲港…… フロート… (145.5667 43.28333)
#> 4 1997  4     KR    釧路         北海道 釧路市 港町…… フロート… (144.3833 42.96667)
#> 5 1997  5     HK    函館         北海道 函館市 海岸町…… フロート… (140.7333 41.78333)
#> 6 1997  6     B3    小樽         北海道 小樽市 色内３丁目…… 音波式……          (141 43.2)
```
