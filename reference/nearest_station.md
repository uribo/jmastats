# Find out neighborhood stations

**\[stable\]**

Return the nearest
[stations](https://uribo.github.io/jmastats/reference/stations.md)
information to the given coordinates.

## Usage

``` r
nearest_station(longitude, latitude, geometry = NULL)

pick_neighbor_stations(
  longitude,
  latitude,
  distance = 1,
  .unit = "m",
  geometry = NULL
)

pick_neighbor_tide_stations(
  year,
  longitude,
  latitude,
  distance = 100,
  .unit = "km",
  geometry = NULL
)
```

## Arguments

- longitude:

  Longitude.

- latitude:

  Latitude.

- geometry:

  XY [sf::sf](https://r-spatial.github.io/sf/reference/sf.html) object.

- distance:

  Distance from station to station to adjustment.

- .unit:

  Unit used for extraction from the point of interest. Default *m*
  (meters). This value is passed to
  [units::as_units](https://r-quantities.github.io/units/reference/units.html).

- year:

  For tide level data. Restricted to the observation points in the
  target year.

## Value

an object of class `sf`.

## Details

- `nearest_station()`: Return single station data.

- `pick_neighbor_stations()`: Pick-up neighbourhood stations.

- `pick_neighbor_tide_stations()`: Pick-up neighbourhood tidal
  observation stations. Filter by distance from target point.

## Examples

``` r
nearest_station(142.9313, 43.70417)
#> Simple feature collection with 1 feature and 5 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 142.93 ymin: 43.75333 xmax: 142.93 ymax: 43.75333
#> Geodetic CRS:  WGS 84
#> # A tibble: 1 × 6
#>   area  station_no station_name block_no distance          geometry
#>   <chr>      <int> <chr>        <chr>         [m]       <POINT [°]>
#> 1 上川       12471 層雲峡       1049        5468. (142.93 43.75333)

pick_neighbor_stations(140.10, 36.08, 300000)
#> Simple feature collection with 393 features and 5 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 136.8317 ymin: 34.07333 xmax: 141.465 ymax: 38.75667
#> Geodetic CRS:  WGS 84
#> # A tibble: 393 × 6
#>    area  station_no station_name block_no distance            geometry
#>    <chr>      <int> <chr>        <chr>         [m]         <POINT [°]>
#>  1 茨城       40336 つくば       47646       3432.  (140.125 36.05667)
#>  2 茨城       40341 土浦         0324       11090.   (140.22 36.10333)
#>  3 茨城       40281 下妻         0322       17038.  (139.945 36.16833)
#>  4 茨城       40241 柿岡         1012       18804. (140.1883 36.23333)
#>  5 茨城       40326 坂東         0323       19240.   (139.8933 36.035)
#>  6 茨城       40426 龍ケ崎       1014       23394.    (140.2117 35.89)
#>  7 茨城       40391 江戸崎       0911       23865.      (140.32 35.96)
#>  8 千葉       45061 我孫子       0376       24109.   (140.11 35.86333)
#>  9 茨城       40231 下館         1530       24562. (139.9883 36.28167)
#> 10 茨城       40251 美野里       1013       26674.  (140.325 36.23667)
#> # ℹ 383 more rows

d <-
  pick_neighbor_stations(140.10, 36.08, 30, "km")
pick_neighbor_stations(geometry = sf::st_point(c(140.1833, 36.23333)),
                       distance = 100)
#> Simple feature collection with 0 features and 5 fields
#> Geometry type: POINT
#> Bounding box:  xmin: NA ymin: NA xmax: NA ymax: NA
#> Geodetic CRS:  WGS 84
#> # A tibble: 0 × 6
#> # ℹ 6 variables: area <chr>, station_no <int>, station_name <chr>,
#> #   block_no <chr>, distance [m], geometry <POINT [°]>

pick_neighbor_tide_stations(longitude = 133.4375, latitude = 34.45833,
                            year = 2020,
                            distance = 100, .unit = "km")
#> Simple feature collection with 3 features and 7 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 132.7167 ymin: 33.86666 xmax: 134.05 ymax: 34.48333
#> Geodetic CRS:  WGS 84
#> # A tibble: 3 × 8
#>   year  id    stn   station_name address                  type   distance
#>   <chr> <chr> <chr> <chr>        <chr>                    <chr>       [m]
#> 1 2020  38    UN    宇野         岡山県 玉野市 宇野１丁目 電波式   47063.
#> 2 2020  40    TA    高松         香川県 高松市 北浜町     電波式   57470.
#> 3 2020  39    MT    松山         愛媛県 松山市 海岸通     電波式   93419.
#> # ℹ 1 more variable: geometry <POINT [°]>
```
