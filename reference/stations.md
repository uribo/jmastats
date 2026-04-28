# Japan Meteorological Agency's Stations list

This data corresponds to the April 1, 2024 update.

## Usage

``` r
stations
```

## Format

A data frame with 1323 rows 14 variables:

- area

- station_no

- station_type

- station_name

- address

- elevation

- observation_begin

- note1

- note1

- note2

- katakana

- prec_no

- block_no

- pref_code

- geometry

## Examples

``` r
head(stations)
#> Simple feature collection with 6 features and 13 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 141.045 ymin: 45.24167 xmax: 142.17 ymax: 45.52
#> Geodetic CRS:  WGS 84
#> # A tibble: 6 × 14
#>   area  station_no station_type station_name address elevation observation_begin
#>   <chr>      <int> <chr>        <chr>        <chr>       <int> <chr>            
#> 1 宗谷       11001 四           宗谷岬       稚内市宗谷岬…        26 昭53.10.30       
#> 2 宗谷       11016 官           稚内         稚内市開運　…         3 #昭50.4.1        
#> 3 宗谷       11046 四           礼文         礼文郡礼文町…        65 平15.10.17       
#> 4 宗谷       11061 官           声問         稚内市大字声…         8 平15.1.1         
#> 5 宗谷       11076 四           浜鬼志別     宗谷郡猿払村…        13 昭53.10.30       
#> 6 宗谷       11091 官           本泊         利尻郡利尻富…        30 平15.1.1         
#> # ℹ 7 more variables: note1 <chr>, note2 <chr>, katakana <chr>, prec_no <chr>,
#> #   block_no <chr>, pref_code <chr>, geometry <POINT [°]>
dim(stations)
#> [1] 1323   14
```
