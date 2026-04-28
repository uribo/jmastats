# Read the csv of the earthquake database

**\[stable\]**

## Usage

``` r
read_eqdb_csv(path, show_metadata = TRUE)
```

## Arguments

- path:

  local file path to earthquake record file.

- show_metadata:

  logical. If *FALSE*, returns only the values observed at each
  location.

## Value

a `tbl` object

## See also

<https://www.data.jma.go.jp/svd/eqdb/data/shindo/index.html>

## Examples

``` r
read_eqdb_csv(system.file("dummy/eqdb.csv", package = "jmastats"))
#> 地震の概要
#> ✖ 発生日時: 2023-01-01 00:00:00
#> ✖ 震央地名: 震央地名ダミー
#> • 緯度: 35°30.9′N
#> • 経度: 140°53.9′E
#> ! 深さ: 37 km
#> ! マグニチュード: 4.6
#> ! 最大震度: 震度２
#> # A tibble: 2 × 4
#>   都道府県       震度  観測点名       気象庁の震度観測点
#>   <chr>          <chr> <chr>          <lgl>             
#> 1 震央地名ダミー 2     震央地名ダミー TRUE              
#> 2 震央地名ダミー 2     震央地名ダミー FALSE             
```
