
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jmastats <img src="man/figures/logo.png" align="right" width="120px" />

<!-- badges: start -->
<!-- badges: end -->

jmastats
は[気象庁](https://www.jma.go.jp/jma/index.html)のウェブサイトで公開される気象、海洋等の各種データをR上で扱えるようにするパッケージです。

## インストール

パッケージはCRANに登録されていません。次のコマンドを実行することでインストールが行われます。

``` r
install.packages(
   "jmastats", 
   repos = c(mm = "https://uribo.r-universe.dev", getOption("repos")))
```

## 特徴

- 過去の気象データのほか、気象庁が公開するさまざまな気象ファイルをRで扱いやすい形式で読み込みます。
  - 地理空間情報が利用できる場合、適切な種類（ポイント、ライン等）のsfオブジェクトに変換します。
  - `parse_unit()`関数により、気象データの単位系（SI単位系）をunitsオブジェクトに変換します。
- 取得した気象データはコンピュータ内にキャッシュとして保存されます。そのため、一度取得したデータはインターネット接続ができない状態でも参照可能です。キャッシュの利用により、取得時の気象庁ウェブサイトへの負荷も軽減されます。

## 使い方

``` r
library(jmastats)
```

### 地点気象データ

取得したい対象データを`item`引数に指定、観測所の位置する `block_no`
と対象の年 `year` 月 `month` 日 `day`を必要に応じて与えて実行します。

``` r
jma_collect(item = "hourly", block_no = 47646, year = 2022, month = 1, day = 1)
```

`block_no` が不明の時は
対象地点の緯度経度を元に`nearest_station()`で検索できます。

``` r
nearest_station(longitude = 140.112, latitude = 36.083)
```

### 台風資料

気象庁のウェブサイト、[RMCS
Tokyo](https://www.jma.go.jp/jma/jma-eng/jma-center/rsmc-hp-pub-eg/trackarchives.html)が提供する台風のベストトラックデータを読み込むための関数があります。

`read_rsmc_besttrack()`関数でベストトラックのファイルが置かれたパスを指定します。

``` r
read_rsmc_besttrack(path = system.file("dummy/bst.txt", package = "jmastats")) |> 
  dplyr::glimpse()
#> Rows: 2
#> Columns: 22
#> $ datetime                                                    <dttm> 1991-09-2…
#> $ indicator_002                                               <chr> "002", "00…
#> $ grade                                                       <chr> "5", "5"
#> $ `central_pressure(hPa)`                                     <dbl> 935, 994
#> $ `maximum_sustained_wind_speed(knot)`                        <dbl> 95, NA
#> $ `_direction_of_the_longest_radius_of_50kt_winds_or_greater` <chr> "3", NA
#> $ `_the_longest_radius_of_50kt_winds_or_greater(nm)`          <chr> "0180", NA
#> $ `_the_shortest_radius_of_50kt_winds_or_greater(nm)`         <chr> "0140", NA
#> $ `_direction_of_the_longest_radius_of_30kt_winds_or_greater` <chr> "3", NA
#> $ `_the_longest_radius_of_30kt_winds_or_greater(nm)`          <chr> "0400", NA
#> $ `_the_shortest_radius_of_30kt_winds_or_greater(nm)`         <chr> "0260", NA
#> $ indicator_of_landfall_or_passage                            <chr> "#", NA
#> $ international_number                                        <chr> "9119", "9…
#> $ geometry                                                    <POINT [°]> POINT (129…
#> $ indicator_66666                                             <dbl> 66666, 666…
#> $ nrow                                                        <dbl> 1, 1
#> $ tropical_cyclone_number                                     <chr> "0045", "0…
#> $ international_number_copy                                   <chr> "9119", "9…
#> $ flag_last_data_line                                         <chr> "0", "0"
#> $ DTM                                                         <dbl> 6, 6
#> $ storm_name                                                  <fct> MIRREILE, …
#> $ last_update                                                 <date> 1992-07-01…
```

URLを直接指定した読み込みも可能です。

``` r
read_rsmc_besttrack(path = "https://www.jma.go.jp/jma/jma-eng/jma-center/rsmc-hp-pub-eg/Besttracks/bst2023.txt")
```

`read_rsmc_besttrack()`関数の返り値は台風の経路を記録したポイントデータ(sf)となっています。`track_combine()`関数では、`read_rsmc_besttrack()`関数で読み込んだベストトラックのデータについて、台風ごとの経路をラインデータとしてまとめます。

``` r
read_rsmc_besttrack(path = system.file("dummy/bst.txt", package = "jmastats")) |> 
  track_combine(group_vars = "storm_name")
```

### 潮汐観測資料

``` r
read_tide_level("https://www.data.jma.go.jp/gmd/kaiyou/data/db/tide/suisan/txt/2020/TK.txt")

read_tide_level(.year = 2020, .month = 2, .stn = "TK")
```

### 震度データベース

[震度データベース検索](https://www.data.jma.go.jp/svd/eqdb/data/shindo/index.html)よりダウンロードしたcscvファイルを読み込む関数として`read_eqdb_csv()`があります。ダミーデータを読み込む例を示します。

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

### データセット

**気象観測地点**

``` r
data("stations", package = "jmastats")
```

`station_type` により観測種目が異なります。

| 略字 | 観測装置の種類     | 観測種目                                                                                   |
|------|:-------------------|:-------------------------------------------------------------------------------------------|
| 四   | 有線ロボット気象計 | 降水量、気温、風向、風速、日照時間                                                         |
| 三   | 有線ロボット気象計 | 降水量、気温、風向、風速                                                                   |
| 官   | 地上気象観測装置   | 降水量、気温、風向、風速、日照時間（一部の観測所を除く）、積雪の深さ（一部の観測所を除く） |
| 雨   | 有線ロボット気象計 | 降水量                                                                                     |
| 雪   | 有線ロボット積雪計 | 積雪量                                                                                     |

## 引用

このパッケージを利用した学術論文の出版、学会発表等を行う際は次のように引用を行ってください。

> Uryu S (2023). *jmastats: Download Weather Data from Japan
> Meteorological Agency Website*. R package version 0.2.0,
> <https://github.com/uribo/jmastats>.

## 関連するパッケージ

- [worldmet](https://cran.r-project.org/package=worldmet)
- [rnoaa](https://cran.r-project.org/package=rnoaa)
- [GSODR](https://cran.r-project.org/package=GSODR)
- [stationaRy](https://cran.r-project.org/package=stationaRy)
- [rdwd](https://cran.r-project.org/package=rdwd)
- [weathercan](https://github.com/ropensci/weathercan)
- [aemet](https://github.com/SevillaR/aemet)
- [climate](https://github.com/bczernecki/climate)
- [rrricanes](https://github.com/ropensci/rrricanes)
