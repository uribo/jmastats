
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jmastats <img src="man/figures/logo.png" align="right" width="120px" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/jmastats)](https://CRAN.R-project.org/package=jmastats)
[![CRANlogs
downloads](https://cranlogs.r-pkg.org/badges/grand-total/jmastats)](https://cran.r-project.org/package=jmastats)
[![DOI](https://zenodo.org/badge/515892382.svg)](https://zenodo.org/badge/latestdoi/515892382)
<!-- badges: end -->

jmastats
は[気象庁](https://www.jma.go.jp/jma/index.html)のウェブサイトで公開される気象、地震、海洋等の各種データをR上で扱うためのパッケージです。

## インストール

CRANからインストールが可能です。

``` r
install.packages("jmastats")
```

開発版を利用したい場合は次のコマンドを実行することでインストールが行われます。

``` r
install.packages(
   "jmastats", 
   repos = c(uribo = "https://uribo.r-universe.dev", getOption("repos")))
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
# データの種類: hourly (１時間ごとの値)
# ブロック番号: 47646 （「つくば」）
# 対象年月日: 2022年1月1日
jma_collect(item = "hourly", block_no = 47646, year = 2022, month = 1, day = 1)
```

`block_no` が不明の時は
対象地点の緯度経度を元に`nearest_station()`関数や後述の気象観測地点データから検索できます。

``` r
# 任意の緯度経度座標から最寄りの観測所と座標との距離を出力
nearest_station(longitude = 140.112, latitude = 36.083)
```

ユーザーが任意の地点、項目、期間等の組みあわせで出力可能な、`過去の気象データ`のcsvファイルを読み込む関数として`read_jma_weather()`関数が利用できます。

``` r
# ダウンロードしたcsvファイルのパスを与えて実行します
read_jma_weather(system.file("dummy/dl_data.csv", package = "jmastats"))
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

### 気象庁防災情報XMLフォーマット

``` r
read_kishou_feed("high", type = "regular")

read_kishou_feed("low", "other")
```

### 潮汐観測資料

``` r
# URLを指定しての読み込み
read_tide_level("https://www.data.jma.go.jp/gmd/kaiyou/data/db/tide/suisan/txt/2020/TK.txt")
# URLを構成するパラメータを指定した読み込み
read_tide_level(.year = 2020, .month = 2, .stn = "TK")
```

``` r
# ローカルに保存したファイルの読み込み（パスを指定）
read_tide_level(system.file("dummy/tide.txt", package = "jmastats"))
#> New names:
#> New names:
#> New names:
#> New names:
#> • `hm` -> `hm...1`
#> • `hm` -> `hm...2`
#> • `hm` -> `hm...3`
#> • `hm` -> `hm...4`
#> # A tibble: 1 × 42
#>   hry_00 hry_01 hry_02 hry_03 hry_04 hry_05 hry_06 hry_07 hry_08 hry_09 hry_10
#>     [cm]   [cm]   [cm]   [cm]   [cm]   [cm]   [cm]   [cm]   [cm]   [cm]   [cm]
#> 1    128    127    122    115    107    102    101    106    117    132    146
#> # ℹ 31 more variables: hry_11 [cm], hry_12 [cm], hry_13 [cm], hry_14 [cm],
#> #   hry_15 [cm], hry_16 [cm], hry_17 [cm], hry_18 [cm], hry_19 [cm],
#> #   hry_20 [cm], hry_21 [cm], hry_22 [cm], hry_23 [cm], date <date>, stn <chr>,
#> #   low_tide_hm_obs1 <time>, low_tide_level_obs1 [cm],
#> #   high_tide_hm_obs1 <time>, high_tide_level_obs1 [cm],
#> #   low_tide_hm_obs2 <time>, low_tide_level_obs2 [cm],
#> #   high_tide_hm_obs2 <time>, high_tide_level_obs2 [cm], …
```

### 震度データベース

[震度データベース検索](https://www.data.jma.go.jp/svd/eqdb/data/shindo/index.html)よりダウンロードしたcsvファイルを読み込む関数として`read_eqdb_csv()`があります。ダミーデータを読み込む例を示します。

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

#### 気象観測地点

気象データを取得するための`block_no`はここからも調べることができます。
観測地点の配置された標高や観測種目のほか、観測地点の位置情報が含まれます。

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

#### 潮位観測地点

潮位観測を行う地点についてのデータです。
stn変数の情報をもとにして、`read_tide_level()`での潮位観測記録の取得が行えます。

``` r
data("tide_station", package = "jmastats")
```

#### 震度観測点

地震・津波を観測するためのポイントを記録したデータです。震度観測点は気象庁のほかに各地方公共団体や国立研究開発法人防災科学技術研究所の観測地点が設置されていますが、このデータには気象庁が管理する地点だけが含まれます。

``` r
data("earthquake_station", package = "jmastats")
```

## 引用

このパッケージを利用した学術論文の出版、学会発表等を行う際は次のように引用を行ってください。

    Uryu S (2024). _jmastats: Download Weather Data from Japan Meteorological
    Agency Website_. R package version 0.2.1,
    <https://CRAN.R-project.org/package=jmastats>.

または

    @Manual{,
      title = {jmastats: Download Weather Data from Japan Meteorological Agency Website},
      author = {Shinya Uryu},
      year = {2024},
      note = {R package version 0.2.1},
      url = {https://CRAN.R-project.org/package=jmastats},
    }

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
