
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jmastats

<!-- badges: start -->

<!-- badges: end -->

jmastats
は[気象庁](https://www.jma.go.jp/jma/index.html)のウェブサイトで公開される気象、海洋等の各種データをR上で扱えるようにするパッケージです。

## インストール

パッケージはCRANに登録されていません。GitLabを経由してインストールを行ってください。

``` r
install.packages("remotes")
remotes::install_gitlab("uribo/jmastats")
```

## 使い方

``` r
library(jmastats)
```

### 地点気象データ

取得したい対象データを`item`引数に指定、観測所の位置する `block_no` と対象の年 `year` 月 `month` 日
`day`を必要に応じて与えて実行します。

``` r
jma_collect(item = "hourly", block_no = 47646, year = 2020, month = 1, day = 1)
```

`block_no` が不明の時は 対象地点の緯度経度を元に`nearest_station()`で検索できます。

``` r
nearest_station(longitude = 140.112, latitude = 36.083)
```

### 台風資料

``` r
read_rsmc_besttrack()
```

### 潮汐観測資料

``` r
read_tide_level("https://www.data.jma.go.jp/gmd/kaiyou/data/db/tide/suisan/txt/2020/TK.txt")

read_tide_level(.year = 2020, .month = 2, .stn = "TK")
```

### データセット

**気象観測地点**

``` r
data("stations", package = "jmastats")
```

`station_type` により観測種目が異なります。

| 略字 | 観測装置の種類   | 観測種目                                          |
| -- | :-------- | :-------------------------------------------- |
| 四  | 有線ロボット気象計 | 降水量、気温、風向、風速、日照時間                             |
| 三  | 有線ロボット気象計 | 降水量、気温、風向、風速                                  |
| 官  | 地上気象観測装置  | 降水量、気温、風向、風速、日照時間（一部の観測所を除く）、積雪の深さ（一部の観測所を除く） |
| 雨  | 有線ロボット気象計 | 降水量                                           |
| 雪  | 有線ロボット積雪計 | 積雪量                                           |
