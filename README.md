
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

  - 地点気象データ
  - 台風資料
  - 潮汐観測資料

### 気象観測地点

``` r
data("stations", package = "jmastats")
```
