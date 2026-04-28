# Read the past weather

**\[experimental\]**

Read the past weather data files downloaded from JMA.

## Usage

``` r
read_jma_weather(path)
```

## Arguments

- path:

  The path to the downloaded file.

## Value

a `tbl` object

## See also

<https://www.data.jma.go.jp/gmd/risk/obsdl/index.php>,
<https://www.data.jma.go.jp/gmd/risk/obsdl/top/help3.html>

## Examples

``` r
read_jma_weather(system.file("dummy/dl_data.csv", package = "jmastats"))
#> Selected station: 徳島 
#> 
#> ── Column specification ────────────────────────────────────────────────────────
#> cols(
#>   date = col_character(),
#>   `徳島_平均気温(℃)` = col_double(),
#>   `徳島_平均気温(℃)品質情報_` = col_double(),
#>   `徳島_平均気温(℃)均質番号_` = col_double(),
#>   `徳島_降水量の合計(mm)` = col_double(),
#>   `徳島_降水量の合計(mm)現象なし情報_` = col_double(),
#>   `徳島_降水量の合計(mm)品質情報_` = col_double(),
#>   `徳島_降水量の合計(mm)均質番号_` = col_double()
#> )
#> # A tibble: 1 × 8
#>   date       `徳島_平均気温(℃)` 徳島_平均気温(℃)品質情報_…¹… 徳島_平均気温(℃)均質番号_…²…
#>   <date>                  <dbl>                     <dbl>                  <dbl>
#> 1 2023-08-05               29.1                         8                      1
#> # ℹ abbreviated names: ¹​`徳島_平均気温(℃)品質情報_`,
#> #   ²​`徳島_平均気温(℃)均質番号_`
#> # ℹ 4 more variables: `徳島_降水量の合計(mm)` <dbl>,
#> #   `徳島_降水量の合計(mm)現象なし情報_` <dbl>,
#> #   `徳島_降水量の合計(mm)品質情報_` <dbl>,
#> #   `徳島_降水量の合計(mm)均質番号_` <dbl>
```
