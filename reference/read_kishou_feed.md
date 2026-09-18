# Read Kishou Disaster Prevention Information Feed

**\[experimental\]**

## Usage

``` r
read_kishou_feed(frequency, type)
```

## Arguments

- frequency:

  Release frequency. Select either high frequency ("high") or long term
  ("low")

- type:

  Feed type. Specify the item to be retrieved as a string. See details
  for the items.

## Value

a `tbl` object

## Details

The following items can be specified in the type argument.

- regular: It will be announced on time.

- extra: It will be announced at any time.

- eqvol: Earthquakes and Volcanoes.

- other: Other informations.

## See also

<https://xml.kishou.go.jp>

## Examples

``` r
# \donttest{
read_kishou_feed("high", type = "regular")
#> Warning: `update_list()` was deprecated in purrr 1.0.0.
#> ℹ The deprecated feature was likely used in the jmastats package.
#>   Please report the issue at <https://github.com/uribo/jmastats/issues>.
#> # A tibble: 757 × 6
#>    title                          id    updated             author content link 
#>    <chr>                          <chr> <dttm>              <chr>  <chr>   <chr>
#>  1 気象警報・注意報（Ｒ０６）（集約通報）…… http… 2026-09-18 03:10:56 気象庁 【警戒・注意… http…
#>  2 気象警報・注意報（Ｒ０６）（集約通報）…… http… 2026-09-18 03:01:20 気象庁 【警戒・注意… http…
#>  3 気象警報・注意報（Ｒ０６）（集約通報）…… http… 2026-09-18 02:50:52 気象庁 【警戒・注意… http…
#>  4 気象警報・注意報時系列情報（Ｒ０６）…… http… 2026-09-18 02:45:28 宮崎地方気… 【宮崎県警戒… http…
#>  5 気象警報・注意報（Ｒ０６）（集約通報）…… http… 2026-09-18 02:40:58 気象庁 【警戒・注意… http…
#>  6 府県天気予報（Ｒ１）           http… 2026-09-18 02:33:44 岡山地方気… 【岡山県府県… http…
#>  7 警報級の可能性（明日まで）     http… 2026-09-18 02:33:44 岡山地方気… 【岡山県警報… http…
#>  8 早期注意情報（明後日まで）     http… 2026-09-18 02:33:44 岡山地方気… 【岡山県早期… http…
#>  9 気象警報・注意報（Ｒ０６）（集約通報）…… http… 2026-09-18 02:30:56 気象庁 【警戒・注意… http…
#> 10 アジア太平洋地上実況図         http… 2026-09-18 02:30:16 気象庁 【アジア太平… http…
#> # ℹ 747 more rows
read_kishou_feed("low", "other")
#> # A tibble: 448 × 6
#>    title                    id          updated             author content link 
#>    <chr>                    <chr>       <dttm>              <chr>  <chr>   <chr>
#>  1 府県気象解説情報（潮位） https://ww… 2026-09-18 03:01:24 富山地方気… 【富山県気象… http…
#>  2 府県気象解説情報（潮位） https://ww… 2026-09-18 03:01:24 金沢地方気… 【石川県気象… http…
#>  3 府県気象解説情報（潮位） https://ww… 2026-09-18 03:01:24 静岡地方気… 【静岡県気象… http…
#>  4 府県気象解説情報（潮位） https://ww… 2026-09-18 03:01:24 津地方気象… 【三重県気象… http…
#>  5 府県気象解説情報（潮位） https://ww… 2026-09-18 03:01:24 福井地方気… 【福井県気象… http…
#>  6 府県気象解説情報（潮位） https://ww… 2026-09-18 03:01:24 神戸地方気… 【兵庫県気象… http…
#>  7 府県気象解説情報（潮位） https://ww… 2026-09-18 03:01:24 岡山地方気… 【岡山県気象… http…
#>  8 府県潮位情報             https://ww… 2026-09-18 03:01:24 松江地方気… 【島根県気象… http…
#>  9 府県潮位情報             https://ww… 2026-09-18 03:01:24 高松地方気… 【香川県気象… http…
#> 10 府県潮位情報             https://ww… 2026-09-18 03:01:24 高知地方気… 【高知県気象… http…
#> # ℹ 438 more rows
# }
```
