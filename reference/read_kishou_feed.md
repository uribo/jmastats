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
#> # A tibble: 228 × 6
#>    title                      id        updated             author content link 
#>    <chr>                      <chr>     <dttm>              <chr>  <chr>   <chr>
#>  1 府県天気予報（Ｒ１）       https://… 2026-09-18 19:35:17 新潟地方気… 【新潟県府県… http…
#>  2 警報級の可能性（明日まで） https://… 2026-09-18 19:35:17 新潟地方気… 【新潟県警報… http…
#>  3 警報級の可能性（明日まで） https://… 2026-09-18 19:35:16 釧路地方気… 【釧路・根室… http…
#>  4 府県天気予報（Ｒ１）       https://… 2026-09-18 19:35:16 釧路地方気… 【釧路・根室… http…
#>  5 府県天気予報（Ｒ１）       https://… 2026-09-18 19:35:02 函館地方気… 【渡島・檜山… http…
#>  6 早期注意情報（明後日まで） https://… 2026-09-18 19:35:02 函館地方気… 【渡島・檜山… http…
#>  7 警報級の可能性（明日まで） https://… 2026-09-18 19:35:02 函館地方気… 【渡島・檜山… http…
#>  8 府県天気概況               https://… 2026-09-18 19:34:52 松山地方気… 【天気概況】… http…
#>  9 早期注意情報（明後日まで） https://… 2026-09-18 19:34:51 鳥取地方気… 【鳥取県早期… http…
#> 10 警報級の可能性（明日まで） https://… 2026-09-18 19:34:51 鳥取地方気… 【鳥取県警報… http…
#> # ℹ 218 more rows
read_kishou_feed("low", "other")
#> # A tibble: 475 × 6
#>    title                          id    updated             author content link 
#>    <chr>                          <chr> <dttm>              <chr>  <chr>   <chr>
#>  1 地方海上警報（Ｈ２８）         http… 2026-09-18 17:35:00 名古屋地方… 【名古屋海上… http…
#>  2 全般海上警報（臨時）（Ｈ２９） http… 2026-09-18 17:16:50 気象庁 【全般海上警… http…
#>  3 地方海上警報（Ｈ２８）         http… 2026-09-18 14:35:05 福岡管区気… 【長崎海上気… http…
#>  4 地方海上警報（Ｈ２８）         http… 2026-09-18 14:35:05 鹿児島地方… 【鹿児島海上… http…
#>  5 地方海上警報（Ｈ２８）         http… 2026-09-18 14:35:04 名古屋地方… 【名古屋海上… http…
#>  6 地方海上警報（Ｈ２８）         http… 2026-09-18 14:35:04 福岡管区気… 【福岡海上気… http…
#>  7 地方海上警報（Ｈ２８）         http… 2026-09-18 14:35:04 高松地方気… 【神戸海上気… http…
#>  8 地方海上警報（Ｈ２８）         http… 2026-09-18 14:35:03 新潟地方気… 【新潟海上気… http…
#>  9 地方海上警報（Ｈ２８）         http… 2026-09-18 14:35:03 気象庁 【東京海上気… http…
#> 10 地方海上警報（Ｈ２８）         http… 2026-09-18 14:35:02 札幌管区気… 【函館海上気… http…
#> # ℹ 465 more rows
# }
```
