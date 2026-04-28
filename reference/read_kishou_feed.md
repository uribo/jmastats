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
#> # A tibble: 523 × 6
#>    title                        id      updated             author content link 
#>    <chr>                        <chr>   <dttm>              <chr>  <chr>   <chr>
#>  1 府県天気予報（Ｒ１）         https:… 2026-04-28 02:21:30 大阪管区気… 【大阪府府県… http…
#>  2 府県天気予報（Ｒ１）         https:… 2026-04-28 02:19:47 和歌山地方… 【和歌山県府… http…
#>  3 大雨危険度通知               https:… 2026-04-28 02:17:57 気象庁 【大雨危険度… http…
#>  4 大雨危険度通知               https:… 2026-04-28 02:07:55 気象庁 【大雨危険度… http…
#>  5 地上実況図                   https:… 2026-04-28 02:06:27 気象庁 【地上実況図… http…
#>  6 大雨危険度通知               https:… 2026-04-28 01:57:47 気象庁 【大雨危険度… http…
#>  7 大雨危険度通知               https:… 2026-04-28 01:47:50 気象庁 【大雨危険度… http…
#>  8 警報級の可能性（明後日以降） https:… 2026-04-28 01:43:15 福島地方気… 【福島県警報… http…
#>  9 警報級の可能性（明後日以降） https:… 2026-04-28 01:43:15 新潟地方気… 【新潟県警報… http…
#> 10 警報級の可能性（明後日以降） https:… 2026-04-28 01:43:15 富山地方気… 【富山県警報… http…
#> # ℹ 513 more rows
read_kishou_feed("low", "other")
#> # A tibble: 526 × 6
#>    title                  id            updated             author content link 
#>    <chr>                  <chr>         <dttm>              <chr>  <chr>   <chr>
#>  1 地方海上予報（Ｈ２８） https://www.… 2026-04-27 21:10:06 沖縄気象台… 【沖縄海上気… http…
#>  2 地方海上予報（Ｈ２８） https://www.… 2026-04-27 21:10:05 福岡管区気… 【長崎海上気… http…
#>  3 地方海上予報（Ｈ２８） https://www.… 2026-04-27 21:10:05 福岡管区気… 【福岡海上気… http…
#>  4 地方海上予報（Ｈ２８） https://www.… 2026-04-27 21:10:05 鹿児島地方… 【鹿児島海上… http…
#>  5 地方海上予報（Ｈ２８） https://www.… 2026-04-27 21:10:04 大阪管区気… 【舞鶴海上気… http…
#>  6 地方海上予報（Ｈ２８） https://www.… 2026-04-27 21:10:04 高松地方気… 【神戸海上気… http…
#>  7 地方海上予報（Ｈ２８） https://www.… 2026-04-27 21:10:03 気象庁 【東京海上気… http…
#>  8 地方海上予報（Ｈ２８） https://www.… 2026-04-27 21:10:03 名古屋地方… 【名古屋海上… http…
#>  9 地方海上予報（Ｈ２８） https://www.… 2026-04-27 21:10:03 新潟地方気… 【新潟海上気… http…
#> 10 地方海上予報（Ｈ２８） https://www.… 2026-04-27 21:10:02 札幌管区気… 【函館海上気… http…
#> # ℹ 516 more rows
# }
```
