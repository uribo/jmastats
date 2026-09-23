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
#> # A tibble: 524 × 6
#>    title                          id    updated             author content link 
#>    <chr>                          <chr> <dttm>              <chr>  <chr>   <chr>
#>  1 気象警報・注意報（Ｒ０６）（集約通報）…… http… 2026-09-23 14:30:51 気象庁 【警戒・注意… http…
#>  2 気象警報・注意報（Ｒ０６）（集約通報）…… http… 2026-09-23 14:20:56 気象庁 【警戒・注意… http…
#>  3 地上実況図                     http… 2026-09-23 14:15:59 気象庁 【地上実況図… http…
#>  4 気象警報・注意報（Ｒ０６）（集約通報）…… http… 2026-09-23 14:10:51 気象庁 【警戒・注意… http…
#>  5 気象警報・注意報（Ｒ０６）（集約通報）…… http… 2026-09-23 14:01:26 気象庁 【警戒・注意… http…
#>  6 気象警報・注意報時系列情報（Ｒ０６）…… http… 2026-09-23 13:55:00 高松地方気… 【香川県警戒… http…
#>  7 気象警報・注意報時系列情報（Ｒ０６）…… http… 2026-09-23 13:55:00 高知地方気… 【高知県警戒… http…
#>  8 気象警報・注意報時系列情報（Ｒ０６）…… http… 2026-09-23 13:55:00 徳島地方気… 【徳島県警戒… http…
#>  9 気象警報・注意報時系列情報（Ｒ０６）…… http… 2026-09-23 13:55:00 名瀬測候所… 【奄美地方（… http…
#> 10 気象警報・注意報時系列情報（Ｒ０６）…… http… 2026-09-23 13:55:00 石垣島地方… 【八重山地方… http…
#> # ℹ 514 more rows
read_kishou_feed("low", "other")
#> # A tibble: 600 × 6
#>    title                          id    updated             author content link 
#>    <chr>                          <chr> <dttm>              <chr>  <chr>   <chr>
#>  1 地方海上警報（Ｈ２８）         http… 2026-09-23 14:35:02 大阪管区気… 【舞鶴海上気… http…
#>  2 地方海上警報（Ｈ２８）         http… 2026-09-23 14:35:01 札幌管区気… 【函館海上気… http…
#>  3 地方海上警報（Ｈ２８）         http… 2026-09-23 14:35:01 新潟地方気… 【新潟海上気… http…
#>  4 地方海上警報（Ｈ２８）         http… 2026-09-23 14:35:00 札幌管区気… 【札幌海上気… http…
#>  5 全般海上警報（定時）（Ｒ０８） http… 2026-09-23 14:32:14 気象庁 【全般海上警… http…
#>  6 全般海上警報（定時）（Ｈ２９） http… 2026-09-23 14:32:14 気象庁 【全般海上警… http…
#>  7 地方海上予報（Ｈ２８）         http… 2026-09-23 09:10:06 沖縄気象台… 【沖縄海上気… http…
#>  8 地方海上予報（Ｈ２８）         http… 2026-09-23 09:10:05 福岡管区気… 【長崎海上気… http…
#>  9 地方海上予報（Ｈ２８）         http… 2026-09-23 09:10:05 福岡管区気… 【福岡海上気… http…
#> 10 地方海上予報（Ｈ２８）         http… 2026-09-23 09:10:05 鹿児島地方… 【鹿児島海上… http…
#> # ℹ 590 more rows
# }
```
