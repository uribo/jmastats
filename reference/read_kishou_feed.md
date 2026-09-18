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
#> # A tibble: 538 × 6
#>    title                          id    updated             author content link 
#>    <chr>                          <chr> <dttm>              <chr>  <chr>   <chr>
#>  1 気象警報・注意報（Ｒ０６）（集約通報）…… http… 2026-09-18 16:30:52 気象庁 【警戒・注意… http…
#>  2 気象警報・注意報（Ｒ０６）（集約通報）…… http… 2026-09-18 16:20:56 気象庁 【警戒・注意… http…
#>  3 気象警報・注意報（Ｒ０６）（集約通報）…… http… 2026-09-18 16:10:50 気象庁 【警戒・注意… http…
#>  4 気象警報・注意報（Ｒ０６）（集約通報）…… http… 2026-09-18 16:01:25 気象庁 【警戒・注意… http…
#>  5 気象警報・注意報（Ｒ０６）（集約通報）…… http… 2026-09-18 15:50:52 気象庁 【警戒・注意… http…
#>  6 気象警報・注意報（Ｒ０６）（集約通報）…… http… 2026-09-18 15:40:59 気象庁 【警戒・注意… http…
#>  7 気象警報・注意報（Ｒ０６）（集約通報）…… http… 2026-09-18 15:30:52 気象庁 【警戒・注意… http…
#>  8 気象警報・注意報（Ｒ０６）（集約通報）…… http… 2026-09-18 15:20:56 気象庁 【警戒・注意… http…
#>  9 気象警報・注意報（Ｒ０６）（集約通報）…… http… 2026-09-18 15:10:50 気象庁 【警戒・注意… http…
#> 10 気象警報・注意報（Ｒ０６）（集約通報）…… http… 2026-09-18 15:01:12 気象庁 【警戒・注意… http…
#> # ℹ 528 more rows
read_kishou_feed("low", "other")
#> # A tibble: 473 × 6
#>    title                  id            updated             author content link 
#>    <chr>                  <chr>         <dttm>              <chr>  <chr>   <chr>
#>  1 地方海上警報（Ｈ２８） https://www.… 2026-09-18 14:35:05 福岡管区気… 【長崎海上気… http…
#>  2 地方海上警報（Ｈ２８） https://www.… 2026-09-18 14:35:05 鹿児島地方… 【鹿児島海上… http…
#>  3 地方海上警報（Ｈ２８） https://www.… 2026-09-18 14:35:04 名古屋地方… 【名古屋海上… http…
#>  4 地方海上警報（Ｈ２８） https://www.… 2026-09-18 14:35:04 福岡管区気… 【福岡海上気… http…
#>  5 地方海上警報（Ｈ２８） https://www.… 2026-09-18 14:35:04 高松地方気… 【神戸海上気… http…
#>  6 地方海上警報（Ｈ２８） https://www.… 2026-09-18 14:35:03 新潟地方気… 【新潟海上気… http…
#>  7 地方海上警報（Ｈ２８） https://www.… 2026-09-18 14:35:03 気象庁 【東京海上気… http…
#>  8 地方海上警報（Ｈ２８） https://www.… 2026-09-18 14:35:02 札幌管区気… 【函館海上気… http…
#>  9 地方海上警報（Ｈ２８） https://www.… 2026-09-18 14:35:02 仙台管区気… 【仙台海上気… http…
#> 10 地方海上警報（Ｈ２８） https://www.… 2026-09-18 14:35:02 札幌管区気… 【札幌海上気… http…
#> # ℹ 463 more rows
# }
```
