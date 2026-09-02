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
#> # A tibble: 774 × 6
#>    title                          id    updated             author content link 
#>    <chr>                          <chr> <dttm>              <chr>  <chr>   <chr>
#>  1 地上実況図                     http… 2026-09-02 05:12:56 気象庁 【地上実況図… http…
#>  2 気象警報・注意報（Ｒ０６）（集約通報）…… http… 2026-09-02 05:10:50 気象庁 【警戒・注意… http…
#>  3 地方季節予報（２週間気温予報） http… 2026-09-02 05:06:44 気象庁 【２週間気温… http…
#>  4 全般季節予報（２週間気温予報） http… 2026-09-02 05:06:17 気象庁 【２週間気温… http…
#>  5 気象警報・注意報（Ｒ０６）（集約通報）…… http… 2026-09-02 05:01:23 気象庁 【警戒・注意… http…
#>  6 気象警報・注意報（Ｒ０６）（集約通報）…… http… 2026-09-02 04:50:58 気象庁 【警戒・注意… http…
#>  7 気象警報・注意報（Ｒ０６）（集約通報）…… http… 2026-09-02 04:40:50 気象庁 【警戒・注意… http…
#>  8 気象警報・注意報（Ｒ０６）（集約通報）…… http… 2026-09-02 04:30:57 気象庁 【警戒・注意… http…
#>  9 気象警報・注意報（Ｒ０６）（集約通報）…… http… 2026-09-02 04:20:49 気象庁 【警戒・注意… http…
#> 10 気象警報・注意報（Ｒ０６）（集約通報）…… http… 2026-09-02 04:10:55 気象庁 【警戒・注意… http…
#> # ℹ 764 more rows
read_kishou_feed("low", "other")
#> # A tibble: 468 × 6
#>    title                          id    updated             author content link 
#>    <chr>                          <chr> <dttm>              <chr>  <chr>   <chr>
#>  1 地方海上警報（Ｈ２８）         http… 2026-09-02 02:35:05 沖縄気象台… 【沖縄海上気… http…
#>  2 地方海上警報（Ｈ２８）         http… 2026-09-02 02:35:04 鹿児島地方… 【鹿児島海上… http…
#>  3 地方海上警報（Ｈ２８）         http… 2026-09-02 02:35:04 福岡管区気… 【長崎海上気… http…
#>  4 地方海上警報（Ｈ２８）         http… 2026-09-02 02:35:03 新潟地方気… 【新潟海上気… http…
#>  5 地方海上警報（Ｈ２８）         http… 2026-09-02 02:35:03 高松地方気… 【神戸海上気… http…
#>  6 地方海上警報（Ｈ２８）         http… 2026-09-02 02:35:02 札幌管区気… 【函館海上気… http…
#>  7 地方海上警報（Ｈ２８）         http… 2026-09-02 02:35:02 仙台管区気… 【仙台海上気… http…
#>  8 地方海上警報（Ｈ２８）         http… 2026-09-02 02:35:01 札幌管区気… 【札幌海上気… http…
#>  9 全般海上警報（定時）（Ｈ２９） http… 2026-09-02 02:34:01 気象庁 【全般海上警… http…
#> 10 全般海上警報（定時）（Ｒ０８） http… 2026-09-02 02:34:01 気象庁 【全般海上警… http…
#> # ℹ 458 more rows
# }
```
