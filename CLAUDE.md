# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## プロジェクト概要

jmastats は気象庁（JMA）が公開する気象データを R から取得・整形する R パッケージ。過去の気象データ検索（1976 年以降）、台風のベストトラック、地震観測記録、潮位データを扱う。CRAN 登録済み（現在 0.3.0、開発版 0.3.0.9000）。

- GitHub: `uribo/jmastats`
- pkgdown: <https://uribo.github.io/jmastats/>
- Issue: <https://github.com/uribo/jmastats/issues>（未解決の問題・捨てた方法はここに記録する。`TODO.md` は置かない）

README.md は README.Rmd から生成される（`devtools::build_readme()`）。**README.Rmd を編集し、README.md を直接編集しない**。

## 開発コマンド

```sh
Rscript -e 'devtools::load_all()'
Rscript -e 'devtools::document()'   # roxygen2 で man/ と NAMESPACE を再生成
Rscript -e 'devtools::test()'       # 現状 38 passed / 2 skipped / 0 failed
Rscript -e 'devtools::check()'
Rscript -e 'devtools::build_readme()'
air format R tests                  # 整形（編集時 hook でも自動実行）
```

スキップされる 2 件は `{lwgeom}` 未導入時の `nearest_station()` 系（`tests/testthat/test-nearest_station.R`）。CI では `Suggests` が入るので実行される。

**テストはすべてオフラインで完結する**。ネットワークを叩くテストは無く、URL 文字列の組み立て（`tests/testthat/test-internals.R`）と `inst/dummy/` の固定入力（`bst.txt` / `eqdb.csv` / `tide.txt` / `dl_data.csv`）で検証する。新しいテストを足すときもこの方針を守り、実サイトへのリクエストをテストに入れない。

## アーキテクチャ

### 公開 API

| 入口 | 対象 | ファイル |
| --- | --- | --- |
| `jma_collect()` | 過去の気象データ検索（スクレイピング） | [R/jma_collect.R](R/jma_collect.R) |
| `read_rsmc_besttrack()` / `track_combine()` | 台風ベストトラック | [R/track.R](R/track.R) |
| `read_eqdb_csv()` | 震度データベース CSV | [R/read_eqdb.R](R/read_eqdb.R) |
| `read_tide_level()` / `pivot_tide_level()` | 潮位観測（1997 年以降） | [R/read_tide_level.R](R/read_tide_level.R), [R/pivot_tide_level.R](R/pivot_tide_level.R) |
| `read_jma_weather()` | ダウンロードした CSV の読み込み | [R/read_jma_weather.R](R/read_jma_weather.R) |
| `read_kishou_feed()` | 防災情報 XML フィード | [R/kishou_feed.R](R/kishou_feed.R) |
| `nearest_station()` / `pick_neighbor_stations()` / `pick_neighbor_tide_stations()` | 最寄り観測所の検索 | [R/nearest_station.R](R/nearest_station.R) |
| `scale_*_jma_absolute()` / `scale_*_jma_relative()` | 気象庁配色の ggplot2 スケール | [R/ggplot2_scales.R](R/ggplot2_scales.R) |
| `parse_unit()` | 列名に埋まった単位を `units` に変換 | [R/parse_unit.R](R/parse_unit.R) |
| `reset_cache()` | キャッシュ全削除 | [R/appdir.R](R/appdir.R) |

### `jma_collect()` の 4 層

1. **URL 構築** — `detect_target()` → `check_block_no()` で block_no を検証し、`jma_url()` が `item` × 観測所種別ごとに気象庁のページ URL を組み立てる。観測所種別（`prec_no` と `s`/`a`）は同梱データ `stations` を `detect_station_info()` が引く
2. **キャッシュ** — `rappdirs::user_cache_dir("jmastats")` 配下の `.rds`。パスは `search_cache_file()` が `item` + `station_type` + URL クエリから組み立てる
3. **取得・整形** — `slow_jma_collect()` → `jma_collect_raw()` → `item` ごとの `.jma_collect_*()` → `tweak_df()`（`convert_error()` で欠測記号を処理 → 記号除去 → trim → `readr::type_convert()`）
4. **パック** — `pack_df()` が `temperature_*` などの列を接頭辞ごとに `tidyr::pack()` でネストする（`hourly` / `10min` は対象外）

新しい `item` を足すときは、`jma_url()` の `arg_match()` の候補・URL 分岐、`jma_collect_raw()` の列名割り当て、`.jma_collect_*()` の整形の 3 か所を触ることになる。

### 取得のレート制限を外さない

`slow_jma_collect` は `purrr::slowly(rate = purrr::rate_delay(pause = 7))` で 1 リクエスト 7 秒の間隔を強制している。**この待機を短縮・削除しない**。相手は公的機関の公開サイトで、待機は礼儀であると同時にブロック回避でもある。「テストが遅い」は理由にならない（テストはネットワークを叩かない）。

### キャッシュにスキーマ版が無い

`search_cache_file()` のキーは `item` / `station_type` / URL クエリだけで、**出力スキーマの版を含まない**。整形層を変えて列構成が変わっても、既存の `.rds` は同じキーで読まれ続けるため、**古い形のデータが黙って返る**。出力の形を変えたときは:

- NEWS.md に「`reset_cache()` の実行が必要」と明記する
- 手元での確認は `cache = FALSE` を使うか `reset_cache()` を先に実行する

キーに版を足す改修を入れるなら、`search_cache_file()` の 1 か所で完結する（`pick_out_cache()` も同じ関数を経由している）。

### 観測所種別の特例

`detect_station_info()` は `station_type` が「官」なら `s`、それ以外を `a` に落としたうえで、**高松（`station_no == 23281`）だけを `a` に上書きしている**。上流のデータと URL 体系の食い違いを吸収する意図的な例外なので消さない。日別・時別系（`annually` / `rank` / `nml_daily` 以外）では、さらに `s1` / `a1` のように `1` を付けた種別が URL に入る。

### 非 ASCII 文字をソースに直接書かない

`R/` と `tests/` の日本語リテラルはエスケープで書き、ソースを ASCII に保つ（R CMD check の非 ASCII 警告回避）。**現状は 2 つの書き方が混在している**: `intToUtf8()`（[R/jma_collect.R](R/jma_collect.R) と [R/read_eqdb.R](R/read_eqdb.R) の大半、`R/utils.R` に 1 か所）と、`"\u9707\u5ea6"` のような Unicode エスケープ（両ファイル計 14 行）。

- **新規に書くときは Unicode エスケープ（`"\u5b98"` = 官）を使う**。`intToUtf8()` は可読性が低く、コードポイントの取り違えが型としては通ってしまう（姉妹プロジェクト jpops で「年齢」が「年」になるバグを出した実績がある）
- 既存の `intToUtf8()` 呼び出しの一括置換は、指示があるまで行わない。触るファイルの周辺だけ合わせる
- roxygen コメント内は例外で、`R/parse_unit.R` の `temperature(℃)` のように実例として非 ASCII を直接書いてよい
- `data-raw/` は `.Rbuildignore` されるため日本語リテラルを直接書いてよい

### 同梱データと再生成

| オブジェクト | 内容 | 生成元 |
| --- | --- | --- |
| `stations` | 気象観測所（`block_no` / `prec_no` / `station_type` / 位置） | [data-raw/station_list.R](data-raw/station_list.R) |
| `tide_station` | 潮位観測所 | 同上 |
| `earthquake_station` | 震度観測点 | 同上 |
| `df_jma_pages`（内部） | 気象庁ページの `item` → `page_id` 対応 | [data-raw/internal.R](data-raw/internal.R) → `R/sysdata.rda` |

`stations` の更新は `jma_collect()` の URL 構築を直接左右する。データを更新したら NEWS.md に版と時点を記録し（過去の記載形式は `## Datasets` の節を参照）、`devtools::test()` を通してから取り込む。

`data-raw/` のスクリプトは `%>%` や `library()` を使った探索的なコードで、パッケージ本体のコーディング規約（ネイティブパイプ・名前空間プレフィックス）の対象外。ただし**整形の対象からは外れていない**: `air.toml` に `exclude` が無いため、編集時 hook はここのファイルも整形する（現状 `air format data-raw --check` は通る）。CI の `air-format.yaml` が見ているのは `R` と `tests` だけ。ここが使う `ensurer` / `assertr` / `stringi` / `here` などは `DESCRIPTION` に宣言されていない（`.Rbuildignore` されるため `R CMD check` は通る）。再生成には別途インストールが必要。

### R CMD check 対策

データマスキングで参照する列名は `utils::globalVariables()` か関数冒頭の `NULL` 代入で「no visible binding」NOTE を抑える（[R/utils.R](R/utils.R) 冒頭の `utils::globalVariables("where")` が例）。新しい列名を導入したら同様に追加する。

## コーディング規約

- パイプはネイティブ `|>`。`%>%` はパッケージ本体では使わない（`data-raw/` の既存コードにのみ残る）
- `Depends: R (>= 4.1)`。この下限を上げるのはユーザー影響が大きいので、必要になった理由とともにユーザーへ確認する
- 関数にはパッケージ名前空間プレフィックスを付ける（`dplyr::filter()`）。NAMESPACE の `importFrom` は roxygen で管理する
- フォーマッタは air（`air.toml` は空＝既定値）。R/qmd を編集したら `air format` を実行する（Claude Code では PostToolUse hook が自動実行）
- モダン tidyverse パターンを使う（`.by`、`join_by()`、`purrr::map() |> list_rbind()`）。superseded パターンを避ける
- 変数名・列名は英語のみ。コメントは英語。散文ドキュメントは日本語でよい
- エラー・警告は `rlang::abort()` / `rlang::warn()` / `rlang::inform()` を使う
- ユーザーに見える変更は `NEWS.md` に 1 行追加する。ドキュメントの些細な修正や内部リファクタリングには不要
- testthat は edition 2（`DESCRIPTION` に `Config/testthat/edition` が無い）。3e への移行は独立した作業として扱い、ついでに変えない

## CI

| workflow | 起動 | 内容 |
| --- | --- | --- |
| `R-CMD-check.yaml` | push / PR（main） | macOS・Windows・Ubuntu × devel / release / oldrel-1、および `ubuntu-22.04` + R 4.1（`Depends` の下限を直接検証する） |
| `air-format.yaml` | push / PR（main） | `air format R tests --check` |
| `pkgdown.yaml` | push / PR（main）・release | pkgdown サイトを gh-pages へ配備 |
| `rhub.yaml` | `workflow_dispatch` | CRAN 提出前の R-hub チェック（`rhub::rhub_setup()` 生成物。手で書き換えない） |

`R-CMD-check.yaml` と `air-format.yaml` は 2026-09-02 に追加し、PR #27 で 6 行すべて pass した（R 4.1 も通ったので `Depends` の引き上げは不要）。

**所要時間はキャッシュの有無で桁が変わる**。`ubuntu-22.04` + R 4.1 と `ubuntu-latest` + devel は RSPM のバイナリが無く `sf` / `units` / `lwgeom` をソースからビルドするため、`setup-r-dependencies` のキャッシュが無い初回は **18 分・27 分**かかった。キャッシュが乗った次の run では **5 分・6 分**で、他の行（4-7 分）と変わらない。依存を変えるとキャッシュキーが変わって再び cold になるので、そのときの 20-30 分をハングと読み違えない。

全 job に `timeout-minutes` を置いてある（R-CMD-check 60 分、air-format 10 分、pkgdown 30 分）。既定の 6 時間を放置すると、action がハングしたときに 1 リポジトリあたり 360 runner 分を無駄にする（2026-08-19 に `r-lib/actions/setup-r@v2` で実際に起きた）。cold cache の最遅が 27 分なので 60 分は 2.2 倍の余裕がある。超えるようになったら、**上限を上げるのであって外さない**。

`R-CMD-check` と `air-format` には `concurrency` グループがあり、PR ブランチを押し直すと追い越された run が破棄される。`cancel-in-progress` を `pull_request` に限定してあるのは、main 上の run はバッジとリリース監査が読む履歴なので完走させるため。`pkgdown.yaml` の `concurrency` は r-lib の定型で gh-pages への deploy を制御しており、別物なので触らない。

## CRAN

`cran-comments.md` が提出時のコメント。CRAN 提出前は `/r-lib:cran-extrachecks` スキルの項目（URL 検証・DESCRIPTION 記法・examples の実行時間）を確認する。`\donttest{}` で囲んだ例は実サイトを叩くので、CRAN の実行対象から外れていることを維持する。

## 作業時の注意

### GitHub Issue 操作

- **Issue 作成前に必ず既存 Issue を確認する**（`gh issue list --state all`）。重複 Issue を作らない
- Issue 番号は連番で不可逆なため、誤作成で欠番が生じないよう慎重に操作する

### Git ブランチ操作

- **ブランチの作成・切り替えは必ずユーザーに確認を求めてから実行する**。勝手にブランチを作成しない

### 破壊的操作

- GitHub 上の操作（issue 作成・クローズ、PR 作成、push）は取り消しが困難。実行前にユーザーへ確認する
- `reset_cache()` はユーザーのキャッシュを全削除する。エージェントセッションから自動実行しない

### Codex への委任（`prompts/`）

Codex へ実装を委任するときだけ、ブリーフを `prompts/YYYYMMDD-HHMM-<topic>.md`（JST: `TZ=Asia/Tokyo date '+%Y%m%d-%H%M'`）に書く。ディレクトリは必要になった時点で作る（`prompts/*.md` は gitignored）。**HANDOFF にブリーフの正確なパスを書く** — Codex は `prompts/` を自動では読まず、読むのは `memory/project-status.md` の HANDOFF・`git status`・`git diff` だけ（[AGENTS.md](AGENTS.md)）。

## 新しいファイルを追加したら `.Rbuildignore` を確認する

CRAN パッケージなので、**トップレベルに置いたファイルは原則すべて `.Rbuildignore` に足す**。漏れると `R CMD check` が NOTE を出す。確認方法:

```sh
R CMD build . && tar tzf jmastats_*.tar.gz | grep -E 'CLAUDE|AGENTS|memory|\.claude|\.codex|\.vscode'
```

何も出なければ正しい（確認後 tarball は削除する）。

## Memory

プロジェクト固有の知識を `memory/` に蓄積し、会話間で引き継ぐ。

- インデックス: [memory/MEMORY.md](memory/MEMORY.md)（200 行以内に保つ）
- 各メモリは個別 `.md`。フロントマターに `name` / `description` / `type` / `updated`
- type: `feedback` | `project` | `reference`（`user` はグローバル CLAUDE.md に集約）
- コード・git 履歴から導出可能な情報は書かない
- [memory/project-status.md](memory/project-status.md) 先頭の「引き継ぎ（HANDOFF）」欄は、方針を決めた時・試行を捨てた時・検証を実行した時・セッションを終える時に更新する

## コミット規約

**Conventional Commits v1.0.0** に準拠する。コミットメッセージは英語。`Co-Authored-By:` フッターは付けない。

### scope

| scope | 対象 |
| --- | --- |
| `collect` | `jma_collect()` 系（`R/jma_collect.R`, `R/appdir.R`） |
| `track` | ベストトラック（`R/track.R`） |
| `tide` | 潮位（`R/read_tide_level.R`, `R/pivot_tide_level.R`） |
| `eqdb` | 地震・震度（`R/read_eqdb.R`） |
| `station` | 観測所検索・同梱データ（`R/nearest_station.R`, `data/`） |
| `viz` | ggplot2 スケール（`R/ggplot2_scales.R`） |
| `data` | `data-raw/` の生成スクリプト |
| `ci` | `.github/workflows/` |
| `config` | エージェント・エディタ設定 |

```
feat(collect): support 10min interval for AMeDAS stations
fix(tide): keep missing markers out of type_convert()
chore(data): refresh station list to 2026-01 revision
docs: document cache invalidation after schema changes
```

## Skills（Claude Code 向け）

- `/r-modern-tidyverse`: R コード記述・レビュー時。superseded パターンの回避
- `/r-rlang-programming`: tidy eval を使う関数を書く際（`jma_url()` の `enquo()`/`eval_tidy()` 周辺）
- `/r-lib:r-package-development`: devtools / roxygen2 / パッケージ構造
- `/r-lib:testing-r-packages`: テスト追加時
- `/r-lib:cran-extrachecks`: CRAN 提出前
- `/r-lib:lifecycle`: 関数・引数の非推奨化（`lifecycle` は既に Imports）
- `/simplify`: コード変更後のレビュー

### エージェント

- `auto-committer`: 作業単位の完了時に自律的にコミット（Conventional Commits 準拠）
- `memory-updater`: コミット後・方針決定時・セッション終了時に `memory/project-status.md` を更新
