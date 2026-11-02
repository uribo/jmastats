# Copilot への指示（jmastats）

jmastats は気象庁（JMA）の公開データ（過去の気象データ検索、台風ベストトラック、震度データベース、潮位）を R から取得・整形する CRAN パッケージ。本体は `R/`、テストは `tests/testthat/`、同梱データは `data/` と `R/sysdata.rda`（生成元は `data-raw/`）。規約の詳細は `AGENTS.md` にあり、ここには PR で止めたい観点だけを置く。

**指摘は日本語で書く。**

## この repo で優先して見てほしいこと

- **取得のレート制限の後退**: `slow_jma_collect` の `purrr::rate_delay(pause = 7)` を短縮・削除する変更、または待機を経由せずに気象庁のサイトへリクエストする経路を足す変更。
- **ネットワークを叩くテスト**: テストはすべてオフラインで完結する前提（URL 文字列の組み立てと `inst/dummy/` の固定入力で検証する）。実サイトへのリクエストや、ネットワークが無いと失敗するテストを足していないか。`\donttest{}` の外に実サイトを叩く examples を置いていないか。
- **出力の形を変えたのに NEWS.md に `reset_cache()` の案内が無い**: キャッシュのキー（`search_cache_file()`）は出力スキーマの版を含まないため、整形層（`tweak_df()`、`pack_df()`、`.jma_collect_*()`）で列構成を変えると古い `.rds` が黙って返る。
- **同梱データを名前だけで参照していないか**: 名前空間内のコードは `stations` / `tide_station` / `earthquake_station` を `jmastats::stations` の形で参照する（attach しないと見つからない。#35）。これらを `utils::globalVariables()` に足して NOTE を消す変更も止める。
- **ソースに非 ASCII 文字を直接書いていないか**: `R/` と `tests/` の日本語リテラルは `"\u5b98"`（官）のような Unicode エスケープで書く。新規の `intToUtf8()` も避ける（コードポイントの取り違えが通ってしまう）。エスケープしたコードポイントが、コメントや周辺の記述が示す文字と一致しているかも確かめる。roxygen コメント内と `data-raw/` は例外。
- **観測所種別の特例を消していないか**: `detect_station_info()` の高松（`station_no == 23281`）を `a` に上書きする処理は意図的な例外。
- **`Depends: R (>= 4.1)` を超える機能**: R 4.2 以降の `|>` のプレースホルダ `_` など、4.1 で動かない書き方を入れていないか。
- **R CMD check の NOTE の原因**: データマスキングで新しい列名を参照したのに `utils::globalVariables()` か関数冒頭の `NULL` 代入が無い。トップレベルに新しいファイルを置いたのに `.Rbuildignore` に足していない。
- **新しい `item` の追加が 3 か所揃っているか**: `jma_url()` の候補と URL 分岐、`jma_collect_raw()` の列名、`.jma_collect_*()` の整形。
- **ユーザーに見える変更に NEWS.md の 1 行があるか**。
- **文書と実体の食い違い**: `AGENTS.md` や README.Rmd に書かれたテスト件数・関数名・ファイルパス・版番号が、変更後のコードと一致しているか。
- エラー・警告に `stop()` / `warning()` を使っていないか（`rlang::abort()` / `rlang::warn()` / `rlang::inform()` を使う）。

## 指摘しなくてよいこと

- **整形**。R は air formatter（`air.toml` は既定値）に従い、CI の `air-format.yaml` が検査する。インデント・改行位置・パイプの折り返しは挙げない。
- **Markdown 散文のハードラップ**。段落・リスト項目は 1 行で書いて soft-wrap させる規約なので、行が長いことを問題として挙げない。
- 日本語の文体・語尾の統一。
- `data-raw/` の `%>%`、`library()`、名前空間プレフィックスの欠落、`DESCRIPTION` に無いパッケージの使用（`.Rbuildignore` される探索的なスクリプトで、本体の規約の対象外）。
- 既存の `intToUtf8()` 呼び出しを Unicode エスケープへ一括置換すること（指示があるまで行わない。触る行の周辺だけ合わせればよい）。
- testthat edition 3 への移行の提案（edition 2 のまま。移行は独立した作業）。
- 生成物: `man/*.Rd`、`NAMESPACE`（roxygen2 が生成）、`README.md`（`README.Rmd` から生成）、`.github/workflows/rhub.yaml`（`rhub::rhub_setup()` の生成物）。ただし `README.md` だけを直接編集して `README.Rmd` を直していない変更は指摘する。
- `memory/` 配下のノート（エージェント間の引き継ぎ記録）の文体・書式。ただし、記述どうしの矛盾や、コード・テスト結果との事実の食い違いは指摘する。

## 記法の規約

- コミットメッセージは Conventional Commits（英語）。scope は `collect` / `track` / `tide` / `eqdb` / `station` / `viz` / `data` / `ci` / `config` / `memory`。
- パッケージ本体はネイティブパイプ `|>` と名前空間プレフィックス（`dplyr::filter()`）を使う。`importFrom` は roxygen で管理する。
- 変数名・列名は英語のみ。コード中のコメントは英語。

## 触れない方が良いもの

- `inst/dummy/` の固定入力（`bst.txt` / `eqdb.csv` / `tide.txt` / `dl_data.csv`）。テストの期待値はこの中身に依存するので、値を直す提案をしない。
- NEWS.md のリリース済みの節（`# jmastats 0.3.1` 以前）。過去の版の記述は書き換えない。
- `cran-comments.md`（CRAN 提出時のコメントの記録）。
- `.github/workflows/` の `timeout-minutes` と `concurrency`。外す・緩める提案をしない（理由は `AGENTS.md` の CI の節）。
