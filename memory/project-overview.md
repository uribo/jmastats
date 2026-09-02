---
name: project-overview
description: jmastats の目的・構成・技術スタック
type: project
updated: 2026-09-02
---

# jmastats — Overview

**Why:** 気象庁（JMA）が公開する気象データ — 過去の気象データ検索（1976 年以降）、台風ベストトラック、地震・震度記録、潮位観測 — を R から tidy な形で取得・整形するための CRAN パッケージ。配布元は `uribo/jmastats`。

## 技術スタック

- R パッケージ（`Depends: R (>= 4.1)`）。tidyverse 依存（dplyr / purrr / stringr / tidyr / readr）＋ 空間は sf、単位は units、UI は cli / rlang
- 取得はスクレイピング（rvest / xml2）。キャッシュは rappdirs
- テストは testthat edition 2。**すべてオフライン**（URL 組み立て検証と `inst/dummy/` の固定入力）
- フォーマッタは air。renv・targets は使わない（解析プロジェクトではなくパッケージのため）
- CI: R-CMD-check / air-format / pkgdown / rhub（`.github/workflows/`）

## 構成

- 公開関数と各ファイルの対応、`jma_collect()` の 4 層構造、キャッシュ・観測所種別の落とし穴は [CLAUDE.md](../CLAUDE.md)「アーキテクチャ」を参照
- 同梱データ（`stations` / `tide_station` / `earthquake_station` / 内部 `df_jma_pages`）の再生成は `data-raw/`
- 未解決の問題・捨てた方法は GitHub Issue に記録する（`TODO.md` は置かない）

**How to apply:** 新しいタスクに着手する前にこのファイルで全体像を確認する。詳細な規約は `CLAUDE.md` を参照。
