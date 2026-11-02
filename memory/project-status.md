---
name: project-status
description: 現在の進捗・直近の作業・次のステップ
type: project
updated: 2026-11-02
---

# jmastats — Status

## 引き継ぎ（HANDOFF）

> 別のエージェント（Codex 等）や次のセッションが**この欄だけ読めば再開できる**状態を保つ。残すのは今使っている判断だけで、検討しただけの案は書かない。方針を決めた時・試行を捨てた時・検証を実行した時・セッションを終える時に更新する。

- **次に行う作業（1 つ）**: PR #36（branch `fix/lazydata-namespace`、OPEN）をユーザーがマージする。マージ後に `gh pr view 36 --json state,mergeCommit,baseRefName` で確認し、main を同期して Issue #35 がクローズされたか確かめる。
- **最後に実行した検証と結果**（対象コミット 09b5cf7。以後の b387e03・1ca1ca0・71e7f6c・089a1b4 はドキュメントと指示ファイルのみ）:
  - `devtools::test()`: FAIL 0 / SKIP 2 / PASS 51。`devtools::check()`: 0 errors / 0 warnings / 0 notes
  - 一時ライブラリへ install し、jmastats を attach せず影響する 5 関数を呼んで、エラーなしを確認
  - CI（09b5cf7）: R-CMD-check success（24 分 42 秒、cold cache）、air-format success
- **現在採用している方針**: Issue #35（jmastats を attach しないとバンドル済みデータが見つからない）の修正。データは `jmastats::stations` / `jmastats::tide_station` と名前空間で参照し、`R/data.R` のデータ用 `globalVariables()` 行は削除して R CMD check で回帰を検出させる。静的テストは `tests/testthat/test-data.R` の `codetools::findGlobals()`。
  - 方針は実装前に Codex と検討した。実装後の PR #36 差分は Codex がレビューし、正しさの不具合は無し、ドキュメント指摘 3 件（AGENTS.md の件数、memory/ 除外を文体のみに限定、`memory` scope 追加）をすべて採用した。仕分けコメントは PR #36 に投稿済み。Copilot は指摘なし（承認推奨）。
  - 本文の判断と経緯の詳細は PR #36 と git 履歴（`git log 09b5cf7^..HEAD`）を正本とする。
- **試して失敗したこと**: Issue #35 に提案された callr 版テストは採らない。callr は installed package を読むため、`devtools::test()` では load_all() の修正版でなく古い版を検査してしまう。callr も Suggests に無い。
- **未確認の項目**:
  - PR #36 の最新 head 089a1b4 の CI（R-CMD-check 各 OS・pkgdown）は実行中（確認時点）。完了と結果を見るまで全 pass とは扱わない。
  - Issue #35 は確認時点で OPEN のまま（マージ後の状態は未確認）。

**How to apply:** セッション終了時に進捗が変化したらこのファイルを更新する。「引き継ぎ（HANDOFF）」欄は方針を決めた時・試行を捨てた時・検証を実行した時にも更新し、Codex 等へ引き継ぐときはこの欄を先に読ませる（グローバル指示「Codex への委任と引き継ぎ」）。
