---
name: project-status
description: 現在の進捗・直近の作業・次のステップ
type: project
updated: 2026-11-02
---

# jmastats — Status

## 引き継ぎ（HANDOFF）

> 別のエージェント（Codex 等）や次のセッションが**この欄だけ読めば再開できる**状態を保つ。残すのは今使っている判断だけで、検討しただけの案は書かない。方針を決めた時・試行を捨てた時・検証を実行した時・セッションを終える時に更新する。

- **次に行う作業（1 つ）**: PR #36（branch `fix/lazydata-namespace`、push 済み）のレビュー待ち。Copilot は GitHub UI、Codex は `/codex:review --base main` の結果を待つ（どちらもユーザーが依頼する）。両方届いたら `pr-review-flow` skill で 1 回だけ仕分けし、PR #36 に仕分けコメントを 1 件投稿する。
- **最後に実行した検証と結果**（対象: 09b5cf7。後続の b387e03 は `.github/copilot-instructions.md` の追加のみ）:
  - `devtools::test()`: FAIL 0 / SKIP 2 / PASS 51
  - `devtools::check()`: 0 errors / 0 warnings / 0 notes
  - 一時ライブラリへ install し、jmastats を attach せず影響する 5 関数を呼んで、エラーなしを確認
- **現在採用している方針**: Issue #35（jmastats を attach しないとバンドル済みデータが見つからない）の修正。データは `jmastats::stations` / `jmastats::tide_station` と名前空間で参照する。`R/data.R` のデータ用 `globalVariables()` 行は削除し、R CMD check で回帰を検出させる。`tests/testthat/test-data.R` に `codetools::findGlobals()` による静的テストを置く。Codex によるレビュー済み。
  - PR #36 は 09b5cf7（fix）と b387e03（Copilot 用の指示ファイル）を含む。
- **試して失敗したこと**: Issue #35 に提案された callr 版テストは採らない。callr は installed package を読むため、`devtools::test()` では load_all() の修正版でなく古い版を検査してしまう。callr も Suggests に無い。
- **未確認の項目**:
  - AGENTS.md の「現状 38 passed」は実数 51 と合わない（修正前は 50）。AGENTS.md は未修正。
  - PR #36 の CI（R-CMD-check 等）の結果は未確認。
  - Copilot / Codex のレビューは未着で、仕分けは未実施。

**How to apply:** セッション終了時に進捗が変化したらこのファイルを更新する。「引き継ぎ（HANDOFF）」欄は方針を決めた時・試行を捨てた時・検証を実行した時にも更新し、Codex 等へ引き継ぐときはこの欄を先に読ませる（グローバル指示「Codex への委任と引き継ぎ」）。
