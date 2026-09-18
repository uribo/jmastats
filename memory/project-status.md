---
name: project-status
description: 現在の進捗・直近の作業・次のステップ
type: project
updated: 2026-09-19
---

# jmastats — Status

## 引き継ぎ（HANDOFF）

> 別のエージェント（Codex 等）や次のセッションが**この欄だけ読めば再開できる**状態を保つ。残すのは今使っている判断だけで、検討しただけの案は書かない。方針を決めた時・試行を捨てた時・検証を実行した時・セッションを終える時に更新する。

- **次に行う作業（1 つ）**: Issue #31 の dataset refresh を始める（data-raw の依存をインストール → `data-raw/station_list.R` 再実行）。
- **最後に実行した検証と結果**: PR #30 merged（ee188ca）。`devtools::test()` → 43 passed / 2 skipped / 0 failed。README.Rmd + inst/CITATION で citation 年月を同期。
- **現在採用している方針**: 次リリースは 0.3.1（patch）: #26（cache-dir）+ #29（CITATION）+ dataset refresh。#6（cache overwrite option）と #5 は 0.4.0 へ defer。理由は #26 が fresh install で cache=TRUE を壊すため小さく早く出す。Release checklist は Issue #31（usethis::use_release_issue() + dataset-refresh 項目を手動追加）。
- **試して失敗したこと**: （前回から変化なし）
- **未確認の項目**: CRAN check 結果（0.3.0、curl deny のため未確認）。Issue #31 の dataset 生成完了後にテスト実行確認が必要。

- **現在フェーズ**: 0.3.0 リリース後の開発（`0.3.0.9000`）
- **直近の作業**: air フォーマッタの全面適用（`6a94e5f`）→ エージェント作業環境の整備
- **次のステップ**: 未着手。GitHub Issue を参照

**How to apply:** セッション終了時に進捗が変化したらこのファイルを更新する。「引き継ぎ（HANDOFF）」欄は方針を決めた時・試行を捨てた時・検証を実行した時にも更新し、Codex 等へ引き継ぐときはこの欄を先に読ませる（グローバル指示「Codex への委任と引き継ぎ」）。
