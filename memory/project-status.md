---
name: project-status
description: 現在の進捗・直近の作業・次のステップ
type: project
updated: 2026-09-19
---

# jmastats — Status

## 引き継ぎ（HANDOFF）

> 別のエージェント（Codex 等）や次のセッションが**この欄だけ読めば再開できる**状態を保つ。残すのは今使っている判断だけで、検討しただけの案は書かない。方針を決めた時・試行を捨てた時・検証を実行した時・セッションを終える時に更新する。

- **次に行う作業（1 つ）**: PR #30 の CI を待ってマージする。ブランチ `fix/readme-citation`。
- **最後に実行した検証と結果**: `devtools::test()` → 43 passed / 2 skipped / 0 failed（前回 38→43）。`devtools::build_readme()` → OK（README に動的生成された citation 反映済み）。R CMD check は未実行。
- **現在採用している方針**: Issue #29（README の citation 版年が不一致）は inst/CITATION を追加して解決。版は `meta$Version`、年は `Date/Publication`（無ければビルドした年）、URL は CRAN に固定。README.Rmd は `citation("jmastats")` を評価して出力する（PR #30）。
- **試して失敗したこと**: plain eval of citation() without inst/CITATION — dev DESCRIPTION に Date が無く、年が `????` になり警告も出る。inst/CITATION で明示的に固定化した。
- **未確認の項目**: R CMD check を CI で実行待ち。PR #30 の全 CI が通ることが merge 条件。

- **現在フェーズ**: 0.3.0 リリース後の開発（`0.3.0.9000`）
- **直近の作業**: air フォーマッタの全面適用（`6a94e5f`）→ エージェント作業環境の整備
- **次のステップ**: 未着手。GitHub Issue を参照

**How to apply:** セッション終了時に進捗が変化したらこのファイルを更新する。「引き継ぎ（HANDOFF）」欄は方針を決めた時・試行を捨てた時・検証を実行した時にも更新し、Codex 等へ引き継ぐときはこの欄を先に読ませる（グローバル指示「Codex への委任と引き継ぎ」）。
