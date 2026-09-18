---
name: project-status
description: 現在の進捗・直近の作業・次のステップ
type: project
updated: 2026-09-19
---

# jmastats — Status

## 引き継ぎ（HANDOFF）

> 別のエージェント（Codex 等）や次のセッションが**この欄だけ読めば再開できる**状態を保つ。残すのは今使っている判断だけで、検討しただけの案は書かない。方針を決めた時・試行を捨てた時・検証を実行した時・セッションを終える時に更新する。

- **次に行う作業（1 つ）**: win-builder results を受け取ったら cran-comments.md に追加し、ブランチをプッシュして PR を開く。
- **最後に実行した検証と結果**: chore/release-0.3.1-prep で NEWS 修正・cran-comments.md 再作成（422a57c）。devtools::check(remote = TRUE, manual = TRUE) 0 errors / 0 warnings / 2 notes（.9000 と Tidy 古さ、期待通り）；CRAN 0.3.0 all 14 flavors OK；urlchecker all OK；build_readme() no diff。check_win_devel() submitted 03:40 JST（結果未着）。
- **現在採用している方針**: Issue #31 の release checklist に従う。PR #33 merged 後、chore/release-0.3.1-prep ブランチで CRAN 検証→ push → PR open を進める。use_version('patch') は PR merged 後に実行。
- **試して失敗したこと**: （なし）
- **未確認の項目**: win-builder results（03:40 JST submission、結果待ち）。

- **現在フェーズ**: 0.3.0 リリース後の開発（`0.3.0.9000`）
- **直近の作業**: air フォーマッタの全面適用（`6a94e5f`）→ エージェント作業環境の整備
- **次のステップ**: 未着手。GitHub Issue を参照

**How to apply:** セッション終了時に進捗が変化したらこのファイルを更新する。「引き継ぎ（HANDOFF）」欄は方針を決めた時・試行を捨てた時・検証を実行した時にも更新し、Codex 等へ引き継ぐときはこの欄を先に読ませる（グローバル指示「Codex への委任と引き継ぎ」）。
