---
name: project-status
description: 現在の進捗・直近の作業・次のステップ
type: project
updated: 2026-09-02
---

# jmastats — Status

## 引き継ぎ（HANDOFF）

> 別のエージェント（Codex 等）や次のセッションが**この欄だけ読めば再開できる**状態を保つ。残すのは今使っている判断だけで、検討しただけの案は書かない。方針を決めた時・試行を捨てた時・検証を実行した時・セッションを終える時に更新する。

- **現在採用している方針**: エージェント作業環境（`CLAUDE.md` / `AGENTS.md` / `.claude/settings.json` / `.codex/config.toml` / `.vscode/` / `memory/`）を `research-project-template` に倣って整備した。ただしテンプレートは renv + targets + Quarto 前提であり、jmastats はそのいずれも使わない R パッケージなので、**renv 関連の hook（PreToolUse の lockfile ゲート、Stop の drift チェック）と `.Rprofile` / `Renviron.example` / `_dependencies.R` / `notes/` / `paper/` / `TODO.md` は移植しなかった**。ロケール固定は環境変数の層（`.claude/settings.json` の `env` と `.codex/config.toml` の `set`）のみで行う。CI は jpops に倣って `R-CMD-check.yaml` と `air-format.yaml` を追加した。
- **次に行う作業（1 つ）**: ユーザーが未コミット差分をレビューする（`R CMD build` の生成物 `jmastats_0.3.0.9000.tar.gz` が作業ツリーに残っている。gitignored だが削除はユーザーが行う — セッション中に削除許可が下りなかった）。
- **試して失敗したこと**: 特になし。
- **未確認の項目**: `.claude/skills/` へのスキル配備（`r-modern-tidyverse` 等）は未実施。CLAUDE.md「Skills」節はまだ絵に描いた餅で、conf-macos の `deploy/manifest.tsv` に jmastats を足して `deploy/deploy.sh --apply` を回すまで有効にならない（repo 外の作業なのでユーザーが行う）。配備後は `git status` に `.claude/skills/` の symlink が出ていないか確認する（kumagusu は `.claude/skills/.gitignore` で対処している）。CI は PR #27 で初回実行し全 6 行 pass（R 4.1 = 18 分、devel = 27 分。どちらもソースビルドのため遅いだけで、`Depends` の引き上げは不要だった）。
- **最後に実行した検証と結果**（すべて 2026-09-02 JST に実行済み）: `devtools::test()` → 38 passed / 2 skipped（`{lwgeom}` 未導入）/ 0 failed。エージェントセッションと同じロケールを再現した `LC_COLLATE=C LC_TIME=C R_ENVIRON_USER=/dev/null Rscript -e 'devtools::test()'` でも同じ 38 / 2 / 0 で、ロケール固定に依存して壊れるテストは無い。`air format R tests --check` と `air format data-raw --check` → いずれも exit 0。`jq` / `tomllib` / `yaml::read_yaml()` による設定ファイルの parse → すべて正常。`R CMD build` した tarball に `CLAUDE.md` / `AGENTS.md` / `memory` / `.claude` / `.codex` / `.vscode` が含まれないことを確認。`git check-ignore` で `.vscode/settings.json` と `.claude/settings.json` が追跡対象、`.claude/settings.local.json` が無視対象になっていることを確認。`air format README.Rmd` はバイト無変更（hook の正規表現に `Rmd` が入っているが no-op）。PR #27 の CI は 6 行すべて success。

- **現在フェーズ**: 0.3.0 リリース後の開発（`0.3.0.9000`）
- **直近の作業**: air フォーマッタの全面適用（`6a94e5f`）→ エージェント作業環境の整備
- **次のステップ**: 未着手。GitHub Issue を参照

**How to apply:** セッション終了時に進捗が変化したらこのファイルを更新する。「引き継ぎ（HANDOFF）」欄は方針を決めた時・試行を捨てた時・検証を実行した時にも更新し、Codex 等へ引き継ぐときはこの欄を先に読ませる（グローバル指示「Codex への委任と引き継ぎ」）。
