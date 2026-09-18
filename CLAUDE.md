@AGENTS.md

## Claude Code 固有の設定

> 規約の正典は [AGENTS.md](AGENTS.md) で、上の 1 行の import で全文が読み込まれる。**このファイルに書いた内容は Codex には届かない**ので、両方のエージェントに効かせたい規約は AGENTS.md 側に書く。

### スキル

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

### hook（`.claude/settings.json`）

Claude Code 経由の操作にだけ効く層。ターミナルからのコミットには `.githooks/pre-commit` が対応する（clone ごとに `git config core.hooksPath .githooks` で有効化）。

- **PreToolUse（Bash）**: `git commit` の前に `AGENTS.md` が Codex の 32 KiB 上限を超えていないか検査し、超えていればコミットを止める
- **PostToolUse（Edit・Write）**: `.R` / `.qmd` / `.Rmd` を `air format` で整形 / `AGENTS.md` の編集後にサイズを検査
