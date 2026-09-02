# Codex project instructions

Read and follow `CLAUDE.md` as the primary source of project knowledge and conventions. The rules below add Codex-specific constraints.

This repository is an R package published on CRAN. Two consequences apply to every change:

- Anything added at the top level must also be listed in `.Rbuildignore`, or `R CMD check` reports a NOTE. Verify with `R CMD build . && tar tzf jmastats_*.tar.gz`, then delete the tarball.
- `README.md` is generated from `README.Rmd` via `devtools::build_readme()`. Never edit `README.md` directly.

## Network access

- Tests must stay offline. They verify URL construction and read the fixtures in `inst/dummy/`. Do not add a test that requests the JMA website.
- `slow_jma_collect` enforces a 7-second delay between requests (`purrr::slowly()`). Do not shorten or remove it.

## Credential handling

- Never read, edit, print, search, summarize, or otherwise expose `.Renviron`, `.env`, credential JSON files, private keys, or files whose purpose is to store secrets.
- Do not bypass `.codex/config.toml` environment filtering or override `R_ENVIRON_USER` unless the user explicitly approves access for a specific task.
- If a task needs authenticated access, explain which credential or environment variable is required and obtain approval before enabling it. Never include credential values in prompts, logs, command output, or commits.

## Handoff from Claude Code

- Before starting, read the "引き継ぎ（HANDOFF）" block at the top of `memory/project-status.md`, then check `git status` and `git diff`. Do not discard existing changes.
- Treat recorded decisions as claims: confirm them against the code and test results before building on them.
- When you finish or stop, update the HANDOFF block (current approach, the single next task, failed attempts, unverified items, last verification command and result).
