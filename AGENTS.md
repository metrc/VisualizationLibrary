# VisualizationLibrary: Agent Guide

METRC R package that generates standardized tables and figures (enrollment, baseline and injury characteristics, follow-up status, adverse events, deviations) from study analytic datasets built by `AnalyticCodebase`. User-level code lives in `R/`; system-level helpers come from `VisualizationTools`.

## ADS Workspace (read first)

This repo lives inside the **ADS Workspace** folder alongside the other METRC repos, and
agents are started at the ADS Workspace root. The full guide (layout, setup steps for the
user and the agent, and where the keys live) is in the "The ADS Workspace" section of
`../AnalyticCodebase/AGENTS.md`. Read it at the start of every session.

> **⚠️ WARNING: Never work outside the ADS Workspace folder.** Do not read, list, search,
> copy, or edit anything outside `ADS Workspace/` (Desktop, Downloads, home folder, other
> projects) unless the user names that exact file or folder and tells you to use it.
> These folders hold study data and PHI. If a task seems to need something outside,
> stop and ask.

If the working folder is not `ADS Workspace`, or `Coding Workspace/redcap_keys.json` is
missing from it, the workspace is not set up: stop, tell the user, and walk them through
the setup steps in that guide. REDCap keys are read only from
`ADS Workspace/Coding Workspace/` and never copied anywhere else. R sessions run with
`Coding Workspace/` as their working directory, so cached study data
(`<study>-cached_data/`) stays there and never lands in this repo.

## Skills

Reusable workflows live in `.agents/skills/<name>/SKILL.md` (the open Agent Skills format).
Codex and Cursor read `.agents/skills` directly; `.claude/skills` is a symlink to it so
Claude Code finds the same skills. Any other agent should
read the relevant file directly when the task matches. See `.agents/skills/skills_help.txt`
for the SKILL.md format.

| Skill | Read it when |
|-------|--------------|
| _(none yet)_ | |

Scratch outputs for the user (drafts, comparisons, one-off scripts) go in `.agents/artifacts/`.
