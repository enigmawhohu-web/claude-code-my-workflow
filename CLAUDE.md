# CLAUDE.MD -- Public Opinion on Fentanyl Policy

<!-- Keep this file under ~150 lines — Claude loads it every session.
     See the guide at https://hugosantanna.github.io/clo-author/ for full documentation. -->

**Project:** Public Opinion on Fentanyl Policy: Cooperation vs. Punishment (2024–2025)
**Branch:** main

---

## Core Principles

- **Plan first** -- enter plan mode before non-trivial tasks; save plans to `quality_reports/plans/`
- **Verify after** -- compile and confirm output at the end of every task
- **Single source of truth** -- `prism/manuscript.tex` is authoritative; talks and supplements derive from it
- **Quality gates** -- weighted aggregate score; nothing ships below 80/100; see `quality.md`
- **Worker-critic pairs** -- every creator has a paired critic; critics never edit files
- **[LEARN] tags** -- when corrected, save `[LEARN:category] wrong → right` to MEMORY.md

---

## Getting Started

1. Run `/discover interview fentanyl public opinion` to build the research specification
2. Or run `/new-project fentanyl policy preferences` for the full orchestrated pipeline

---

## Folder Structure

```
poll-2025-clo/
├── CLAUDE.MD                    # This file
├── .claude/                     # Rules, skills, agents, hooks
├── Bibliography_base.bib        # Centralized bibliography
├── Paper/                       # Main LaTeX manuscript (source of truth)
│   ├── main.tex                 # Primary paper file
│   └── sections/                # Section-level .tex files
├── Talks/                       # Derivative Beamer presentations
│   ├── job_market_talk.tex      # 45-60 min, full results
│   ├── seminar_talk.tex         # 30-45 min, standard seminar
│   ├── short_talk.tex           # 15 min, conference session
│   └── lightning_talk.tex       # 5 min, spiel/elevator pitch
├── Data/                        # Project data
│   ├── raw/                     # Original survey files (gitignored)
│   ├── cleaned/                 # Processed datasets ready for analysis
│   └── codebooks/               # Variable documentation and survey instruments
├── Output/                      # Intermediate results (logs, temp files)
├── Figures/                     # Final figures (.pdf, .png) referenced in paper
├── Tables/                      # Final tables (.tex) referenced in paper
├── Supplementary/               # Online appendix and supplements
├── Replication/                 # Replication package for deposit
├── Preambles/header.tex         # LaTeX headers / shared preamble
├── scripts/                     # Analysis code (R, Stata, Python, Julia)
├── quality_reports/             # Plans, session logs, reviews, scores
├── explorations/                # Research sandbox (see rules)
├── templates/                   # Session log, quality report templates
└── master_supporting_docs/      # Reference papers and data docs
```

---

## Commands

```bash
# Paper compilation (3-pass, XeLaTeX + biber — biblatex-chicago requires biber)
cd prism && xelatex -interaction=nonstopmode manuscript.tex
biber manuscript
xelatex -interaction=nonstopmode manuscript.tex
xelatex -interaction=nonstopmode manuscript.tex

# Alternative: pdfLaTeX (preamble auto-detects via iftex)
cd prism && pdflatex -interaction=nonstopmode manuscript.tex
biber manuscript
pdflatex -interaction=nonstopmode manuscript.tex
pdflatex -interaction=nonstopmode manuscript.tex

# Talk compilation (when talks exist)
cd Talks && TEXINPUTS=../Preambles:$TEXINPUTS xelatex -interaction=nonstopmode talk.tex
```

---

## Quality Thresholds

| Score | Gate | Applies To |
|-------|------|------------|
| 80 | Commit | Weighted aggregate (blocking) |
| 90 | PR | Weighted aggregate (blocking) |
| 95 | Submission | Aggregate + all components >= 80 |
| -- | Advisory | Talks (reported, non-blocking) |

See `quality.md` for weighted aggregation formula.

---

## Skills Quick Reference

| Command | What It Does |
|---------|-------------|
| `/new-project [topic]` | Full pipeline: idea → paper (orchestrated) |
| `/discover [mode] [topic]` | Discovery: interview, literature, data, ideation |
| `/strategize [question]` | Identification strategy or pre-analysis plan |
| `/analyze [dataset]` | End-to-end data analysis |
| `/write [section]` | Draft paper sections + humanizer pass |
| `/review [file/--flag]` | Quality reviews (routes by target: paper, code, peer) |
| `/revise [report]` | R&R cycle: classify + route referee comments |
| `/talk [mode] [format]` | Create, audit, or compile Beamer presentations |
| `/submit [mode]` | Journal targeting → package → audit → final gate |
| `/tools [subcommand]` | Utilities: commit, compile, validate-bib, journal, etc. |

---

## Beamer Custom Environments (Talks)

*Not yet configured — will be set up when talks are created.*

---

## Current Project State

| Component | File | Status | Description |
|-----------|------|--------|-------------|
| Paper | `prism/manuscript.tex` | draft complete | "When Warm Feelings Harden" — issue-bounded conditionality, warmth × blame |
| Preamble | `prism/manuscript-preamble.tex` | complete | Chicago author-date (biblatex-chicago/biber), XeLaTeX + pdfLaTeX dual support |
| Bibliography | `prism/references.bib` | complete | Zotero-exported, ~50+ entries |
| Tables | `prism/tables/` | complete | 7 main tables + 8 appendix tables |
| Figures | `prism/figures/` | complete | 4 main figures + 2 appendix figures |
| Data | `Data/raw/` | not yet placed | Original 2024 and 2025 YouGov survey waves |
| Analysis | `scripts/R/` | not yet placed | R scripts for survey-weighted analysis |
| Replication | `Replication/` | not started | Replication package for deposit |
| Talks | `Talks/` | -- | Not yet planned |
