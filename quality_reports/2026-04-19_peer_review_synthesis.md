# Peer Review Synthesis — "When Warm Feelings Harden"

**Date:** 2026-04-19
**Manuscript:** `prism/manuscript.tex` (48 pages, clean compile)
**Target journal:** Public Opinion Quarterly (POQ)
**Referees dispatched:** domain-referee + methods-referee (blind, independent)

---

## Verdict

| Referee | Score | Recommendation |
|---------|-------|---------------|
| Domain-referee | 73/100 | **Major Revisions** |
| Methods-referee | 69/100 | **Major Revisions** |
| **Paper quality (average)** | **71/100** | **Major Revisions** |

**Editorial decision (Orchestrator synthesis):** **Major Revisions.** Both referees converge on Major Revisions with three overlapping MAJOR concerns. Neither recommends rejection; both acknowledge theoretical interest and rigorous analytic scaffolding. Convergence on the placebo failure is the single most important signal — this is not an idiosyncratic reviewer concern.

---

## Weighted Project Score (Current State)

| Component | Weight | Score | Weighted |
|-----------|--------|-------|----------|
| Literature coverage | 10% | n/a (not re-scored this cycle) | — |
| Data quality | 10% | n/a | — |
| Identification validity | 25% | n/a | — |
| Code quality (coder-critic) | 15% | 88 | 13.2 |
| Paper quality (referees) | 25% | 71 | 17.75 |
| Manuscript polish (writer-critic) | 10% | 95 | 9.5 |
| Replication (verifier) | 5% | PASS (100) | 5.0 |

Renormalizing over scored components (15+25+10+5 = 55%):
**Adjusted weighted aggregate: (13.2 + 17.75 + 9.5 + 5.0) / 0.55 = 81.7/100**

**Gate status:** Passes COMMIT (>=80), below PR (90) and SUBMISSION (95). Paper quality component of 71 falls below the 80 per-component minimum for submission. **Blocked from submission until paper quality reaches 80+.**

---

## Convergence Map — Both Referees Agree

These are the MAJOR issues that both referees independently flagged. High priority for revision.

### M-1. Placebo failure undermines the instrument-specificity claim
- **Domain-referee Concern 1:** "The paper's defense cannot stand as written. The paper's own abstract, introduction, and theory section explicitly advance a claim about instruments, not merely about domains... If a sanctions-plus-generic-cooperation pairing triggers the same pattern, the paper has not demonstrated that the S+A instrument combination is theoretically meaningful."
- **Methods-referee Concern 3:** "The paper must also clarify what empirical evidence would falsify the claim of tool-specificity given that the current placebo does not. Domain-specificity (the 8-probe result) survives, but the tool-specificity claim does not."
- **Classification:** **DISAGREE → NEW ANALYSIS** (hybrid). The substantive claim that "engagement-compatible pressure" survives is defensible but currently unfalsifiable. User decision required on framing; new analysis required to report the placebo interaction at the full warmth × blame grid.

### M-2. Question wording not reproduced verbatim (POQ requirement)
- **Domain-referee Concern 2:** "POQ referees treat this as a non-negotiable standard... The exact wording of the warmth item, the blame attribution battery (including all response options in the multi-select), the four posture items... must be reproduced verbatim."
- **Methods-referee Moderate 5/Minor 11:** Cronbach's alpha, item wording, and PC1 not reported.
- **Classification:** **CLARIFICATION** (writer task). Pull from codebooks in `Data/codebooks/`, add a Measurement Appendix.

### M-3. 15% listwise deletion: extend MI to full MNL and specificity results
- **Domain-referee Concern 3:** "The paper should report the full multinomial and specificity results under multiple imputation, not only the OLS interaction coefficient."
- **Methods-referee Minor 13:** Listwise deletion direction of bias must be discussed explicitly.
- **Classification:** **NEW ANALYSIS** (coder task). Extend `07_mi_check.R` (or equivalent) to compute pooled estimates for Phase 4 MNL, Table 3 predicted probabilities, and Phase 5 probe contrasts.

---

## Referee-Specific MAJOR Comments

### Domain-referee only

| # | Comment | Classification | Route |
|---|---------|----------------|-------|
| DR-A | "Requiring action by the Chinese government" item construct validity — reproduce verbatim, argue why it reads as engagement | CLARIFICATION + partial DISAGREE on construct | writer + user review |

### Methods-referee only

| # | Comment | Classification | Route |
|---|---------|----------------|-------|
| MR-A | IIA heuristic inadequate — run Hausman-McFadden or adopt nested/mixed logit | NEW ANALYSIS | coder → coder-critic |
| MR-B | Acquiescence-control inconsistency across scripts (10-item vs 12-item) | NEW ANALYSIS (harmonization) + CLARIFICATION | coder + writer |
| MR-C | MTC (BH correction) table for 8-probe battery must appear in main text, not commented-out | CLARIFICATION | writer (uncomment + reference) |

---

## Moderate Comments — Classification Table

| # | Source | Comment | Classification | Route |
|---|--------|---------|----------------|-------|
| MOD-1 | Methods-ref 5 | MI m=10 with FMI=0.001: report V_B, V_W, describe imputation model | NEW ANALYSIS | coder |
| MOD-2 | Methods-ref 6 | Design-based SEs for MNL (svymultinom or bootstrap) | NEW ANALYSIS | coder |
| MOD-3 | Methods-ref 7, Domain-ref 5 | TOST equivalence-bound ε: anchor to substantive threshold | CLARIFICATION + NEW ANALYSIS (intermediate bounds 0.15, 0.20) | coder + writer |
| MOD-4 | Methods-ref 8 | Warmth×blame×year triple interaction | NEW ANALYSIS | coder |
| MOD-5 | Domain-ref 6 | Scope conditions operationalized post-hoc (Table 1 probe-scope mapping) | DISAGREE → writer for diplomatic rebuttal | user review + writer |
| MOD-6 | Domain-ref 7 | Social desirability (Busan 2025 frame); compare 2024 vs 2025 wave within-wave | NEW ANALYSIS | coder |
| MOD-7 | Domain-ref 8 | Wave-comparability: document question wording identical, pre/post-tariff context | CLARIFICATION | writer |
| MOD-8 | Domain-ref 9 | Engage Sylvester, Haeder, Callaghan (2022) more directly | CLARIFICATION | writer |

---

## Minor Comments — Classification Table

| # | Source | Comment | Classification | Route |
|---|--------|---------|----------------|-------|
| MIN-1 | Both refs | `\hline` in tab2 → booktabs | MINOR | writer |
| MIN-2 | Domain-ref 11, Methods-ref 9 | Rounding precision and SE reporting in text | MINOR | writer |
| MIN-3 | Domain-ref 12 | Replace "cooperation" with "leverage" systematically | MINOR | writer |
| MIN-4 | Domain-ref 13 | Remove JEL codes; replace with keyword list | MINOR | writer |
| MIN-5 | Domain-ref 14 | State "pooled-wave moments" at every posture_z reference | MINOR | writer |
| MIN-6 | Domain-ref 15 | Clarify IIA "unavailable" footnote | CLARIFICATION | writer |
| MIN-7 | Methods-ref 11-14 | Cronbach alpha, effective n, DEFF, listwise-deletion direction | CLARIFICATION | writer |
| MIN-8 | Methods-ref 14 | Mechanism section (Section 4.4) uses raw means; add covariate adjustment or note | NEW ANALYSIS (small) or CLARIFICATION | coder (small) or writer |

---

## Missing Literature (Domain-referee)

All routed to **librarian → writer** (CLARIFICATION):

1. Krosnick, Jon A. (1989). "Attitude Importance and Attitude Accessibility." *Psychological Bulletin*.
2. Bishop, George F. (2005). *The Illusion of Public Opinion.* Rowman & Littlefield.
3. Schuman, Howard, and Stanley Presser (1981). *Questions and Answers in Attitude Surveys.* Academic Press.
4. Berinsky, Adam J. (2002). "Silent Voices." *AJPS*.
5. Smeltz et al. Chicago Council annual surveys (systematic comparator engagement).
6. Tomz, Michael, and Jessica Weeks (2013). "Public Opinion and the Democratic Peace." *APSR*.

---

## User Decisions Required (DISAGREE items)

Per protocol, the orchestrator never autonomously pushes back on referees. Flagging:

**UD-1. Placebo reframing (from M-1 above).** Two paths:
- **(a)** Reframe contribution from instrument-specificity to domain-level blame activation. Concedes ground but honestly reports what the data show.
- **(b)** Keep instrument-specificity framing; report placebo result prominently in Results, not Discussion; add falsification-test language ("IBC predicts X; placebo does not show X; therefore tool-level IBC is not supported but domain-level IBC is").

**Recommendation:** Path (b) — report placebo prominently, separate domain vs. tool claims. The paper has domain-specificity evidence (8-probe test); it does not have instrument-specificity evidence. Be explicit.

**UD-2. Scope-condition post-hoc concern (MOD-5).** Referee argues scope assignments in Table 1 are "asserted rather than demonstrated." Response options:
- **(a)** Concede and reframe Table 1 as descriptive classification rather than theoretical derivation.
- **(b)** Diplomatic rebuttal: scope conditions were specified via theoretical text BEFORE data analysis; probe-scope mapping is a theory-derived classification, not an empirical finding. Cite the theory-development timeline.

**Recommendation:** (b) with clearer timeline in text.

**UD-3. IIA "unavailable" footnote.** Methods-referee Q1: "What specific technical constraint prevented running this test?" Options:
- **(a)** Run the test now (in the revision cycle).
- **(b)** Report what actually happened and why.

**Recommendation:** (a). The `mlogit::hmftest()` call should be attempted. If it fails, document the failure. If it succeeds, drop the heuristic.

---

## Revision Plan (Ordered by Dependency)

### Phase 1: NEW ANALYSIS (coder + coder-critic)
Estimated time: 1–2 days
1. **MR-A:** Run `mlogit::hmftest()` formal IIA test; if unavailable, fit nested logit + mixed logit robustness (5 hr)
2. **MR-B:** Harmonize acquiescence control to 10-item exclusion across all scripts + generate comparison table (2 hr)
3. **M-3:** Extend MI to Phase 4 MNL, Table 3 predicted probabilities, Phase 5 probe contrasts; report pooled estimates (4 hr)
4. **MOD-1:** Compute Rubin's V_B, V_W, describe imputation model; verify m=10 convergence (1 hr)
5. **MOD-2:** Design-based MNL SEs via `svymultinom` or survey bootstrap (3 hr)
6. **MOD-3:** TOST sensitivity at ε = 0.10, 0.15, 0.20, 0.25 × SD_boundary (1 hr)
7. **MOD-4:** Warmth × blame × year triple interaction + wave-specific estimates (1 hr)
8. **MOD-6:** Wave-specific within-2024 and within-2025 interaction estimates (1 hr)
9. **M-1:** Placebo interaction at full warmth × blame grid + acquiescence-adjusted comparison (2 hr)

### Phase 2: LITERATURE ADDITIONS (librarian → writer)
Estimated time: 4 hr
- Add 6 missing references to `references.bib`
- Writer integrates citations into Introduction / Theory / Discussion

### Phase 3: CLARIFICATION + MINOR (writer + writer-critic)
Estimated time: 1 day
- M-2: Add Measurement Appendix with verbatim question wording + Cronbach alpha + PC1
- DR-A: Strengthen construct-validity argument for "Chinese government action" item
- MR-C: Uncomment BH table reference and discuss in main text
- MOD-5 through MOD-8 (diplomatic/evidentiary responses)
- MIN-1 through MIN-7 (format/notation/reporting cleanup)
- UD-1: Reorganize Results to lead with the placebo result and separate domain vs. tool claims

### Phase 4: RE-REVIEW
- writer-critic re-scores manuscript (target 95+)
- Dispatch fresh domain-referee + methods-referee (same journal calibration)
- Re-synthesize
- Target: paper quality >= 85, overall >= 90

---

## Response Letter Outline

Per revision protocol, every revised-paper submission requires a point-by-point response. Structure:

1. **Opening:** Thank both referees; summarize the major changes (three NEW ANALYSIS blocks, one theoretical reframe, one Measurement Appendix).
2. **Major Comments (Domain + Methods, interleaved by topic):** Each cited verbatim; response describes the specific change; page/line reference to the revision.
3. **Moderate Comments:** Same structure, more compact.
4. **Minor Comments:** Brief acknowledgment; page/line reference.
5. **Missing Literature:** List of added references with one-sentence integration.
6. **Questions:** Each answered directly (Q1–Q7 across both referees).

Response letter lives at `Replication/response_letter.tex` (to be drafted after Phase 3 above).

---

## Triage Priority (if time-constrained)

**Must address (blocks submission):** M-1, M-2, M-3, MR-A, MR-B, UD-1.
**Should address (affects score):** MR-C, MOD-1, MOD-2, MOD-3, MOD-4, DR-A.
**Can defer/compress:** MOD-5 through MOD-8, minor comments.

---

**Orchestrator recommendation:** Execute Phase 1 NEW ANALYSIS block in a single coder invocation (one plan, one pass). Then Phase 2–3 in the writer. Then re-dispatch both referees. Do not submit revised paper to POQ until paper quality >= 85.

*Synthesis produced by Orchestrator. Neither referee has seen the other's report.*
