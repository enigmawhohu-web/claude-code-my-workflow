# Phase 4 Re-Review Synthesis — "When Warm Feelings Harden"

**Date:** 2026-04-21
**Round:** 2 (response to Major Revisions)
**Target journal:** Public Opinion Quarterly (POQ)

---

## Scores

| Referee | Score | Recommendation |
|---------|-------|----------------|
| Domain-referee | 81/100 | Accept with Minor Revisions |
| Methods-referee | 79/100 | Major Revisions (targeted) |
| **Paper quality (average)** | **80/100** | **Major Revisions — targeted** |

**vs. Round 1:** Domain 73→81 (+8), Methods 69→79 (+10). Clear progress but paper quality still below the 85 target.

---

## Editorial Decision

**Minor-Major split: targeted second round of revisions.**

The paper has made genuine progress. The domain referee is ready to accept with minor revisions; the methods referee wants one more targeted pass on three methodological issues. These are not new concerns — they arise directly from the new analyses added in Phase 1 (IIA formal test, MI extension, design-based MNL) and involve either a misinterpretation in the text or incomplete reporting of a new result.

---

## Convergence Map — Both Referees Agree

| # | Issue | Classification | Priority |
|---|-------|----------------|----------|
| C-1 | M-2: verbatim question wording still a TODO placeholder | CLARIFICATION (writer) | BLOCKER |
| C-2 | IIA rejection text: "confirms distinctiveness" is methodologically incorrect | CLARIFICATION (writer) + NEW ANALYSIS (nested logit) | MAJOR |
| C-3 | MI Phase 5 convergence failure for Δ_d; sign flip in composite boundary; no individual probe MI estimates | NEW ANALYSIS (coder) | MAJOR |

---

## Domain-Only Moderate/Minor

| # | Issue | Classification |
|---|-------|----------------|
| D-1 | Design-based MNL interaction p=0.240 should be noted in main text (not just limitations) | CLARIFICATION (writer) |
| D-2 | Placebo equivalence p-value (p=0.758) should appear in main text, not just appendix | MINOR (writer) |
| D-3 | Acquiescence table: note which endorsement-count definition is used (10-item or 12-item) | MINOR (writer) |
| D-4 | 2025 interaction amplification (b=0.185 vs 0.096) — add theoretical engagement in Discussion | CLARIFICATION (writer) |

---

## Methods-Only Issues

| # | Issue | Classification |
|---|-------|----------------|
| M-1 | IIA rejection: no nested logit alternative estimated | NEW ANALYSIS (coder) |
| M-2 | Design-based SEs not translated to probability scale; interaction p=0.240 | NEW ANALYSIS (small coder task) |
| M-3 | MI diagnostics: df values implausibly large (millions) — should be capped at complete-data df | NEW ANALYSIS (coder) |
| M-4 | TOST power not discussed (given SE≈bound, limited power to detect non-negligible effects) | CLARIFICATION (writer) |
| M-5 | Within-wave vs. triple interaction numerical inconsistency (0.096 vs 0.104) | MINOR (coder to reconcile) |

---

## Phase 5 Revision Plan

### Phase 5a: NEW ANALYSIS (coder) — UPDATED 2026-04-22
1. ~~**Nested logit**~~ **REMOVED** — per user direction, nested logit is too econometric for the political-science / area-studies audience (POQ, JCC, ISQ). The IIA concern is addressed via revised text in Phase 5b item 2 instead. The mlogit fit also failed to converge in the prototype run, reinforcing the decision.
2. **Design-based SEs on probability scale** — delta method or margins from binary svyglm; produce direct SE comparison table → `app_tab_design_mnl_prob.tex` ✅
3. **MI Phase 5 target probe fix** — diagnose convergence failure; implement alternative (impute predictors only, recompute is_SA from MNL probs; or hot-deck); report Δ_d under MI → `app_tab_mi_phase5_v2.tex` ✅
4. **MI df cap** — apply Barnard-Rubin df cap at complete-data df (n-k ≈ 4,218) in diagnostics table → `app_tab_mi_diagnostics_v2.tex` ✅
5. **Within-wave/triple inconsistency** — reconcile 0.096 vs 0.104 (2024 coefficient across tables); confirm same sample and covariates → log-only (no new table) ✅

### Phase 5b: CLARIFICATION + MINOR (writer)
1. ~~**M-2** — Measurement Appendix~~ **SKIPPED 2026-04-22** — verbatim question-wording appendix de-scoped per user direction. Question wording is already documented in the YouGov instruments (referenced in §3.1) and partially excerpted in main text where definitionally needed. Domain-referee weight on this item is modest; deferred to potential R&R round if a referee specifically demands it.
2. **IIA text fix (revised, no nested logit)** — correct the "confirms distinctiveness" misinterpretation. Replace with: rejection of IIA implies a non-proportional substitution structure (i.e., dropping one option would shift probability mass unevenly across the others) rather than independence per se; main MNL estimates therefore remain informative as conditional associations within the four-alternative menu, but should not be extrapolated to settings where the option set changes. No nested-logit alternative is presented — the conceptual clarification is the fix.
3. **D-1** — Note design-based interaction p=0.240 in main text §4.2 or footnote
4. **D-2** — Add placebo p-value in main text §5 ¶2 ✅ (already present: "p = 0.758" in line 323; verified 2026-04-22)
5. **D-4** — Add sentence in Discussion engaging 2025 amplification (post-tariff context) ✅ (§5 ¶3, line 328 — wave-specific b values + descriptive vs. statistical attribution caveat)
6. **M-4** — Add TOST power note in footnote ✅ (§3 footnote at line 156 — effective n, clustering, robustness across bound choices)
7. **D-3** — Clarify acquiescence-count item definition in table notes ✅ (Appendix Table app_acquiescence note expanded — definition + exclusion of S+A constituent items + within-wave computation)

---

## Remaining Score Gap

Current paper quality: **80/100**
Target for submission: **≥ 85/100**
Gap: **5 points**

The gap is concentrated in two areas:
- Methods (IIA misinterpretation + MI Phase 5): ~6 points of methods deduction
- M-2 question wording: ~5 points of domain deduction

Resolving C-1 (M-2), C-2 (IIA text + nested logit), and C-3 (MI Phase 5 fix) should recover ≈10 points across both referees → projected score ~88–90/100.

