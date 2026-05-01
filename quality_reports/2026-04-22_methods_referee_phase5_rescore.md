# Methods Referee Report — Phase 5 Re-score

**Date:** 2026-04-22
**Paper:** "When Warm Feelings Harden" — Issue-bounded conditionality, warmth × blame
**Calibrated to:** POQ / JCC / ISQ / IO (political science / area studies — NOT econometrics journal)
**Recommendation (methods component):** **Minor Revisions**
**New Overall Score:** **86/100** (Δ = +7 vs. prior 79)

## Summary

Phase 5 substantively addressed the prior methods-referee concerns. The IIA reinterpretation (C-2) is now exemplary — the paper distinguishes "rejection of IIA" from "categories are distinct" with appropriate epistemic modesty, and bounds MNL inference to within-menu associations. The design-based vs. model-based SE comparison on the probability scale (M-2) is informative and the right thing to show. MI on Phase 5 probes (C-3) now produces populated estimates with sensible documentation of the Approach B rationale. Barnard-Rubin df cap (M-3) is correctly handled. The D-1 footnote noting that the warmth × blame interaction at the MNL level is not design-significant is the kind of disclosure POQ/IO referees reward. Remaining concerns are second-order and largely interpretive rather than methodological.

## Dimension Scores

| Dimension | Weight | Score | Notes |
|-----------|--------|-------|-------|
| Identification | 35% | 84 | Correctly framed as associational; scope conditions pre-specified; no overclaiming |
| Estimation | 25% | 87 | MNL appropriately bounded; design-vs-model SE comparison adds rigor |
| Inference | 20% | 86 | Design-based SEs on probability scale, MI Barnard-Rubin cap, TOST sensitivity all addressed |
| Robustness | 15% | 88 | Within-wave, triple-interaction, placebo, MI, IIA, post-treatment, acquiescence — comprehensive |
| Replication | 5% | 82 | C-1 (verbatim wording) deferred; otherwise documented |
| **Weighted** | 100% | **86** | |

## Resolution of Prior Concerns

| Code | Verdict | Notes |
|------|---------|-------|
| C-1 (verbatim wording) | Partial / acceptable for now | Deferred is reasonable for POQ/JCC at first submission; will be a referee ask in R&R. Deduction: −2. |
| C-2 (IIA misreading) | Resolved | The §4.2 footnote and Appendix NA-3 text are now near-best-practice. Within-menu interpretation is correct. |
| C-2a (nested logit) | Resolved | Appropriate to drop given audience; convergence rationale documented. |
| C-3 (MI Phase 5) | Resolved | Approach B documented; recomputing outcome from MNL probabilities is non-standard but defensible. Deduction: −1. |
| M-2 (design-based prob-scale SE) | Resolved | Table app_tab_design_mnl_prob is the right artifact. SE ratio striking; worth a sentence in main text §4.2. Deduction: −1. |
| M-3 (Barnard-Rubin df cap) | Resolved | Cap is correctly applied and well-explained. |
| M-4 (TOST power) | Resolved | Effective n ≈ 14,768 footnote with stringent ε=0.10 SD bound is sufficient. |
| M-5 (within-wave reconciliation) | Resolved | Documented in code log. |
| D-1 (design p=0.240) | Resolved | Footnote pivoting substantive claim to precise blame main effect is a model of methodological honesty. |
| D-3 (acquiescence-count def) | Resolved | Note now spells out exclusion of S+A-defining items. |
| D-4 (2025 amplification) | Resolved | §5 ¶3 correctly characterizes between-wave amplification as descriptive. |

## Top 3 Remaining Concerns

1. **Promote the design-vs-Hessian SE-ratio finding into the main text (−2).** Currently buried in app_tab_design_mnl_prob. A single sentence in §4.2 would preempt the obvious referee question "why are you reporting model-based SEs for predicted probabilities?"

2. **Approach B (MI for Phase 5) reconstructs the outcome from imputed predictors via the MNL (−1).** This induces dependence between the imputed predictor set and the reconstructed `is_SA` outcome that pure Rubin's rules do not strictly accommodate. A one-line caveat in the table note would close this off.

3. **§4.2 ¶3 (acquiescence) and §5 ¶2 describe the 38% attenuation as a single point estimate (−2).** A bootstrap or simulation-based interval on the attenuation magnitude itself would harden the "qualitative pattern persists" claim.

## Minor Comments

- (−1) §3 footnote 156: "the test in fact rejects at every bound considered" — worth one more clause emphasizing how stringent ε=0.10 SD is.
- (−1) Probe-mapping pre-specification: state explicitly whether a PAP was filed.
- (−2) Within-wave amplification: brief sentence on alternative political shifts between 2024-03 and 2025-04 (election, partisan panel composition, etc.) beyond tariff/Busan.
- (−1) app_tab_mi_diagnostics note: state that the cap is not standard `mice` output and was applied manually.

## Technical Suggestions

- Verify that ε=0.10 SD in the TOST is computed against the *boundary* SD rather than the full-sample SD, and state which.
- For the placebo contrast, consider a paired-bootstrap version (resample respondents, refit both models) for next revision.
- For POQ specifically, the question-wording reproduction (deferred C-1) WILL come up in R&R. Worth queueing this work now.

## Questions for the Authors

1. In Approach B for Phase 5 MI, is `is_SA` reconstructed using imputed predictors fed into the *original* MNL fit on complete cases, or is the MNL re-fit within each imputation?
2. Is there wave-by-wave evidence on whether the ~38% acquiescence-count attenuation is itself stable across 2024 vs. 2025?
3. For the boundary-probe SD used in the TOST, is it the pooled boundary SD across all seven non-target probes, or the within-respondent SD?

## Verdict (methods component, alone): Minor Revisions

The Phase 5 fixes were targeted, correctly executed, and (in the case of C-2 and D-1) genuinely raised the methodological caliber of the paper. At a political science / area studies submission target, the methods now clear the bar for first-round review at POQ/IO/JCC/ISQ.

**New score: 86/100. Δ = +7 vs. prior 79.**
