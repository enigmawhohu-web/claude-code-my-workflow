# Peer Review — "When Warm Feelings Harden"

**Date:** 2026-03-19
**Manuscript:** `prism/manuscript.tex`

---

## Editorial Decision: Major Revisions

**Aggregate score:** 70/100 (domain: 72, methods: 68)

Both referees find the paper's core concept interesting and the empirical architecture creative, but converge on several issues that require substantive revision before the paper can advance. The concerns are addressable — none is fatal — but they collectively require more than cosmetic changes.

---

## Convergent Concerns (raised by both referees)

These are the highest-priority items — both reviewers flagged them independently:

### 1. The placebo result undermines tool-specificity
The sanctions x international-cooperation placebo produces a comparable blame-driven shift (Delta = 0.327 vs. 0.306). Both referees see this as the paper's most significant weakness. The distinction between "tool-specificity" (weak) and "domain-specificity" (strong) needs to be elevated from a discussion caveat to a central structural feature of the argument. Consider whether "issue-bounded conditionality" needs to be reframed as "domain-concentrated conditionality" or whether additional tests can rescue tool-specificity.

### 2. Post-treatment bias in Step 2 (conditioning on posture_z)
Both referees identify this as a serious analytic concern. Conditioning on posture_z — which is itself a function of warmth and blame — blocks part of the causal pathway. **Action required:** Present the MNL *without* posture_z as the primary specification, and the conditioned version as a "within-posture" sensitivity check. This is likely the most straightforward revision.

### 3. The 38% acquiescence attenuation is underplayed
Endorsement controls reduce the S+A blame effect substantially. Both referees want the corrected estimates treated as the primary estimates (or at least co-equal), with explicit discussion of what effect size is "large enough" to support the theoretical claims.

### 4. Listwise deletion (15.3%) with non-random missingness
Dropped respondents are younger, warmer, and less likely to blame China — exactly the counterfactual-relevant subgroup. **Action required:** At minimum, report the ideology-omitted sensitivity analysis in an appendix table. Better: implement multiple imputation or IPW as a robustness check.

### 5. Eight scenario probes not described in main text
Neither referee could fully evaluate the falsification/specificity test because the seven boundary probes are never described. **Action required:** Add a summary table of all eight probes (at minimum in the main text, with full question wording in appendix).

### 6. Table formatting
Tables use `\hline` instead of `booktabs`, and variable names appear as raw R output (`warmth\_z`, `race\_ethBlack`). Not publication-ready.

---

## Domain Referee — Unique Concerns

| # | Concern | Severity |
|---|---------|----------|
| D1 | Theoretical framework is structural/observational, not mechanistic — cannot distinguish IBC from domain-specific social desirability or media framing effects | Major |
| D2 | Scope conditions are post hoc — paper should identify other crises that satisfy/violate them to make the framework falsifiable | Major |
| D3 | No R-squared reported for OLS specifications | Minor |
| D4 | Year FE coefficient (0.11) deserves substantive discussion | Minor |
| D5 | Asian American coefficient (-0.30) deserves a sentence | Minor |
| D6 | "I" vs. "we" inconsistency | Minor |
| D7 | Typos: "Conceputal" (Sec 2.2 heading), "deminsions" (Introduction) | Minor |
| D8 | Missing literature: Tomz & Weeks (2013), Kertzer (2022 AJPS), Coppock & McClellan (2019) on YouGov, Mutz & Kim (2017), Whang/McLean/Kuber (2013), Peksen (2019) | Minor |

**Domain referee questions for authors:**
1. Can you report MNL results *without* posture_z conditioning?
2. What are the eight scenario probes? Provide question wording.
3. Has the author tested whether the placebo and main S+A coefficients are statistically distinguishable?
4. Can you conduct MI or bounding for the 15.3% missing data?
5. Does the warmth x blame interaction differ for Asian Americans?
6. Can you report 3-way party x warmth x blame point estimates, even if underpowered?
7. Can you identify ex ante which other bilateral crises satisfy/violate your scope conditions?

---

## Methods Referee — Unique Concerns

| # | Concern | Severity |
|---|---------|----------|
| M1 | "Falsification test" is mislabeled — it's a specificity/concentration test, not a falsification test in the econometric sense | Major |
| M2 | Model-based SEs throughout a complex survey design — should use `svyglm`/design-based inference, at least for Steps 1 & 3 | Major |
| M3 | TOST equivalence bound (0.10 x SD) is aggressive; needs justification vs. the 0.25 x SD alternative | Minor |
| M4 | P_hat(S+A) continuous specification has generated-regressor problem (SE=0.995, p=0.614) — needs bootstrap or removal | Minor |
| M5 | BH-FDR: only 2/7 pairwise comparisons survive correction — paper should be transparent about this | Minor |
| M6 | Alternative blame measure (Panel B) loses significance in wave-specific models — more prominent discussion needed | Minor |
| M7 | Dispersion parameters below 1.0 (0.68-0.75) suggest underdispersion; discuss OLS variance assumption | Minor |
| M8 | Stacked OLS: clarify whether respondent effects are fixed or random, and whether covariates are fully interacted with scenario | Minor |
| M9 | IIA assumption in MNL not tested — consider multinomial probit or Hausman test | Minor |
| M10 | Rationale for excluding Neither/Action-only from 8-probe battery? Including them would increase power | Minor |

**Methods referee questions for authors:**
1. What is the effective unweighted sample size for the warm-and-blaming subgroup?
2. Why not design-based SEs as the primary framework?
3. Are placebo and main S+A coefficients statistically distinguishable?
4. In stacked OLS, are covariates fully interacted with scenario indicators?
5. Why exclude Neither and Action-only respondents from the 8-probe analysis?

---

## Suggested Revision Priority

### Must address (blocking)
1. Present MNL without posture_z as primary specification
2. Restructure the placebo discussion — elevate from caveat to central limitation
3. Relabel "falsification test" as "specificity test" or "concentration test"
4. Report acquiescence-adjusted estimates more prominently
5. Add probe description table to main text
6. Report sensitivity analysis for missing data (at minimum, ideology-omitted results in appendix)

### Should address (expected)
7. Use design-based SEs for OLS models (svyglm)
8. Clean table formatting (booktabs, human-readable labels)
9. Fix typos
10. Add missing literature citations
11. Justify TOST equivalence bound choice
12. Discuss alternative blame measure's weakness in wave-specific models
13. Discuss year FE and Asian American coefficients

### May address (strengthening)
14. Multiple imputation for missing data
15. IIA test or multinomial probit robustness
16. Bootstrap P_hat specification or drop it
17. 3-way party interaction point estimates
18. Scope condition predictions for other bilateral crises

---

## Scores

| Dimension | Domain (wt) | Methods (wt) |
|-----------|-------------|--------------|
| Contribution / Design | 72 (30%) | 62 (30%) |
| Literature / Estimation | 78 (20%) | 72 (25%) |
| Arguments / Inference | 65 (25%) | 65 (20%) |
| Ext. Validity / Robustness | 68 (15%) | 75 (15%) |
| Writing / Transparency | 82 (10%) | 78 (10%) |
| **Weighted total** | **72** | **68** |
| **Average** | **70** | |
