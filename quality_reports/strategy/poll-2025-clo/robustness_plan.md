# Robustness Plan
**Project:** Poll-2025-CLO
**Date:** 2026-04-09

---

## Already Implemented (confirm in replication package)

| Check | Step | Script | Status |
|-------|------|--------|--------|
| Alternative blame measure (`blame_china_alt`, single-choice) | 1 | 03_models.R Panel B | Implemented |
| Behavior index substituted for blame in interaction | 1 | 03_models.R appendix | Implemented |
| Ordinal logistic on individual posture items | 1 | 03_models.R app_tab_phase3_svyolr | Implemented |
| Acquiescence control (n_approaches_main + miss indicator) | 2 | 04_mechanisms.R | Implemented |
| Acquiescence control (n_approaches_zero, zero-imputed) | 2 | 04_mechanisms.R | Implemented |
| Placebo DV (sanctions × international cooperation) | 2 | 04_mechanisms.R | Implemented |
| MNL full coefficients reported | 2 | 04_mechanisms.R app tab | Implemented |
| BH-FDR pairwise divergence tests | 3 | 05_probes.R app tab | Implemented |
| Alternative continuous P̂(S+A) in place of binary classifier | 3 | 05_probes.R app_phat | Implemented |
| Zero-imputed endorsement counts | 3 | 05_probes.R app_zero | Implemented |
| Full twelve-item endorsement count | 3 | 05_probes.R app_nexcl | Implemented |
| TOST wider bound (ε = 0.25 × SD) | 3 | 05_probes.R | Implemented |
| Ideology-omission sensitivity (recovers listwise-dropped cases) | All | 03_models.R | Implemented |

---

## Priority Additions (address key threats)

### R1. Bootstrap SEs for MNL blame contrast (Step 2)
**Rationale:** Model-based SEs from `nnet::multinom` do not reflect survey design. Bootstrapped SEs using the survey weights will quantify whether model-based SEs understate uncertainty.
**Implementation:** Resample respondents with replacement (B = 1000), refit MNL, extract P̂(S+A | blame=1) − P̂(S+A | blame=0) at each warmth value. Report SD of bootstrap distribution alongside model-based SE.
**Expected outcome:** If bootstrap SE ≈ model-based SE, no further action needed. If bootstrap SE > model-based SE by >20%, update reported CIs for key blame contrasts.

### R2. IIA diagnostic for MNL (Step 2)
**Rationale:** S+A and Sanctions-only may violate IIA. No diagnostic is currently reported.
**Implementation:** Run Hausman-McFadden test (drop each alternative in turn). Alternatively, refit key specification using `mlogit::mlogit()` with probit kernel and compare blame-effect estimates.
**Expected outcome:** If IIA passes (p > 0.10), report test statistic in footnote. If IIA fails, consider nested logit grouping S+A and Sanctions-only as a "coercive" nest with cooperation dimension as within-nest choice.

### R3. Multiple imputation for listwise deletion
**Rationale:** 15.3% listwise deletion is non-random; dropped respondents are younger, warmer, and less likely to blame China. This potentially attenuates the interaction estimate.
**Implementation:** `mice` package, m = 20 imputations, predictive mean matching on ideology (primary source of missingness). Pool estimates via Rubin's rules. Report MI results alongside listwise-deletion estimates for the main Step 1 and Step 2 specifications.
**Expected outcome:** If MI estimates are within ±0.02 of listwise estimates, the attrition concern is addressed. Larger divergence should be reported and discussed.

### R4. Wave × interaction test (Step 1)
**Rationale:** The 2024 and 2025 waves reflect different political environments (fentanyl tariffs, Busan agreement). If the interaction is significantly stronger in one wave, it would enrich the finding and motivate the panel extension.
**Implementation:** In the pooled model, add a three-way interaction `warmth_z × blame_china × year`. Report F-test for the three-way term.
**Expected outcome:** If the three-way interaction is insignificant, pooling is justified and the finding is robust to political context variation.

### R5. Placebo panorama test (Step 3 applied to placebo DV)
**Rationale:** The placebo DV (sanctions × international cooperation) shows a blame contrast comparable to S+A. If the placebo also shows domain-specific concentration in the panorama test (concentrated on probe d only), this would undermine the claim that domain specificity is a property of the fentanyl-specific S+A classification. If the placebo shows a different pattern (broader distribution across probes), this would strengthen the main finding.
**Implementation:** Using `placebo_tool_config` from `04_mechanisms.R`, restrict to "Sanctions+IntlCoop" vs "Sanctions-only" respondents and run the same stacked OLS panorama design in `05_probes.R`. Report Δ_d and Δ_boundary for the placebo.
**Expected outcome:** If placebo panorama shows broader concentration (e.g., significant effects across multiple probes), the S+A classification is more domain-specific than the placebo. If concentrated similarly, the discussion of Threat 3 in the strategy memo applies.

---

## Lower Priority (but useful for completeness)

| Check | Step | Rationale | Notes |
|-------|------|-----------|-------|
| Media consumption × blame interaction | 1 | Proxy for social desirability exposure | Requires identifying a media consumption variable in the instrument |
| Fentanyl personal salience moderator | 1, 2 | Issue publics framework predicts stronger effects for personally affected respondents | `exposure_index` is already in controls; test as moderator |
| Coarsened exact matching on covariates | 1 | Reduce confounding from demographic imbalance between blame=0 and blame=1 cells | Not necessary given explicit associational framing, but available if referee presses |
| Probe framing coding table | 3 | Document that boundary probes differ from target on scope conditions, not on concreteness/valence alone | Narrative addition, not new estimation |
| Sensitivity to equivalence bound ε | 3 | ε = 0.10 × SD is liberal; report at ε = 0.05 × SD and ε = 0.25 × SD | ε = 0.25 already in appendix; add ε = 0.05 |
