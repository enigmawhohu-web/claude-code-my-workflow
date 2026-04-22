# Falsification and Placebo Tests
**Project:** Poll-2025-CLO
**Date:** 2026-04-09

---

## Tests Already Implemented

### F1. Seven-probe boundary test (Step 3 — primary falsification)
**Logic:** If S+A classification reflects IBC rather than generalized endorsement, S+A-versus-S-only contrast should be near-zero for all 7 boundary probes. The boundary probes are explicitly chosen to violate one or both IBC scope conditions (tool-structure for security probes; problem-structure for non-crisis probes).
**Prediction under null (generalized acquiescence):** Δ_s ≈ Δ_d for all s (uniform positive effect across all probes).
**Prediction under IBC:** Δ_d >> 0; Δ_s ≈ 0 for s ≠ d.
**Result:** Δ_d = +0.282 (p = 0.002); boundary mean = +0.042 (p = 0.337); omnibus divergence = +0.240 (p = 0.008). TOST confirms boundary mean is negligibly small (p = 0.009). Consistent with IBC.
**Status:** Implemented in 05_probes.R; reported in main Table 4.

### F2. Sanctions × international cooperation placebo DV (Step 2)
**Logic:** If the blame → S+A effect is specific to the combination of sanctions with Chinese-government-action demands, then replacing the China-specific action item with a non-bilateral cooperation item (improved international cooperation) should attenuate or eliminate the blame effect.
**Prediction under tool-specificity:** Blame effect on placebo DV significantly smaller than on S+A.
**Prediction under leverage orientation (paper's reframing):** Both pairings represent engagement-compatible pressure; blame activates a lever orientation that applies to either cooperative item.
**Result:** Blame contrast on placebo ≈ Δ = 0.327, comparable to S+A Δ = 0.306. This weakens tool-level specificity but does not dislodge domain-level specificity (which relies on Step 3, not Step 2).
**Status:** Implemented in 04_mechanisms.R; reported in Appendix Figure A2 and app_tab_placebo_models.

### F3. Endorsement-count acquiescence control (Step 2)
**Logic:** If the S+A blame effect is entirely driven by a general tendency to endorse survey items, controlling for endorsement count should eliminate the blame effect.
**Prediction under generalized acquiescence:** Blame contrast ≈ 0 after controlling for n_approaches.
**Prediction under genuine preference differentiation:** Blame contrast attenuates but remains positive and interpretable.
**Result:** ~38% attenuation (Δ drops from 0.306 to 0.191 among warm respondents). Effect persists. Consistent with genuine differentiation, not pure acquiescence.
**Status:** Implemented in 04_mechanisms.R; reported in Appendix Table A3.

---

## Tests to Add (recommended)

### F4. Placebo × panorama cross-test
**Logic:** Apply the Step 3 stacked OLS panorama design to the placebo DV (sanctions × international cooperation). If the placebo S+A classification also shows domain-specific concentration (Δ_d >> Δ_boundary), this undermines the claim that domain specificity is a property specific to the fentanyl-cooperation S+A category. If the placebo shows a broader distribution across probes, the main finding is strengthened.
**Prediction under IBC:** The China-specific S+A classification shows concentration on probe d; the placebo classification does not (because it is not anchored to a China-specific compliance demand).
**Implementation:** Restrict to "Sanctions+IntlCoop" vs "Sanctions-only" from `placebo_tool_config`, run same stacked design in 05_probes.R.
**Status:** Not currently implemented. High priority.

### F5. Threat-perception falsification (Step 1)
**Logic:** The image-theory baseline predicts that blame should independently predict posture (main effect). The paper finds b_blame = -0.05 (p = 0.096), a small and marginally insignificant direct effect. A falsification would confirm that the main effect is substantively negligible: run the regression without the interaction term and report the blame main-effect estimate. If blame alone is small, this rules out a simple "threat activation" account.
**Prediction under IBC:** Blame has a small, non-significant direct effect on posture; the interaction with warmth is the key quantity.
**Prediction under threat-activation baseline:** Blame has a large, significant direct effect on posture independent of warmth.
**Implementation:** Rerun Step 1 models without the `warmth_z × blame_china` interaction; report blame main effect. Already partially visible in the main results (Table 2 main effect), but a clean falsification framing would sharpen the contrast with the image-theory prediction.
**Status:** Inferrable from existing Table 2 but not framed as a falsification. Add as footnote or brief paragraph in §4.1.

### F6. Non-fentanyl issue domain probe as "action-specific" test (within Step 3)
**Logic:** Among the boundary probes, probe f (China stopped selling low-cost electronics to the US) involves a China-specific economic action that could plausibly be read as a compliance scenario (China withdrawing from the US market → could China be pressured to resume?). If Δ_f ≈ 0, this strengthens the claim that domain specificity (health crisis + traceable blame) rather than any China-action framing drives the target contrast. If Δ_f > 0, it would raise the concern that "China doing something" is sufficient to activate the contrast, regardless of the IBC scope conditions.
**Status:** Δ_f is reported in the panorama table. Needs to be explicitly called out in the text as a within-boundary comparison that tests whether action-framing alone is sufficient.

### F7. Non-China bilateral cooperation item test
**Logic:** If the S+A pattern reflects a leverage orientation toward China specifically, the pattern should not appear when the "action" item in the tool pair refers to a non-China actor (e.g., "requiring action by the Mexican government"). If the paper has a Mexican-government action item in the instrument, using it to construct an analogous Sanctions × Mexico-Action DV and testing whether blame attribution to China predicts that pairing would directly test whether the blame-cooperation linkage is China-specific.
**Implementation:** Check `approach_mexico_action` variable visible in 04_mechanisms.R `approach_vars_all`. If this is a survey item on the same checklist, construct Sanctions × Mexico-Action DV and test.
**Status:** Variable `approach_mexico_action` exists in the data. Not currently used as a falsification. Medium priority.

---

## Summary Table

| Test | Step | Addresses | Status |
|------|------|-----------|--------|
| F1. 7-probe boundary | 3 | Generalized acquiescence | Implemented (main) |
| F2. Placebo DV (intl coop) | 2 | Tool specificity | Implemented (appendix) |
| F3. Endorsement count control | 2 | Generalized acquiescence | Implemented (appendix) |
| F4. Placebo × panorama | 3 | Domain specificity of placebo | Not implemented — add |
| F5. Blame main effect only | 1 | Threat-activation baseline | Partially visible — reframe |
| F6. Electronics probe (Δ_f) | 3 | Action-framing vs scope conditions | Exists — needs callout |
| F7. Sanctions × Mexico-action | 2 | China-specificity of blame-leverage link | Not implemented — check instrument |
