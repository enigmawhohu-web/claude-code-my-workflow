# Strategy Memo: "When Warm Feelings Harden"
**Project:** Poll-2025-CLO — Fentanyl Crisis Blame and U.S. Public Opinion on China
**Date:** 2026-04-09
**Phase:** Strategy (Medium / Constructive)
**Author:** Strategist agent

---

## 1. Estimand Clarity

The paper pursues three distinct estimands across its three steps, and it is important to state them precisely.

**Step 1 (OLS — posture index).** The estimand is the conditional association between the warmth × blame interaction and the posture index (`posture_z`), holding demographics, ideology, political attention, fentanyl exposure, and year FE constant. The paper does not claim a causal average treatment effect; it explicitly frames this as associational. The estimand is therefore a conditional association — approximately a weighted average of within-cell differences in posture across the warmth × blame space. This is well-defined and appropriately modest in its framing.

**Step 2 (MNL — tool configuration).** The estimand is the blame-attributable shift in the probability of falling into the S+A (versus Sanctions-only, Action-only, or Neither) category, conditional on posture_z. Because posture_z is included as a control and it is itself downstream of blame, this is a **within-posture composition effect** — the paper says so explicitly. This is the right way to interpret Step 2, but it is a substantially narrower estimand than it first appears. It is not an estimate of blame's total effect on tool configuration; it is the effect of blame on tool composition conditional on having a given posture level. The paper acknowledges this, though it may need to be stated more prominently in the results narrative.

**Step 3 (stacked OLS — probe panorama).** The estimand is the scenario-specific S+A-versus-S-only contrast (Δ_s) across 8 probes, and the omnibus divergence Δ_d − (1/7)Σ Δ_x. This tests whether the classification is domain-specific. The estimand is clearly defined and the restriction to S+A and Sanctions-only respondents is well-motivated. The key quantity is sign-and-magnitude of Δ_d relative to boundary Δ values.

**Overall verdict on estimand clarity:** Sound. The three steps address distinct estimands that correspond to distinct theoretical claims. The main area for tightening is the within-posture framing in Step 2, which deserves a more prominent flag in the main text.

---

## 2. Design Validity and Key Assumptions

### Step 1 — Survey-weighted OLS, `svyglm`

**Key assumptions:**

- *Linearity in the interaction:* The warmth × blame interaction is modeled as a product of a continuous × binary term. This is standard and appropriate. The sign and magnitude of the interaction coefficient (β₃ = 0.13 pooled) is the central claim. This is consistent with the theoretical prediction that blame amplifies the warmth gradient rather than shifting the intercept.
- *Selection on observables:* The association may reflect unmeasured confounders (e.g., China hawks who are also high-blame attributors may differ from low-blame attributors in unmeasured ways). The paper acknowledges this explicitly. The claim is associational, so this is a transparency issue, not a design failure.
- *Measurement of posture_z:* The four-item posture index is standardized using pooled-wave moments — this is the right approach for cross-wave comparability. PCA and Cronbach's alpha checks in `03_models.R` are appropriate diagnostics. The paper should report the alpha and the PC1 variance share in either the main text or a footnote.
- *Design-based SEs via `svyglm`*: Correct. The survey design object `svydesign(ids = ~1, weights = ~weight)` specifies no clustering and no stratification beyond weighting — appropriate for the YouGov matched-sample design. This is a potential area of conservative understatement if there are design effects beyond propensity-weighting, but it is the standard approach for YouGov data.

**Plausibility:** High. OLS with an interaction in a well-powered survey (n ≈ 4,234 after listwise deletion) is credible for this estimand.

### Step 2 — Survey-weighted MNL, `nnet::multinom` with frequency weights

**Key assumptions:**

- *Independence of Irrelevant Alternatives (IIA):* Multinomial logit assumes IIA across the four tool-configuration categories. This is a substantive concern: if S+A and Sanctions-only are near-substitutes for some respondents, collapsing them violates IIA. The paper does not test IIA explicitly (e.g., Hausman-McFadden test or mixed logit alternative). This is a known limitation of `nnet::multinom`.
- *Frequency weights as proxy for design weights:* `nnet::multinom` does not accept survey design objects, so frequency (propensity) weights are used instead of design-based weights. This yields point estimates that approximate design-weighted estimates but produces model-based SEs that do not account for the complex survey design. The paper acknowledges that "standard errors are model-based rather than design-based, which may understate uncertainty." This acknowledgment is appropriate but the potential magnitude of the understatement is unquantified.
- *Post-treatment conditioning (posture_z):* Including posture_z in Step 2 controls for a variable that is itself downstream of blame. This recovers a within-posture estimand but blocks part of the total effect pathway from blame to tool choice. The paper handles this well by distinguishing the two interpretations. The issue is flagged correctly.

**Plausibility:** Moderate to high. IIA is the weakest link; a sensitivity check using multinomial probit or Hausman-McFadden test would address this. The frequency-weight SEs are the second concern; bootstrapped confidence intervals using the survey weights directly would help.

### Step 3 — Stacked OLS, `svyglm`, clustered by respondent

**Key assumptions:**

- *Probe validity as negative controls:* The seven boundary probes are claimed to violate one or both IBC scope conditions. This claim is theoretically motivated but descriptively unverified. The paper asserts, for example, that the Taiwan military action probe violates the tool-structure condition because military instruments are relationship-ending. This is a plausible characterization, but it is not verified with respondent-level data (e.g., whether respondents actually perceive Taiwan-related tools as less reversible). The boundary probe logic is the paper's most important identifying assumption in Step 3, and it rests entirely on the researcher's theoretical reading.
- *Respondent-level clustering:* `ids = ~respondent_id` in the stacked design is correct and necessary given that each respondent contributes 8 observations. This is implemented correctly in `05_probes.R` via the `build_contrast_vector` approach, which extracts scenario × is_SA interactions from the fully interacted stacked model.
- *Equivalence bound choice (ε = 0.10 × SD_boundary):* The 0.10 × SD bound is described in a footnote, with a 0.25 × SD sensitivity. The choice of ε = 0.10 as the primary bound is on the liberal end of what equivalence testing conventions suggest. The paper should state explicitly why this bound is substantively defensible (e.g., what size effect would matter for the theoretical claim).

**Plausibility:** Moderate. The stacked design is well-executed. The main vulnerability is that probe validity as negative controls is an assumption, not a demonstrated property.

---

## 3. Inference

**Step 1:** `svyglm` produces design-based linearization SEs that correctly propagate the survey weighting. This is appropriate and correctly implemented. The survey design objects (`svydesign(ids = ~1, weights = ~weight)`) omit explicit stratification and clustering variables, which is standard for YouGov matched samples where the design is captured in the weights rather than a formal stratum/cluster structure.

**Step 2:** Model-based SEs from `nnet::multinom` are the most important inferential concern. Because the MNL uses frequency weights, variance estimates do not reflect the sampling distribution of the survey design. The paper correctly flags this as a limitation. A constructive suggestion is to compute bootstrapped SEs using the survey weights to quantify the potential inflation — even a brief comparison of model-based versus bootstrap SEs for the key blame-effect contrasts would give readers a sense of the magnitude of the discrepancy.

**Step 3:** Respondent-level clustering is correctly specified. The TOST equivalence test uses the stacked model's coefficient-level SEs. The omnibus divergence (Δ_d − mean Δ_boundary) is computed from the same stacked model vcov, which is correct — the SEs account for the correlation between Δ_d and each Δ_x because all are extracted from the same model. This is methodologically sound.

**Multiple testing:** The paper applies Benjamini-Hochberg correction to the pairwise divergence tests in the appendix, which is appropriate. The main text reports the omnibus divergence rather than individual pairwise tests, which sidesteps the multiple comparison problem for the primary inference.

**Listwise deletion (n = 4,234, 15.3% attrition):** Dropped respondents are younger, warmer toward China, and less likely to attribute blame. This is a form of non-random attrition that could bias the interaction estimate — the observed interaction may be understated if more blame-attributing warm respondents were retained. The paper notes this and offers an ideology-omission sensitivity, which recovers cases. The direction of bias (if any) should be characterized more explicitly.

---

## 4. Scope Conditions

The IBC framework specifies two conditions for the mixed S+A pattern to emerge: (a) problem structure (salient, traceable, compliance-dependent) and (b) tool structure (bounded/reversible instruments). The paper maps these conditions onto the 8-probe battery explicitly in Table 3 (probe_map), which is a strength.

**What is clearly within scope:** The fentanyl scenario, which the paper argues satisfies both conditions. The mapping is plausible.

**What is explicitly outside scope:** The three security probes (Taiwan, US-China collision, third-party collision) on tool-structure grounds; the four non-crisis probes (adoptions, electronics, scholarships, pandas) on problem-structure grounds.

**A scope condition that is underspecified:** The framework says blame must be "traceable to an identifiable foreign actor." In the fentanyl case, blame is measured at the individual respondent level (multi-select item). But the theoretical claim about why the scope condition is met relies on the objective characteristics of the fentanyl situation (China is the precursor-chemical source), not solely on the respondent's blame attribution. The paper should clarify whether IBC is a claim about objective problem properties, respondent-level belief structures, or both, since this distinction matters for how the framework would apply to new cases.

**Cross-wave scope:** The 2024 and 2025 waves are treated as pooled cross-sections with year FE. The 2025 wave reflects a politically different environment (fentanyl tariffs, Busan agreement mentioned in the Introduction). If the political salience of fentanyl shifted between waves in ways that alter the scope conditions themselves, pooling may obscure wave-specific dynamics. Wave × interaction tests are a natural extension; the paper reports wave-specific figures but does not formally test whether the interaction coefficient differs across waves.

---

## 5. Open Threats

**Threat 1: Domain-specific social desirability.** The most significant unaddressed confound. If media framing during 2024-2025 established "sanctions plus cooperation" as the socially expected response to fentanyl specifically — and not to military or trade issues — respondents may be echoing that frame rather than revealing a differentiated preference structure. The specificity test rules out generalized acquiescence but not domain-specific social desirability. The paper acknowledges this explicitly in the Discussion, which is appropriate. However, it has no empirical response beyond acknowledgment.

**Threat 2: IIA violation in the MNL.** S+A and Sanctions-only are theoretically close substitutes for respondents who are primarily coercion-minded. If IIA fails, the MNL coefficients may be biased in unpredictable directions. The Hausman-McFadden test or a multinomial probit comparison would directly test this assumption. The current paper provides no IIA diagnostic.

**Threat 3: The placebo pairing weakens tool-specificity.** The appendix reports that a placebo DV (sanctions × international cooperation, not China-specific) produces a blame-driven shift of comparable magnitude (Δ ≈ 0.327 vs Δ ≈ 0.306 for the main S+A estimate). The paper reframes this finding under the "leverage orientation" reading, arguing both pairings are engagement-compatible. This reframing is intellectually defensible but somewhat post-hoc. A referee will press on this: if the blame effect is equally strong with a non-China-specific cooperative item, the S+A category may not be measuring China-specific leverage logic at all.

**Threat 4: Non-random listwise deletion.** Dropped respondents are systematically warmer toward China and less likely to blame China — the two key predictors. If the warmth × blame interaction is stronger among warm blamers (who are underrepresented in the analytic sample), the main interaction estimate is attenuated relative to the population. Multiple imputation on the ideology item (the primary source of missingness, per the paper) would be a natural robustness check that goes beyond the current "omit ideology" sensitivity.

**Threat 5: Probe framing as a confound in Step 3.** The eight probe scenarios vary not only in their scope-condition properties but also in their emotional valence, concreteness, and specificity. Probe d (fentanyl cooperation) names a very specific, proximate, personally salient scenario. Probes g and h (scholarships, pandas) are diffuse and low-stakes. If the S+A contrast is concentrated on probe d partly because it is more concretely framed rather than because of its scope-condition properties, the discrimination test conflates domain specificity with framing differences. The paper would benefit from a brief discussion of whether the target probe differs from boundary probes on dimensions beyond the two scope conditions.

---

## 6. Constructive Suggestions

**For Threat 1 (social desirability):** Report whether the blame × S+A relationship is significantly stronger among respondents who report high media consumption about fentanyl/China (a plausible proxy for social desirability exposure). An attenuation pattern by media exposure would be consistent with framing; the absence of such a pattern would modestly reduce the concern. This can be done within the existing data.

**For Threat 2 (IIA):** Run the Hausman-McFadden test using the `mlogtest` function from the `mlogit` package, or compare key estimates under multinomial probit (available via `mlogit`). If IIA passes, report the test statistic in a footnote. If it fails, consider a nested logit specification that groups S+A and Sanctions-only as a "coercive" nest.

**For Threat 3 (placebo).** The most direct response is to re-examine the placebo results through the domain-level Step 3 design: does the sanctions × international-cooperation placebo also show domain-specific concentration in the panorama test, or does it appear across multiple probes? If the placebo's concentration pattern is broader than the main S+A result, that would strengthen the paper's interpretation. This test is already architecturally available given the existing `placebo_tool_config` variable in `04_mechanisms.R`.

**For Threat 4 (missing data):** Implement multiple imputation via `mice` for the ideology variable. Report main estimates under listwise deletion and under imputed samples side-by-side. Given that the paper already has the ideology-omission sensitivity, MI is a modest incremental step that would directly address the attrition bias concern.

**For Threat 5 (probe framing):** Add a brief table mapping each probe on concreteness, salience, and emotional valence alongside the two scope conditions. This does not require new data — it is a coding exercise on the existing probe descriptions. It serves to document that the discrimination is about scope conditions rather than an uncontrolled framing difference, which a referee will appreciate.

**For Step 2 inferential gap:** Bootstrap confidence intervals (B = 1000, using survey weights directly) for the blame-effect contrast in the MNL would directly address the model-based SE concern. This is computationally feasible with the existing data structure.

---

## Summary: Primary Strategy Assessment

| Dimension | Assessment |
|-----------|-----------|
| Estimand clarity | Sound; within-posture framing in Step 2 needs more prominent disclosure |
| Design validity (Step 1) | High — design-based SEs, appropriate interaction specification |
| Design validity (Step 2) | Moderate — IIA untested, model-based SEs flagged but unquantified |
| Design validity (Step 3) | Moderate — stacked OLS correctly executed; boundary probe validity is assumed not demonstrated |
| Inference | Appropriate for Steps 1 and 3; Step 2 SEs are the main gap |
| Scope conditions | Well-specified in theory; cross-wave scope and concreteness-vs-domain confound warrant brief discussion |
| Top threats | Social desirability (acknowledged, unaddressed), IIA (undiagnosed), placebo reframing (defensible but exposed) |

The three-step design is coherent and the theoretical-empirical correspondence is explicit, which is a genuine strength. The paper's main vulnerabilities are the IIA assumption in Step 2 (a mechanical fix), the model-based SEs in Step 2 (addressable with bootstrap), and the domain-specific social desirability alternative (acknowledgment is honest but leaves the reader without an empirical response). Addressing these three points would meaningfully harden the paper against referee objections.

---

## 7. Revised Threat Analysis
**Appended:** 2026-04-09 | **Reason:** Strategist-critic FAIL (78/100) — three issues elevated for correction

---

### Threat 1A (PRIMARY): Acquiescence Attenuation — Is the Floor Estimate Substantively Meaningful?

The original memo listed acquiescence controls as a supporting concern. The critic is right to elevate this: the ~38% attenuation (Δ ≈ 0.306 → Δ ≈ 0.191 among warm respondents after endorsement-count adjustment) is large enough that the corrected estimate must stand independently as evidence of IBC, not merely as a sensitivity check.

**Is Δ ≈ 0.191 substantively meaningful?** Yes, but the paper must make this case explicitly rather than leaving it implicit. A shift of roughly 19 percentage points on the S+A margin — among respondents who are both warmth-high and blame-attributing — is a real and policy-relevant differentiation. It is not a null result dressed up as a finding. To put it in context: the acquiescence-corrected estimate still exceeds the unadjusted boundary-probe average (which by design should cluster near zero), so the discrimination remains visible even in the conservative specification. The IBC claim does not require Δ ≈ 0.306; it requires a positive, non-trivial blame-driven shift toward S+A among warm blamers, and Δ ≈ 0.191 clears that bar.

**Recommended framing:** The acquiescence-adjusted estimate should be foregrounded as the preferred estimate in the main results, with the unadjusted Δ ≈ 0.306 reported as the upper bound. This reframing is more defensible than presenting Δ ≈ 0.306 as the headline figure and treating the adjusted estimate as a robustness footnote. A sentence in the Results section should read approximately: "Our preferred estimate, adjusted for acquiescence tendency, is Δ ≈ 0.191 (SE = [x]); the unadjusted estimate of Δ ≈ 0.306 should be treated as an upper bound." Foregrounding the conservative number disarms the referee rather than inviting the challenge.

---

### Threat 1B (PRIMARY): Placebo Reversal — The Ordering Problem Is Real

The original memo described the placebo result as "comparable magnitude." This was imprecise and must be corrected. The actual ordering is Δ_placebo ≈ 0.327 > Δ_main ≈ 0.306. The placebo exceeds the main estimate. This is not a minor rounding issue — it is a directional reversal that directly challenges the tool-specificity component of IBC. If blame drives respondents toward sanctions-plus-cooperation-type pairings at least as strongly when the cooperative item is not China-action-specific, then the S+A category may be capturing a general "pressure-plus-engagement" orientation rather than IBC's bounded-tool logic.

**What this implies for the Discussion-section response:** The paper's current reframing — that the placebo reflects a "leverage orientation" compatible with both pairings — is intellectually coherent but is post-hoc relative to the IBC framework as prespecified. A referee will note this and press on whether the leverage-orientation interpretation was articulated before the placebo results were examined. If it was not pre-registered, the paper should be explicit that this interpretation is offered as an exploratory account of an unexpected pattern, not as a confirmatory result.

**Recommended action:** The paper needs one of the following two responses, and the weaker framing should be dropped. Option A (preferred): Run the Step 3 panorama test on the placebo DV. If the placebo's blame-driven shift is not domain-concentrated — if it appears across multiple probes rather than being specific to the fentanyl scenario — that structural difference in concentration pattern distinguishes the main result from the placebo even though the marginal magnitudes are similar. This is already architecturally available (see Section 6, Threat 3 suggestion above) and would constitute a genuine empirical answer. Option B: Formally test whether Δ_placebo − Δ_main is distinguishable from zero. If the difference (≈ 0.021) is not statistically significant, the paper can note that the two estimates are statistically indistinguishable, reframe the placebo as evidence of a broader engagement orientation, and acknowledge that tool-specificity in the IBC sense is not confirmed at the marginal level — with domain-concentration in Step 3 carrying the primary specificity burden. Either response is defensible; neither response (status quo) is not.

---

### Threat 2 (MEDIUM): Multiple Testing Across the Three-Step Sequence

The paper correctly applies BH-FDR within Step 3 and the main text sidesteps multiple comparison issues by reporting the omnibus divergence rather than pairwise tests. The critic's concern is whether a familywise correction is also required across the three primary claims of Steps 1, 2, and 3.

**Assessment:** A familywise correction is not conceptually required here, and the paper should explain why rather than silently omitting it. The three steps do not constitute three independent tests of a single null hypothesis — the textbook scenario where familywise correction is indicated. Instead, they form a sequential chain of distinct theoretical implications: Step 1 tests whether warmth conditions the blame-posture association (IBC's affective conditionality claim); Step 2 tests whether blame shifts tool composition toward S+A conditional on posture (IBC's bounded-tool claim); Step 3 tests whether the S+A concentration is domain-specific rather than generalized (IBC's scope-boundary claim). These are three different implications of IBC, each testing something the theory uniquely predicts. Applying a Bonferroni correction across them would be analogous to applying familywise correction across a paper's introduction, body, and conclusion — it conflates the number of theoretical claims with the number of independent hypotheses drawn from a single test pool.

**Recommended language:** The paper should add one to two sentences in the Methods or Appendix explaining this logic: "The three-step empirical sequence tests three distinct theoretical implications of IBC rather than three independent tests of a single null. We therefore apply multiple-testing corrections within each step (BH-FDR in Step 3) but do not apply a familywise correction across steps, which would be conceptually inappropriate for a sequential-implication design." This preempts the objection without conceding ground that does not need to be conceded. If a reviewer pushes back, the response is that familywise correction is designed for the case of fishing across many tests of one hypothesis, not for a pre-specified sequential theory test — and the IBC framework was articulated before the data were analyzed.
