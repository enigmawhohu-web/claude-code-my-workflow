# Pseudo-Code: Main Estimation Steps
**Project:** Poll-2025-CLO
**Date:** 2026-04-09

---

## Step 1 — Warmth × Blame OLS (03_models.R)

```
# Inputs
data_pooled       <- pooled.rds         # n ≈ 4,234 after listwise deletion
data_2024, _2025  <- wave-specific rds

# Standardize using pooled moments (computed before split)
warmth_z  <- (warmth - mean_pooled) / sd_pooled
posture_z <- (posture_index - mean_pooled) / sd_pooled

# Survey design (YouGov weights, no explicit cluster/strata)
des <- svydesign(ids = ~1, weights = ~weight, data = data_pooled)

# Main specification (pooled)
m_pooled <- svyglm(
  posture_z ~ warmth_z * blame_china         # key interaction
            + party3 + ideo5_clean           # political controls
            + educ_college + age + female     # demographic controls
            + race_eth + newsint_attn
            + exposure_index
            + year,                           # year FE
  design = des, family = gaussian()
)

# Key quantity: interaction coefficient beta_3 = warmth_z:blame_china
# Interpretation: blame amplifies the warmth gradient by beta_3 units per SD

# Wave-specific replications (clean_2024, clean_2025) — same spec, no year FE
# Robustness:
#   (a) blame_china_alt (single-choice "most responsible")
#   (b) add behavior_index_z to specification
#   (c) ordinal logistic on individual posture items
```

---

## Step 2 — MNL Tool Configuration (04_mechanisms.R)

```
# DV construction (4-category)
tool_config <- case_when(
  sanctions_bin == 0 & action_bin == 0 ~ "Neither",
  sanctions_bin == 1 & action_bin == 0 ~ "Sanctions-only",
  sanctions_bin == 0 & action_bin == 1 ~ "Action-only",
  sanctions_bin == 1 & action_bin == 1 ~ "S+A"
)
# Reference level: "Neither"

# Model (frequency weights, not design-based — nnet::multinom limitation)
m_mnl <- multinom(
  tool_config ~ warmth_z * blame_china
              + party3 + ideo5_clean + educ_college + age + female
              + race_eth + newsint_attn + exposure_index
              + posture_z,       # NOTE: post-treatment covariate → within-posture estimand
  weights = weight,
  data = data_pooled
)

# Key estimand: predicted P(S+A | blame=1, warmth=w) - P(S+A | blame=0, warmth=w)
# Evaluated at warmth_z ∈ {-1, 0, +1} via marginaleffects::avg_comparisons()

# Acquiescence robustness:
m_mnl_acq <- same spec + n_approaches_main + n_approaches_miss
# Expected: blame effect attenuates ~38% but remains qualitatively consistent

# Placebo DV:
placebo_tool_config <- sanctions x intl_cooperation (not China-specific)
# Same spec; compare blame contrast to main S+A estimate
```

---

## Step 3 — Stacked OLS Panorama (05_probes.R)

```
# Sample restriction
sample <- pooled %>% filter(tool_config %in% c("S+A", "Sanctions-only"))
# n ≈ 1,846

# Stack: long format (1 row per respondent × scenario)
long_data <- pivot_longer(sample, cols = opinion_change_a:opinion_change_h,
                          names_to = "scenario", values_to = "probe_y")

# Survey design with respondent-level clustering
des_stacked <- svydesign(ids = ~respondent_id, weights = ~weight, data = long_data)

# Fully interacted stacked model (SUR-equivalent)
m_stacked <- svyglm(
  probe_y ~ scenario                   # scenario intercepts (α_s)
          + is_SA                      # baseline S+A vs S-only for ref scenario (a)
          + scenario:is_SA             # scenario-specific S+A contrasts (λ_s)
          + scenario:(party3 + ideo5_clean + educ_college + age + female
                      + race_eth + newsint_attn + exposure_index),
  design = des_stacked, family = gaussian()
)
# NOTE: clustering at respondent level accounts for 8 repeated observations per person

# Extract scenario-specific contrasts
Delta_s <- build_contrast_vector(coef(m_stacked), scenario_letter = s)
# For scenario a: Delta_a = coef["is_SA"]
# For scenario x != a: Delta_x = coef["is_SA"] + coef["scenariox:is_SA"]

# Key quantities
Delta_d       <- Delta for probe d (fentanyl cooperation — target)
Delta_boundary <- mean(Delta_a, Delta_b, Delta_c, Delta_e, Delta_f, Delta_g, Delta_h)

omnibus_divergence <- Delta_d - Delta_boundary
# SE from vcov(m_stacked) via delta method on the linear combination

# TOST equivalence test (boundary probes)
# H0: |Delta_boundary| >= epsilon   vs   H1: |Delta_boundary| < epsilon
# epsilon = 0.10 * SD(boundary probe scores)
# Report two one-sided t-tests
```

---

## Recommended Additional Checks (Not Currently Implemented)

```
# IIA test for Step 2
hausman_mcfadden_test(m_mnl, drop_category = "Action-only")
# Or: refit with mlogit() and compare to multinomial probit

# Bootstrap SEs for Step 2 blame contrast
boot_contrast <- replicate(1000, {
  boot_ids <- sample(nrow(data_pooled), replace = TRUE)
  boot_data <- data_pooled[boot_ids, ]
  fit_mnl_boot(boot_data)  # same spec
  extract_phat_SA_blame_contrast(fit)
})
sd(boot_contrast)  # compare to model-based SE

# Multiple imputation for listwise deletion
library(mice)
imputed <- mice(data_pooled[, model_vars], m = 20, method = "pmm")
m_mi <- with(imputed, svyglm(posture_z ~ warmth_z * blame_china + controls,
                              design = svydesign(ids=~1, weights=~weight)))
pool(m_mi)  # Rubin's rules

# Placebo × panorama cross-test (Step 3 applied to placebo DV)
# Use placebo_tool_config as the basis for is_SA_placebo classification
# Run same stacked OLS — does placebo show domain concentration?
```
