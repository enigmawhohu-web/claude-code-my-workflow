#' ============================================================================
#' 03_models.R — Phase 3: Main Results (v3.2)
#' Poll 2025: Fentanyl Policy Survey Analysis
#' PAX sapiens Foundation
#'
#' @author PAX sapiens Foundation
#'
#' @description Survey-weighted OLS: warmth x blame → posture, with symmetric
#'   blame measures, behavior index extension, and Devil's Advocate hardening.
#'
#' @question Is warmth toward China associated with more dovish foreign policy
#'   preferences, and does blame attribution for fentanyl moderate this
#'   association?
#'
#' @input  data/clean/pooled.rds, data/clean/clean_2024.rds,
#'         data/clean/clean_2025.rds
#' @output output/tables/tab2_phase3_main_models (.tex + .csv)
#'         output/tables/app_tab_phase3_r1_models (.tex + .csv)
#'         output/tables/app_tab_phase3_svyolr_q15items (.tex + .csv)
#'         output/figures/fig2a_posture_warmthXblame_byyear.pdf
#'         output/figures/fig2a_posture_warmthXblameALT_byyear.pdf
#'         output/figures/fig_coef_forest_interaction.pdf
#'         output/figures/fig_sanity_binned_means.pdf
#'         output/tables/phase3_objects.rds
#'         output/tables/phase3_log.txt
#'
#' @depends here, tidyverse, survey, srvyr, kableExtra, scales,
#'          marginaleffects, psych, texreg
#'
#' @plan    quality_reports/plans/2026-02-19_03-models.md (v3.2)
#' ============================================================================

# === Section 0: Setup ========================================================

library(here)
library(tidyverse)
library(survey)
library(srvyr)
library(kableExtra)
library(scales)
library(marginaleffects)
library(psych)
library(texreg)

set.seed(20250217)

dir.create(here("output", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "figures"), recursive = TRUE, showWarnings = FALSE)

# --- Logging ---
# log_msg: writes to file AND console (use sparingly for major milestones)
# log_only: writes to file only (use for all diagnostic details)
log_env <- new.env(parent = emptyenv())
log_env$entries <- character()

log_only <- function(msg) {
  log_env$entries <- c(log_env$entries, msg)
}

log_msg <- function(msg) {
  log_only(msg)
  message(msg)
}

log_only("========================================================================")
log_only("PHASE 3 LOG: Warmth x Blame -> Posture Models (v3.2)")
log_only(sprintf("Timestamp: %s", Sys.time()))
log_only("========================================================================")
message("Phase 3: Starting warmth x blame -> posture models (v3.2)")

# --- PAX palette ---
pax_dark       <- "#1a1a2e"
pax_blue       <- "#16213e"
pax_accent     <- "#0f3460"
pax_highlight  <- "#e94560"
neutral_gray   <- "#6c757d"
positive_green <- "#198754"
negative_red   <- "#dc3545"

year_colors  <- c("2024" = pax_accent, "2025" = pax_highlight)
blame_colors <- c("0" = pax_accent, "1" = pax_highlight)
spec_colors  <- c("A" = pax_accent, "B" = pax_highlight)

#' Custom ggplot2 theme for PAX sapiens publications
theme_pax <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title       = element_text(face = "bold", size = base_size + 2),
      plot.subtitle    = element_text(color = neutral_gray),
      legend.position  = "bottom",
      panel.grid.minor = element_blank(),
      strip.text       = element_text(face = "bold")
    )
}

#' Save table as LaTeX (.tex) and CSV (.csv)
save_table <- function(tbl_df, name, caption = "") {
  csv_path <- here("output", "tables", paste0(name, ".csv"))
  write_csv(tbl_df, csv_path)
  tex_path <- here("output", "tables", paste0(name, ".tex"))
  latex_out <- tbl_df %>%
    kbl(format = "latex", booktabs = TRUE, caption = caption, linesep = "") %>%
    kable_styling(latex_options = c("hold_position"))
  writeLines(as.character(latex_out), tex_path)
  log_msg(sprintf("  Saved: %s.tex + %s.csv", name, name))
}

#' Save ggplot figure as vector PDF
save_figure <- function(plot, name, w = 6.5, h = 4.5) {
  pdf_path <- here("output", "figures", paste0(name, ".pdf"))
  dev <- tryCatch(
    {
      tmp <- tempfile(fileext = ".pdf")
      grDevices::cairo_pdf(tmp); dev.off(); unlink(tmp)
      cairo_pdf
    },
    error = function(e) "pdf"
  )
  ggsave(pdf_path, plot = plot, width = w, height = h,
         device = dev, dpi = 300, bg = "white")
  dev_label <- if (identical(dev, cairo_pdf)) "cairo_pdf" else "pdf"
  log_msg(sprintf("  Saved: %s.pdf (%s x %s in, device=%s)",
                  name, w, h, dev_label))
}

#' Format p-value with significance stars
fmt_p <- function(p) {
  stars <- case_when(
    is.na(p)  ~ "",
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ ""
  )
  ifelse(is.na(p), "\u2014",
         paste0(formatC(p, format = "f", digits = 3), stars))
}

# --- Load data ---
log_only("")
log_only("--- Loading Data ---")
pooled     <- readRDS(here("data", "clean", "pooled.rds"))
clean_2024 <- readRDS(here("data", "clean", "clean_2024.rds"))
clean_2025 <- readRDS(here("data", "clean", "clean_2025.rds"))
log_msg(sprintf("  Loaded: pooled=%d, 2024=%d, 2025=%d",
                nrow(pooled), nrow(clean_2024), nrow(clean_2025)))


# === Section 1: Variable Construction ========================================

log_only("")
log_msg("=== Section 1: Variable Construction ===")

#' Construct analysis variables for Phase 3 models (v3.2)
#'
#' Creates posture_index, posture_z, warmth_z, behavior_index_z,
#' exposure_index, exposure_unsure, newsint_attn, ideo5_clean.
#' Standardisation uses pooled-level moments passed as arguments
#' so all datasets share the same z-scale.
#'
#' @param df Data frame (clean_2024, clean_2025, or pooled)
#' @param warmth_mu Pooled mean of warmth
#' @param warmth_sd Pooled SD of warmth
#' @param posture_mu Pooled mean of posture_index
#' @param posture_sd Pooled SD of posture_index
#' @param behavior_mu Pooled mean of behavior_index
#' @param behavior_sd Pooled SD of behavior_index
#' @return Data frame with new variables appended
build_analysis_vars <- function(df, warmth_mu, warmth_sd,
                                posture_mu, posture_sd,
                                behavior_mu, behavior_sd) {
  df %>%
    mutate(
      # --- Posture index (DV): higher = more dovish/cooperative ---
      us_pref_a   = as.numeric(us_preferred_a),
      us_pref_b   = as.numeric(us_preferred_b),
      us_pref_c_r = 8 - as.numeric(us_preferred_c),
      us_pref_d   = as.numeric(us_preferred_d),

      posture_n_valid = rowSums(!is.na(cbind(
        us_pref_a, us_pref_b, us_pref_c_r, us_pref_d
      ))),
      posture_index = ifelse(
        posture_n_valid >= 3,
        rowMeans(cbind(us_pref_a, us_pref_b, us_pref_c_r, us_pref_d),
                 na.rm = TRUE),
        NA_real_
      ),
      posture_z = (posture_index - posture_mu) / posture_sd,

      # Ordered factors for item-level robustness
      us_pref_a_ord   = ordered(us_pref_a),
      us_pref_b_ord   = ordered(us_pref_b),
      us_pref_c_r_ord = ordered(us_pref_c_r),
      us_pref_d_ord   = ordered(us_pref_d),

      # --- Warmth (IV 1) ---
      warmth_z = (warmth - warmth_mu) / warmth_sd,

      # --- Behavior index (IV 2): higher = more positive ---
      china_beh_a   = as.numeric(china_behavior_a),
      china_beh_b   = as.numeric(china_behavior_b),
      china_beh_c_r = 8 - as.numeric(china_behavior_c),
      china_beh_d   = as.numeric(china_behavior_d),

      behavior_n_valid = rowSums(!is.na(cbind(
        china_beh_a, china_beh_b, china_beh_c_r, china_beh_d
      ))),
      behavior_index = ifelse(
        behavior_n_valid >= 3,
        rowMeans(cbind(china_beh_a, china_beh_b, china_beh_c_r, china_beh_d),
                 na.rm = TRUE),
        NA_real_
      ),
      behavior_index_z = (behavior_index - behavior_mu) / behavior_sd,

      # --- Exposure index (main spec: Unsure → NA) ---
      know_died_bin = case_when(
        as.numeric(know_someone_died) == 1 ~ 1L,
        as.numeric(know_someone_died) == 2 ~ 0L,
        TRUE ~ NA_integer_
      ),
      overdose_bin = case_when(
        as.numeric(personal_overdose) == 1 ~ 1L,
        as.numeric(personal_overdose) == 2 ~ 0L,
        TRUE ~ NA_integer_
      ),
      # If one item is NA (Unsure) and other is non-NA, impute NA→0
      # (conservative: "no exposure" for unknown). Both NA → NA entirely.
      # See DA#15: Unsure→0 robustness tested in Section 4 sensitivity.
      exposure_index = case_when(
        is.na(know_died_bin) & is.na(overdose_bin) ~ NA_integer_,
        TRUE ~ coalesce(know_died_bin, 0L) + coalesce(overdose_bin, 0L)
      ),
      # Flag: did respondent answer "Unsure" on either item?
      exposure_unsure = as.integer(
        as.numeric(know_someone_died) == 3 |
          as.numeric(personal_overdose) == 3
      ),

      # --- Political attention (reversed: higher = more attentive) ---
      newsint_attn = {
        ni <- as.numeric(newsint)
        ifelse(ni %in% 1:4, 5L - ni, NA_integer_)
      },

      # --- Ideology: keep 1-5, recode 6 ("Not sure") → NA ---
      ideo5_clean = {
        id <- as.numeric(ideo5)
        ifelse(id %in% 1:5, id, NA_real_)
      }
    )
}

# --- Compute pooled moments BEFORE applying build_analysis_vars ---
pooled_raw <- pooled %>%
  mutate(
    pi_a   = as.numeric(us_preferred_a),
    pi_b   = as.numeric(us_preferred_b),
    pi_c_r = 8 - as.numeric(us_preferred_c),
    pi_d   = as.numeric(us_preferred_d),
    pi_n   = rowSums(!is.na(cbind(pi_a, pi_b, pi_c_r, pi_d))),
    posture_index_raw = ifelse(pi_n >= 3,
      rowMeans(cbind(pi_a, pi_b, pi_c_r, pi_d), na.rm = TRUE), NA_real_),
    bi_a   = as.numeric(china_behavior_a),
    bi_b   = as.numeric(china_behavior_b),
    bi_c_r = 8 - as.numeric(china_behavior_c),
    bi_d   = as.numeric(china_behavior_d),
    bi_n   = rowSums(!is.na(cbind(bi_a, bi_b, bi_c_r, bi_d))),
    behavior_index_raw = ifelse(bi_n >= 3,
      rowMeans(cbind(bi_a, bi_b, bi_c_r, bi_d), na.rm = TRUE), NA_real_)
  )

warmth_mu   <- mean(pooled$warmth, na.rm = TRUE)
warmth_sd   <- sd(pooled$warmth, na.rm = TRUE)
posture_mu  <- mean(pooled_raw$posture_index_raw, na.rm = TRUE)
posture_sd  <- sd(pooled_raw$posture_index_raw, na.rm = TRUE)
behavior_mu <- mean(pooled_raw$behavior_index_raw, na.rm = TRUE)
behavior_sd <- sd(pooled_raw$behavior_index_raw, na.rm = TRUE)

stopifnot("warmth_sd must be positive"   = warmth_sd > 0)
stopifnot("posture_sd must be positive"   = posture_sd > 0)
stopifnot("behavior_sd must be positive"  = behavior_sd > 0)

log_only(sprintf("  Pooled warmth:    mean=%.3f, sd=%.3f", warmth_mu, warmth_sd))
log_only(sprintf("  Pooled posture:   mean=%.3f, sd=%.3f", posture_mu, posture_sd))
log_only(sprintf("  Pooled behavior:  mean=%.3f, sd=%.3f", behavior_mu, behavior_sd))

# Apply to all three datasets
pooled     <- build_analysis_vars(pooled, warmth_mu, warmth_sd,
                                  posture_mu, posture_sd,
                                  behavior_mu, behavior_sd)
clean_2024 <- build_analysis_vars(clean_2024, warmth_mu, warmth_sd,
                                  posture_mu, posture_sd,
                                  behavior_mu, behavior_sd)
clean_2025 <- build_analysis_vars(clean_2025, warmth_mu, warmth_sd,
                                  posture_mu, posture_sd,
                                  behavior_mu, behavior_sd)

# --- Step 7: Cronbach's alpha ---
log_only("")
log_only("  Cronbach's alpha (unweighted, pooled):")

posture_items_mat <- pooled %>%
  select(us_pref_a, us_pref_b, us_pref_c_r, us_pref_d) %>%
  drop_na()
alpha_posture <- psych::alpha(posture_items_mat, check.keys = FALSE)
log_only(sprintf("    Posture: raw_alpha=%.3f, std_alpha=%.3f",
                alpha_posture$total$raw_alpha, alpha_posture$total$std.alpha))
for (item in rownames(alpha_posture$item.stats)) {
  log_only(sprintf("      %s: r.drop=%.3f", item,
                  alpha_posture$item.stats[item, "r.drop"]))
}

behavior_items_mat <- pooled %>%
  select(china_beh_a, china_beh_b, china_beh_c_r, china_beh_d) %>%
  drop_na()
alpha_behavior <- psych::alpha(behavior_items_mat, check.keys = FALSE)
log_only(sprintf("    Behavior: raw_alpha=%.3f, std_alpha=%.3f",
                alpha_behavior$total$raw_alpha, alpha_behavior$total$std.alpha))
for (item in rownames(alpha_behavior$item.stats)) {
  log_only(sprintf("      %s: r.drop=%.3f", item,
                  alpha_behavior$item.stats[item, "r.drop"]))
}

# --- Step 8: PCA unidimensionality (DA#2) ---
log_only("")
log_only("  PCA unidimensionality check:")

pca_posture <- prcomp(posture_items_mat, center = TRUE, scale. = TRUE)
posture_pct <- summary(pca_posture)$importance["Proportion of Variance", ]
log_only(sprintf("    Posture PC1: %.1f%% variance", posture_pct[1] * 100))
if (posture_pct[1] < 0.60) {
  log_only("    WARNING: PC1 < 60%% — posture index may not be unidimensional")
}

pca_behavior <- prcomp(behavior_items_mat, center = TRUE, scale. = TRUE)
behavior_pct <- summary(pca_behavior)$importance["Proportion of Variance", ]
log_only(sprintf("    Behavior PC1: %.1f%% variance", behavior_pct[1] * 100))
if (behavior_pct[1] < 0.60) {
  log_only("    WARNING: PC1 < 60%% — behavior index may not be unidimensional")
}

# --- Step 9: Correlation checks ---
log_only("")
log_only("  Correlation checks:")
cor_wb <- cor(pooled$warmth_z, pooled$behavior_index_z, use = "complete.obs")
log_only(sprintf("    cor(warmth_z, behavior_index_z) = %.3f", cor_wb))
if (abs(cor_wb) > 0.7) {
  log_only("    WARNING: r > 0.7 — multicollinearity concern for Model B")
}

cor_wb_blame <- cor(pooled$warmth_z, pooled$blame_china, use = "complete.obs")
log_only(sprintf("    cor(warmth_z, blame_china) = %.3f  [DA#1: endogeneity diagnostic]",
                cor_wb_blame))

# --- Step 10: blame_count diagnostic (DA#3) ---
log_only("")
log_only("  blame_count diagnostic (DA#3):")
resp_vars <- c("responsible_drug_users", "responsible_doctors",
               "responsible_pharma", "responsible_cartels",
               "responsible_state_local", "responsible_federal",
               "responsible_china", "responsible_mexico")
pooled$blame_count <- rowSums(
  sapply(resp_vars, function(v) as.numeric(pooled[[v]]) == 1),
  na.rm = TRUE
)
bc_by_blame <- pooled %>%
  group_by(blame_china) %>%
  summarise(mean_blame_count = mean(blame_count, na.rm = TRUE),
            n = n(), .groups = "drop")
for (i in seq_len(nrow(bc_by_blame))) {
  log_only(sprintf("    blame_china=%d: mean_blame_count=%.2f (n=%d)",
                  bc_by_blame$blame_china[i],
                  bc_by_blame$mean_blame_count[i],
                  bc_by_blame$n[i]))
}

# --- Step 11: Weighted vs. unweighted SD comparison (DA#10) ---
log_only("")
log_only("  Weighted vs. unweighted SD comparison (DA#10):")
log_only("    (z-scores use unweighted pooled moments; linear rescaling does not affect inference)")
des_tmp <- svydesign(ids = ~1, weights = ~weight, data = pooled)
for (vname in c("warmth", "posture_index", "behavior_index")) {
  vals <- na.omit(pooled[[vname]])
  uw_sd <- sd(vals)
  w_var <- tryCatch(
    as.numeric(svyvar(as.formula(paste0("~", vname)), design = des_tmp, na.rm = TRUE)),
    error = function(e) NA_real_
  )
  w_sd <- sqrt(w_var)
  log_only(sprintf("    %s: unweighted_sd=%.3f, weighted_sd=%.3f",
                  vname, uw_sd, w_sd))
}
rm(des_tmp)

# --- Step 12: Histogram / boundary check (DA#4) ---
log_only("")
log_only("  Posture index boundary check (DA#4):")
n_total <- sum(!is.na(pooled$posture_index))
n_floor <- sum(pooled$posture_index <= 1.5, na.rm = TRUE)
n_ceil  <- sum(pooled$posture_index >= 6.5, na.rm = TRUE)
log_only(sprintf("    N=%d, floor(<=1.5): %d (%.1f%%), ceiling(>=6.5): %d (%.1f%%)",
                n_total, n_floor, 100 * n_floor / n_total,
                n_ceil, 100 * n_ceil / n_total))
if ((n_floor + n_ceil) / n_total > 0.10) {
  log_only("    WARNING: >10%% at boundaries — consider fractional logit robustness")
}

# --- Step 13: Audit all new variables ---
log_only("")
log_only("  Variable audits:")
for (v in c("posture_index", "posture_z", "warmth_z", "behavior_index",
            "behavior_index_z", "exposure_index", "exposure_unsure",
            "newsint_attn", "ideo5_clean")) {
  vals <- na.omit(as.numeric(pooled[[v]]))
  log_only(sprintf("    %s: n=%d, NAs=%d, range=[%.2f, %.2f], mean=%.3f",
                  v, length(vals), sum(is.na(pooled[[v]])),
                  min(vals), max(vals), mean(vals)))
}

# Exposure unsure counts
log_only(sprintf("  Unsure on know_someone_died: %d",
                sum(as.numeric(pooled$know_someone_died) == 3, na.rm = TRUE)))
log_only(sprintf("  Unsure on personal_overdose: %d",
                sum(as.numeric(pooled$personal_overdose) == 3, na.rm = TRUE)))

# Verify posture_index range
stopifnot(
  "posture_index out of [1,7]" =
    all(na.omit(pooled$posture_index) >= 1 &
        na.omit(pooled$posture_index) <= 7)
)
log_only("  AUDIT PASS: posture_index within [1, 7]")

stopifnot(
  "behavior_index out of [1,7]" =
    all(na.omit(pooled$behavior_index) >= 1 &
        na.omit(pooled$behavior_index) <= 7)
)
log_only("  AUDIT PASS: behavior_index within [1, 7]")


# === Section 2: Survey Design Objects ========================================

log_only("")
log_msg("=== Section 2: Survey Design Objects ===")

# ids = ~1: appropriate for online panel (no geographic clustering)
des_2024 <- svydesign(ids = ~1, weights = ~weight, data = clean_2024)
des_2025 <- svydesign(ids = ~1, weights = ~weight, data = clean_2025)
des_pool <- svydesign(ids = ~1, weights = ~weight, data = pooled)

stopifnot("Pooled N must equal sum of wave Ns" =
            nrow(pooled) == nrow(clean_2024) + nrow(clean_2025))
log_only(sprintf("  N check: %d / %d / %d PASS",
                nrow(clean_2024), nrow(clean_2025), nrow(pooled)))

for (wv in c("2024", "2025")) {
  d <- if (wv == "2024") clean_2024 else clean_2025
  w_mean <- mean(d$weight, na.rm = TRUE)
  w_sum  <- sum(d$weight, na.rm = TRUE)
  log_only(sprintf("  %s: weight mean=%.6f, sum=%.1f", wv, w_mean, w_sum))
  if (abs(w_mean - 1.0) > 0.01) {
    log_only(sprintf("  WARNING: %s weight mean deviates from 1.0", wv))
  }
}


# === Section 3: Controls Formula =============================================

log_only("")
log_msg("=== Section 3: Controls Formula ===")

ctrl_rhs <- "party3 + ideo5_clean + educ_college + age + female + race_eth + newsint_attn + exposure_index"

# --- Listwise deletion counts ---
model_vars_base <- c("posture_z", "warmth_z", "blame_china",
                      "behavior_index_z", "party3", "ideo5_clean",
                      "educ_college", "age", "female", "race_eth",
                      "newsint_attn", "exposure_index")

for (wv in c("2024", "2025", "pooled")) {
  d <- switch(wv, "2024" = clean_2024, "2025" = clean_2025, pooled)
  n_complete <- sum(complete.cases(d[, model_vars_base]))
  n_total <- nrow(d)
  log_only(sprintf("  %s: %d/%d complete cases (%.1f%% lost to listwise deletion)",
                  wv, n_complete, n_total,
                  100 * (1 - n_complete / n_total)))
}

# --- Listwise deletion sensitivity diagnostic (DA#14) ---
log_only("")
log_only("  Listwise deletion sensitivity (DA#14):")
log_only("  Comparing complete-case vs. dropped observations:")

complete_mask <- complete.cases(pooled[, model_vars_base])
for (lbl in c("complete", "dropped")) {
  mask <- if (lbl == "complete") complete_mask else !complete_mask
  sub <- pooled[mask, ]
  log_only(sprintf("    %s (n=%d): warmth=%.2f, blame_china=%.2f, age=%.1f",
                  lbl, nrow(sub),
                  mean(sub$warmth, na.rm = TRUE),
                  mean(sub$blame_china, na.rm = TRUE),
                  mean(sub$age, na.rm = TRUE)))
  # Party distribution
  if (nrow(sub) > 0 && "party3" %in% names(sub)) {
    party_tbl <- table(sub$party3, useNA = "no")
    party_pct <- round(100 * party_tbl / sum(party_tbl), 1)
    log_only(sprintf("      party3: %s",
                    paste(names(party_pct), "=", party_pct, "%", collapse = ", ")))
  }
}


# === Section 4: Primary Models ===============================================

log_only("")
log_msg("=== Section 4: Primary Models (14 OLS) ===")

#' Fit a single svyglm specification with error trapping
fit_spec <- function(fmla_rhs, des, label) {
  fmla <- as.formula(paste0("posture_z ~ ", fmla_rhs))
  tryCatch(
    svyglm(fmla, design = des, family = gaussian()),
    error = function(e) {
      log_msg(sprintf("  FAILED %s: %s", label, e$message))
      NULL
    }
  )
}

#' Log summary for a fitted model (with NA/NaN/Inf check)
log_model <- function(mod, label, interaction_term) {
  if (is.null(mod)) return(invisible(NULL))
  n_obs <- nobs(mod)
  # Pseudo-R2 for svyglm: 1 - deviance/null.deviance (weighted RSS ratio)
  r2 <- 1 - mod$deviance / mod$null.deviance
  coefs <- summary(mod)$coefficients
  # Check for problematic coefficients
  bad_coefs <- coef(mod)
  bad_idx <- is.na(bad_coefs) | is.nan(bad_coefs) | is.infinite(bad_coefs)
  if (any(bad_idx)) {
    log_msg(sprintf("  WARNING: %s has NA/NaN/Inf coefficients: %s",
                    label, paste(names(bad_coefs)[bad_idx], collapse = ", ")))
  }
  log_msg(sprintf("  %s: N=%d, R2=%.4f", label, n_obs, r2))
  for (cv in c("warmth_z", interaction_term)) {
    if (cv %in% rownames(coefs)) {
      log_msg(sprintf("    %s: b=%.4f, SE=%.4f, p=%s",
                      cv, coefs[cv, "Estimate"], coefs[cv, "Std. Error"],
                      fmt_p(coefs[cv, "Pr(>|t|)"])))
    }
  }
}

# --- Panel A: blame_china ---
log_msg("  Panel A: blame_china")
a24   <- fit_spec(paste0("warmth_z * blame_china + ", ctrl_rhs),
                  des_2024, "a24")
a25   <- fit_spec(paste0("warmth_z * blame_china + ", ctrl_rhs),
                  des_2025, "a25")
apool <- fit_spec(paste0("warmth_z * blame_china + factor(year) + ", ctrl_rhs),
                  des_pool, "apool")

b24   <- fit_spec(paste0("warmth_z * blame_china + behavior_index_z + ", ctrl_rhs),
                  des_2024, "b24")
b25   <- fit_spec(paste0("warmth_z * blame_china + behavior_index_z + ", ctrl_rhs),
                  des_2025, "b25")
bpool <- fit_spec(paste0("warmth_z * blame_china + behavior_index_z + factor(year) + ", ctrl_rhs),
                  des_pool, "bpool")

r1pool <- fit_spec(paste0("behavior_index_z * blame_china + factor(year) + ", ctrl_rhs),
                   des_pool, "r1pool")

for (m in c("a24", "a25", "apool", "b24", "b25", "bpool")) {
  log_model(get(m), m, "warmth_z:blame_china")
}
# r1pool uses behavior_index_z, not warmth_z
log_model(r1pool, "r1pool", "behavior_index_z:blame_china")

# --- Panel B: blame_china_alt ---
log_only("")
log_msg("  Panel B: blame_china_alt")
a24_alt   <- fit_spec(paste0("warmth_z * blame_china_alt + ", ctrl_rhs),
                      des_2024, "a24_alt")
a25_alt   <- fit_spec(paste0("warmth_z * blame_china_alt + ", ctrl_rhs),
                      des_2025, "a25_alt")
apool_alt <- fit_spec(paste0("warmth_z * blame_china_alt + factor(year) + ", ctrl_rhs),
                      des_pool, "apool_alt")

b24_alt   <- fit_spec(paste0("warmth_z * blame_china_alt + behavior_index_z + ", ctrl_rhs),
                      des_2024, "b24_alt")
b25_alt   <- fit_spec(paste0("warmth_z * blame_china_alt + behavior_index_z + ", ctrl_rhs),
                      des_2025, "b25_alt")
bpool_alt <- fit_spec(paste0("warmth_z * blame_china_alt + behavior_index_z + factor(year) + ", ctrl_rhs),
                      des_pool, "bpool_alt")

r1pool_alt <- fit_spec(paste0("behavior_index_z * blame_china_alt + factor(year) + ", ctrl_rhs),
                       des_pool, "r1pool_alt")

for (m in c("a24_alt", "a25_alt", "apool_alt", "b24_alt", "b25_alt", "bpool_alt", "r1pool_alt")) {
  int_term <- if (grepl("r1", m)) "behavior_index_z:blame_china_alt" else "warmth_z:blame_china_alt"
  log_model(get(m), m, int_term)
}

# --- Sensitivity: apool without ideo5_clean (DA#14) ---
log_only("")
log_only("  Sensitivity: apool without ideo5_clean (DA#14):")
ctrl_vars_no_ideo <- setdiff(strsplit(ctrl_rhs, " \\+ ")[[1]], "ideo5_clean")
ctrl_no_ideo <- paste(ctrl_vars_no_ideo, collapse = " + ")
apool_no_ideo <- fit_spec(
  paste0("warmth_z * blame_china + factor(year) + ", ctrl_no_ideo),
  des_pool, "apool_no_ideo"
)
if (!is.null(apool_no_ideo) && !is.null(apool)) {
  c_main <- coef(summary(apool))["warmth_z:blame_china", ]
  c_sens <- coef(summary(apool_no_ideo))["warmth_z:blame_china", ]
  log_only(sprintf("    Main apool intxn:    b=%.4f, p=%s (N=%d)",
                  c_main["Estimate"], fmt_p(c_main["Pr(>|t|)"]), nobs(apool)))
  log_only(sprintf("    No-ideo apool intxn: b=%.4f, p=%s (N=%d)",
                  c_sens["Estimate"], fmt_p(c_sens["Pr(>|t|)"]), nobs(apool_no_ideo)))
  if (sign(c_main["Estimate"]) == sign(c_sens["Estimate"])) {
    log_msg("    PASS: interaction sign stable after dropping ideo5_clean")
  } else {
    log_msg("    FLAG: interaction sign CHANGED — listwise deletion may bias results")
  }
}

# --- Sensitivity: exposure Unsure→0 and exposure_unsure flag (DA#15) ---
log_only("")
log_only("  Sensitivity: exposure_index robustness:")

# Create alternative pooled with Unsure→0
pooled_exp0 <- pooled %>%
  mutate(
    know_died_0 = ifelse(as.numeric(know_someone_died) == 1, 1L, 0L),
    overdose_0  = ifelse(as.numeric(personal_overdose) == 1, 1L, 0L),
    exposure_index_v0 = know_died_0 + overdose_0
  )
des_pool_exp0 <- svydesign(ids = ~1, weights = ~weight, data = pooled_exp0)

ctrl_exp0 <- gsub("exposure_index", "exposure_index_v0", ctrl_rhs)
apool_exp0 <- fit_spec(
  paste0("warmth_z * blame_china + factor(year) + ", ctrl_exp0),
  des_pool_exp0, "apool_exp0"
)
if (!is.null(apool_exp0) && !is.null(apool)) {
  c_exp0 <- coef(summary(apool_exp0))["warmth_z:blame_china", ]
  log_only(sprintf("    Unsure->0 apool intxn: b=%.4f, p=%s (N=%d)",
                  c_exp0["Estimate"], fmt_p(c_exp0["Pr(>|t|)"]), nobs(apool_exp0)))
}

# With exposure_unsure flag
ctrl_unsure <- paste0(ctrl_rhs, " + exposure_unsure")
apool_unsure <- fit_spec(
  paste0("warmth_z * blame_china + factor(year) + ", ctrl_unsure),
  des_pool, "apool_unsure_flag"
)
if (!is.null(apool_unsure)) {
  c_uns <- coef(summary(apool_unsure))["warmth_z:blame_china", ]
  log_only(sprintf("    +unsure_flag apool intxn: b=%.4f, p=%s (N=%d)",
                  c_uns["Estimate"], fmt_p(c_uns["Pr(>|t|)"]), nobs(apool_unsure)))
}
rm(pooled_exp0, des_pool_exp0)


# === Section 5: Core Tables ==================================================

log_only("")
log_msg("=== Section 5: Regression Tables ===")

# --- Table 2: Main inference set (12 models, two panels) ---
# Panel A
panel_a_models <- list(a24, a25, apool, b24, b25, bpool)
panel_a_names  <- c("A:2024", "A:2025", "A:Pool", "B:2024", "B:2025", "B:Pool")

# Panel B
panel_b_models <- list(a24_alt, a25_alt, apool_alt, b24_alt, b25_alt, bpool_alt)
panel_b_names  <- c("A:2024", "A:2025", "A:Pool", "B:2024", "B:2025", "B:Pool")

# Write Panel A LaTeX
tex_path_a <- here("output", "tables", "tab2_phase3_panel_a.tex")
texreg(panel_a_models,
       custom.model.names = panel_a_names,
       caption = "Panel A: blame\\_china (Multi-Select, $\\sim$49\\%)",
       label = "tab:phase3_panel_a",
       float.pos = "htbp",
       use.packages = FALSE,
       file = tex_path_a)

# Write Panel B LaTeX
tex_path_b <- here("output", "tables", "tab2_phase3_panel_b.tex")
texreg(panel_b_models,
       custom.model.names = panel_b_names,
       caption = "Panel B: blame\\_china\\_alt (Most Responsible, $\\sim$11\\%)",
       label = "tab:phase3_panel_b",
       float.pos = "htbp",
       use.packages = FALSE,
       file = tex_path_b)

# Combine into single Table 2 file
panel_a_tex <- readLines(tex_path_a)
panel_b_tex <- readLines(tex_path_b)
writeLines(c(panel_a_tex, "", "% --- Panel B ---", "", panel_b_tex),
           here("output", "tables", "tab2_phase3_main_models.tex"))
file.remove(tex_path_a, tex_path_b)
log_only("  Saved: tab2_phase3_main_models.tex (two panels)")

# Console preview
log_only("  Table 2 Panel A preview:")
sr_a <- capture.output(screenreg(panel_a_models, custom.model.names = panel_a_names))
for (line in sr_a) log_only(paste("    ", line))

log_only("  Table 2 Panel B preview:")
sr_b <- capture.output(screenreg(panel_b_models, custom.model.names = panel_b_names))
for (line in sr_b) log_only(paste("    ", line))

# CSV export for all 14 OLS models
all_models <- list(a24 = a24, a25 = a25, apool = apool,
                   b24 = b24, b25 = b25, bpool = bpool,
                   r1pool = r1pool,
                   a24_alt = a24_alt, a25_alt = a25_alt, apool_alt = apool_alt,
                   b24_alt = b24_alt, b25_alt = b25_alt, bpool_alt = bpool_alt,
                   r1pool_alt = r1pool_alt)

csv_rows <- list()
for (mn in names(all_models)) {
  mod <- all_models[[mn]]
  if (is.null(mod)) next
  tidy_df <- broom::tidy(mod, conf.int = TRUE)
  tidy_df$model <- mn
  csv_rows[[mn]] <- tidy_df
}
csv_out <- bind_rows(csv_rows)
write_csv(csv_out, here("output", "tables", "tab2_phase3_main_models.csv"))
log_only("  Saved: tab2_phase3_main_models.csv (all 14 OLS models)")

# --- Appendix: R1 replacement models ---
texreg(list(r1pool, r1pool_alt),
       custom.model.names = c("R1: blame_china", "R1: blame_china_alt"),
       caption = "Replacement Robustness: Behavior Index $\\times$ Blame (Pooled)",
       label = "tab:phase3_r1",
       float.pos = "htbp",
       use.packages = FALSE,
       file = here("output", "tables", "app_tab_phase3_r1_models.tex"))
log_only("  Saved: app_tab_phase3_r1_models.tex")

r1_csv <- bind_rows(
  broom::tidy(r1pool, conf.int = TRUE) %>% mutate(model = "r1pool"),
  broom::tidy(r1pool_alt, conf.int = TRUE) %>% mutate(model = "r1pool_alt")
)
write_csv(r1_csv, here("output", "tables", "app_tab_phase3_r1_models.csv"))
log_only("  Saved: app_tab_phase3_r1_models.csv")

# --- Coefficient forest plot (DA#11) ---
log_only("")
log_only("  Building coefficient forest plot (DA#11)...")

extract_intxn <- function(mod, label, panel, spec) {
  if (is.null(mod)) return(NULL)
  # Determine the interaction term name
  intxn_name <- if (spec == "R1") {
    if (panel == "blame_china") "behavior_index_z:blame_china"
    else "behavior_index_z:blame_china_alt"
  } else {
    if (panel == "blame_china") "warmth_z:blame_china"
    else "warmth_z:blame_china_alt"
  }
  coefs <- coef(summary(mod))
  if (!intxn_name %in% rownames(coefs)) return(NULL)
  b  <- coefs[intxn_name, "Estimate"]
  se <- coefs[intxn_name, "Std. Error"]
  t_crit <- qt(0.975, mod$df.residual)
  tibble(label = label, panel = panel, spec = spec,
         estimate = b, conf.low = b - t_crit * se, conf.high = b + t_crit * se)
}

forest_data <- bind_rows(
  extract_intxn(a24,   "A:2024", "blame_china", "A"),
  extract_intxn(a25,   "A:2025", "blame_china", "A"),
  extract_intxn(apool, "A:Pool", "blame_china", "A"),
  extract_intxn(b24,   "B:2024", "blame_china", "B"),
  extract_intxn(b25,   "B:2025", "blame_china", "B"),
  extract_intxn(bpool, "B:Pool", "blame_china", "B"),
  extract_intxn(a24_alt,   "A:2024", "blame_china_alt", "A"),
  extract_intxn(a25_alt,   "A:2025", "blame_china_alt", "A"),
  extract_intxn(apool_alt, "A:Pool", "blame_china_alt", "A"),
  extract_intxn(b24_alt,   "B:2024", "blame_china_alt", "B"),
  extract_intxn(b25_alt,   "B:2025", "blame_china_alt", "B"),
  extract_intxn(bpool_alt, "B:Pool", "blame_china_alt", "B")
) %>%
  mutate(
    panel_label = ifelse(panel == "blame_china",
                         "Panel A: blame_china (~49%)",
                         "Panel B: blame_china_alt (~11%)"),
    label = factor(label, levels = rev(c("A:2024", "A:2025", "A:Pool",
                                          "B:2024", "B:2025", "B:Pool")))
  )

p_forest <- ggplot(forest_data, aes(x = estimate, y = label,
                                     color = spec, shape = spec)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = neutral_gray) +
  geom_pointrange(aes(xmin = conf.low, xmax = conf.high),
                  position = position_dodge(width = 0.3), size = 0.5) +
  facet_wrap(~panel_label, scales = "free_x") +
  scale_color_manual(values = spec_colors, name = "Specification") +
  labs(title = "Interaction Coefficient: warmth_z \u00d7 blame",
       subtitle = "Point estimates with 95% CI across all Table 2 models",
       x = "Coefficient estimate",
       y = NULL) +
  theme_pax()

save_figure(p_forest, "fig_coef_forest_interaction", w = 6.5, h = 5)


# === Section 6: Core Figures (Interaction Plots) ==============================

log_only("")
log_msg("=== Section 6: Interaction Plots ===")

#' Robust prediction: try marginaleffects, fallback to manual vcov-based SE
safe_predictions <- function(model, newdata) {
  preds <- tryCatch(
    {
      p <- marginaleffects::predictions(model, newdata = newdata)
      data.frame(newdata,
                 estimate  = p$estimate,
                 std.error = p$std.error,
                 conf.low  = p$conf.low,
                 conf.high = p$conf.high)
    },
    error = function(e) NULL
  )
  if (!is.null(preds)) return(preds)

  log_only("  marginaleffects failed; using manual predict + vcov fallback")
  fit <- predict(model, newdata = newdata, type = "response")
  mm <- model.matrix(formula(model)[-2], data = newdata)
  V <- vcov(model)
  se <- sqrt(pmax(0, diag(mm %*% V %*% t(mm))))
  t_crit <- qt(0.975, model$df.residual)
  data.frame(newdata,
             estimate  = as.numeric(fit),
             std.error = se,
             conf.low  = as.numeric(fit) - t_crit * se,
             conf.high = as.numeric(fit) + t_crit * se)
}

#' Build prediction grid for a wave model
build_pred_grid <- function(d, blame_var) {
  get_mode <- function(x) {
    x <- na.omit(x)
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  grid <- expand.grid(
    warmth_z         = seq(-2, 2, by = 0.25),
    blame_val        = c(0, 1),
    behavior_index_z = median(d$behavior_index_z, na.rm = TRUE),
    party3           = get_mode(d$party3),
    ideo5_clean      = median(d$ideo5_clean, na.rm = TRUE),
    educ_college     = get_mode(d$educ_college),
    age              = median(d$age, na.rm = TRUE),
    female           = get_mode(d$female),
    race_eth         = get_mode(d$race_eth),
    newsint_attn     = median(d$newsint_attn, na.rm = TRUE),
    exposure_index   = median(d$exposure_index, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  names(grid)[names(grid) == "blame_val"] <- blame_var
  grid$party3   <- factor(grid$party3, levels = levels(d$party3))
  grid$race_eth <- factor(grid$race_eth, levels = levels(d$race_eth))
  grid
}

#' Create interaction plot for one blame measure
make_interaction_plot <- function(m24, m25, d24, d25, blame_var, filename) {
  grid_24 <- build_pred_grid(d24, blame_var)
  grid_25 <- build_pred_grid(d25, blame_var)

  # For Model A, behavior_index_z won't be used (not in model)
  # but safe_predictions handles extra columns gracefully
  preds_24 <- safe_predictions(m24, grid_24) %>% mutate(year = "2024")
  preds_25 <- safe_predictions(m25, grid_25) %>% mutate(year = "2025")
  preds_all <- bind_rows(preds_24, preds_25) %>%
    mutate(blame_label = ifelse(.data[[blame_var]] == 1,
                                paste0(blame_var, " = 1"),
                                paste0(blame_var, " = 0")))

  p <- ggplot(preds_all, aes(x = warmth_z, y = estimate,
                              color = blame_label, fill = blame_label)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2,
                color = NA) +
    geom_line(linewidth = 1) +
    facet_wrap(~year) +
    scale_color_manual(values = setNames(c(pax_accent, pax_highlight),
                                          c(paste0(blame_var, " = 0"),
                                            paste0(blame_var, " = 1"))),
                       name = NULL) +
    scale_fill_manual(values = setNames(c(pax_accent, pax_highlight),
                                         c(paste0(blame_var, " = 0"),
                                           paste0(blame_var, " = 1"))),
                      name = NULL) +
    labs(
      title = paste0("Predicted Posture by Warmth \u00d7 ", blame_var),
      subtitle = "Survey-weighted OLS; controls at medians/modes; 95% CI",
      x = "Warmth toward China (z-score)",
      y = "Predicted Posture Index (z-score)\n<< Hawkish | Dovish >>"
    ) +
    theme_pax() +
    geom_hline(yintercept = 0, linetype = "dashed", color = neutral_gray,
               alpha = 0.5)

  save_figure(p, filename)
  invisible(preds_all)
}

fig2a_data <- make_interaction_plot(a24, a25, clean_2024, clean_2025,
                                    "blame_china",
                                    "fig2a_posture_warmthXblame_byyear")

fig2a_alt_data <- make_interaction_plot(a24_alt, a25_alt, clean_2024, clean_2025,
                                         "blame_china_alt",
                                         "fig2a_posture_warmthXblameALT_byyear")


# === Section 7: Robustness — Ordered Logit ===================================

log_only("")
log_msg("=== Section 7: Ordered Logit Robustness ===")

run_ordinal_robustness <- function(des_24, des_25, des_pl, ctrl_rhs) {
  items <- c("us_pref_a_ord", "us_pref_b_ord",
             "us_pref_c_r_ord", "us_pref_d_ord")
  item_labels <- c("Defensive (a)", "Cooperative (b)",
                    "Friendly (c, rev.)", "Constructive (d)")

  results <- list()
  idx <- 1

  for (i in seq_along(items)) {
    item <- items[i]
    label <- item_labels[i]

    designs <- list("2024" = des_24, "2025" = des_25, "Pooled" = des_pl)

    for (wv in names(designs)) {
      des <- designs[[wv]]
      fmla_rhs <- paste0("warmth_z * blame_china + ", ctrl_rhs)
      if (wv == "Pooled") fmla_rhs <- paste0(fmla_rhs, " + factor(year)")
      fmla <- as.formula(paste0(item, " ~ ", fmla_rhs))

      fit <- tryCatch(
        svyolr(fmla, design = des),
        error = function(e) {
          log_only(sprintf("  svyolr FAILED: %s [%s]: %s", item, wv, e$message))
          NULL
        }
      )

      if (is.null(fit)) {
        results[[idx]] <- tibble(
          item = label, wave = wv,
          warmth_b = NA_real_, warmth_se = NA_real_, warmth_p = NA_character_,
          blame_b = NA_real_, blame_se = NA_real_, blame_p = NA_character_,
          intxn_b = NA_real_, intxn_se = NA_real_, intxn_p = NA_character_
        )
      } else {
        coefs <- summary(fit)$coefficients
        extract_coef <- function(name) {
          if (name %in% rownames(coefs)) {
            b  <- coefs[name, "Value"]
            se <- coefs[name, "Std. Error"]
            z  <- coefs[name, "t value"]
            p  <- 2 * pnorm(-abs(z))
            list(b = b, se = se, p = fmt_p(p))
          } else {
            list(b = NA_real_, se = NA_real_, p = NA_character_)
          }
        }

        w  <- extract_coef("warmth_z")
        bl <- extract_coef("blame_china")
        ix <- extract_coef("warmth_z:blame_china")

        results[[idx]] <- tibble(
          item = label, wave = wv,
          warmth_b = w$b, warmth_se = w$se, warmth_p = w$p,
          blame_b = bl$b, blame_se = bl$se, blame_p = bl$p,
          intxn_b = ix$b, intxn_se = ix$se, intxn_p = ix$p
        )
      }
      idx <- idx + 1
    }
  }

  bind_rows(results)
}

olr_summary <- run_ordinal_robustness(des_2024, des_2025, des_pool, ctrl_rhs)

olr_display <- olr_summary %>%
  mutate(
    warmth = sprintf("%.3f (%.3f) %s", warmth_b, warmth_se, warmth_p),
    blame  = sprintf("%.3f (%.3f) %s", blame_b, blame_se, blame_p),
    intxn  = sprintf("%.3f (%.3f) %s", intxn_b, intxn_se, intxn_p)
  ) %>%
  select(Item = item, Wave = wave,
         `Warmth (b/SE/p)` = warmth,
         `Blame (b/SE/p)` = blame,
         `Interaction (b/SE/p)` = intxn)

save_table(olr_display, "app_tab_phase3_svyolr_q15items",
           caption = "Ordered Logit Robustness: Individual Posture Items")

log_only("  Ordered logit summary:")
for (i in seq_len(nrow(olr_summary))) {
  r <- olr_summary[i, ]
  log_only(sprintf("    %s [%s]: warmth=%.3f, blame=%.3f, intxn=%.3f",
                  r$item, r$wave, r$warmth_b, r$blame_b, r$intxn_b))
}


# === Section 8: Results Interpretation ========================================

log_only("")
log_msg("=== Section 8: Results Interpretation ===")

#' Extract conditional slopes for a model with warmth_z * blame interaction
#' @return Named list with slope_0, slope_1, CI bounds
conditional_slopes <- function(mod, intxn_term) {
  if (is.null(mod)) return(list(slope_0 = NA, slope_1 = NA,
                                ci_0_lo = NA, ci_0_hi = NA,
                                ci_1_lo = NA, ci_1_hi = NA))
  coefs <- coef(summary(mod))
  V <- vcov(mod)
  t_crit <- qt(0.975, mod$df.residual)

  b_warmth <- coefs["warmth_z", "Estimate"]
  se_warmth <- coefs["warmth_z", "Std. Error"]

  if (!intxn_term %in% rownames(coefs)) {
    return(list(slope_0 = b_warmth, slope_1 = NA,
                ci_0_lo = b_warmth - t_crit * se_warmth,
                ci_0_hi = b_warmth + t_crit * se_warmth,
                ci_1_lo = NA, ci_1_hi = NA))
  }

  b_intxn <- coefs[intxn_term, "Estimate"]

  slope_0 <- b_warmth
  slope_1 <- b_warmth + b_intxn

  # SE for slope_1 via delta method: var(b_warmth) + var(b_intxn) + 2*cov(b_warmth, b_intxn)
  se_0 <- se_warmth
  var_1 <- V["warmth_z", "warmth_z"] +
    V[intxn_term, intxn_term] +
    2 * V["warmth_z", intxn_term]
  se_1 <- sqrt(max(0, var_1))

  list(slope_0 = slope_0, slope_1 = slope_1,
       ci_0_lo = slope_0 - t_crit * se_0, ci_0_hi = slope_0 + t_crit * se_0,
       ci_1_lo = slope_1 - t_crit * se_1, ci_1_hi = slope_1 + t_crit * se_1)
}

log_only("")
log_msg("  PRIMARY CONFIRMATORY SPECIFICATION: apool (DA#5)")
log_msg("  All other models are robustness/exploratory.")

# --- Conditional slopes for all 12 Table 2 models ---
log_only("")
log_only("  Conditional warmth slopes (all 12 Table 2 models):")

table2_models <- list(
  a24 = a24, a25 = a25, apool = apool,
  b24 = b24, b25 = b25, bpool = bpool,
  a24_alt = a24_alt, a25_alt = a25_alt, apool_alt = apool_alt,
  b24_alt = b24_alt, b25_alt = b25_alt, bpool_alt = bpool_alt
)

intxn_sign_count <- c(positive = 0L, negative = 0L, zero = 0L)

for (mn in names(table2_models)) {
  mod <- table2_models[[mn]]
  intxn_term <- if (grepl("_alt", mn)) "warmth_z:blame_china_alt" else "warmth_z:blame_china"
  cs <- conditional_slopes(mod, intxn_term)

  log_only(sprintf("    %s:", mn))
  log_only(sprintf("      slope(blame=0) = %.4f  [%.4f, %.4f]",
                  cs$slope_0, cs$ci_0_lo, cs$ci_0_hi))
  log_only(sprintf("      slope(blame=1) = %.4f  [%.4f, %.4f]",
                  cs$slope_1, cs$ci_1_lo, cs$ci_1_hi))

  # Track interaction direction
  if (!is.null(mod) && intxn_term %in% rownames(coef(summary(mod)))) {
    b_int <- coef(summary(mod))[intxn_term, "Estimate"]
    if (b_int > 0.001) intxn_sign_count["positive"] <- intxn_sign_count["positive"] + 1L
    else if (b_int < -0.001) intxn_sign_count["negative"] <- intxn_sign_count["negative"] + 1L
    else intxn_sign_count["zero"] <- intxn_sign_count["zero"] + 1L
  }
}

# --- Three-branch interpretation (DA#8) ---
log_only("")
if (!is.null(apool)) {
  cs_primary <- conditional_slopes(apool, "warmth_z:blame_china")
  b_int_primary <- coef(summary(apool))["warmth_z:blame_china", "Estimate"]
  p_int_primary <- coef(summary(apool))["warmth_z:blame_china", "Pr(>|t|)"]

  log_msg(sprintf("  Primary spec (apool): interaction b=%.4f, p=%s",
                  b_int_primary, fmt_p(p_int_primary)))

  if (cs_primary$slope_1 < cs_primary$slope_0 - 0.01) {
    log_msg("  FINDING: ATTENUATION — blame weakens the warmth-posture link")
  } else if (cs_primary$slope_1 > cs_primary$slope_0 + 0.01) {
    log_msg("  FINDING: AMPLIFICATION — blame strengthens the warmth-posture link")
    log_msg("  Possible mechanism: blame activates a 'loyalty test' frame")
  } else {
    log_msg("  FINDING: NO MODERATION — blame does not meaningfully alter the association")
  }
}

# --- Consistency-based synthesis (DA#5) ---
log_only("")
log_msg(sprintf("  Consistency: interaction is positive in %d, negative in %d of 12 models",
                intxn_sign_count["positive"], intxn_sign_count["negative"]))

# --- Model B mediator/confounder note (DA#9) ---
log_only("")
log_only("  Mediator/confounder note (DA#9):")
log_only("  behavior_index_z could be confounder OR mediator (warmth->behavior->posture)")
log_only("  Model A = total association; Model B = partial (net of perceived behavior)")
log_only("  Attenuation in B is consistent with EITHER interpretation")
if (!is.null(apool) && !is.null(bpool)) {
  warmth_a <- coef(summary(apool))["warmth_z", "Estimate"]
  warmth_b <- coef(summary(bpool))["warmth_z", "Estimate"]
  pct_change <- 100 * (warmth_b - warmth_a) / abs(warmth_a)
  log_only(sprintf("  warmth_z main effect: A=%.4f, B=%.4f (%.1f%% change)",
                  warmth_a, warmth_b, pct_change))
}

# --- Power note for blame_china_alt (DA#7) ---
log_only("")
log_only("  Power note (DA#7): blame_china_alt wave-specific models are EXPLORATORY")
log_only("  Non-significance does NOT imply no effect — may reflect inadequate power")
for (mn in c("a24_alt", "a25_alt")) {
  mod <- table2_models[[mn]]
  if (!is.null(mod)) {
    # Use model.frame for actual analytic sample (after listwise deletion)
    mf <- model.frame(mod)
    n_blame1 <- sum(mf$blame_china_alt == 1, na.rm = TRUE)
    log_only(sprintf("    %s: N(blame_alt=1) ~ %d in analytic sample", mn, n_blame1))
  }
}

# --- Limitations ---
log_only("")
log_only("  LIMITATIONS:")
log_only("  - Causal identification: cross-sectional, observational. All associations correlational.")
log_only("  - Cannot rule out reverse causality or omitted confounders (nationalism, media diet).")
log_only("  - Repeated cross-sections: wave differences could reflect attitude shifts or composition changes.")

interpretation_text <- log_env$entries[grep("Section 8", log_env$entries):length(log_env$entries)]


# === Section 9: Sanity Check & Verification ==================================

log_only("")
log_msg("=== Section 9: Sanity Checks ===")

# --- Binned means sanity plot (weighted, DA#17) ---
log_only("  Building binned means sanity plot (weighted means)...")

brk <- seq(-3, 3, by = 0.75)
sanity_df <- pooled %>%
  filter(!is.na(warmth_z) & !is.na(posture_z) & !is.na(blame_china) &
           !is.na(weight)) %>%
  mutate(
    warmth_bin = cut(warmth_z, breaks = brk, include.lowest = TRUE),
    blame_label = ifelse(blame_china == 1,
                         "blame_china = 1", "blame_china = 0")
  ) %>%
  group_by(warmth_bin, blame_label, year) %>%
  summarise(
    mean_posture = weighted.mean(posture_z, weight, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  filter(n >= 10) %>%
  mutate(
    warmth_mid = (brk[as.integer(warmth_bin)] + brk[as.integer(warmth_bin) + 1]) / 2
  )

p_sanity <- ggplot() +
  geom_point(data = sanity_df,
             aes(x = warmth_mid, y = mean_posture,
                 color = blame_label, size = n),
             alpha = 0.7) +
  geom_line(data = fig2a_data,
            aes(x = warmth_z, y = estimate, color = blame_label),
            linewidth = 0.8, linetype = "dashed") +
  facet_wrap(~year) +
  scale_color_manual(values = c("blame_china = 0" = pax_accent,
                                 "blame_china = 1" = pax_highlight),
                     name = NULL) +
  scale_size_continuous(range = c(1, 5), guide = "none") +
  labs(title = "Sanity Check: Weighted Binned Means vs. Model Predictions",
       subtitle = "Points = weighted binned sample means; dashed lines = OLS predictions",
       x = "Warmth toward China (z-score bins)",
       y = "Mean Posture (z-score)") +
  theme_pax()

save_figure(p_sanity, "fig_sanity_binned_means")


# === Section 10: Save Objects & Log ==========================================

log_only("")
log_msg("=== Section 10: Save Objects & Log ===")

phase3_objects <- list(
  # Panel A: blame_china
  a24 = a24, a25 = a25, apool = apool,
  b24 = b24, b25 = b25, bpool = bpool,
  r1pool = r1pool,
  # Panel B: blame_china_alt
  a24_alt = a24_alt, a25_alt = a25_alt, apool_alt = apool_alt,
  b24_alt = b24_alt, b25_alt = b25_alt, bpool_alt = bpool_alt,
  r1pool_alt = r1pool_alt,
  # Diagnostics
  posture_alpha    = alpha_posture,
  behavior_alpha   = alpha_behavior,
  pca_posture      = pca_posture,
  pca_behavior     = pca_behavior,
  warmth_behavior_cor = cor_wb,
  warmth_blame_cor    = cor_wb_blame,
  fig2a_data       = fig2a_data,
  fig2a_alt_data   = fig2a_alt_data,
  forest_data      = forest_data,
  robustness_summary = olr_summary,
  interpretation   = interpretation_text,
  warmth_moments   = c(mu = warmth_mu, sd = warmth_sd),
  posture_moments  = c(mu = posture_mu, sd = posture_sd),
  behavior_moments = c(mu = behavior_mu, sd = behavior_sd)
)
saveRDS(phase3_objects, here("output", "tables", "phase3_objects.rds"))
log_only("  Saved phase3_objects.rds")

# File manifest
output_files <- c(
  "output/tables/tab2_phase3_main_models.tex",
  "output/tables/tab2_phase3_main_models.csv",
  "output/tables/app_tab_phase3_r1_models.tex",
  "output/tables/app_tab_phase3_r1_models.csv",
  "output/tables/app_tab_phase3_svyolr_q15items.tex",
  "output/tables/app_tab_phase3_svyolr_q15items.csv",
  "output/figures/fig2a_posture_warmthXblame_byyear.pdf",
  "output/figures/fig2a_posture_warmthXblameALT_byyear.pdf",
  "output/figures/fig_coef_forest_interaction.pdf",
  "output/figures/fig_sanity_binned_means.pdf",
  "output/tables/phase3_objects.rds",
  "output/tables/phase3_log.txt"
)

log_only("  File manifest:")
for (f in output_files) {
  exists_flag <- ifelse(file.exists(here(f)), "EXISTS", "MISSING")
  if (grepl("phase3_log", f)) exists_flag <- "PENDING"
  log_only(sprintf("    %s [%s]", f, exists_flag))
}

# sessionInfo
log_only("")
log_only("  sessionInfo():")
si <- capture.output(sessionInfo())
for (line in si) log_only(paste("    ", line))

log_only("")
log_only("========================================================================")
log_only("PHASE 3 COMPLETE (v3.2)")
log_only("========================================================================")
message("Phase 3 COMPLETE (v3.2)")

writeLines(log_env$entries, here("output", "tables", "phase3_log.txt"))
message(sprintf("Phase 3 log saved to: %s",
                here("output", "tables", "phase3_log.txt")))
