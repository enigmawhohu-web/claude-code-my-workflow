#' ============================================================================
#' 09_wave_models.R — R&R Referee Additions: Triple Interaction and
#'   Within-Wave Replication (MOD-4, MOD-6)
#' Poll 2025: Fentanyl Policy Survey Analysis
#' PAX sapiens Foundation
#'
#' @author PAX sapiens Foundation
#'
#' @description Standalone script addressing two R&R referee comments:
#'   - MOD-4: Triple interaction warmth_z * blame_china * year, testing
#'     whether the two-way warmth x blame interaction differs between
#'     the 2024 and 2025 survey waves.
#'   - MOD-6: Within-wave OLS estimates for 2024 and 2025 separately,
#'     replicating the main Phase 3 spec (posture_z ~ warmth_z *
#'     blame_china + controls) in each wave independently.
#'
#' @question Does the warmth x blame moderation effect vary across waves,
#'   and what are the wave-specific within-sample interaction estimates?
#'
#' @input  Data/cleaned/pooled.rds
#'         Data/cleaned/clean_2024.rds
#'         Data/cleaned/clean_2025.rds
#'
#' @output prism/tables/app_tab_triple_interaction.{tex,csv}
#'           Three-row summary: 2024 interaction, 2025-2024 differential
#'           (the triple-interaction term), and the implied 2025
#'           interaction (delta-method sum).
#'         prism/tables/app_tab_within_wave.{tex,csv}
#'           Two-column comparison (2024, 2025, Pooled) of the main
#'           warmth_z:blame_china coefficient.
#'         Output/09_wave_models_log.txt
#'           Full diagnostic log.
#'
#' @depends here, tidyverse, survey, srvyr, kableExtra
#'
#' @notes  This script is STANDALONE — it does not source 03_models.R.
#'         It re-runs the same variable construction and model estimation
#'         from scratch using the same pooled moments and formula.
#'         Output files with the same name as 03_models.R outputs are
#'         guarded: the script will not overwrite them if they already
#'         exist; a notice is written to the log instead.
#' ============================================================================

# === Section 0: Setup =========================================================

library(here)
library(tidyverse)
library(survey)
library(srvyr)
library(kableExtra)

set.seed(20250217)  # Same seed as 03_models.R for reproducibility

dir.create(here("prism", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("Output"),          recursive = TRUE, showWarnings = FALSE)

# --- Logging ------------------------------------------------------------------
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
log_only("09_wave_models.R — MOD-4 (Triple Interaction) + MOD-6 (Within-Wave)")
log_only(sprintf("Timestamp: %s", Sys.time()))
log_only("========================================================================")
message("09_wave_models.R: Starting MOD-4 and MOD-6 wave analysis")

# --- Helper: p-value formatter ------------------------------------------------
fmt_p <- function(p) {
  stars <- dplyr::case_when(
    is.na(p)  ~ "",
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ ""
  )
  ifelse(is.na(p), "\u2014",
         paste0(formatC(p, format = "f", digits = 3), stars))
}

# --- Helper: save table (guard against overwrite) -----------------------------
#' Save a tibble as .tex (bare tabular) and .csv.
#' If the .tex file already exists, logs a notice and skips the write.
#'
#' @param tbl_df  Data frame to write
#' @param name    Base name (no extension) under prism/tables/
#' @param force   If TRUE, overwrite even if file exists (default FALSE)
save_table <- function(tbl_df, name, force = FALSE) {
  csv_path <- here("prism", "tables", paste0(name, ".csv"))
  tex_path <- here("prism", "tables", paste0(name, ".tex"))

  if (!force && file.exists(tex_path)) {
    log_msg(sprintf("  SKIP (exists): %s.tex  [set force=TRUE to overwrite]", name))
    return(invisible(NULL))
  }

  write_csv(tbl_df, csv_path)

  latex_out <- tbl_df %>%
    kbl(format = "latex", booktabs = TRUE, caption = NULL, linesep = "")
  writeLines(as.character(latex_out), tex_path)
  log_msg(sprintf("  Saved: %s.tex + %s.csv", name, name))
}


# === Section 1: Load Data =====================================================

log_only("")
log_msg("=== Section 1: Load Data ===")

pooled     <- readRDS(here("Data", "cleaned", "pooled.rds"))
clean_2024 <- readRDS(here("Data", "cleaned", "clean_2024.rds"))
clean_2025 <- readRDS(here("Data", "cleaned", "clean_2025.rds"))

log_msg(sprintf("  Loaded: pooled=%d, 2024=%d, 2025=%d",
                nrow(pooled), nrow(clean_2024), nrow(clean_2025)))

stopifnot("Pooled N must equal sum of wave Ns" =
            nrow(pooled) == nrow(clean_2024) + nrow(clean_2025))
log_only("  PASS: pooled N == 2024 N + 2025 N")


# === Section 2: Variable Construction =========================================

log_only("")
log_msg("=== Section 2: Variable Construction ===")
log_only("  Using identical construction to 03_models.R (v3.2).")
log_only("  Standardisation uses pooled-level moments for z-scores.")

#' Build analysis variables (mirrors build_analysis_vars in 03_models.R v3.2).
#' All three datasets share the same z-scale (pooled moments).
#'
#' @param df          Data frame (pooled, clean_2024, or clean_2025)
#' @param warmth_mu   Pooled mean of warmth
#' @param warmth_sd   Pooled SD of warmth
#' @param posture_mu  Pooled mean of raw posture index
#' @param posture_sd  Pooled SD of raw posture index
#' @param behavior_mu Pooled mean of behavior index
#' @param behavior_sd Pooled SD of behavior index
#' @return df with new analysis variables appended
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

      # --- Warmth (IV 1) ---
      warmth_z = (warmth - warmth_mu) / warmth_sd,

      # --- Behavior index (for completeness; not used in MOD-4/MOD-6 specs) ---
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

      # --- Exposure index (Unsure → NA; identical to 03_models.R) ---
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
      exposure_index = case_when(
        is.na(know_died_bin) & is.na(overdose_bin) ~ NA_integer_,
        TRUE ~ coalesce(know_died_bin, 0L) + coalesce(overdose_bin, 0L)
      ),

      # --- Political attention (reversed) ---
      newsint_attn = {
        ni <- as.numeric(newsint)
        ifelse(ni %in% 1:4, 5L - ni, NA_integer_)
      },

      # --- Ideology (recode "Not sure" 6 → NA) ---
      ideo5_clean = {
        id <- as.numeric(ideo5)
        ifelse(id %in% 1:5, id, NA_real_)
      }
    )
}

# --- Pooled moments (unweighted, identical to 03_models.R) -------------------
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
warmth_sd   <- sd(pooled$warmth,   na.rm = TRUE)
posture_mu  <- mean(pooled_raw$posture_index_raw,  na.rm = TRUE)
posture_sd  <- sd(pooled_raw$posture_index_raw,   na.rm = TRUE)
behavior_mu <- mean(pooled_raw$behavior_index_raw, na.rm = TRUE)
behavior_sd <- sd(pooled_raw$behavior_index_raw,  na.rm = TRUE)

stopifnot("warmth_sd must be positive"  = warmth_sd  > 0)
stopifnot("posture_sd must be positive" = posture_sd > 0)
stopifnot("behavior_sd must be positive" = behavior_sd > 0)

log_only(sprintf("  Pooled warmth:    mean=%.3f, sd=%.3f", warmth_mu, warmth_sd))
log_only(sprintf("  Pooled posture:   mean=%.3f, sd=%.3f", posture_mu, posture_sd))
log_only(sprintf("  Pooled behavior:  mean=%.3f, sd=%.3f", behavior_mu, behavior_sd))

# Apply to all three datasets --------------------------------------------------
pooled     <- build_analysis_vars(pooled, warmth_mu, warmth_sd,
                                  posture_mu, posture_sd,
                                  behavior_mu, behavior_sd)
clean_2024 <- build_analysis_vars(clean_2024, warmth_mu, warmth_sd,
                                  posture_mu, posture_sd,
                                  behavior_mu, behavior_sd)
clean_2025 <- build_analysis_vars(clean_2025, warmth_mu, warmth_sd,
                                  posture_mu, posture_sd,
                                  behavior_mu, behavior_sd)

log_only("  Variable construction complete (posture_z, warmth_z, exposure_index, newsint_attn, ideo5_clean)")


# === Section 3: Survey Design Objects =========================================

log_only("")
log_msg("=== Section 3: Survey Design Objects ===")

# ids = ~1: appropriate for online panel (no geographic clustering)
des_2024 <- svydesign(ids = ~1, weights = ~weight, data = clean_2024)
des_2025 <- svydesign(ids = ~1, weights = ~weight, data = clean_2025)
des_pool <- svydesign(ids = ~1, weights = ~weight, data = pooled)

log_only("  des_2024, des_2025, des_pool created")
log_only(sprintf("  Weight check: 2024 mean=%.6f, 2025 mean=%.6f",
                 mean(clean_2024$weight, na.rm = TRUE),
                 mean(clean_2025$weight, na.rm = TRUE)))


# === Section 4: Controls Formula ==============================================

log_only("")
log_msg("=== Section 4: Controls Formula ===")

# Identical to 03_models.R ctrl_rhs
ctrl_rhs <- "party3 + ideo5_clean + educ_college + age + female + race_eth + newsint_attn + exposure_index"

log_only(sprintf("  ctrl_rhs: %s", ctrl_rhs))

# Listwise deletion counts
model_vars_base <- c("posture_z", "warmth_z", "blame_china",
                     "party3", "ideo5_clean", "educ_college",
                     "age", "female", "race_eth",
                     "newsint_attn", "exposure_index")

for (wv in c("2024", "2025", "pooled")) {
  d <- switch(wv, "2024" = clean_2024, "2025" = clean_2025, pooled)
  n_complete <- sum(complete.cases(d[, model_vars_base]))
  n_total    <- nrow(d)
  log_only(sprintf("  %s: %d/%d complete cases (%.1f%% lost)",
                   wv, n_complete, n_total,
                   100 * (1 - n_complete / n_total)))
}


# === Section 5: Model Estimation Helper =======================================

log_only("")
log_msg("=== Section 5: Fit survey-weighted OLS models ===")

#' Fit a single svyglm specification with error trapping.
#'
#' @param fmla_rhs  Right-hand side formula string
#' @param des       svydesign object
#' @param label     Label for logging
#' @return Fitted svyglm object, or NULL on failure
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

#' Log key coefficients for a model.
#'
#' @param mod            Fitted svyglm (or NULL)
#' @param label          Label for logging
#' @param target_terms   Character vector of term names to log
log_model_terms <- function(mod, label, target_terms) {
  if (is.null(mod)) {
    log_msg(sprintf("  %s: FAILED (NULL model)", label))
    return(invisible(NULL))
  }
  cs   <- coef(summary(mod))
  n    <- nobs(mod)
  r2   <- 1 - mod$deviance / mod$null.deviance
  log_msg(sprintf("  %s: N=%d, R2=%.4f", label, n, r2))
  for (tv in target_terms) {
    if (tv %in% rownames(cs)) {
      log_msg(sprintf("    %s: b=%.4f, SE=%.4f, p=%s",
                      tv,
                      cs[tv, "Estimate"],
                      cs[tv, "Std. Error"],
                      fmt_p(cs[tv, "Pr(>|t|)"])))
    } else {
      log_only(sprintf("    %s: term not found in model", tv))
    }
  }
}


# === Section 6: MOD-4 — Triple Interaction ====================================

log_only("")
log_msg("=== Section 6: MOD-4 — Triple Interaction (warmth_z * blame_china * factor(year)) ===")
log_only("")
log_only("  Specification: posture_z ~ warmth_z * blame_china * factor(year) + [controls]")
log_only("  Estimator: svyglm (gaussian), weights = ~weight (des_pool)")
log_only("  Purpose: Test whether the warmth x blame interaction is stable across waves.")
log_only("  The three-way term `warmth_z:blame_china:factor(year)2025` is the key quantity;")
log_only("  a near-zero, non-significant coefficient supports cross-wave stability.")

# Fit triple-interaction model on pooled data
apool_triple <- fit_spec(
  paste0("warmth_z * blame_china * factor(year) + ", ctrl_rhs),
  des_pool, "apool_triple"
)

log_model_terms(apool_triple,
  "apool_triple",
  c("warmth_z", "blame_china", "warmth_z:blame_china",
    "factor(year)2025", "warmth_z:factor(year)2025",
    "blame_china:factor(year)2025",
    "warmth_z:blame_china:factor(year)2025"))

# --- Build output table -------------------------------------------------------
triple_output <- NULL

if (!is.null(apool_triple)) {
  cs_t   <- coef(summary(apool_triple))
  vcov_t <- vcov(apool_triple)
  n_t    <- nobs(apool_triple)

  int_main_name <- "warmth_z:blame_china"

  # Locate the triple-interaction term (robust to factor label suffix)
  tri_candidates <- grep("^warmth_z:blame_china:factor\\(year\\)",
                         rownames(cs_t), value = TRUE)
  tri_name <- if (length(tri_candidates) >= 1) tri_candidates[1] else NA_character_

  if (int_main_name %in% rownames(cs_t) && !is.na(tri_name)) {

    # 2024 interaction = warmth_z:blame_china (baseline year is 2024 when year is factor)
    b_main  <- cs_t[int_main_name, "Estimate"]
    se_main <- cs_t[int_main_name, "Std. Error"]
    p_main  <- cs_t[int_main_name, "Pr(>|t|)"]

    # Triple-interaction term: 2025 - 2024 differential
    b_diff  <- cs_t[tri_name, "Estimate"]
    se_diff <- cs_t[tri_name, "Std. Error"]
    p_diff  <- cs_t[tri_name, "Pr(>|t|)"]

    # Delta-method: implied 2025 interaction = main + diff
    v_main <- vcov_t[int_main_name, int_main_name]
    v_diff <- vcov_t[tri_name, tri_name]
    cov_md <- vcov_t[int_main_name, tri_name]
    b_2025  <- b_main + b_diff
    se_2025 <- sqrt(v_main + v_diff + 2 * cov_md)
    # Normal-approximation p-value (df is large for survey data)
    z_2025  <- b_2025 / se_2025
    p_2025  <- 2 * pnorm(-abs(z_2025))

    ci_lo_main <- b_main  - 1.96 * se_main
    ci_hi_main <- b_main  + 1.96 * se_main
    ci_lo_diff <- b_diff  - 1.96 * se_diff
    ci_hi_diff <- b_diff  + 1.96 * se_diff
    ci_lo_2025 <- b_2025  - 1.96 * se_2025
    ci_hi_2025 <- b_2025  + 1.96 * se_2025

    log_msg(sprintf("  [MOD-4] 2024 interaction (warmth_z:blame_china):          b=%.4f, SE=%.4f, p=%s",
                    b_main, se_main, fmt_p(p_main)))
    log_msg(sprintf("  [MOD-4] 2025-2024 differential (%s): b=%.4f, SE=%.4f, p=%s",
                    tri_name, b_diff, se_diff, fmt_p(p_diff)))
    log_msg(sprintf("  [MOD-4] 2025 implied interaction (delta-method sum):       b=%.4f, SE=%.4f, p=%s",
                    b_2025, se_2025, fmt_p(p_2025)))

    # Stability assessment
    if (abs(b_diff) < 0.05 || p_diff > 0.10) {
      log_msg("  [MOD-4] Stability: triple-interaction is small or non-significant (p>.10) — cross-wave stability supported")
    } else {
      log_msg("  [MOD-4] Stability: triple-interaction is non-trivial — interaction strength may differ by wave")
    }

    triple_output <- tibble(
      Quantity = c(
        "2024 interaction (warmth x blame)",
        "2025 vs. 2024 differential (triple term)",
        "2025 implied interaction (delta-method)"
      ),
      Estimate = sprintf("%.4f", c(b_main, b_diff, b_2025)),
      SE       = sprintf("%.4f", c(se_main, se_diff, se_2025)),
      `95% CI` = c(
        sprintf("[%.4f, %.4f]", ci_lo_main, ci_hi_main),
        sprintf("[%.4f, %.4f]", ci_lo_diff, ci_hi_diff),
        sprintf("[%.4f, %.4f]", ci_lo_2025, ci_hi_2025)
      ),
      `p-value` = c(fmt_p(p_main), fmt_p(p_diff), fmt_p(p_2025)),
      N = rep(n_t, 3)
    )

    log_only("")
    log_only("  Triple interaction table:")
    for (i in seq_len(nrow(triple_output))) {
      log_only(sprintf("    %s: est=%s, SE=%s, CI=%s, p=%s",
                       triple_output$Quantity[i],
                       triple_output$Estimate[i],
                       triple_output$SE[i],
                       triple_output$`95% CI`[i],
                       triple_output$`p-value`[i]))
    }

    # Save (guard: do not overwrite if already produced by 03_models.R)
    save_table(triple_output, "app_tab_triple_interaction")

  } else {
    log_msg("  [MOD-4] WARNING: expected coefficient names not found in apool_triple — skipping table")
    log_only(sprintf("  Available terms: %s", paste(rownames(cs_t), collapse = ", ")))
  }
} else {
  log_msg("  [MOD-4] apool_triple failed to fit — skipping table")
}


# === Section 7: MOD-6 — Within-Wave OLS =======================================

log_only("")
log_msg("=== Section 7: MOD-6 — Within-Wave OLS (2024, 2025 separately) ===")
log_only("")
log_only("  Specification: posture_z ~ warmth_z * blame_china + [controls]")
log_only("  No year FE (each model is a single wave; year is constant within each sample).")
log_only("  Estimator: svyglm (gaussian), weights = ~weight")
log_only("  Purpose: Within-wave replication of the main interaction for each wave separately,")
log_only("    plus pooled model for direct comparison.")

# --- Fit the three models -----------------------------------------------------
a24 <- fit_spec(
  paste0("warmth_z * blame_china + ", ctrl_rhs),
  des_2024, "a24_w09"
)

a25 <- fit_spec(
  paste0("warmth_z * blame_china + ", ctrl_rhs),
  des_2025, "a25_w09"
)

apool <- fit_spec(
  paste0("warmth_z * blame_china + factor(year) + ", ctrl_rhs),
  des_pool, "apool_w09"
)

# Log key terms
for (obj_name in c("a24", "a25", "apool")) {
  mod <- get(obj_name)
  extra_term <- if (obj_name == "apool") "factor(year)2025" else character(0)
  log_model_terms(mod, obj_name,
                  c("warmth_z", "blame_china", "warmth_z:blame_china",
                    extra_term))
}

# --- Build within-wave comparison table ---------------------------------------
#' Extract a single row for the comparison table.
#'
#' @param mod         Fitted svyglm (or NULL)
#' @param wave_label  Label for the Wave column
#' @return One-row tibble
extract_wave_row <- function(mod, wave_label) {
  na_row <- tibble(
    Wave                = wave_label,
    N                   = NA_integer_,
    `Warmth (b/SE)`     = "\u2014",
    `Blame (b/SE)`      = "\u2014",
    `Interaction (b)`   = "\u2014",
    `Interaction (SE)`  = "\u2014",
    `Interaction (p)`   = "\u2014",
    `Interaction 95% CI` = "\u2014"
  )
  if (is.null(mod)) return(na_row)

  cs    <- coef(summary(mod))
  vcov_ <- vcov(mod)

  get_term <- function(name, col) {
    if (name %in% rownames(cs)) cs[name, col] else NA_real_
  }

  b_w   <- get_term("warmth_z",          "Estimate")
  se_w  <- get_term("warmth_z",          "Std. Error")
  b_bl  <- get_term("blame_china",       "Estimate")
  se_bl <- get_term("blame_china",       "Std. Error")
  b_i   <- get_term("warmth_z:blame_china", "Estimate")
  se_i  <- get_term("warmth_z:blame_china", "Std. Error")
  p_i   <- get_term("warmth_z:blame_china", "Pr(>|t|)")

  ci_lo <- if (!is.na(b_i) && !is.na(se_i)) b_i - 1.96 * se_i else NA_real_
  ci_hi <- if (!is.na(b_i) && !is.na(se_i)) b_i + 1.96 * se_i else NA_real_

  tibble(
    Wave                = wave_label,
    N                   = nobs(mod),
    `Warmth (b/SE)`     = if (!is.na(b_w))  sprintf("%.4f (%.4f)", b_w,  se_w)  else "\u2014",
    `Blame (b/SE)`      = if (!is.na(b_bl)) sprintf("%.4f (%.4f)", b_bl, se_bl) else "\u2014",
    `Interaction (b)`   = if (!is.na(b_i))  sprintf("%.4f", b_i)  else "\u2014",
    `Interaction (SE)`  = if (!is.na(se_i)) sprintf("%.4f", se_i) else "\u2014",
    `Interaction (p)`   = fmt_p(p_i),
    `Interaction 95% CI` = if (!is.na(ci_lo) && !is.na(ci_hi))
                             sprintf("[%.4f, %.4f]", ci_lo, ci_hi)
                           else "\u2014"
  )
}

wave_table <- bind_rows(
  extract_wave_row(a24,   "2024"),
  extract_wave_row(a25,   "2025"),
  extract_wave_row(apool, "Pooled (year FE)")
)

log_msg("")
log_msg("  [MOD-6] Within-wave interaction summary:")
for (i in seq_len(nrow(wave_table))) {
  log_msg(sprintf("    %s: N=%s, intxn=%s (SE=%s), p=%s, CI=%s",
                  wave_table$Wave[i],
                  as.character(wave_table$N[i]),
                  wave_table$`Interaction (b)`[i],
                  wave_table$`Interaction (SE)`[i],
                  wave_table$`Interaction (p)`[i],
                  wave_table$`Interaction 95% CI`[i]))
}

# Save — app_tab_within_wave is new (03_models.R writes app_tab_wave_within)
save_table(wave_table, "app_tab_within_wave")


# === Section 8: Cross-Check ===================================================

log_only("")
log_msg("=== Section 8: Cross-Check Against 03_models.R ===")
log_only("  Comparing this script's interaction estimates against phase3_objects.rds (if available).")

phase3_path <- here("prism", "tables", "phase3_objects.rds")
if (file.exists(phase3_path)) {
  p3 <- tryCatch(readRDS(phase3_path), error = function(e) NULL)

  if (!is.null(p3)) {
    for (obj_name in c("a24", "a25", "apool")) {
      p3_mod  <- p3[[obj_name]]
      this_mod <- get(obj_name)
      if (is.null(p3_mod) || is.null(this_mod)) next

      p3_cs   <- coef(summary(p3_mod))
      this_cs <- coef(summary(this_mod))

      if ("warmth_z:blame_china" %in% rownames(p3_cs) &&
          "warmth_z:blame_china" %in% rownames(this_cs)) {
        b_p3   <- p3_cs["warmth_z:blame_china", "Estimate"]
        b_this <- this_cs["warmth_z:blame_china", "Estimate"]
        diff_b <- abs(b_p3 - b_this)
        status <- if (diff_b < 1e-4) "MATCH" else if (diff_b < 1e-3) "CLOSE" else "DIVERGE"
        log_msg(sprintf("  %s warmth_z:blame_china: 03_models=%.6f, 09_wave=%.6f, diff=%.6f [%s]",
                        obj_name, b_p3, b_this, diff_b, status))
      }
    }
  } else {
    log_only("  phase3_objects.rds loaded but failed to parse — skipping cross-check")
  }
} else {
  log_only("  phase3_objects.rds not found — run 03_models.R first for cross-check")
}


# === Section 9: Session Info & Log Save =======================================

log_only("")
log_msg("=== Section 9: Session Info ===")
si <- capture.output(sessionInfo())
for (line in si) log_only(paste("  ", line))

log_only("")
log_only("========================================================================")
log_only("09_wave_models.R COMPLETE")
log_only(sprintf("Timestamp: %s", Sys.time()))
log_only("========================================================================")

message("09_wave_models.R COMPLETE")

# File manifest
output_files <- c(
  "prism/tables/app_tab_triple_interaction.tex",
  "prism/tables/app_tab_triple_interaction.csv",
  "prism/tables/app_tab_within_wave.tex",
  "prism/tables/app_tab_within_wave.csv",
  "Output/09_wave_models_log.txt"
)

log_only("  File manifest:")
for (f in output_files) {
  flag <- if (grepl("_log\\.txt$", f)) "PENDING" else
            if (file.exists(here(f))) "EXISTS" else "MISSING"
  log_only(sprintf("    %s [%s]", f, flag))
}

writeLines(log_env$entries, here("Output", "09_wave_models_log.txt"))
message(sprintf("Log saved to: %s", here("Output", "09_wave_models_log.txt")))
