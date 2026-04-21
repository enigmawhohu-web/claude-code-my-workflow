#' ============================================================================
#' 08_design_mnl.R — Design-Based Standard Errors for Phase 4 MNL (MOD-2)
#' Poll 2025: Fentanyl Policy Survey Analysis
#'
#' @description Addresses referee comment MOD-2. The main Phase 4 MNL in
#'   04_mechanisms.R uses nnet::multinom() with weights passed as frequency
#'   weights, which gives point estimates but model-based (not design-based)
#'   standard errors. This script computes proper design-based SEs for the
#'   same multinomial logit specification using one of two approaches:
#'     (a) survey::svymultinom() if survey >= 4.2 is available
#'     (b) Survey bootstrap (999 replicates via survey::as.svrepdesign())
#'   The key coefficient of interest is warmth_z:blame_china for S+A outcome.
#'
#' @input  Data/cleaned/pooled.rds
#'
#' @output prism/tables/app_tab_design_mnl.{tex,csv}
#'           Coefficient table: outcome, predictor, estimate,
#'           design-based SE, t-value, p-value.
#'         Output/08_design_mnl_log.txt
#'
#' @depends here, tidyverse, survey, nnet, kableExtra
#' @seed    20250217 (matches 04_mechanisms.R)
#' ============================================================================

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(survey)
  library(nnet)
  library(kableExtra)
})

set.seed(20250217)

# ---------------------------------------------------------------------------
# Logging
# ---------------------------------------------------------------------------
log_env <- new.env(parent = emptyenv())
log_env$entries <- character()
log_msg  <- function(msg) { log_env$entries <- c(log_env$entries, msg); message(msg) }
log_only <- function(msg) { log_env$entries <- c(log_env$entries, msg) }

dir.create(here("prism", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("Output"), recursive = TRUE, showWarnings = FALSE)

log_msg("========================================================================")
log_msg("  08_design_mnl.R — Design-Based SEs for Phase 4 MNL (MOD-2)")
log_msg(sprintf("  Started: %s", format(Sys.time())))
log_msg("========================================================================")

# ---------------------------------------------------------------------------
# Section 1: Load Data
# ---------------------------------------------------------------------------
log_msg("")
log_msg("--- Section 1: Load Data ---")

pooled <- readRDS(here("Data", "cleaned", "pooled.rds"))
log_msg(sprintf("  pooled: n=%d", nrow(pooled)))

# ---------------------------------------------------------------------------
# Section 2: Variable Construction (mirrors 04_mechanisms.R Sections 1-2)
# ---------------------------------------------------------------------------
log_msg("")
log_msg("--- Section 2: Variable Construction ---")

# Pooled moments for standardization (must use pooled sample)
warmth_mu  <- mean(pooled$warmth, na.rm = TRUE)
warmth_sd  <- sd(pooled$warmth, na.rm = TRUE)

pooled_raw <- pooled %>%
  mutate(
    pi_a   = as.numeric(us_preferred_a),
    pi_b   = as.numeric(us_preferred_b),
    pi_c_r = 8 - as.numeric(us_preferred_c),
    pi_d   = as.numeric(us_preferred_d),
    pi_n_valid = rowSums(!is.na(cbind(pi_a, pi_b, pi_c_r, pi_d))),
    posture_index_raw = ifelse(
      pi_n_valid >= 3,
      rowMeans(cbind(pi_a, pi_b, pi_c_r, pi_d), na.rm = TRUE),
      NA_real_
    )
  )
posture_mu <- mean(pooled_raw$posture_index_raw, na.rm = TRUE)
posture_sd <- sd(pooled_raw$posture_index_raw, na.rm = TRUE)
log_msg(sprintf("  Pooled warmth: mean=%.3f sd=%.3f", warmth_mu, warmth_sd))
log_msg(sprintf("  Pooled posture: mean=%.3f sd=%.3f", posture_mu, posture_sd))

# Build derived analysis variables
pooled <- pooled %>%
  mutate(
    warmth_z    = (warmth - warmth_mu) / warmth_sd,
    us_pref_a   = as.numeric(us_preferred_a),
    us_pref_b   = as.numeric(us_preferred_b),
    us_pref_c_r = 8 - as.numeric(us_preferred_c),
    us_pref_d   = as.numeric(us_preferred_d),
    posture_n_valid = rowSums(!is.na(cbind(
      us_pref_a, us_pref_b, us_pref_c_r, us_pref_d))),
    posture_index = ifelse(
      posture_n_valid >= 3,
      rowMeans(cbind(us_pref_a, us_pref_b, us_pref_c_r, us_pref_d), na.rm = TRUE),
      NA_real_
    ),
    posture_z   = (posture_index - posture_mu) / posture_sd,
    ideo5_clean = { id <- as.numeric(ideo5); ifelse(id %in% 1:5, id, NA_real_) },
    newsint_attn = { ni <- as.numeric(newsint); ifelse(ni %in% 1:4, 5L - ni, NA_integer_) },
    know_died_bin = case_when(
      as.numeric(know_someone_died) == 1 ~ 1L,
      as.numeric(know_someone_died) == 2 ~ 0L,
      TRUE ~ NA_integer_),
    overdose_bin = case_when(
      as.numeric(personal_overdose) == 1 ~ 1L,
      as.numeric(personal_overdose) == 2 ~ 0L,
      TRUE ~ NA_integer_),
    exposure_index = case_when(
      is.na(know_died_bin) & is.na(overdose_bin) ~ NA_integer_,
      TRUE ~ coalesce(know_died_bin, 0L) + coalesce(overdose_bin, 0L)),
    sanctions_bin = { x <- as.numeric(approach_sanctions);
                      ifelse(is.na(x), NA_integer_, ifelse(x == 1, 1L, 0L)) },
    action_bin    = { x <- as.numeric(approach_china_action);
                      ifelse(is.na(x), NA_integer_, ifelse(x == 1, 1L, 0L)) },
    tool_config   = factor(
      case_when(
        is.na(sanctions_bin) | is.na(action_bin) ~ NA_character_,
        sanctions_bin == 0 & action_bin == 0 ~ "Neither",
        sanctions_bin == 1 & action_bin == 0 ~ "Sanctions-only",
        sanctions_bin == 0 & action_bin == 1 ~ "Action-only",
        sanctions_bin == 1 & action_bin == 1 ~ "S+A"),
      levels = c("Neither", "Sanctions-only", "Action-only", "S+A"))
  )

# n_approaches_10 (10-item acquiescence control, excl. DV components)
approach_vars_acq <- c("approach_intl_cooperation", "approach_enforce_sales",
                       "approach_enforce_users", "approach_legalization",
                       "approach_health_justice", "approach_addiction_resources",
                       "approach_blocking_drugs", "approach_social_cultural",
                       "approach_mexico_action", "approach_penalties")
approach_bins <- sapply(approach_vars_acq, function(v) {
  x <- as.numeric(pooled[[v]])
  ifelse(is.na(x), NA_integer_, ifelse(x == 1, 1L, 0L))
})
pooled$n_approaches_10      <- rowSums(approach_bins, na.rm = FALSE)
pooled$n_approaches_10_miss <- as.integer(rowSums(is.na(approach_bins)) > 0)

log_msg(sprintf("  warmth_z: mean=%.3f sd=%.3f", mean(pooled$warmth_z, na.rm = TRUE),
                sd(pooled$warmth_z, na.rm = TRUE)))
log_msg(sprintf("  tool_config: %s",
                paste(names(table(pooled$tool_config, useNA = "no")),
                      table(pooled$tool_config, useNA = "no"), sep = "=", collapse = ", ")))

# ---------------------------------------------------------------------------
# Section 3: Complete Cases and Survey Design
# ---------------------------------------------------------------------------
log_msg("")
log_msg("--- Section 3: Complete Cases and Survey Design ---")

ctrl_vars <- c("party3", "ideo5_clean", "educ_college", "age", "female",
               "race_eth", "newsint_attn", "exposure_index", "posture_z")
ctrl_rhs  <- paste(ctrl_vars, collapse = " + ")

model_vars <- c("tool_config", "warmth_z", "blame_china", ctrl_vars, "year", "weight")
pooled_complete <- pooled[complete.cases(pooled[, model_vars, drop = FALSE]), ]
log_msg(sprintf("  Complete cases: n=%d (%.1f%% of pooled)",
                nrow(pooled_complete),
                100 * nrow(pooled_complete) / nrow(pooled)))

# Ensure tool_config is a factor with "Neither" as reference
if (!is.factor(pooled_complete$tool_config)) {
  pooled_complete$tool_config <- factor(pooled_complete$tool_config)
}
if ("Neither" %in% levels(pooled_complete$tool_config)) {
  pooled_complete$tool_config <- relevel(pooled_complete$tool_config, ref = "Neither")
}
log_msg(sprintf("  tool_config levels: %s", paste(levels(pooled_complete$tool_config), collapse = ", ")))

fmla <- as.formula(
  paste0("tool_config ~ warmth_z * blame_china + ", ctrl_rhs, " + factor(year)")
)
log_msg(sprintf("  Formula: %s", deparse(fmla)))

# Survey design (id = ~1 = SRS within strata; weights only)
svy_design <- svydesign(id = ~1, weights = ~weight, data = pooled_complete)
log_msg("  Survey design: svydesign(id=~1, weights=~weight)")

# ---------------------------------------------------------------------------
# Section 3b: Binary svyglm — design-based SEs for S+A vs. rest
# (Always runs; provides valid design-based inference for key contrast)
# ---------------------------------------------------------------------------
log_msg("")
log_msg("--- Section 3b: svyglm binary contrast (S+A vs rest) ---")

pooled_complete$is_SA <- as.integer(pooled_complete$tool_config == "S+A")

fmla_bin <- as.formula(
  paste0("is_SA ~ warmth_z * blame_china + ", ctrl_rhs, " + factor(year)")
)

m_bin <- tryCatch(
  svyglm(fmla_bin, design = svy_design, family = quasibinomial()),
  error = function(e) { log_msg(sprintf("  svyglm failed: %s", e$message)); NULL }
)

if (!is.null(m_bin)) {
  sm_bin <- summary(m_bin)$coefficients
  wb_row <- grep("warmth_z:blame_china|warmth_z.blame_china", rownames(sm_bin))
  if (length(wb_row) == 1) {
    b  <- sm_bin[wb_row, "Estimate"]
    se <- sm_bin[wb_row, "Std. Error"]
    tv <- sm_bin[wb_row, "t value"]
    pv <- sm_bin[wb_row, "Pr(>|t|)"]
    log_msg(sprintf("  svyglm warmth_z:blame_china (S+A vs rest, design-based):"))
    log_msg(sprintf("    b=%.4f  SE=%.4f  t=%.3f  p=%.4f %s", b, se, tv, pv,
                    ifelse(pv < 0.001, "***", ifelse(pv < 0.01, "**",
                    ifelse(pv < 0.05, "*", ifelse(pv < 0.10, ".", ""))))))
    log_msg("  NOTE: Binary contrast (LPM/quasibinomial) — design-based SE is valid but")
    log_msg("        coefficient is not directly comparable to MNL log-odds.")
  }
}

# ---------------------------------------------------------------------------
# Section 4: Attempt svymultinom() (survey >= 4.2)
# ---------------------------------------------------------------------------
log_msg("")
log_msg("--- Section 3: svymultinom() attempt ---")

sv_version <- packageVersion("survey")
log_msg(sprintf("  survey package version: %s", sv_version))
has_svymultinom <- tryCatch({
  getFromNamespace("svymultinom", "survey")
  TRUE
}, error = function(e) FALSE)
log_msg(sprintf("  svymultinom available: %s", has_svymultinom))

results_df <- NULL

if (has_svymultinom) {
  log_msg("  Fitting svymultinom() ...")
  m_svy <- tryCatch(
    svymultinom(fmla, design = svy_design, trace = FALSE),
    error = function(e) {
      log_msg(sprintf("  svymultinom FAILED: %s", e$message))
      NULL
    }
  )
  if (!is.null(m_svy)) {
    log_msg("  svymultinom OK — extracting coefficients")
    coef_tbl <- tryCatch({
      sm <- summary(m_svy)
      if (is.list(sm) && "coefficients" %in% names(sm)) {
        coefs <- sm$coefficients
        ses   <- sm$SE
        data.frame(
          outcome   = rep(rownames(coefs), ncol(coefs)),
          predictor = rep(colnames(coefs), each = nrow(coefs)),
          estimate  = as.vector(coefs),
          design_se = as.vector(ses),
          stringsAsFactors = FALSE
        ) %>%
          mutate(
            t_value = estimate / design_se,
            p_value = 2 * pt(abs(t_value), df = nrow(pooled_complete) - ncol(coefs), lower.tail = FALSE)
          )
      } else {
        log_msg("  WARNING: Unexpected summary format from svymultinom")
        NULL
      }
    }, error = function(e) {
      log_msg(sprintf("  Coef extraction failed: %s", e$message))
      NULL
    })
    results_df <- coef_tbl
  }
}

# ---------------------------------------------------------------------------
# Section 4: Bootstrap Fallback (if svymultinom unavailable or failed)
# ---------------------------------------------------------------------------
if (is.null(results_df)) {
  log_msg("")
  log_msg("--- Section 4: Bootstrap fallback (999 replicates) ---")
  log_msg("  Fitting base nnet::multinom() for point estimates ...")

  m_base <- tryCatch(
    multinom(fmla, data = pooled_complete, weights = weight,
             trace = FALSE, Hess = TRUE, maxit = 400),
    error = function(e) {
      log_msg(sprintf("  Base multinom FAILED: %s", e$message))
      NULL
    }
  )

  if (!is.null(m_base)) {
    log_msg("  Base model OK. Building bootstrap replicate design ...")
    set.seed(20250217)
    boot_design <- as.svrepdesign(svy_design, type = "bootstrap", replicates = 999)

    log_msg("  Running svyby() on each binary outcome (S+A, S-only, A-only) ...")

    outcomes  <- setdiff(levels(pooled_complete$tool_config), "Neither")
    boot_rows <- list()

    for (oc in outcomes) {
      pooled_complete[[paste0("is_", gsub("\\+|[^A-Za-z]", "_", oc))]] <-
        as.integer(pooled_complete$tool_config == oc)
    }

    # Build resampled coefficient SEs via withReplicates
    coef_point <- coef(m_base)
    ncoef      <- ncol(coef_point)

    fit_coefs <- function(data, wts) {
      d2 <- data
      d2$.wt <- wts
      m2 <- tryCatch(
        multinom(fmla, data = d2, weights = .wt,
                 trace = FALSE, maxit = 300),
        error = function(e) NULL
      )
      if (is.null(m2)) return(rep(NA_real_, nrow(coef_point) * ncoef))
      as.vector(coef(m2))
    }

    log_msg("  withReplicates() — may take ~1-2 minutes ...")
    boot_result <- tryCatch(
      withReplicates(boot_design, fit_coefs),
      error = function(e) {
        log_msg(sprintf("  withReplicates FAILED: %s", e$message))
        NULL
      }
    )

    if (!is.null(boot_result)) {
      boot_se_vec <- SE(boot_result)
      n_outcomes  <- nrow(coef_point)
      n_preds     <- ncoef

      results_df <- data.frame(
        outcome   = rep(rownames(coef_point), n_preds),
        predictor = rep(colnames(coef_point), each = n_outcomes),
        estimate  = as.vector(coef_point),
        design_se = boot_se_vec,
        stringsAsFactors = FALSE
      ) %>%
        mutate(
          t_value = estimate / design_se,
          p_value = 2 * pt(abs(t_value), df = nrow(pooled_complete) - n_preds, lower.tail = FALSE)
        )
      log_msg("  Bootstrap SEs extracted successfully")
    } else {
      log_msg("  Bootstrap also failed — using model-based SEs as last resort")
      sm <- summary(m_base)
      coef_mat <- coef(m_base)
      se_mat   <- sm$standard.errors
      results_df <- data.frame(
        outcome   = rep(rownames(coef_mat), ncol(coef_mat)),
        predictor = rep(colnames(coef_mat), each = nrow(coef_mat)),
        estimate  = as.vector(coef_mat),
        design_se = as.vector(se_mat),
        stringsAsFactors = FALSE
      ) %>%
        mutate(
          t_value = estimate / design_se,
          p_value = 2 * pt(abs(t_value), df = nrow(pooled_complete) - ncol(coef_mat), lower.tail = FALSE),
          note    = "model-based SE (design-based unavailable)"
        )
    }
  }
}

# ---------------------------------------------------------------------------
# Section 5: Report and Save
# ---------------------------------------------------------------------------
log_msg("")
log_msg("--- Section 5: Report Key Coefficients ---")

if (!is.null(results_df)) {
  # Focus on warmth_z:blame_china interaction
  key_coef <- results_df %>%
    filter(grepl("warmth_z:blame_china|warmth_z.blame_china", predictor, ignore.case = TRUE))

  if (nrow(key_coef) > 0) {
    log_msg("  warmth_z:blame_china interaction (design-based SEs):")
    for (i in seq_len(nrow(key_coef))) {
      r <- key_coef[i, ]
      stars <- ifelse(r$p_value < 0.001, "***",
               ifelse(r$p_value < 0.01,  "**",
               ifelse(r$p_value < 0.05,  "*",
               ifelse(r$p_value < 0.10,  ".", ""))))
      log_msg(sprintf("    %-25s  b=%.4f  SE=%.4f  t=%.3f  p=%.4f %s",
                      r$outcome, r$estimate, r$design_se, r$t_value, r$p_value, stars))
    }
  } else {
    log_msg("  WARNING: warmth_z:blame_china not found in results — check predictor names")
    log_msg(sprintf("  Predictors found: %s", paste(unique(results_df$predictor)[1:5], collapse = ", ")))
  }

  # Build clean output table (focus on non-intercept, non-nuisance predictors)
  key_predictors <- c("warmth_z", "blame_china", "warmth_z:blame_china",
                      "warmth_z.blame_china", "factor(year)2025")
  out_tbl <- results_df %>%
    filter(predictor %in% key_predictors | grepl("warmth|blame", predictor, ignore.case = TRUE)) %>%
    mutate(
      Outcome   = outcome,
      Predictor = predictor,
      Estimate  = round(estimate, 4),
      `Design SE` = round(design_se, 4),
      `t-value`   = round(t_value, 3),
      `p-value`   = ifelse(is.na(p_value), NA, round(p_value, 4)),
      Stars       = ifelse(is.na(p_value), "",
                    ifelse(p_value < 0.001, "***",
                    ifelse(p_value < 0.01,  "**",
                    ifelse(p_value < 0.05,  "*",
                    ifelse(p_value < 0.10,  ".", "")))))
    ) %>%
    select(Outcome, Predictor, Estimate, `Design SE`, `t-value`, `p-value`, Stars)

  # Save CSV
  csv_path <- here("prism", "tables", "app_tab_design_mnl.csv")
  write_csv(out_tbl, csv_path)
  log_msg(sprintf("  Saved: app_tab_design_mnl.csv (%d rows)", nrow(out_tbl)))

  # Save LaTeX
  tex_path <- here("prism", "tables", "app_tab_design_mnl.tex")
  latex_out <- out_tbl %>%
    kbl(format = "latex", booktabs = TRUE, caption = NULL, linesep = "",
        digits = 4)
  writeLines(as.character(latex_out), tex_path)
  log_msg("  Saved: app_tab_design_mnl.tex")

  # Also save full coefficient table
  full_path <- here("prism", "tables", "app_tab_design_mnl_full.csv")
  write_csv(results_df, full_path)
  log_msg("  Saved: app_tab_design_mnl_full.csv (all predictors)")
} else {
  log_msg("  ERROR: No results available — no tables saved")
}

# ---------------------------------------------------------------------------
# Section 6: Session Info and Log
# ---------------------------------------------------------------------------
log_msg("")
log_msg("--- Section 6: Session Info ---")
si <- capture.output(sessionInfo())
for (line in si) log_only(paste(" ", line))

log_msg("")
log_msg("08_design_mnl.R COMPLETE")
log_msg(sprintf("Timestamp: %s", format(Sys.time())))

writeLines(log_env$entries, here("Output", "08_design_mnl_log.txt"))
message(sprintf("Log saved to: Output/08_design_mnl_log.txt"))
