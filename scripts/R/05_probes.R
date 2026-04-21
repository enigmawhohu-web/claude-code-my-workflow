#' ============================================================================
#' 05_probes.R — Phase 5: 8-Probe Panorama Discrimination Test
#' Poll 2025: Fentanyl Policy Survey Analysis
#' PAX sapiens Foundation
#'
#' @description 8-probe panorama discrimination test: reports estimates,
#'   confidence intervals, and p-values for the target probe (fentanyl
#'   cooperation), composite boundary mean, and omnibus divergence.
#'   Descriptive reporting throughout — no categorical verdicts or decision
#'   rules. Reader interprets the pattern.
#'   - TARGET: probe d (fentanyl cooperation) — S+A vs S-only effect
#'   - BOUNDARY: probes a,b,c,e,f,g,h (7 non-target scenarios)
#'
#' @context Phase 4 found suggestive attenuation under n_approaches control
#'   and non-specific placebo pairing. Phase 5 examines whether the S+A
#'   effect is specific to fentanyl cooperation (probe d) or generalizes
#'   across all 8 scenarios. Reports Delta_d, composite boundary, omnibus
#'   divergence, and TOST equivalence test descriptively.
#'
#' @acquiescence_note (MR-B harmonization — names aligned)
#'   The main probe models in this script control for acquiescence using
#'   `n_approaches_10`: a 10-item count of endorsed fentanyl-policy
#'   approaches that EXCLUDES `approach_sanctions` and `approach_china_action`
#'   to avoid mechanical collinearity with the S+A treatment indicator
#'   `is_SA`. The full 12-item count `n_approaches_12` is constructed in
#'   parallel and enters only as a sensitivity model (`mX_d`, Section 5).
#'   Both 04_mechanisms.R and 05_probes.R now use `n_approaches_10` for the
#'   10-item (DV-excluding) count and `n_approaches_12` for the full 12-item
#'   count. The two scripts' 10-item counts are definitionally identical.
#'
#' @design PRIMARY: stacked long-format svyglm with binary is_SA indicator,
#'   clustered by respondent. All controls are fully interacted with scenario
#'   (equivalent to SUR), so each scenario gets its own control coefficients.
#'   Yields scenario-specific S+A vs S-only effects via scenario:is_SA
#'   interactions. Omnibus test compares Delta_d to the mean of 7 boundary
#'   Delta values with exact SE from stacked vcov matrix.
#'
#' @scenarios
#'   a: China launched military action against Taiwan (Security)
#'   b: Collision between Chinese and US military ship/plane (Security)
#'   c: Collision between Chinese and another country's military (Security)
#'   d: China prevented export of fentanyl precursor chemicals (Health/TARGET)
#'   e: China resumed adoptions by US parents (Cultural)
#'   f: China stopped selling low-cost electronics to US (Economic)
#'   g: Chinese gov gave scholarships to US students (Cultural)
#'   h: Chinese gov sent more pandas to US zoos (Cultural)
#'
#' @variables Cross-wave mapping:
#'   2025 q16_a-q16_h / 2024 q17_a-q17_h -> harmonized opinion_change_a-h
#'   All 1-7 Likert scale items.
#'
#' @input  data/clean/pooled.rds, data/clean/clean_2024.rds,
#'         data/clean/clean_2025.rds,
#'         output/tables/phase4_objects.rds (for P_hat(S+A) extraction)
#'
#' @output Main:
#'         output/figures/fig4_probes_panorama.pdf
#'         output/tables/tab4_phase5_contrasts (.tex + .csv)
#'         output/tables/phase5_objects.rds
#'         output/tables/phase5_log.txt
#'         Appendix:
#'         output/tables/app_tab_phase5_panorama (.tex + .csv)
#'         output/tables/app_tab_phase5_divergence (.tex + .csv)
#'         output/tables/app_tab_phase5_phat_sa (.tex + .csv)
#'         output/tables/app_tab_phase5_napproaches_zero (.tex + .csv)
#'         output/tables/app_tab_phase5_nexcl (.tex + .csv)
#'
#' @depends here, tidyverse, survey, nnet, kableExtra, scales
#' ============================================================================


# --- Section 0: Setup --------------------------------------------------------

library(here)
library(tidyverse)
library(survey)
library(nnet)          # For multinomial fallback if phase4_objects.rds unavailable
library(kableExtra)
library(scales)

set.seed(20250217)  # project-canonical seed

dir.create(here("output", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "figures"), recursive = TRUE, showWarnings = FALSE)

# --- Logging infrastructure ---
# Set VERBOSE = TRUE for full console output; FALSE for quiet (log file only)
VERBOSE <- TRUE
log_env <- new.env(parent = emptyenv())
log_env$entries <- character()

log_msg <- function(msg) {
  log_env$entries <- c(log_env$entries, msg)
  if (VERBOSE) message(msg)
}

log_msg("========================================================================")
log_msg("PHASE 5 LOG: 8-Probe Panorama Discrimination Test")
log_msg(sprintf("Timestamp: %s", Sys.time()))
log_msg("========================================================================")

# --- Scenario constants ---
SCENARIO_MAP <- c(
  a = "China launched military action against Taiwan",
  b = "Collision between Chinese and US military ship/plane",
  c = "Collision between Chinese and another country's military",
  d = "China prevented export of fentanyl precursor chemicals",
  e = "China resumed adoptions by US parents of Chinese babies",
  f = "China stopped selling low-cost electronics to US",
  g = "Chinese gov gave scholarships to US students to study Chinese",
  h = "Chinese gov sent more pandas to US zoos"
)

SCENARIO_SHORT <- c(
  a = "Taiwan military action",
  b = "US-China military collision",
  c = "Other military collision",
  d = "Fentanyl cooperation",
  e = "Resuming adoptions",
  f = "Electronics sales stopped",
  g = "Student scholarships",
  h = "Pandas to US zoos"
)

DOMAIN_MAP <- c(
  a = "Security", b = "Security", c = "Security",
  d = "Health",
  e = "Cultural",
  f = "Economic",
  g = "Cultural", h = "Cultural"
)

TARGET_PROBE <- "d"
BOUNDARY_PROBES <- c("a", "b", "c", "e", "f", "g", "h")
ALL_PROBES <- letters[1:8]

# Domain colors for figure
domain_colors <- c(
  "Security" = "#dc3545",
  "Health"   = "#198754",
  "Economic" = "#0f3460",
  "Cultural" = "#e94560"
)

# PAX sapiens color palette
pax_dark       <- "#1a1a2e"
pax_blue       <- "#16213e"
pax_accent     <- "#0f3460"
pax_highlight  <- "#e94560"
neutral_gray   <- "#6c757d"
positive_green <- "#198754"
negative_red   <- "#dc3545"

theme_pax <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title    = element_text(face = "bold", size = base_size + 2),
      plot.subtitle = element_text(color = neutral_gray),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold")
    )
}

# --- Utility functions ---

#' Save a table as LaTeX (.tex) and CSV (.csv)
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

#' Save a ggplot figure as vector PDF
save_figure <- function(plot, name, w = 6.5, h = 4.5, bg = "white") {
  pdf_path <- here("output", "figures", paste0(name, ".pdf"))
  dev <- tryCatch({
    tf <- tempfile(fileext = ".pdf")
    on.exit(unlink(tf), add = TRUE)
    grDevices::cairo_pdf(tf); dev.off(); cairo_pdf
  }, error = function(e) "pdf")
  ggsave(pdf_path, plot = plot, width = w, height = h,
         device = dev, bg = bg, dpi = 300)
  dev_label <- if (identical(dev, cairo_pdf)) "cairo_pdf" else "pdf"
  log_msg(sprintf("  Saved: %s.pdf (%s x %s in, vector, device=%s)",
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
  em_dash <- intToUtf8(0x2014)
  ifelse(is.na(p), em_dash,
         paste0(formatC(p, format = "f", digits = 3), stars))
}

#' Fit a survey-weighted OLS model with error handling
fit_probe_model <- function(dv, fmla_rhs, des, label) {
  fmla <- as.formula(paste0(dv, " ~ ", fmla_rhs))
  tryCatch(
    svyglm(fmla, design = des, family = gaussian()),
    error = function(e) {
      log_msg(sprintf("  FAILED %s: %s", label, e$message))
      NULL
    }
  )
}

#' Log key diagnostics for a fitted OLS model
log_model <- function(mod, label) {
  if (is.null(mod)) return(invisible(NULL))
  n_obs <- nobs(mod)
  r2 <- 1 - mod$deviance / mod$null.deviance
  log_msg(sprintf("  %s: N=%d, R2=%.4f", label, n_obs, r2))
  bad_coefs <- coef(mod)
  bad_idx <- is.na(bad_coefs) | is.nan(bad_coefs) | is.infinite(bad_coefs)
  if (any(bad_idx)) {
    log_msg(sprintf("  WARNING: %s has NA/NaN/Inf coefficients: %s",
                    label, paste(names(bad_coefs)[bad_idx], collapse = ", ")))
  }
}

#' Build contrast vector for a specific scenario from stacked model
#' With binary is_SA and scenario factor (reference = "a"):
#'   For scenario a: c["is_SA"] = 1  (Delta_a = beta[is_SA])
#'   For scenario x: c["is_SA"] = 1, c["scenariox:is_SA"] = 1
#'     (Delta_x = beta[is_SA] + beta[scenariox:is_SA])
#' @param beta_names Character vector of coefficient names
#' @param scenario_letter One of "a" through "h"
#' @return Named numeric vector (contrast weights)
build_contrast_vector <- function(beta_names, scenario_letter) {
  cvec <- rep(0, length(beta_names))
  names(cvec) <- beta_names
  if (!"is_SA" %in% beta_names) {
    warning("is_SA not found in coefficient names")
    return(cvec)
  }
  cvec["is_SA"] <- 1
  if (scenario_letter != "a") {
    int_name <- paste0("scenario", scenario_letter, ":is_SA")
    if (int_name %in% beta_names) {
      cvec[int_name] <- 1
    } else {
      warning(sprintf("%s not found in coefficient names", int_name))
    }
  }
  cvec
}

#' Extract S+A vs S-only contrast from a model with binary is_SA predictor
#' (for 8 separate wide-format models and robustness checks)
extract_is_sa_contrast <- function(mod, label) {
  if (is.null(mod)) return(list(est = NA, se = NA, lo = NA, hi = NA, p = NA))
  coefs <- coef(mod)
  V <- vcov(mod)
  if (!"is_SA" %in% names(coefs)) {
    log_msg(sprintf("  WARNING: is_SA not found in %s", label))
    return(list(est = NA, se = NA, lo = NA, hi = NA, p = NA))
  }
  est <- unname(coefs["is_SA"])
  se_val <- sqrt(V["is_SA", "is_SA"])
  df_r <- df.residual(mod)
  t_val <- est / se_val
  p_val <- 2 * pt(-abs(t_val), df = df_r)
  ci <- est + c(-1, 1) * qt(0.975, df_r) * se_val
  list(est = est, se = se_val, lo = ci[1], hi = ci[2], p = p_val)
}


# --- Section 1: Load Data & Construct Variables ------------------------------

log_msg("")
log_msg("--- Section 1: Load Data & Construct Variables ---")

pooled     <- readRDS(here("data", "cleaned", "pooled.rds"))
clean_2024 <- readRDS(here("data", "cleaned", "clean_2024.rds"))
clean_2025 <- readRDS(here("data", "cleaned", "clean_2025.rds"))
log_msg(sprintf("  Loaded: pooled=%d, 2024=%d, 2025=%d",
                nrow(pooled), nrow(clean_2024), nrow(clean_2025)))

# ==========================================================================
# SHARED FUNCTIONS (canonical source: 04_mechanisms.R lines 174-312)
# If modifying, update ALL scripts that copy this block: 03, 04, 05
# Last sync: 2026-02-27
# ==========================================================================

# --- 1a. build_analysis_vars (copied from 04_mechanisms.R) ---
build_analysis_vars <- function(df, warmth_mu, warmth_sd,
                                posture_mu, posture_sd) {
  df %>%
    mutate(
      us_pref_a = as.numeric(us_preferred_a),
      us_pref_b = as.numeric(us_preferred_b),
      us_pref_c_r = 8 - as.numeric(us_preferred_c),
      us_pref_d = as.numeric(us_preferred_d),
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
      warmth_z = (warmth - warmth_mu) / warmth_sd,
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
      newsint_attn = {
        ni <- as.numeric(newsint)
        ifelse(ni %in% 1:4, 5L - ni, NA_integer_)
      },
      ideo5_clean = {
        id <- as.numeric(ideo5)
        ifelse(id %in% 1:5, id, NA_real_)
      }
    )
}

# Compute pooled moments for standardization
pooled_raw <- pooled %>%
  mutate(
    pi_a = as.numeric(us_preferred_a),
    pi_b = as.numeric(us_preferred_b),
    pi_c_r = 8 - as.numeric(us_preferred_c),
    pi_d = as.numeric(us_preferred_d),
    pi_n_valid = rowSums(!is.na(cbind(pi_a, pi_b, pi_c_r, pi_d))),
    posture_index_raw = ifelse(
      pi_n_valid >= 3,
      rowMeans(cbind(pi_a, pi_b, pi_c_r, pi_d), na.rm = TRUE),
      NA_real_
    )
  )

warmth_mu  <- mean(pooled$warmth, na.rm = TRUE)
warmth_sd  <- sd(pooled$warmth, na.rm = TRUE)
posture_mu <- mean(pooled_raw$posture_index_raw, na.rm = TRUE)
posture_sd <- sd(pooled_raw$posture_index_raw, na.rm = TRUE)

stopifnot("warmth_sd must be positive" = warmth_sd > 0)
stopifnot("posture_sd must be positive" = posture_sd > 0)

log_msg(sprintf("  Pooled warmth: mean=%.3f, sd=%.3f", warmth_mu, warmth_sd))

pooled     <- build_analysis_vars(pooled, warmth_mu, warmth_sd,
                                  posture_mu, posture_sd)
clean_2024 <- build_analysis_vars(clean_2024, warmth_mu, warmth_sd,
                                  posture_mu, posture_sd)
clean_2025 <- build_analysis_vars(clean_2025, warmth_mu, warmth_sd,
                                  posture_mu, posture_sd)

# --- 1b. Construct tool_config ---
log_msg("")
log_msg("  Constructing tool_config...")

#' Construct tool_config: 4-level factor from sanctions and china_action items
#' @param df Data frame with approach_sanctions and approach_china_action columns
#' @return Data frame with added sanctions_bin, action_bin, tool_config columns
build_tool_config <- function(df) {
  df %>% mutate(
    sanctions_bin = {
      x <- as.numeric(approach_sanctions)
      ifelse(is.na(x), NA_integer_, ifelse(x == 1, 1L, 0L))
    },
    action_bin = {
      x <- as.numeric(approach_china_action)
      ifelse(is.na(x), NA_integer_, ifelse(x == 1, 1L, 0L))
    },
    tool_config = case_when(
      is.na(sanctions_bin) | is.na(action_bin) ~ NA_character_,
      sanctions_bin == 0 & action_bin == 0 ~ "Neither",
      sanctions_bin == 1 & action_bin == 0 ~ "Sanctions-only",
      sanctions_bin == 0 & action_bin == 1 ~ "Action-only",
      sanctions_bin == 1 & action_bin == 1 ~ "S+A"
    ) %>% factor(levels = c("Neither", "Sanctions-only", "Action-only", "S+A"))
  )
}

pooled     <- build_tool_config(pooled)
clean_2024 <- build_tool_config(clean_2024)
clean_2025 <- build_tool_config(clean_2025)

# --- 1c. Construct n_approaches (all 12 items, incl. DV components) ---
log_msg("  Constructing n_approaches (acquiescence control)...")

approach_vars <- c("approach_intl_cooperation", "approach_enforce_sales",
                   "approach_enforce_users", "approach_legalization",
                   "approach_health_justice", "approach_addiction_resources",
                   "approach_blocking_drugs", "approach_social_cultural",
                   "approach_sanctions", "approach_china_action",
                   "approach_mexico_action", "approach_penalties")

#' Construct n_approaches: count of endorsed fentanyl policy approaches (all 12 items)
#' Includes approach_sanctions and approach_china_action (tool_config components).
#' Used ONLY for the 12-item sensitivity model (mX_d). Main probe models use
#' n_approaches_10 (10-item) built below. See @acquiescence_note in header;
#' 04_mechanisms.R's n_approaches_10 is the 10-item analog of this script's
#' n_approaches_10 (names now aligned after MR-B rename).
#' @param df Data frame with approach_* columns
#' @return Data frame with added n_approaches_12, n_approaches_12_miss, n_approaches_12_zero
build_n_approaches <- function(df) {
  approach_bins <- vapply(approach_vars, function(v) {
    x <- as.numeric(df[[v]])
    ifelse(is.na(x), NA_integer_, ifelse(x == 1, 1L, 0L))
  }, integer(nrow(df)))
  df$n_approaches_12 <- rowSums(approach_bins, na.rm = FALSE)
  df$n_approaches_12_miss <- as.integer(rowSums(is.na(approach_bins)) > 0)
  df$n_approaches_12_zero <- rowSums(
    replace(approach_bins, is.na(approach_bins), 0L), na.rm = TRUE
  )
  df
}

pooled     <- build_n_approaches(pooled)
clean_2024 <- build_n_approaches(clean_2024)
clean_2025 <- build_n_approaches(clean_2025)

# Construct n_approaches_10 (10 items; excludes tool_config components)
approach_vars_excl <- setdiff(approach_vars,
                              c("approach_sanctions", "approach_china_action"))

#' Construct n_approaches_10: 10-item count excluding sanctions & china_action
#' Avoids mechanical collinearity with is_SA (which derives from those 2 items)
#' @param df Data frame with approach_* columns
#' @return Data frame with added n_approaches_10, n_approaches_10_miss,
#'   n_approaches_10_zero columns
build_n_approaches_10 <- function(df) {
  approach_bins_excl <- vapply(approach_vars_excl, function(v) {
    x <- as.numeric(df[[v]])
    ifelse(is.na(x), NA_integer_, ifelse(x == 1, 1L, 0L))
  }, integer(nrow(df)))
  df$n_approaches_10 <- rowSums(approach_bins_excl, na.rm = FALSE)
  df$n_approaches_10_miss <- as.integer(rowSums(is.na(approach_bins_excl)) > 0)
  df$n_approaches_10_zero <- rowSums(
    replace(approach_bins_excl, is.na(approach_bins_excl), 0L), na.rm = TRUE
  )
  df
}

pooled     <- build_n_approaches_10(pooled)
clean_2024 <- build_n_approaches_10(clean_2024)
clean_2025 <- build_n_approaches_10(clean_2025)


# --- Section 2: Construct Phase 5 Variables ----------------------------------

log_msg("")
log_msg("--- Section 2: Phase 5 Variables ---")

# --- 2.1 Build all 8 probes ---
log_msg("  2.1 Build all 8 probes")

for (s in ALL_PROBES) {
  var_name <- paste0("opinion_change_", s)
  probe_name <- paste0("probe_", s)
  if (var_name %in% names(pooled)) {
    pooled[[probe_name]] <- as.numeric(pooled[[var_name]])
  } else {
    log_msg(sprintf("  WARNING: %s not found in pooled — setting probe_%s to NA", var_name, s))
    pooled[[probe_name]] <- NA_real_
  }
}

# Distribution audit for all 8 probes
probe_cols <- paste0("probe_", ALL_PROBES)
sd_probes <- numeric(8)
names(sd_probes) <- ALL_PROBES
binary_flags <- logical(8)
names(binary_flags) <- ALL_PROBES
binary_flag_details <- list()

for (s in ALL_PROBES) {
  pv <- paste0("probe_", s)
  vals <- na.omit(pooled[[pv]])
  sd_probes[s] <- sd(vals)
  log_msg(sprintf("  %s (%s): N=%d, NAs=%d, mean=%.3f, sd=%.3f",
                  pv, SCENARIO_SHORT[s],
                  length(vals), sum(is.na(pooled[[pv]])),
                  mean(vals), sd(vals)))

  # Full distribution
  tab <- prop.table(table(pooled[[pv]], useNA = "no"))
  dist_str <- paste(sprintf("%s=%.1f%%", names(tab), 100 * tab), collapse = ", ")
  log_msg(sprintf("    Distribution: %s", dist_str))

  # Endpoint check: values 1 and 7 only (not max of any category)
  pct_floor   <- if ("1" %in% names(tab)) unname(tab["1"]) else 0
  pct_ceiling <- if ("7" %in% names(tab)) unname(tab["7"]) else 0
  if (pct_floor > 0.40 || pct_ceiling > 0.40) {
    which_end <- if (pct_floor > 0.40) sprintf("floor (=1): %.1f%%", 100 * pct_floor)
                 else sprintf("ceiling (=7): %.1f%%", 100 * pct_ceiling)
    log_msg(sprintf("    BINARY FLAG: %s at %s", pv, which_end))
    binary_flags[s] <- TRUE
    binary_flag_details[[s]] <- list(
      floor_pct = pct_floor, ceiling_pct = pct_ceiling,
      triggered_by = if (pct_floor > 0.40) "floor" else "ceiling"
    )
  }
}

log_msg(sprintf("  Binary robustness triggered for: %s",
                if (any(binary_flags)) paste(names(binary_flags)[binary_flags], collapse = ", ")
                else "NONE"))

# --- 2.2 tool_config audit ---
log_msg("")
log_msg("  2.2 tool_config distribution")
tc_tab <- table(pooled$tool_config, useNA = "ifany")
for (cat in names(tc_tab)) {
  log_msg(sprintf("    %s: %d (%.1f%%)", cat, tc_tab[cat],
                  100 * tc_tab[cat] / sum(tc_tab)))
}

# --- 2.3 n_approaches audit ---
log_msg("")
log_msg("  2.3 n_approaches summary")
for (v in c("n_approaches_12", "n_approaches_12_miss",
            "n_approaches_10", "n_approaches_10_miss")) {
  vals <- na.omit(as.numeric(pooled[[v]]))
  log_msg(sprintf("    %s: n=%d, NAs=%d, range=[%.0f, %.0f], mean=%.3f",
                  v, length(vals), sum(is.na(pooled[[v]])),
                  min(vals), max(vals), mean(vals)))
}

# --- 2.4 P_hat(S+A) from Phase 4 ---
log_msg("")
log_msg("  2.4 P_hat(S+A) extraction from Phase 4")

p4_path <- here("output", "tables", "phase4_objects.rds")
pooled$p_hat_sa <- NA_real_

if (file.exists(p4_path)) {
  p4 <- readRDS(p4_path)
  if (!is.null(p4$mpool)) {
    log_msg("  Loaded phase4_objects.rds; extracting P_hat(S+A) from mpool")
    p4_vars <- tryCatch(all.vars(formula(p4$mpool))[-1], error = function(e) NULL)
    if (!is.null(p4_vars)) {
      p4_vars_avail <- intersect(p4_vars, names(pooled))
      if (length(p4_vars_avail) == length(p4_vars)) {
        has_p4 <- complete.cases(pooled[, p4_vars, drop = FALSE])
        pred_probs <- tryCatch(
          predict(p4$mpool, newdata = pooled[has_p4, ], type = "probs"),
          error = function(e) {
            log_msg(sprintf("  WARNING: predict(mpool) failed: %s", e$message))
            NULL
          }
        )
        if (!is.null(pred_probs)) {
          sa_col <- if ("S+A" %in% colnames(pred_probs)) "S+A" else
                    grep("S\\+A|SA|Sanctions.Action", colnames(pred_probs), value = TRUE)[1]
          if (!is.na(sa_col)) {
            pooled$p_hat_sa[has_p4] <- pred_probs[, sa_col]
            log_msg(sprintf("  P_hat(S+A): N predicted=%d, mean=%.4f, sd=%.4f",
                            sum(has_p4),
                            mean(pooled$p_hat_sa, na.rm = TRUE),
                            sd(pooled$p_hat_sa, na.rm = TRUE)))
          } else {
            log_msg("  WARNING: Could not find S+A column in predict() output")
          }
        }
      } else {
        log_msg(sprintf("  WARNING: Phase 4 predictors missing: %s",
                        paste(setdiff(p4_vars, p4_vars_avail), collapse = ", ")))
      }
    }
  } else {
    log_msg("  WARNING: phase4_objects$mpool is NULL")
  }
} else {
  log_msg("  WARNING: phase4_objects.rds not found — P_hat(S+A) will be NA")
}

# Fallback: refit multinomial if P_hat extraction failed
if (all(is.na(pooled$p_hat_sa))) {
  log_msg("  FALLBACK: Refitting Phase 4 pooled multinomial for P_hat(S+A)")
  ctrl_rhs_p4 <- "party3 + ideo5_clean + educ_college + age + female + race_eth + newsint_attn + exposure_index"
  fmla_p4 <- as.formula(paste0("tool_config ~ warmth_z * blame_china + ",
                                ctrl_rhs_p4, " + factor(year)"))
  p4_model_vars <- all.vars(fmla_p4)
  p4_complete <- complete.cases(pooled[, p4_model_vars, drop = FALSE])
  mpool_refit <- tryCatch(
    multinom(fmla_p4, data = pooled[p4_complete, ], weights = weight,
             trace = FALSE, Hess = TRUE, maxit = 300),
    error = function(e) {
      log_msg(sprintf("  FALLBACK FAILED: %s", e$message))
      NULL
    }
  )
  if (!is.null(mpool_refit)) {
    refit_probs <- predict(mpool_refit, newdata = pooled[p4_complete, ], type = "probs")
    sa_col <- if ("S+A" %in% colnames(refit_probs)) "S+A" else
              grep("S\\+A|SA", colnames(refit_probs), value = TRUE)[1]
    if (!is.na(sa_col)) {
      pooled$p_hat_sa[p4_complete] <- refit_probs[, sa_col]
      log_msg(sprintf("  FALLBACK OK: P_hat(S+A) computed for %d obs", sum(p4_complete)))
    }
  }
}

# --- 2.5 n_approaches_10 audit ---
log_msg("")
log_msg("  2.5 n_approaches_10 (10-item, main spec)")
log_msg(sprintf("    mean=%.3f, sd=%.3f, range=[%.0f, %.0f]",
                mean(pooled$n_approaches_10, na.rm = TRUE),
                sd(pooled$n_approaches_10, na.rm = TRUE),
                min(pooled$n_approaches_10, na.rm = TRUE),
                max(pooled$n_approaches_10, na.rm = TRUE)))


# --- Section 3: Survey Design Objects ----------------------------------------

log_msg("")
log_msg("--- Section 3: Survey Design Objects ---")

# Full pooled design created on-demand in Sec 4.3a for P_hat model
log_msg(sprintf("  pooled (full): N=%d, mean(weight)=%.4f",
                nrow(pooled), mean(pooled$weight)))

# --- Binary subsetting: S+A vs S-only only ---
log_msg("")
log_msg("  Binary subsetting to S+A and S-only respondents")
pooled_sa <- pooled[pooled$tool_config %in% c("Sanctions-only", "S+A"), ]
pooled_sa <- droplevels(pooled_sa)
pooled_sa$is_SA <- as.integer(pooled_sa$tool_config == "S+A")
log_msg(sprintf("  pooled_sa: N=%d (S+A=%d, S-only=%d)",
                nrow(pooled_sa),
                sum(pooled_sa$is_SA == 1),
                sum(pooled_sa$is_SA == 0)))

# --- Complete-case sample (all 8 probes + controls) ---
# posture_z excluded: it is downstream of tool_config (post-treatment) and would
# bias the S+A vs S-only contrast. build_analysis_vars() still constructs it for
# Phase 4 compatibility but it is not used in any Phase 5 model.
ctrl_rhs <- "party3 + ideo5_clean + educ_college + age + female + race_eth + newsint_attn + exposure_index"  # formula string (long by design)
ctrl_vars <- c("warmth_z", "blame_china", "party3", "ideo5_clean",
               "educ_college", "age", "female", "race_eth",
               "newsint_attn", "exposure_index")
model_vars_no_probes <- c("is_SA", "n_approaches_10", "n_approaches_10_miss",
                           ctrl_vars, "year", "weight")
analytic_vars <- c(probe_cols, model_vars_no_probes)

pooled_complete <- pooled_sa[complete.cases(pooled_sa[, analytic_vars]), ]
pooled_complete$respondent_id <- seq_len(nrow(pooled_complete))
log_msg(sprintf("  pooled_complete (all 8 probes + controls): N=%d (%.1f%% of pooled_sa)",
                nrow(pooled_complete),
                100 * nrow(pooled_complete) / nrow(pooled_sa)))
log_msg(sprintf("    S+A=%d, S-only=%d",
                sum(pooled_complete$is_SA == 1),
                sum(pooled_complete$is_SA == 0)))

# --- Reshape to long format ---
long_data <- pooled_complete %>%
  pivot_longer(cols = all_of(probe_cols),
               names_to = "scenario", names_prefix = "probe_",
               values_to = "probe_value") %>%
  mutate(scenario = factor(scenario, levels = ALL_PROBES))

log_msg(sprintf("  long_data: N=%d (= %d respondents x 8 scenarios)",
                nrow(long_data), nrow(pooled_complete)))

# PRIMARY survey design: cluster by respondent for correct SEs
des_long <- svydesign(ids = ~respondent_id, weights = ~weight, data = long_data)
log_msg(sprintf("  des_long (clustered): N_long=%d, N_clusters=%d, mean(weight)=%.4f",
                nrow(long_data), nrow(pooled_complete),
                mean(long_data$weight)))

# Wide-format design for 8-separate appendix models (same sample as stacked)
des_sa_complete <- svydesign(ids = ~1, weights = ~weight, data = pooled_complete)


# --- Section 4: Model Specifications & Fitting --------------------------------

log_msg("")
log_msg("--- Section 4: Model Fitting ---")

# --- 4.1 PRIMARY: Stacked long model (fully interacted controls) ---
# Fully interacting scenario with all controls makes the stacked model
# equivalent to SUR (8 separate regressions in one model). Each scenario
# gets its own control coefficients, eliminating bias from assuming
# homogeneous control effects. The joint vcov gives exact SEs for the
# omnibus discrimination contrast.
log_msg("  4.1 PRIMARY: Stacked long model (scenario * (is_SA + controls), clustered)")

stacked_fmla <- as.formula(paste0(
  "probe_value ~ scenario * (is_SA + ",
  "n_approaches_10 + n_approaches_10_miss + ",
  "warmth_z * blame_china + ", ctrl_rhs, " + factor(year))"
))

m_stacked <- tryCatch(
  svyglm(stacked_fmla, design = des_long, family = gaussian()),
  error = function(e) {
    log_msg(sprintf("  CRITICAL: Stacked model FAILED: %s", e$message))
    NULL
  }
)

if (!is.null(m_stacked)) {
  n_coefs <- length(coef(m_stacked))
  n_na_coefs <- sum(is.na(coef(m_stacked)))
  log_msg(sprintf("  Stacked model: N_long=%d, N_respondents=%d, n_coefs=%d, n_NA_coefs=%d",
                  nobs(m_stacked), nrow(pooled_complete), n_coefs, n_na_coefs))
  log_msg(sprintf("  df.residual=%d (= N_clusters - 1 for clustered design)",
                  df.residual(m_stacked)))
  if (n_na_coefs > 0) {
    na_names <- names(coef(m_stacked))[is.na(coef(m_stacked))]
    log_msg(sprintf("  WARNING: NA coefficients: %s", paste(na_names, collapse = ", ")))
  }
} else {
  log_msg("  CRITICAL: Cannot proceed without stacked model")
}

# --- 4.2 APPENDIX: 8 separate wide-format models ---
log_msg("")
log_msg("  4.2 APPENDIX: 8 separate wide-format models (is_SA, same sample)")

rhs_main_sa <- paste0("is_SA + n_approaches_10 + n_approaches_10_miss + ",
                       "warmth_z * blame_china + ", ctrl_rhs, " + factor(year)")

main_models <- list()
for (s in ALL_PROBES) {
  dv <- paste0("probe_", s)
  label <- sprintf("separate_%s", s)
  main_models[[s]] <- fit_probe_model(dv, rhs_main_sa, des_sa_complete, label)
  log_model(main_models[[s]], label)
}

# --- 4.3 APPENDIX: Robustness (probe d only) ---
log_msg("")
log_msg("  4.3 APPENDIX: Robustness checks (probe d only, directional consistency)")

# 4.3a P_hat(S+A) continuous specification (full pooled sample)
log_msg("  4.3a P_hat continuous specification")
log_msg("  NOTE: SEs do not propagate Stage 1 uncertainty; directional only.")
mB_d <- NULL
if (!all(is.na(pooled$p_hat_sa))) {
  rhs_phat <- paste0("p_hat_sa + n_approaches_10 + n_approaches_10_miss + ",
                      "warmth_z * blame_china + ", ctrl_rhs, " + factor(year)")
  des_pool <- svydesign(ids = ~1, weights = ~weight, data = pooled)
  mB_d <- fit_probe_model("probe_d", rhs_phat, des_pool, "mB_d")
  log_model(mB_d, "mB_d (probe_d ~ p_hat_sa, full sample)")
} else {
  log_msg("  SKIPPED: P_hat(S+A) is all NA")
}

# 4.3b Zero-imputed n_approaches
log_msg("  4.3b Zero-imputed n_approaches")
rhs_zero_sa <- paste0("is_SA + n_approaches_10_zero + ",
                       "warmth_z * blame_china + ", ctrl_rhs, " + factor(year)")
mZ_d <- fit_probe_model("probe_d", rhs_zero_sa, des_sa_complete, "mZ_d")
log_model(mZ_d, "mZ_d (probe_d ~ n_approaches_10_zero)")

# 4.3c All-12 n_approaches (includes DV components) — 12-item SENSITIVITY.
# Main spec is mZ_d (above) using n_approaches_10 (10-item, DV-excluding).
# mX_d reports the coefficient when the two DV-component approach items are
# included in the acquiescence count — shown in appendix only.
log_msg("  4.3c All-12 n_approaches robustness (12-item sensitivity)")
rhs_all12_sa <- paste0("is_SA + n_approaches_12 + n_approaches_12_miss + ",
                        "warmth_z * blame_china + ", ctrl_rhs, " + factor(year)")
mX_d <- fit_probe_model("probe_d", rhs_all12_sa, des_sa_complete, "mX_d")
log_model(mX_d, "mX_d (probe_d ~ n_approaches_12, 12-item sensitivity)")

# --- 4.4 CONDITIONAL: Binary robustness ---
log_msg("")
binary_models <- list()

if (any(binary_flags)) {
  log_msg("  4.4 Binary robustness (endpoint > 40% detected)")
  for (s in names(binary_flags)[binary_flags]) {
    pv <- paste0("probe_", s)
    detail <- binary_flag_details[[s]]
    if (detail$triggered_by == "floor") {
      # Floor effect: binary = above floor (>1)
      pooled_complete[[paste0(pv, "_bin")]] <- as.integer(pooled_complete[[pv]] > 1)
      bin_label <- sprintf("P(%s > 1)", pv)
    } else {
      # Ceiling effect: binary = at ceiling (=7)
      pooled_complete[[paste0(pv, "_bin")]] <- as.integer(pooled_complete[[pv]] == 7)
      bin_label <- sprintf("P(%s = 7)", pv)
    }
    des_bin <- svydesign(ids = ~1, weights = ~weight, data = pooled_complete)
    bin_fmla <- as.formula(paste0(pv, "_bin ~ ", rhs_main_sa))
    binary_models[[s]] <- tryCatch(
      svyglm(bin_fmla, design = des_bin, family = quasibinomial()),
      error = function(e) {
        log_msg(sprintf("  mBin_%s FAILED: %s", s, e$message))
        NULL
      }
    )
    if (!is.null(binary_models[[s]])) {
      cbin <- extract_is_sa_contrast(binary_models[[s]], sprintf("mBin_%s", s))
      log_msg(sprintf("  mBin_%s (%s): is_SA est=%.4f, p=%s",
                      s, bin_label, cbin$est, fmt_p(cbin$p)))
    }
  }
} else {
  log_msg("  4.4 Binary robustness: SKIPPED (no endpoint > 40%)")
}

# --- 4.5 APPENDIX: Available-case stacked sensitivity ---
log_msg("")
log_msg("  4.5 Available-case stacked sensitivity")

m_stacked_avail <- NULL
n_wide_avail <- NA_integer_
avail_case_results <- list()

# Check if any respondents are recovered by relaxing complete-on-all-8
pooled_sa$has_any_probe <- rowSums(!is.na(pooled_sa[, probe_cols])) > 0
pooled_avail <- pooled_sa[pooled_sa$has_any_probe &
                            complete.cases(pooled_sa[, model_vars_no_probes]), ]
n_wide_avail <- nrow(pooled_avail)
n_recovered <- n_wide_avail - nrow(pooled_complete)

log_msg(sprintf("  N_wide_avail=%d, N_wide_complete=%d, recovered=%d",
                n_wide_avail, nrow(pooled_complete), n_recovered))

if (n_recovered > 0) {
  pooled_avail$respondent_id <- seq_len(nrow(pooled_avail))
  long_avail <- pooled_avail %>%
    pivot_longer(cols = all_of(probe_cols),
                 names_to = "scenario", names_prefix = "probe_",
                 values_to = "probe_value") %>%
    filter(!is.na(probe_value)) %>%
    mutate(scenario = factor(scenario, levels = ALL_PROBES))

  des_long_avail <- svydesign(ids = ~respondent_id, weights = ~weight,
                               data = long_avail)

  m_stacked_avail <- tryCatch(
    svyglm(stacked_fmla, design = des_long_avail, family = gaussian()),
    error = function(e) {
      log_msg(sprintf("  Available-case model FAILED: %s", e$message))
      NULL
    }
  )

  if (!is.null(m_stacked_avail)) {
    log_msg(sprintf("  Available-case model: N_long=%d, N_respondents=%d",
                    nobs(m_stacked_avail), n_wide_avail))
  }
} else {
  log_msg("  No additional respondents recovered — available-case = complete-case")
}


# --- Section 5: Post-Estimation Contrasts — CORE ----------------------------

log_msg("")
log_msg("--- Section 5: Post-Estimation Contrasts ---")
log_msg("  NOTE: All estimates are ASSOCIATIONAL, not causal.")

# --- 5a. Extract 8 scenario-specific contrasts from stacked model ---
log_msg("")
log_msg("  5a. Panorama: 8 scenario-specific S+A vs S-only contrasts")

panorama_df <- NULL

if (!is.null(m_stacked)) {
  beta <- coef(m_stacked)
  V <- vcov(m_stacked)
  df_r <- df.residual(m_stacked)
  nm <- names(beta)

  panorama_rows <- list()
  for (s in ALL_PROBES) {
    cvec <- build_contrast_vector(nm, s)
    est <- as.numeric(cvec %*% beta)
    se_val <- sqrt(as.numeric(t(cvec) %*% V %*% cvec))
    t_val <- est / se_val
    p_val <- 2 * pt(-abs(t_val), df = df_r)
    ci <- est + c(-1, 1) * qt(0.975, df_r) * se_val

    panorama_rows[[s]] <- tibble(
      probe = s,
      scenario = SCENARIO_SHORT[s],
      domain = DOMAIN_MAP[s],
      estimate = est,
      SE = se_val,
      CI_lo = ci[1],
      CI_hi = ci[2],
      p = p_val
    )

    log_msg(sprintf("    Delta_%s: %.4f (SE=%.4f) [%.4f, %.4f] p=%s",
                    s, est, se_val, ci[1], ci[2], fmt_p(p_val)))
  }
  panorama_df <- bind_rows(panorama_rows)
}

# --- 5b. Omnibus discrimination test ---
log_msg("")
log_msg("  5b. Omnibus discrimination test: Delta_d - mean(Delta_boundary)")

omnibus_est <- NA_real_
omnibus_se  <- NA_real_
omnibus_ci  <- c(NA_real_, NA_real_)
omnibus_p   <- NA_real_

if (!is.null(m_stacked)) {
  # Build omnibus contrast: c_d - (1/7) * sum(c_boundary)
  c_d <- build_contrast_vector(nm, TARGET_PROBE)
  c_boundary_sum <- Reduce("+", lapply(BOUNDARY_PROBES, function(s) build_contrast_vector(nm, s)))
  c_omnibus <- c_d - (1 / length(BOUNDARY_PROBES)) * c_boundary_sum

  omnibus_est <- as.numeric(c_omnibus %*% beta)
  omnibus_se  <- sqrt(as.numeric(t(c_omnibus) %*% V %*% c_omnibus))
  t_omni      <- omnibus_est / omnibus_se
  omnibus_p   <- 2 * pt(-abs(t_omni), df = df_r)
  omnibus_ci  <- omnibus_est + c(-1, 1) * qt(0.975, df_r) * omnibus_se

  log_msg(sprintf("  Omnibus: %.4f (SE=%.4f) [%.4f, %.4f] p=%s",
                  omnibus_est, omnibus_se, omnibus_ci[1], omnibus_ci[2],
                  fmt_p(omnibus_p)))
}

# --- 5c. Composite boundary mean ---
log_msg("")
log_msg("  5c. Composite boundary mean: mean(Delta_boundary)")

composite_est <- NA_real_
composite_se  <- NA_real_
composite_ci  <- c(NA_real_, NA_real_)
composite_p   <- NA_real_

if (!is.null(m_stacked)) {
  c_composite <- (1 / length(BOUNDARY_PROBES)) * c_boundary_sum
  composite_est <- as.numeric(c_composite %*% beta)
  composite_se  <- sqrt(as.numeric(t(c_composite) %*% V %*% c_composite))
  t_comp        <- composite_est / composite_se
  composite_p   <- 2 * pt(-abs(t_comp), df = df_r)
  composite_ci  <- composite_est + c(-1, 1) * qt(0.975, df_r) * composite_se

  log_msg(sprintf("  Composite boundary: %.4f (SE=%.4f) [%.4f, %.4f] p=%s",
                  composite_est, composite_se, composite_ci[1], composite_ci[2],
                  fmt_p(composite_p)))
}

# --- 5d. Pairwise divergence (appendix) ---
log_msg("")
log_msg("  5d. Pairwise divergence: Delta_d - Delta_x for each boundary probe")

pairwise_div <- NULL

if (!is.null(m_stacked)) {
  pw_rows <- list()
  for (s in BOUNDARY_PROBES) {
    c_x <- build_contrast_vector(nm, s)
    c_pair <- c_d - c_x
    pw_est <- as.numeric(c_pair %*% beta)
    pw_se  <- sqrt(as.numeric(t(c_pair) %*% V %*% c_pair))
    pw_t   <- pw_est / pw_se
    pw_p   <- 2 * pt(-abs(pw_t), df = df_r)
    pw_ci  <- pw_est + c(-1, 1) * qt(0.975, df_r) * pw_se

    pw_rows[[s]] <- tibble(
      boundary_probe = s,
      scenario = SCENARIO_SHORT[s],
      divergence = pw_est,
      SE = pw_se,
      CI_lo = pw_ci[1],
      CI_hi = pw_ci[2],
      p = pw_p
    )

    log_msg(sprintf("    d vs %s: %.4f (SE=%.4f) [%.4f, %.4f] p=%s",
                    s, pw_est, pw_se, pw_ci[1], pw_ci[2], fmt_p(pw_p)))
  }
  pairwise_div <- bind_rows(pw_rows) %>%
    mutate(
      p_fdr = p.adjust(p, method = "BH")
    )
  log_msg("    Pairwise divergence p-values are exploratory; BH-FDR-adjusted p values computed for appendix.")
  for (i in seq_len(nrow(pairwise_div))) {
    r <- pairwise_div[i, ]
    log_msg(sprintf("      d vs %s: p_raw=%s, p_fdr=%s",
                    r$boundary_probe, fmt_p(r$p), fmt_p(r$p_fdr)))
  }
}

# --- 5e. TOST on composite boundary (dual epsilon) ---
log_msg("")
log_msg("  5e. TOST equivalence test on composite boundary mean")

# Compute SDs from analytic subsample (S+A/S-only, complete cases)
sd_probes_analytic <- vapply(paste0("probe_", ALL_PROBES), function(pv) {
  sd(pooled_complete[[pv]], na.rm = TRUE)
}, numeric(1))
names(sd_probes_analytic) <- ALL_PROBES
sd_boundary          <- mean(sd_probes_analytic[BOUNDARY_PROBES])
epsilon_main         <- 0.10 * sd_boundary
epsilon_tight_inter  <- 0.15 * sd_boundary   # MOD-3 intermediate bound
epsilon_loose_inter  <- 0.20 * sd_boundary   # MOD-3 intermediate bound
epsilon_lax          <- 0.25 * sd_boundary
tost_p_main          <- NA_real_
tost_p_tight_inter   <- NA_real_
tost_p_loose_inter   <- NA_real_
tost_p_lax           <- NA_real_

log_msg(sprintf("  SD_boundary (mean of 7 probes): %.4f", sd_boundary))
log_msg(sprintf("  epsilon 0.10xSD: %.4f", epsilon_main))
log_msg(sprintf("  epsilon 0.15xSD: %.4f", epsilon_tight_inter))
log_msg(sprintf("  epsilon 0.20xSD: %.4f", epsilon_loose_inter))
log_msg(sprintf("  epsilon 0.25xSD: %.4f", epsilon_lax))

# Helper: compute TOST p-value for a given epsilon (symmetric bounds)
tost_p_for_eps <- function(est, se, eps, df) {
  # H0: |theta| >= eps vs H1: |theta| < eps
  t_upper <- (est - eps) / se
  t_lower <- (est + eps) / se
  p_upper <- pt(t_upper, df = df)
  p_lower <- pt(t_lower, df = df, lower.tail = FALSE)
  max(p_upper, p_lower)
}

if (!is.na(composite_est) && !is.null(m_stacked)) {
  tost_p_main         <- tost_p_for_eps(composite_est, composite_se, epsilon_main, df_r)
  tost_p_tight_inter  <- tost_p_for_eps(composite_est, composite_se, epsilon_tight_inter, df_r)
  tost_p_loose_inter  <- tost_p_for_eps(composite_est, composite_se, epsilon_loose_inter, df_r)
  tost_p_lax          <- tost_p_for_eps(composite_est, composite_se, epsilon_lax, df_r)

  log_msg(sprintf("  TOST 0.10xSD (eps=%.4f): p = %s [main]",
                  epsilon_main, fmt_p(tost_p_main)))
  log_msg(sprintf("  TOST 0.15xSD (eps=%.4f): p = %s [MOD-3]",
                  epsilon_tight_inter, fmt_p(tost_p_tight_inter)))
  log_msg(sprintf("  TOST 0.20xSD (eps=%.4f): p = %s [MOD-3]",
                  epsilon_loose_inter, fmt_p(tost_p_loose_inter)))
  log_msg(sprintf("  TOST 0.25xSD (eps=%.4f): p = %s [lax]",
                  epsilon_lax, fmt_p(tost_p_lax)))

  # --- MOD-3: TOST sensitivity table across 4 epsilon bounds ---
  tost_sens_df <- tibble::tibble(
    `Bound (fraction of SD)` = c("0.10", "0.15", "0.20", "0.25"),
    `Epsilon`                = sprintf("%.4f", c(epsilon_main, epsilon_tight_inter,
                                                  epsilon_loose_inter, epsilon_lax)),
    `Composite estimate`     = sprintf("%.4f", rep(composite_est, 4)),
    `SE`                     = sprintf("%.4f", rep(composite_se, 4)),
    `TOST p-value`           = c(fmt_p(tost_p_main), fmt_p(tost_p_tight_inter),
                                  fmt_p(tost_p_loose_inter), fmt_p(tost_p_lax)),
    `Reject H0 (alpha=.05)`  = ifelse(c(tost_p_main, tost_p_tight_inter,
                                         tost_p_loose_inter, tost_p_lax) < 0.05,
                                       "Yes", "No")
  )
  save_table(tost_sens_df, "app_tab_tost_sensitivity",
             caption = "")
}

# --- 5f. Interpretation logic ---
log_msg("")
log_msg("  5f. Discrimination interpretation")

interp_text <- "inconclusive"
delta_d_est <- NA_real_
delta_d_p   <- NA_real_
n_sig_boundary <- NA_integer_

if (!is.null(panorama_df) && !is.na(omnibus_p)) {
  delta_d_row <- panorama_df[panorama_df$probe == TARGET_PROBE, ]
  delta_d_est <- delta_d_row$estimate
  delta_d_p   <- delta_d_row$p

  # Diagnostic count (appendix only)
  n_sig_boundary <- sum(panorama_df$probe %in% BOUNDARY_PROBES &
                          panorama_df$p < 0.05)

  # Extract Delta_d CI
  delta_d_ci <- c(delta_d_row$CI_lo, delta_d_row$CI_hi)
  delta_d_se <- delta_d_row$SE

  # Build descriptive interpretation paragraph
  interp_text <- sprintf(
    paste0(
      "The target probe (fentanyl cooperation) shows Delta_d = %.3f ",
      "(SE = %.3f, 95%% CI [%.3f, %.3f], p = %s). ",
      "The composite boundary mean across 7 non-target probes is %.3f ",
      "(SE = %.3f, p = %s). ",
      "The omnibus divergence (Delta_d minus composite boundary) is %.3f ",
      "(SE = %.3f, 95%% CI [%.3f, %.3f], p = %s). ",
      "A TOST equivalence test on the composite boundary (eps = %.4f, ",
      "0.10 x SD_boundary) yields p = %s."
    ),
    delta_d_est, delta_d_se, delta_d_ci[1], delta_d_ci[2], fmt_p(delta_d_p),
    composite_est, composite_se, fmt_p(composite_p),
    omnibus_est, omnibus_se, omnibus_ci[1], omnibus_ci[2], fmt_p(omnibus_p),
    epsilon_main, fmt_p(tost_p_main)
  )

  log_msg(sprintf("  INTERPRETATION: %s", interp_text))
}

# --- 5g. Stacked vs separate consistency ---
log_msg("")
log_msg("  5g. Stacked vs separate model consistency")

max_discrepancy <- NA_real_
if (!is.null(panorama_df) && length(main_models) == 8) {
  discrepancies <- vapply(ALL_PROBES, function(s) {
    stacked_est  <- panorama_df$estimate[panorama_df$probe == s]
    separate_est <- extract_is_sa_contrast(main_models[[s]], sprintf("sep_%s", s))$est
    abs(stacked_est - separate_est)
  }, numeric(1))
  max_discrepancy <- max(discrepancies, na.rm = TRUE)
  log_msg(sprintf("  Max |stacked - separate| = %.6f (across 8 probes)", max_discrepancy))
}

# --- 5h. Available-case sensitivity ---
log_msg("")
log_msg("  5h. Available-case sensitivity results")

delta_d_avail  <- NA_real_
omnibus_avail  <- NA_real_

if (!is.null(m_stacked_avail)) {
  beta_av <- coef(m_stacked_avail)
  V_av    <- vcov(m_stacked_avail)
  df_av   <- df.residual(m_stacked_avail)
  nm_av   <- names(beta_av)

  # Delta_d from available-case
  c_d_av <- build_contrast_vector(nm_av, TARGET_PROBE)
  delta_d_avail <- as.numeric(c_d_av %*% beta_av)

  # Omnibus from available-case
  c_d_av2 <- build_contrast_vector(nm_av, TARGET_PROBE)
  c_bnd_av <- Reduce("+", lapply(BOUNDARY_PROBES, function(s) build_contrast_vector(nm_av, s)))
  c_omni_av <- c_d_av2 - (1 / length(BOUNDARY_PROBES)) * c_bnd_av
  omnibus_avail <- as.numeric(c_omni_av %*% beta_av)

  log_msg(sprintf("  Available-case Delta_d: %.4f (complete-case: %.4f)",
                  delta_d_avail, delta_d_est))
  log_msg(sprintf("  Available-case omnibus: %.4f (complete-case: %.4f)",
                  omnibus_avail, omnibus_est))

  # Direction consistency check
  d_consistent <- sign(delta_d_avail) == sign(delta_d_est)
  o_consistent <- sign(omnibus_avail) == sign(omnibus_est)
  log_msg(sprintf("  Direction consistency: Delta_d=%s, omnibus=%s",
                  if (d_consistent) "CONSISTENT" else "** FLIP **",
                  if (o_consistent) "CONSISTENT" else "** FLIP **"))
} else if (n_recovered == 0) {
  log_msg("  Skipped (available-case = complete-case)")
} else {
  log_msg("  Skipped (available-case model failed)")
}


# --- Section 6: Figure 4 — Panorama Forest Plot -----------------------------

log_msg("")
log_msg("--- Section 6: Figure 4 ---")

if (!is.null(panorama_df)) {
  # Build plot data with domain ordering
  plot_df <- panorama_df %>%
    mutate(
      is_target = probe == TARGET_PROBE,
      domain = factor(domain, levels = c("Security", "Health", "Economic", "Cultural")),
      label = paste0(scenario, " (", probe, ")"),
      sig = ifelse(p < 0.05, "*", "")
    )

  # Order: Security probes first (a,b,c), then Health (d), Economic (f), Cultural (e,g,h)
  probe_order <- c("a", "b", "c", "d", "f", "e", "g", "h")
  plot_df$label <- factor(plot_df$label,
                          levels = rev(plot_df$label[match(probe_order, plot_df$probe)]))

  # Add composite boundary row
  composite_row <- tibble(
    probe = "composite", scenario = "Composite boundary mean",
    domain = factor(NA, levels = levels(plot_df$domain)),
    estimate = composite_est, SE = composite_se,
    CI_lo = composite_ci[1], CI_hi = composite_ci[2],
    p = composite_p, is_target = FALSE,
    label = factor("Composite boundary mean"),
    sig = ifelse(composite_p < 0.05, "*", "")
  )

  fig4 <- ggplot(plot_df, aes(x = estimate, y = label, color = domain)) +
    # Reference line at 0
    geom_vline(xintercept = 0, linetype = "dashed", color = neutral_gray, linewidth = 0.5) +
    # Point estimates and CIs
    geom_errorbar(aes(xmin = CI_lo, xmax = CI_hi), width = 0.25, linewidth = 0.6) +
    geom_point(aes(size = is_target, shape = is_target), fill = "white") +
    # Significance stars
    geom_text(aes(label = sig, x = CI_hi + 0.02), hjust = 0, size = 5, show.legend = FALSE) +
    # Composite boundary as diamond below
    geom_point(data = composite_row,
               aes(x = estimate, y = label),
               shape = 18, size = 4, color = neutral_gray) +
    geom_errorbar(data = composite_row,
                  aes(x = estimate, y = label, xmin = CI_lo, xmax = CI_hi),
                  width = 0.2, linewidth = 0.6, color = neutral_gray) +
    # Scales
    scale_color_manual(values = domain_colors, na.value = neutral_gray,
                       name = "Domain") +
    scale_size_manual(values = c("FALSE" = 2.5, "TRUE" = 4), guide = "none") +
    scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 17), guide = "none") +
    # Labels
    labs(
      title = "Panorama Falsification Test: S+A vs S-only Effect by Scenario",
      subtitle = "Adjusted estimates from stacked model (scenario-specific controls), clustered by respondent\n* p < 0.05",
      x = expression(Delta ~ "(S+A minus S-only)"),
      y = NULL
    ) +
    theme_pax() +
    theme(
      legend.position = "right",  # Override bottom default; right-side fits forest plot layout
      axis.text.y = element_text(size = 11)
    )

  save_figure(fig4, "fig4_probes_panorama", w = 9, h = 6)
} else {
  log_msg("  WARNING: Cannot create Figure 4 (panorama_df is NULL)")
}

# Delete old dual-probe figure
old_fig <- here("output", "figures", "fig4_probes_boundary.pdf")
if (file.exists(old_fig)) {
  file.remove(old_fig)
  log_msg("  Deleted old: fig4_probes_boundary.pdf")
}


# --- Section 7: Tables -------------------------------------------------------

log_msg("")
log_msg("--- Section 7: Tables ---")

# --- 7.1 Main Table 4: Compact contrasts ---
if (!is.null(panorama_df) && !is.na(omnibus_est)) {
  delta_d_row <- panorama_df[panorama_df$probe == TARGET_PROBE, ]

  tab4_df <- tibble(
    Contrast = c(
      sprintf("Delta_d (target: %s)", SCENARIO_SHORT[TARGET_PROBE]),
      "Composite boundary mean(Delta_others)",
      "Omnibus divergence (Delta_d - composite)",
      sprintf("TOST equivalence (eps=%.3f)", epsilon_main)
    ),
    Estimate = sprintf("%.4f", c(delta_d_row$estimate, composite_est,
                                  omnibus_est, composite_est)),
    SE = sprintf("%.4f", c(delta_d_row$SE, composite_se,
                            omnibus_se, composite_se)),
    CI = c(
      sprintf("[%.4f, %.4f]", delta_d_row$CI_lo, delta_d_row$CI_hi),
      sprintf("[%.4f, %.4f]", composite_ci[1], composite_ci[2]),
      sprintf("[%.4f, %.4f]", omnibus_ci[1], omnibus_ci[2]),
      sprintf("+/- %.3f", epsilon_main)
    ),
    p_value = vapply(c(delta_d_row$p, composite_p, omnibus_p, tost_p_main),
                     fmt_p, character(1))
  )

  save_table(tab4_df, "tab4_phase5_contrasts",
             caption = "Phase 5: Panorama Discrimination Contrasts (S+A minus S-only)")
}

# --- 7.2 Appendix: Full panorama ---
if (!is.null(panorama_df)) {
  pan_export <- panorama_df %>%
    mutate(
      Probe = paste0(probe, ": ", scenario),
      Domain = domain,
      Estimate = sprintf("%.4f", estimate),
      SE = sprintf("%.4f", SE),
      CI = sprintf("[%.4f, %.4f]", CI_lo, CI_hi),
      p_value = vapply(p, fmt_p, character(1))
    ) %>%
    select(Probe, Domain, Estimate, SE, CI, p_value)

  save_table(pan_export, "app_tab_phase5_panorama",
             caption = "Phase 5 Appendix: All 8 Scenario-Specific Contrasts (S+A minus S-only)")
}

# --- 7.3 Appendix: Pairwise divergence ---
if (!is.null(pairwise_div)) {
  pw_export <- pairwise_div %>%
    mutate(
      Comparison = paste0("d vs ", boundary_probe, ": ", scenario),
      Divergence = sprintf("%.4f", divergence),
      SE = sprintf("%.4f", SE),
      CI = sprintf("[%.4f, %.4f]", CI_lo, CI_hi),
      p_value = vapply(p, fmt_p, character(1)),
      p_fdr = vapply(p_fdr, fmt_p, character(1))
    ) %>%
    select(Comparison, Divergence, SE, CI, p_value, p_fdr)

  save_table(pw_export, "app_tab_phase5_divergence",
             caption = paste(
               "Phase 5 Appendix: Pairwise Divergence (Delta\\_d minus Delta\\_x),",
               "exploratory p-values with BH-FDR adjustment"
             ))
}

# --- 7.4 Appendix: P_hat(S+A) ---
if (!is.null(mB_d)) {
  coefs_phat <- summary(mB_d)$coefficients
  if ("p_hat_sa" %in% rownames(coefs_phat)) {
    row <- coefs_phat["p_hat_sa", ]
    phat_tab <- tibble(
      DV = "probe_d",
      Estimate = row["Estimate"],
      SE = row["Std. Error"],
      t_value = row["t value"],
      p_value = row["Pr(>|t|)"],
      Note = "SEs do not propagate Stage 1 uncertainty; directional only"
    )
    save_table(phat_tab, "app_tab_phase5_phat_sa",
               caption = "Phase 5 Appendix: P\\_hat(S+A) Continuous Specification (probe d)")
  }
} else {
  log_msg("  SKIPPED: P_hat appendix table (model not available)")
}

# --- 7.5 Appendix: Zero-imputed n_approaches ---
if (!is.null(mZ_d)) {
  cz_d <- extract_is_sa_contrast(mZ_d, "mZ_d")
  zero_tab <- tibble(
    Probe = "probe_d",
    Estimate = cz_d$est,
    SE = cz_d$se,
    CI_low = cz_d$lo,
    CI_high = cz_d$hi,
    p = cz_d$p
  )
  save_table(zero_tab, "app_tab_phase5_napproaches_zero",
             caption = "Phase 5 Appendix: Contrast under Zero-Imputed n\\_approaches (probe d)")

  if (!is.na(cz_d$est) && !is.null(panorama_df)) {
    stacked_d <- panorama_df$estimate[panorama_df$probe == TARGET_PROBE]
    consistent <- sign(stacked_d) == sign(cz_d$est)
    log_msg(sprintf("  Zero-imputed directional consistency: %s",
                    ifelse(consistent, "CONSISTENT", "INCONSISTENT")))
  }
}

# --- 7.6 Appendix: All-12 n_approaches ---
if (!is.null(mX_d)) {
  cx_d <- extract_is_sa_contrast(mX_d, "mX_d")
  excl_tab <- tibble(
    Probe = "probe_d",
    Estimate = cx_d$est,
    SE = cx_d$se,
    CI_low = cx_d$lo,
    CI_high = cx_d$hi,
    p = cx_d$p
  )
  save_table(excl_tab, "app_tab_phase5_nexcl",
             caption = "Phase 5 Appendix: Contrast with all-12 n\\_approaches (probe d)")

  if (!is.na(cx_d$est) && !is.null(panorama_df)) {
    stacked_d <- panorama_df$estimate[panorama_df$probe == TARGET_PROBE]
    consistent <- sign(stacked_d) == sign(cx_d$est)
    log_msg(sprintf("  All-12 n_approaches directional consistency: %s",
                    ifelse(consistent, "CONSISTENT", "INCONSISTENT")))
  }
}


# --- Section 8: Logging & Diagnostics ----------------------------------------

log_msg("")
log_msg("--- Section 8: Diagnostics ---")

# --- 8.1 Correlations ---
log_msg("  Key correlations (pooled):")
cor_vars <- c("warmth_z", "blame_china", "n_approaches_10")
cor_data <- pooled[, cor_vars]
cor_complete <- cor_data[complete.cases(cor_data), ]
if (nrow(cor_complete) > 0) {
  log_msg(sprintf("    cor(warmth_z, blame_china) = %.4f",
                  cor(cor_complete$warmth_z, cor_complete$blame_china)))
  log_msg(sprintf("    cor(n_approaches_10, warmth_z) = %.4f",
                  cor(cor_complete$n_approaches_10, cor_complete$warmth_z)))
  log_msg(sprintf("    cor(n_approaches_10, blame_china) = %.4f",
                  cor(cor_complete$n_approaches_10, cor_complete$blame_china)))
}

# --- 8.2 VIF: Manual lm-based on probe_d separate model ---
log_msg("")
log_msg("  VIF diagnostic (manual lm-based, probe_d separate model):")
vif_result <- NULL

if (!is.null(main_models[["d"]])) {
  vif_result <- tryCatch({
    mm <- model.matrix(main_models[["d"]])
    mm_noint <- mm[, -1, drop = FALSE]
    keep <- complete.cases(mm_noint)
    mm_noint <- mm_noint[keep, , drop = FALSE]
    p <- ncol(mm_noint)
    vif_vals <- numeric(p)
    names(vif_vals) <- colnames(mm_noint)
    for (j in seq_len(p)) {
      y_j <- mm_noint[, j]
      X_j <- cbind(1, mm_noint[, -j, drop = FALSE])
      fit_j <- tryCatch(lm.fit(X_j, y_j), error = function(e) NULL)
      if (!is.null(fit_j)) {
        ss_res <- sum(fit_j$residuals^2, na.rm = TRUE)
        ss_tot <- sum((y_j - mean(y_j, na.rm = TRUE))^2, na.rm = TRUE)
        r2_j <- if (ss_tot > 0) 1 - ss_res / ss_tot else 0
        vif_vals[j] <- if (r2_j < 1) 1 / (1 - r2_j) else Inf
      } else {
        vif_vals[j] <- NA_real_
      }
    }
    vif_vals
  }, error = function(e) {
    log_msg(sprintf("    VIF computation failed: %s", e$message))
    NULL
  })

  if (!is.null(vif_result)) {
    for (nm_v in names(vif_result)) {
      v_val <- vif_result[nm_v]
      flag <- if (!is.na(v_val) && is.finite(v_val) && v_val > 5) " ** FLAG > 5 **" else ""
      log_msg(sprintf("    %s: %.2f%s", nm_v, v_val, flag))
    }
    finite_vifs <- vif_result[is.finite(vif_result) & !is.na(vif_result)]
    if (length(finite_vifs) > 0) {
      max_vif <- max(finite_vifs)
      if (max_vif > 10) {
        log_msg("  WARNING: VIF > 10 — severe multicollinearity concern")
      } else if (max_vif > 5) {
        log_msg("  NOTE: VIF > 5 — moderate multicollinearity; interpret with caution")
      } else {
        log_msg("  VIF check: all values < 5 — no multicollinearity concern")
      }
    }
  }
}

# --- 8.3 Panorama summary ---
log_msg("")
log_msg("  ====================================================================")
log_msg("  PANORAMA SUMMARY")
log_msg("  ====================================================================")
if (!is.null(panorama_df)) {
  for (i in seq_len(nrow(panorama_df))) {
    r <- panorama_df[i, ]
    log_msg(sprintf("  Delta_%s [%s]: %.4f (SE=%.4f) p=%s",
                    r$probe, r$domain, r$estimate, r$SE, fmt_p(r$p)))
  }
}

# --- 8.4 Discrimination estimates ---
log_msg("")
log_msg("  Discrimination estimates:")
log_msg(sprintf("    Delta_d (target): %.4f (SE=%.4f), p=%s",
                ifelse(is.null(panorama_df), NA,
                       panorama_df$estimate[panorama_df$probe == TARGET_PROBE]),
                ifelse(is.null(panorama_df), NA,
                       panorama_df$SE[panorama_df$probe == TARGET_PROBE]),
                ifelse(is.null(panorama_df), "NA",
                       fmt_p(panorama_df$p[panorama_df$probe == TARGET_PROBE]))))
log_msg(sprintf("    Composite boundary: %.4f (SE=%.4f), p=%s",
                composite_est, composite_se, fmt_p(composite_p)))
log_msg(sprintf("    Omnibus divergence: %.4f (SE=%.4f), p=%s",
                omnibus_est, omnibus_se, fmt_p(omnibus_p)))
log_msg(sprintf("    TOST (eps=%.4f): p=%s", epsilon_main, fmt_p(tost_p_main)))
log_msg(sprintf("    N boundary probes p<0.05: %d / %d",
                ifelse(is.na(n_sig_boundary), 0L, n_sig_boundary),
                length(BOUNDARY_PROBES)))

# --- 8.5 Robustness summary (consolidated) ---
log_msg("")
log_msg("  Robustness summary (probe d, direction consistency):")
if (!is.null(panorama_df)) {
  stacked_d <- panorama_df$estimate[panorama_df$probe == TARGET_PROBE]
  rob_lines <- character()

  # P_hat(S+A)
  if (!is.null(mB_d)) {
    phat_coef <- coef(summary(mB_d))
    if ("p_hat_sa" %in% rownames(phat_coef)) {
      phat_est <- phat_coef["p_hat_sa", "Estimate"]
      rob_lines <- c(rob_lines, sprintf("P_hat(S+A)=%.4f (%s)",
                     phat_est, ifelse(phat_est > 0, "+", "-")))
    }
  }
  # Zero-imputed
  if (!is.null(mZ_d)) {
    cz <- extract_is_sa_contrast(mZ_d, "mZ_d")
    rob_lines <- c(rob_lines, sprintf("zero-imputed=%.4f (%s)",
                   cz$est, ifelse(sign(cz$est) == sign(stacked_d), "same", "flip")))
  }
  # All-12
  if (!is.null(mX_d)) {
    cx <- extract_is_sa_contrast(mX_d, "mX_d")
    rob_lines <- c(rob_lines, sprintf("all-12=%.4f (%s)",
                   cx$est, ifelse(sign(cx$est) == sign(stacked_d), "same", "flip")))
  }
  # Available-case
  if (!is.na(delta_d_avail)) {
    rob_lines <- c(rob_lines, sprintf("available-case=%.4f (%s)",
                   delta_d_avail, ifelse(sign(delta_d_avail) == sign(stacked_d), "same", "flip")))
  }
  # Binary
  if (any(binary_flags) && !is.null(binary_models[["d"]]) && !is.null(panorama_df)) {
    cbin <- extract_is_sa_contrast(binary_models[["d"]], "mBin_d")
    if (!is.na(cbin$est)) {
      rob_lines <- c(rob_lines, sprintf("binary=%.4f (%s)",
                     cbin$est, ifelse(sign(cbin$est) == sign(stacked_d), "same", "flip")))
    }
  }

  log_msg(sprintf("    Main estimate: %.4f; %s", stacked_d, paste(rob_lines, collapse = "; ")))
}


# --- Section 9: Save Objects & Log -------------------------------------------

log_msg("")
log_msg("--- Section 9: Save Objects & Log ---")

phase5_objects <- list(
  m_stacked       = m_stacked,
  m_stacked_avail = m_stacked_avail,
  main_models     = main_models,
  panorama        = panorama_df,
  omnibus         = list(est = omnibus_est, se = omnibus_se,
                         ci = omnibus_ci, p = omnibus_p),
  composite       = list(est = composite_est, se = composite_se,
                         ci = composite_ci, p = composite_p),
  pairwise_div    = pairwise_div,
  tost_main       = list(epsilon = epsilon_main, p_tost = tost_p_main,
                         rejected = !is.na(tost_p_main) && tost_p_main < 0.05),
  tost_lax        = list(epsilon = epsilon_lax, p_tost = tost_p_lax,
                         rejected = !is.na(tost_p_lax) && tost_p_lax < 0.05),
  avail_case      = list(n_wide_avail = n_wide_avail,
                         delta_d_avail = delta_d_avail,
                         omnibus_avail = omnibus_avail),
  vif             = vif_result,
  interpretation  = interp_text,
  warmth_moments  = c(mu = warmth_mu, sd = warmth_sd),
  sd_probes       = sd_probes,
  scenario_map    = SCENARIO_MAP,
  domain_map      = DOMAIN_MAP
)
saveRDS(phase5_objects, here("output", "tables", "phase5_objects.rds"))
log_msg("  Saved: phase5_objects.rds")

# File manifest
output_files <- c(
  "output/figures/fig4_probes_panorama.pdf",
  "output/tables/tab4_phase5_contrasts.tex",
  "output/tables/tab4_phase5_contrasts.csv",
  "output/tables/app_tab_phase5_panorama.tex",
  "output/tables/app_tab_phase5_panorama.csv",
  "output/tables/app_tab_phase5_divergence.tex",
  "output/tables/app_tab_phase5_divergence.csv",
  "output/tables/app_tab_phase5_phat_sa.tex",
  "output/tables/app_tab_phase5_phat_sa.csv",
  "output/tables/app_tab_phase5_napproaches_zero.tex",
  "output/tables/app_tab_phase5_napproaches_zero.csv",
  "output/tables/app_tab_phase5_nexcl.tex",
  "output/tables/app_tab_phase5_nexcl.csv",
  "output/tables/phase5_objects.rds",
  "output/tables/phase5_log.txt"
)

log_msg("")
log_msg("  File manifest:")
for (f in output_files) {
  exists_flag <- ifelse(file.exists(here(f)), "EXISTS", "MISSING")
  log_msg(sprintf("    %s: %s", f, exists_flag))
}

# Session info
log_msg("")
log_msg("  sessionInfo():")
si <- capture.output(sessionInfo())
for (line in si) log_msg(paste0("    ", line))

# Write log
writeLines(log_env$entries, here("output", "tables", "phase5_log.txt"))
message("Phase 5 complete. Log written to output/tables/phase5_log.txt")
