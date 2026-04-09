#' ============================================================================
#' 04_mechanisms.R — Phase 4: Tool Configuration as Behavioral Signature
#' Poll 2025: Fentanyl Policy Survey Analysis
#' PAX sapiens Foundation
#'
#' @description Tests whether crisis blame shifts how warm-feeling respondents
#'   pursue cooperation: toward a leveraged strategy pairing sanctions with
#'   demands for action (S+A), rather than indiscriminate punishment
#'   (Sanctions-only). Uses survey-weighted multinomial logit with interaction.
#'
#' @question Does fentanyl blame change the *tools* warm respondents prefer,
#'   even if it does not change their overall cooperative posture?
#'
#' @input  data/clean/pooled.rds, data/clean/clean_2024.rds,
#'         data/clean/clean_2025.rds
#' @output output/tables/tab3_phase4_estimands (.tex + .csv)
#'         output/tables/app_tab_phase4_mnl_coefficients (.tex + .csv)
#'         output/tables/app_tab_phase4_acquiescence (.tex + .csv)
#'         output/tables/app_tab_placebo_models (.tex + .csv)
#'         output/figures/fig3_tool_config_warmthXblame.pdf
#'         output/figures/app_fig_placebo_sanctionsXintlcoop.pdf
#'         output/tables/phase4_objects.rds
#'         output/tables/phase4_log.txt
#'
#' @depends here, tidyverse, survey, srvyr, nnet, kableExtra, scales,
#'          marginaleffects, texreg, broom
#' ============================================================================

# --- Section 0: Setup --------------------------------------------------------

library(here)
library(tidyverse)
library(survey)
library(srvyr)
library(nnet)
library(kableExtra)
library(scales)
library(marginaleffects)
library(texreg)

set.seed(20250217)

dir.create(here("output", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "figures"), recursive = TRUE, showWarnings = FALSE)

# Environment-based logging (same pattern as 01/02/03)
log_env <- new.env(parent = emptyenv())
log_env$entries <- character()

#' Log a message to console and internal log
#' @param msg Character string to log
log_msg <- function(msg) {
  log_env$entries <- c(log_env$entries, msg)
  message(msg)
}

log_msg("========================================================================")
log_msg("PHASE 4 LOG: Tool Configuration as Behavioral Signature")
log_msg(sprintf("Timestamp: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")))
log_msg("========================================================================")

# PAX sapiens color palette (same as 03_models.R)
pax_dark       <- "#1a1a2e"
pax_blue       <- "#16213e"
pax_accent     <- "#0f3460"
pax_highlight  <- "#e94560"
neutral_gray   <- "#6c757d"
positive_green <- "#198754"
negative_red   <- "#dc3545"


# Tool configuration palette
tool_colors <- c(
  "Neither"        = neutral_gray,
  "Sanctions-only" = pax_highlight,
  "Action-only"    = pax_accent,
  "S+A"            = positive_green
)

#' Custom ggplot2 theme for PAX sapiens publications
#' @param base_size Base font size (default 14)
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

#' Save a table as LaTeX (.tex) and CSV (.csv)
#' @param tbl_df Tibble or data frame
#' @param name File stem (no extension)
#' @param caption LaTeX table caption
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

#' Save a ggplot figure as vector PDF (cairo_pdf preferred, pdf fallback)
#' @param plot ggplot object
#' @param name File stem (no extension)
#' @param w Width in inches (default 6.5)
#' @param h Height in inches (default 4.5)
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

#' Get mode of a vector (for prediction grids)
#' @param x Vector
#' @return Most frequent value
get_mode <- function(x) {
  x <- na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


# --- Section 1: Load Data & Construct Variables ------------------------------

log_msg("")
log_msg("--- Section 1: Load Data & Construct Variables ---")

pooled     <- readRDS(here("data", "clean", "pooled.rds"))
clean_2024 <- readRDS(here("data", "clean", "clean_2024.rds"))
clean_2025 <- readRDS(here("data", "clean", "clean_2025.rds"))
log_msg(sprintf("  Loaded: pooled=%d, 2024=%d, 2025=%d",
                nrow(pooled), nrow(clean_2024), nrow(clean_2025)))

# --- 1a. build_analysis_vars (copied from 03_models.R) ---
# Creates warmth_z, ideo5_clean, exposure_index, newsint_attn, posture vars
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
log_msg(sprintf("  Pooled posture: mean=%.3f, sd=%.3f", posture_mu, posture_sd))

pooled     <- build_analysis_vars(pooled, warmth_mu, warmth_sd,
                                  posture_mu, posture_sd)
clean_2024 <- build_analysis_vars(clean_2024, warmth_mu, warmth_sd,
                                  posture_mu, posture_sd)
clean_2025 <- build_analysis_vars(clean_2025, warmth_mu, warmth_sd,
                                  posture_mu, posture_sd)

# --- 1b. Construct tool_config DV ---
log_msg("")
log_msg("  Constructing tool_config DV...")

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
    ) %>% factor(levels = c("Neither", "Sanctions-only", "Action-only", "S+A")),
    # Placebo DV (DA#7): sanctions x intl_cooperation
    intl_coop_bin = {
      x <- as.numeric(approach_intl_cooperation)
      ifelse(is.na(x), NA_integer_, ifelse(x == 1, 1L, 0L))
    },
    placebo_tool_config = case_when(
      is.na(sanctions_bin) | is.na(intl_coop_bin) ~ NA_character_,
      sanctions_bin == 0 & intl_coop_bin == 0 ~ "Neither",
      sanctions_bin == 1 & intl_coop_bin == 0 ~ "Sanctions-only",
      sanctions_bin == 0 & intl_coop_bin == 1 ~ "IntlCoop-only",
      sanctions_bin == 1 & intl_coop_bin == 1 ~ "Sanctions+IntlCoop"
    ) %>% factor(levels = c("Neither", "Sanctions-only",
                             "IntlCoop-only", "Sanctions+IntlCoop"))
  )
}

pooled     <- build_tool_config(pooled)
clean_2024 <- build_tool_config(clean_2024)
clean_2025 <- build_tool_config(clean_2025)

# --- 1c. Construct n_approaches (Fix 1: NA-preserving) ---
log_msg("  Constructing n_approaches (acquiescence control)...")

approach_vars_all <- c("approach_intl_cooperation", "approach_enforce_sales",
                       "approach_enforce_users", "approach_legalization",
                       "approach_health_justice", "approach_addiction_resources",
                       "approach_blocking_drugs", "approach_social_cultural",
                       "approach_sanctions", "approach_china_action",
                       "approach_mexico_action", "approach_penalties")

# Exclude DV components (sanctions/action) from acquiescence control to avoid
# mechanically controlling for the outcome definition itself.
approach_vars_acq <- setdiff(
  approach_vars_all,
  c("approach_sanctions", "approach_china_action")
)

build_n_approaches <- function(df) {
  approach_bins <- sapply(approach_vars_acq, function(v) {
    x <- as.numeric(df[[v]])
    ifelse(is.na(x), NA_integer_, ifelse(x == 1, 1L, 0L))
  })
  df$n_approaches_main <- rowSums(approach_bins, na.rm = FALSE)
  df$n_approaches_miss <- as.integer(rowSums(is.na(approach_bins)) > 0)
  df$n_approaches_zero <- rowSums(
    replace(approach_bins, is.na(approach_bins), 0L), na.rm = TRUE
  )
  df
}

pooled     <- build_n_approaches(pooled)
clean_2024 <- build_n_approaches(clean_2024)
clean_2025 <- build_n_approaches(clean_2025)

# Audit n_approaches
for (v in c("n_approaches_main", "n_approaches_miss", "n_approaches_zero")) {
  vals <- na.omit(as.numeric(pooled[[v]]))
  log_msg(sprintf("    %s: n=%d, NAs=%d, range=[%.0f, %.0f], mean=%.3f",
                  v, length(vals), sum(is.na(pooled[[v]])),
                  min(vals), max(vals), mean(vals)))
}
log_msg(sprintf("    Pct with n_approaches_miss=1: %.1f%%",
                100 * mean(pooled$n_approaches_miss, na.rm = TRUE)))
log_msg("    n_approaches excludes approach_sanctions and approach_china_action (DV components).")
if (mean(pooled$n_approaches_miss, na.rm = TRUE) == 0) {
  log_msg("    NOTE: No missingness in approach items. n_approaches_main == n_approaches_zero.")
  log_msg("    The three-variant acquiescence comparison reduces to baseline vs +n_approaches.")
}

# --- 1d. Audit tool_config ---
log_msg("")
log_msg("  tool_config distribution (pooled):")
tc_tab <- table(pooled$tool_config, useNA = "ifany")
for (cat in names(tc_tab)) {
  log_msg(sprintf("    %s: %d (%.1f%%)", cat, tc_tab[cat],
                  100 * tc_tab[cat] / sum(tc_tab)))
}

# Cross-tab: tool_config x blame_china
log_msg("")
log_msg("  tool_config x blame_china (pooled):")
xtab <- table(pooled$tool_config, pooled$blame_china, useNA = "ifany")
for (cat in rownames(xtab)) {
  log_msg(sprintf("    %s: blame=0: %d, blame=1: %d", cat,
                  xtab[cat, "0"], xtab[cat, "1"]))
}

# (DA#6) Per-wave cell counts with flag
log_msg("")
log_msg("  Per-wave cell counts (DA#6 cell-size gating):")
cell_ok_24 <- TRUE
cell_ok_25 <- TRUE
for (wv in c("2024", "2025")) {
  d <- if (wv == "2024") clean_2024 else clean_2025
  wv_tab <- table(d$tool_config, useNA = "ifany")
  for (cat in names(wv_tab)) {
    flag <- if (wv_tab[cat] < 50) " ** BELOW THRESHOLD **" else ""
    if (wv_tab[cat] < 50) {
      if (wv == "2024") cell_ok_24 <- FALSE
      if (wv == "2025") cell_ok_25 <- FALSE
    }
    log_msg(sprintf("    %s [%s]: %d%s", wv, cat, wv_tab[cat], flag))
  }
}


# --- Section 2: Survey Design Objects ----------------------------------------

log_msg("")
log_msg("--- Section 2: Survey Design Objects ---")

des_2024 <- svydesign(ids = ~1, weights = ~weight, data = clean_2024)
des_2025 <- svydesign(ids = ~1, weights = ~weight, data = clean_2025)
des_pool <- svydesign(ids = ~1, weights = ~weight, data = pooled)

stopifnot(
  "Pooled N must equal sum of wave Ns" =
    nrow(pooled) == nrow(clean_2024) + nrow(clean_2025)
)
log_msg(sprintf("  N check: %d / %d / %d PASS",
                nrow(clean_2024), nrow(clean_2025), nrow(pooled)))

for (wv in c("2024", "2025")) {
  d <- if (wv == "2024") clean_2024 else clean_2025
  w_mean <- mean(d$weight, na.rm = TRUE)
  log_msg(sprintf("  %s weight mean: %.6f", wv, w_mean))
}


# --- Section 3: Controls Formula ---------------------------------------------

log_msg("")
log_msg("--- Section 3: Controls Formula ---")

ctrl_rhs <- "party3 + ideo5_clean + educ_college + age + female + race_eth + newsint_attn + exposure_index + posture_z"
ctrl_rhs_acq1 <- paste0(ctrl_rhs, " + n_approaches_main + n_approaches_miss")
ctrl_rhs_acq2 <- paste0(ctrl_rhs, " + n_approaches_zero")

# Listwise deletion counts
model_vars_base <- c("tool_config", "warmth_z", "blame_china", "party3",
                     "ideo5_clean", "educ_college", "age", "female",
                     "race_eth", "newsint_attn", "exposure_index", "posture_z")

for (wv in c("2024", "2025", "pooled")) {
  d <- switch(wv, "2024" = clean_2024, "2025" = clean_2025, pooled)
  n_complete <- sum(complete.cases(d[, model_vars_base]))
  n_total <- nrow(d)
  log_msg(sprintf("  %s: %d/%d complete cases (%.1f%% lost to listwise deletion)",
                  wv, n_complete, n_total,
                  100 * (1 - n_complete / n_total)))
}


# --- Section 4: Primary Models — Multinomial Logit ---------------------------

log_msg("")
log_msg("--- Section 4: Primary Models (Multinomial Logit) ---")

#' Fit a multinomial logit model with survey weights
#' @param df Data frame (must have tool_config, warmth_z, blame_china, controls)
#' @param ctrl_rhs Character RHS for controls
#' @param include_year_fe Logical: add factor(year)?
#' @return nnet::multinom object or NULL
run_multinomial <- function(df, ctrl_rhs, include_year_fe = FALSE) {
  fmla_rhs <- paste0("warmth_z * blame_china + ", ctrl_rhs)
  if (include_year_fe) fmla_rhs <- paste0(fmla_rhs, " + factor(year)")
  fmla <- as.formula(paste0("tool_config ~ ", fmla_rhs))

  model_vars <- all.vars(fmla)
  df_complete <- df[complete.cases(df[, model_vars, drop = FALSE]), ]

  m <- tryCatch(
    multinom(fmla, data = df_complete, weights = weight,
             trace = FALSE, Hess = TRUE, maxit = 300),
    error = function(e) {
      log_msg(sprintf("  MULTINOM FAILED: %s", e$message))
      NULL
    }
  )

  if (!is.null(m)) {
    n_eq <- nlevels(df_complete$tool_config) - 1
    log_msg(sprintf("  N=%d complete, %d equations, %d coefs/eq",
                    nrow(df_complete), n_eq,
                    ncol(coef(m))))
    if (!m$convergence %in% c(0L, 0)) {
      log_msg("  WARNING: multinom may not have converged")
    }
  }
  m
}

# Pooled model (always primary)
mpool <- run_multinomial(pooled, ctrl_rhs, include_year_fe = TRUE)
log_msg(sprintf("  mpool: %s", ifelse(is.null(mpool), "FAILED", "OK")))

# Wave-specific models (DA#6: only if cell sizes adequate)
m24 <- NULL
m25 <- NULL

if (cell_ok_24) {
  m24 <- run_multinomial(clean_2024, ctrl_rhs, include_year_fe = FALSE)
  log_msg(sprintf("  m24: %s", ifelse(is.null(m24), "FAILED", "OK")))
} else {
  log_msg("  m24: SKIPPED (cell size < 50 in at least one category)")
}

if (cell_ok_25) {
  m25 <- run_multinomial(clean_2025, ctrl_rhs, include_year_fe = FALSE)
  log_msg(sprintf("  m25: %s", ifelse(is.null(m25), "FAILED", "OK")))
} else {
  log_msg("  m25: SKIPPED (cell size < 50 in at least one category)")
}

# Log key coefficients for pooled model
if (!is.null(mpool)) {
  coef_mat <- coef(mpool)
  log_msg("  Pooled model key coefficients:")
  for (eq_name in rownames(coef_mat)) {
    for (cv in c("warmth_z", "blame_china", "warmth_z:blame_china")) {
      if (cv %in% colnames(coef_mat)) {
        log_msg(sprintf("    [%s] %s: %.4f", eq_name, cv, coef_mat[eq_name, cv]))
      }
    }
  }
}


# --- Section 5: Predicted Probabilities --------------------------------------

log_msg("")
log_msg("--- Section 5: Predicted Probabilities ---")

#' Robust prediction for multinomial models (Fix 3)
#' Try marginaleffects::predictions() first; fallback to predict()
#' @param model nnet::multinom object
#' @param newdata Prediction grid
#' @param label Character label for logging
#' @return Data frame with group, estimate, conf.low, conf.high, std.error
safe_mnl_predictions <- function(model, newdata, label) {
  preds <- tryCatch(
    {
      p <- marginaleffects::predictions(model, newdata = newdata, type = "probs")
      as.data.frame(p) %>% mutate(model_label = label)
    },
    error = function(e) NULL
  )
  if (!is.null(preds)) {
    log_msg(sprintf("  %s: marginaleffects OK (%d rows)", label, nrow(preds)))
    return(preds)
  }

  # Fallback: point estimates only
  log_msg(sprintf("  %s: marginaleffects failed; using predict() fallback", label))
  probs <- predict(model, newdata = newdata, type = "probs")
  prob_df <- as.data.frame(probs)
  prob_df$row_id <- seq_len(nrow(prob_df))
  long <- prob_df |>
    tidyr::pivot_longer(cols = -row_id, names_to = "group",
                        values_to = "estimate") |>
    dplyr::left_join(
      dplyr::mutate(newdata, row_id = dplyr::row_number()),
      by = "row_id"
    ) |>
    dplyr::mutate(conf.low = NA_real_, conf.high = NA_real_,
                  std.error = NA_real_, model_label = label) |>
    dplyr::select(-row_id)
  long
}

#' Build prediction grid for multinomial
#' @param d Data frame used to fit model
#' @param include_year Logical: include year column?
#' @return Data frame with warmth_z grid x blame_china x controls at typical
build_mnl_grid <- function(d, include_year = FALSE) {
  grid <- expand.grid(
    warmth_z       = seq(-2, 2, by = 0.25),
    blame_china    = c(0, 1),
    party3         = get_mode(d$party3),
    ideo5_clean    = median(d$ideo5_clean, na.rm = TRUE),
    educ_college   = get_mode(d$educ_college),
    age            = median(d$age, na.rm = TRUE),
    female         = get_mode(d$female),
    race_eth       = get_mode(d$race_eth),
    newsint_attn   = median(d$newsint_attn, na.rm = TRUE),
    exposure_index = median(d$exposure_index, na.rm = TRUE),
    posture_z      = median(d$posture_z, na.rm = TRUE),
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      party3   = factor(party3, levels = levels(d$party3)),
      race_eth = factor(race_eth, levels = levels(d$race_eth))
    )
  if (include_year) grid$year <- get_mode(d$year)
  grid
}

# Generate predictions for each model
fig3_parts <- list()

if (!is.null(mpool)) {
  grid_pool <- build_mnl_grid(pooled, include_year = TRUE)
  fig3_parts[["Pooled"]] <- safe_mnl_predictions(mpool, grid_pool, "Pooled")
}
if (!is.null(m24)) {
  grid_24 <- build_mnl_grid(clean_2024, include_year = FALSE)
  fig3_parts[["2024"]] <- safe_mnl_predictions(m24, grid_24, "2024")
}
if (!is.null(m25)) {
  grid_25 <- build_mnl_grid(clean_2025, include_year = FALSE)
  fig3_parts[["2025"]] <- safe_mnl_predictions(m25, grid_25, "2025")
}

fig3_data <- bind_rows(fig3_parts)

# Sanity: check probs sum to 1
if (nrow(fig3_data) > 0 && "warmth_z" %in% names(fig3_data)) {
  prob_sums <- fig3_data %>%
    group_by(model_label, warmth_z, blame_china) %>%
    summarise(prob_sum = sum(estimate, na.rm = TRUE), .groups = "drop")
  max_dev <- max(abs(prob_sums$prob_sum - 1.0))
  log_msg(sprintf("  Prob sum check: max deviation from 1.0 = %.6f", max_dev))
  if (max_dev > 0.01) log_msg("  WARNING: predicted probabilities do not sum to 1")
}


# --- Section 6: Figure 3 — Main Plot ----------------------------------------

log_msg("")
log_msg("--- Section 6: Figure 3 (Main Plot) ---")

if (nrow(fig3_data) > 0) {
  fig3_plot_data <- fig3_data %>%
    mutate(
      blame_label = ifelse(blame_china == 1,
                           "Blame China = 1", "Blame China = 0")
    )

  p3 <- ggplot(fig3_plot_data,
               aes(x = warmth_z, y = estimate,
                   color = group, fill = group)) +
    geom_line(linewidth = 1) +
    facet_grid(blame_label ~ model_label) +
    scale_color_manual(values = tool_colors, name = "Tool Configuration") +
    scale_fill_manual(values = tool_colors, name = "Tool Configuration") +
    scale_y_continuous(labels = percent_format(), limits = c(0, NA)) +
    labs(
      title = "Predicted Tool Configuration by Warmth, Blame, and Year",
      subtitle = "Multinomial logit; controls at medians/modes",
      x = "Warmth toward China (z-score)",
      y = "Predicted Probability"
    ) +
    theme_pax() +
    theme(legend.position = "bottom")

  # Add CI ribbons only if available
  has_ci <- any(!is.na(fig3_plot_data$conf.low))
  if (has_ci) {
    p3 <- p3 + geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                            alpha = 0.12, color = NA)
  }

  save_figure(p3, "fig3_tool_config_warmthXblame", w = 9, h = 6.5)
} else {
  log_msg("  WARNING: No prediction data — skipping Figure 3")
}


# --- Section 7: Hypothesis Tests — Flagged Reporting -------------------------

log_msg("")
log_msg("--- Section 7: Hypothesis Tests (Flagged Reporting) ---")
log_msg("  NOTE: Results are reported, not gated. Unexpected directions are flagged.")

key_estimands <- NULL

if (!is.null(mpool)) {
  # H4a: Effect of blame on P(S+A) vs P(Sanctions-only) at different warmth levels
  h4a_results <- list()
  for (wz in c(-1, 0, 1)) {
    comps <- tryCatch(
      comparisons(mpool, variables = "blame_china",
                  newdata = datagrid(warmth_z = wz, model = mpool),
                  type = "probs"),
      error = function(e) {
        log_msg(sprintf("  comparisons() failed at warmth_z=%d: %s", wz, e$message))
        NULL
      }
    )
    if (!is.null(comps)) {
      comps_df <- as.data.frame(comps) %>%
        mutate(warmth_z_val = wz)
      h4a_results[[length(h4a_results) + 1]] <- comps_df
    }
  }

  if (length(h4a_results) > 0) {
    h4a_all <- bind_rows(h4a_results)

    log_msg("")
    log_msg("  H4a: Blame effect (Dpp) on each category at different warmth levels:")
    for (wz in c(-1, 0, 1)) {
      sub <- h4a_all %>% filter(warmth_z_val == wz)
      for (g in unique(sub$group)) {
        row <- sub %>% filter(group == g)
        if (nrow(row) == 1) {
          ci_lo <- if (!is.na(row$conf.low)) sprintf("%.4f", row$conf.low) else "NA"
          ci_hi <- if (!is.na(row$conf.high)) sprintf("%.4f", row$conf.high) else "NA"
          log_msg(sprintf("    warmth_z=%+d, %s: Dpp=%.4f [%s, %s]",
                          wz, g, row$estimate, ci_lo, ci_hi))
        }
      }
    }

    # H4a direction check: at warmth_z=+1, Dpp(S+A) > Dpp(S-only)?
    dpp_sa_p1 <- h4a_all %>%
      filter(warmth_z_val == 1, group == "S+A") %>%
      pull(estimate) %>% first()
    dpp_sonly_p1 <- h4a_all %>%
      filter(warmth_z_val == 1, group == "Sanctions-only") %>%
      pull(estimate) %>% first()

    if (!is.null(dpp_sa_p1) && !is.null(dpp_sonly_p1) &&
        !is.na(dpp_sa_p1) && !is.na(dpp_sonly_p1)) {
      if (dpp_sa_p1 > dpp_sonly_p1) {
        log_msg(sprintf("  H4a DIRECTION: Dpp(S+A)=%.4f > Dpp(S-only)=%.4f at warmth=+1 -> EXPECTED",
                        dpp_sa_p1, dpp_sonly_p1))
      } else {
        log_msg(sprintf("  FLAG: H4a direction unexpected: Dpp(S+A)=%.4f <= Dpp(S-only)=%.4f at warmth=+1",
                        dpp_sa_p1, dpp_sonly_p1))
      }
    }

    # Build key_estimands table for Table 3
    extract_est <- function(wz, grp) {
      row <- h4a_all %>% filter(warmth_z_val == wz, group == grp)
      if (nrow(row) == 0) return(list(est = NA, se = NA, lo = NA, hi = NA))
      list(est = row$estimate[1], se = row$std.error[1],
           lo = row$conf.low[1], hi = row$conf.high[1])
    }

    # P(category) at warmth levels via predictions
    # Use the same marginaleffects::datagrid defaults as comparisons() so that
    # Table 3 remains internally consistent: Dpp == P(blame=1) - P(blame=0).
    pred_est <- list()
    table3_grid <- datagrid(warmth_z = c(-1, 1), blame_china = c(0, 1), model = mpool)
    table3_preds <- safe_mnl_predictions(mpool, table3_grid, "Table3")
    for (wz in c(-1, 1)) {
      for (bl in c(0, 1)) {
        p_row <- table3_preds %>%
          filter(abs(warmth_z - wz) < 1e-8, blame_china == bl)
        for (g in c("S+A", "Sanctions-only")) {
          sub <- p_row %>% filter(group == g)
          if (nrow(sub) == 1) {
            key <- sprintf("P_%s_w%+d_b%d",
                           gsub("[^A-Za-z]", "", g), wz, bl)
            pred_est[[key]] <- list(est = sub$estimate, se = sub$std.error,
                                   lo = sub$conf.low, hi = sub$conf.high)
          }
        }
      }
    }

    # Dpp estimates
    dpp_entries <- list()
    for (wz in c(-1, 1)) {
      sa_row <- extract_est(wz, "S+A")
      so_row <- extract_est(wz, "Sanctions-only")
      dpp_entries[[sprintf("Dpp_SA_w%+d", wz)]] <- sa_row
      dpp_entries[[sprintf("Dpp_Sonly_w%+d", wz)]] <- so_row
    }

    all_est <- c(pred_est, dpp_entries)
    key_estimands <- tibble(
      estimand = names(all_est),
      estimate = sapply(all_est, function(x) x$est),
      std.error = sapply(all_est, function(x) x$se),
      conf.low = sapply(all_est, function(x) x$lo),
      conf.high = sapply(all_est, function(x) x$hi)
    )
  }

  # H4b: Slopes of warmth conditional on blame=1
  log_msg("")
  log_msg("  H4b: Slopes of warmth on P(category) | blame=1:")
  h4b_slopes <- tryCatch(
    slopes(mpool, variables = "warmth_z",
           newdata = datagrid(blame_china = 1, model = mpool),
           type = "probs"),
    error = function(e) {
      log_msg(sprintf("  slopes() failed: %s", e$message))
      NULL
    }
  )

  if (!is.null(h4b_slopes)) {
    h4b_df <- as.data.frame(h4b_slopes)
    for (g in unique(h4b_df$group)) {
      row <- h4b_df %>% filter(group == g)
      if (nrow(row) == 1) {
        ci_lo <- if (!is.na(row$conf.low)) sprintf("%.4f", row$conf.low) else "NA"
        ci_hi <- if (!is.na(row$conf.high)) sprintf("%.4f", row$conf.high) else "NA"
        log_msg(sprintf("    %s: slope=%.4f [%s, %s]",
                        g, row$estimate, ci_lo, ci_hi))
      }
    }

    # H4b direction check
    sa_row <- h4b_df %>% filter(group == "S+A") %>% slice(1)
    so_row <- h4b_df %>% filter(group == "Sanctions-only") %>% slice(1)
    slope_sa <- sa_row$estimate %>% first()
    slope_so <- so_row$estimate %>% first()
    sa_lo <- sa_row$conf.low %>% first()
    sa_hi <- sa_row$conf.high %>% first()
    so_lo <- so_row$conf.low %>% first()
    so_hi <- so_row$conf.high %>% first()
    if (!is.na(slope_sa) && !is.na(slope_so)) {
      if (slope_sa > 0 && slope_so < 0) {
        msg <- if (!is.na(sa_lo) && !is.na(so_hi) && sa_lo > 0 && so_hi < 0) {
          "EXPECTED (signs and CIs support directional split)"
        } else {
          "MIXED (signs align, but precision is limited)"
        }
        log_msg(sprintf(
          "  H4b DIRECTION: slope(S+A)=%.4f, slope(S-only)=%.4f | blame=1 -> %s",
          slope_sa, slope_so, msg
        ))
      } else if (slope_sa > slope_so) {
        log_msg(sprintf(
          "  H4b MIXED: slope(S+A)=%.4f > slope(S-only)=%.4f, but signs do not show the expected S+A increase / S-only decrease split",
          slope_sa, slope_so
        ))
      } else {
        log_msg(sprintf("  FLAG: H4b direction unexpected: slope(S+A)=%.4f <= slope(S-only)=%.4f",
                        slope_sa, slope_so))
      }
    }
  }
} else {
  log_msg("  SKIPPED: pooled model not available")
}


# --- Section 8: Table 3 — Estimands Table (Fix 4) ---------------------------

log_msg("")
log_msg("--- Section 8: Table 3 (Estimands + Appendix Coefficients) ---")

# Main text: estimands table
if (!is.null(key_estimands)) {
  # Format for display
  est_display <- key_estimands %>%
    mutate(
      estimate = sprintf("%.4f", estimate),
      std.error = ifelse(is.na(std.error), "—", sprintf("%.4f", std.error)),
      CI = ifelse(is.na(conf.low), "—",
                  sprintf("[%.4f, %.4f]", conf.low, conf.high))
    ) %>%
    select(Estimand = estimand, Estimate = estimate, SE = std.error, `95% CI` = CI)

  save_table(est_display, "tab3_phase4_estimands",
             caption = "Phase 4 Key Estimands: Predicted Probabilities and Blame Effects")
} else {
  log_msg("  WARNING: key_estimands not available; skipping Table 3")
}

# Appendix: coefficient table
if (!is.null(mpool)) {
  mod_list <- list()
  mod_names <- character()

  mod_list[[1]] <- mpool
  mod_names[1] <- "Pooled"

  if (!is.null(m24)) {
    mod_list[[length(mod_list) + 1]] <- m24
    mod_names <- c(mod_names, "2024")
  }
  if (!is.null(m25)) {
    mod_list[[length(mod_list) + 1]] <- m25
    mod_names <- c(mod_names, "2025")
  }

  tex_path <- here("output", "tables", "app_tab_phase4_mnl_coefficients.tex")
  tryCatch(
    texreg(mod_list,
           custom.model.names = mod_names,
           caption = "Multinomial Logit Coefficients: Tool Configuration (Appendix)",
           label = "tab:phase4_mnl_coefs",
           float.pos = "htbp",
           use.packages = FALSE,
           file = tex_path),
    error = function(e) log_msg(sprintf("  texreg failed: %s", e$message))
  )
  log_msg("  Saved: app_tab_phase4_mnl_coefficients.tex")

  # CSV via broom::tidy
  csv_rows <- list()
  for (i in seq_along(mod_list)) {
    tidy_df <- tryCatch(broom::tidy(mod_list[[i]], conf.int = TRUE),
                        error = function(e) NULL)
    if (!is.null(tidy_df)) {
      tidy_df$model <- mod_names[i]
      csv_rows[[i]] <- tidy_df
    }
  }
  if (length(csv_rows) > 0) {
    csv_out <- bind_rows(csv_rows)
    write_csv(csv_out, here("output", "tables",
                            "app_tab_phase4_mnl_coefficients.csv"))
    log_msg("  Saved: app_tab_phase4_mnl_coefficients.csv")
  }
}


# --- Section 9: Acquiescence Robustness (DA#4, Fix 1) -----------------------

log_msg("")
log_msg("--- Section 9: Acquiescence Robustness ---")

acq_results <- list()
acq_baseline_wp1 <- NA_real_
acq_ctrl_wp1 <- NA_real_

# Baseline Dpp (from key_estimands, already computed)
if (!is.null(key_estimands)) {
  for (wz in c(-1, 1)) {
    tag <- sprintf("Dpp_SA_w%+d", wz)
    row <- key_estimands %>% filter(estimand == tag)
    if (nrow(row) == 1) {
      acq_results[[length(acq_results) + 1]] <- tibble(
        variant = "Baseline",
        warmth_z = wz,
        Dpp_SA = row$estimate
      )
    }
  }
}

# Variant 1: + n_approaches_main + n_approaches_miss
mpool_acq1 <- run_multinomial(pooled, ctrl_rhs_acq1, include_year_fe = TRUE)
if (!is.null(mpool_acq1)) {
  log_msg("  mpool_acq1 (+ n_approaches_main + miss): OK")
  for (wz in c(-1, 1)) {
    comps_acq1 <- tryCatch(
      comparisons(mpool_acq1, variables = "blame_china",
                  newdata = datagrid(warmth_z = wz, model = mpool_acq1),
                  type = "probs"),
      error = function(e) NULL
    )
    if (!is.null(comps_acq1)) {
      sa_row <- as.data.frame(comps_acq1) %>% filter(group == "S+A")
      if (nrow(sa_row) == 1) {
        acq_results[[length(acq_results) + 1]] <- tibble(
          variant = "+ main + miss",
          warmth_z = wz,
          Dpp_SA = sa_row$estimate
        )
      }
    }
  }
} else {
  log_msg("  mpool_acq1: FAILED")
}

# Variant 2: + n_approaches_zero
mpool_acq2 <- run_multinomial(pooled, ctrl_rhs_acq2, include_year_fe = TRUE)
if (!is.null(mpool_acq2)) {
  log_msg("  mpool_acq2 (+ n_approaches_zero): OK")
  for (wz in c(-1, 1)) {
    comps_acq2 <- tryCatch(
      comparisons(mpool_acq2, variables = "blame_china",
                  newdata = datagrid(warmth_z = wz, model = mpool_acq2),
                  type = "probs"),
      error = function(e) NULL
    )
    if (!is.null(comps_acq2)) {
      sa_row <- as.data.frame(comps_acq2) %>% filter(group == "S+A")
      if (nrow(sa_row) == 1) {
        acq_results[[length(acq_results) + 1]] <- tibble(
          variant = "+ zero-imputed",
          warmth_z = wz,
          Dpp_SA = sa_row$estimate
        )
      }
    }
  }
} else {
  log_msg("  mpool_acq2: FAILED")
}

# Build comparison table
if (length(acq_results) > 0) {
  acq_df <- bind_rows(acq_results)
  acq_wide <- acq_df %>%
    mutate(warmth_label = sprintf("Dpp_SA(w=%+d)", warmth_z)) %>%
    select(variant, warmth_label, Dpp_SA) %>%
    pivot_wider(names_from = warmth_label, values_from = Dpp_SA)

  log_msg("  Acquiescence comparison:")
  for (i in seq_len(nrow(acq_wide))) {
    log_msg(sprintf("    %s: %s",
                    acq_wide$variant[i],
                    paste(names(acq_wide)[-1], "=",
                          sapply(acq_wide[i, -1], function(x) sprintf("%.4f", x)),
                          collapse = ", ")))
  }

  save_table(acq_wide, "app_tab_phase4_acquiescence",
             caption = "Acquiescence Robustness: Blame Effect on P(S+A) With/Without Endorsement Controls")

  # Data-driven acquiescence conclusion
  acq_baseline_wp1 <- acq_df %>% filter(variant == "Baseline", warmth_z == 1) %>% pull(Dpp_SA) %>% first()
  acq_ctrl_wp1     <- acq_df %>% filter(variant == "+ main + miss", warmth_z == 1) %>% pull(Dpp_SA) %>% first()
  if (is.null(acq_ctrl_wp1) || is.na(acq_ctrl_wp1)) {
    acq_ctrl_wp1 <- acq_df %>% filter(variant == "+ zero-imputed", warmth_z == 1) %>% pull(Dpp_SA) %>% first()
  }
  acq_pct_change <- NA_real_
  if (!is.na(acq_baseline_wp1) && !is.na(acq_ctrl_wp1) && abs(acq_baseline_wp1) > 0.001) {
    acq_pct_change <- 100 * (1 - acq_ctrl_wp1 / acq_baseline_wp1)
    log_msg(sprintf(
      paste0("  Acquiescence summary: Dpp_SA(w=+1) baseline=%.4f -> controlled=%.4f ",
             "(attenuation=%.0f%%)."),
      acq_baseline_wp1, acq_ctrl_wp1, acq_pct_change
    ))
    log_msg("  Interpretation: attenuation is material; report as sensitivity evidence (no binary PASS/FAIL threshold).")
  } else {
    log_msg("  Acquiescence summary: could not compute baseline vs controlled comparison.")
  }
} else {
  acq_pct_change <- NA_real_
  log_msg("  WARNING: Acquiescence comparison not available")
}



# --- Section 10: Placebo Pairing (Appendix) -----------------------------------

dpp_placebo_pair <- NA_real_
dpp_primary_pair <- NA_real_
dpp_primary_conditional <- NA_real_
dpp_placebo_conditional <- NA_real_
dpp_raw_gap <- NA_real_
dpp_conditional_gap <- NA_real_

log_msg("")
log_msg("--- Section 10: Placebo Pairing (Sanctions x IntlCooperation) [Appendix] ---")

fmla_placebo <- as.formula(
  paste0("placebo_tool_config ~ warmth_z * blame_china + ", ctrl_rhs, " + factor(year)")
)
placebo_vars <- all.vars(fmla_placebo)
pooled_placebo_complete <- pooled[complete.cases(pooled[, placebo_vars, drop = FALSE]), ]

mpool_placebo <- tryCatch(
  multinom(fmla_placebo, data = pooled_placebo_complete, weights = weight,
           trace = FALSE, Hess = TRUE, maxit = 300),
  error = function(e) {
    log_msg(sprintf("  Placebo multinom failed: %s", e$message))
    NULL
  }
)

if (!is.null(mpool_placebo)) {
  if (!mpool_placebo$convergence %in% c(0L, 0)) {
    log_msg("  WARNING: mpool_placebo may not have converged")
  }
  log_msg(sprintf("  Placebo model: N=%d, converged=%s",
                  nrow(pooled_placebo_complete),
                  ifelse(mpool_placebo$convergence %in% c(0L, 0), "YES", "NO")))

  # Extract Dpp for P(Sanctions+IntlCoop) at warmth=+1
  placebo_comps <- tryCatch(
    comparisons(mpool_placebo, variables = "blame_china",
                newdata = datagrid(warmth_z = 1, model = mpool_placebo),
                type = "probs"),
    error = function(e) NULL
  )

  if (!is.null(placebo_comps)) {
    pc_df <- as.data.frame(placebo_comps)
    for (g in unique(pc_df$group)) {
      row <- pc_df %>% filter(group == g)
      if (nrow(row) == 1) {
        ci_lo <- if (!is.na(row$conf.low)) sprintf("%.4f", row$conf.low) else "NA"
        ci_hi <- if (!is.na(row$conf.high)) sprintf("%.4f", row$conf.high) else "NA"
        log_msg(sprintf("    %s: Dpp=%.4f [%s, %s]", g, row$estimate, ci_lo, ci_hi))
      }
    }

    # Compare to primary: Dpp(S+A) at warmth=+1
    dpp_placebo_pair <- pc_df %>%
      filter(group == "Sanctions+IntlCoop") %>%
      pull(estimate) %>% first()
    dpp_primary_pair <- if (!is.null(key_estimands)) {
      key_estimands %>% filter(estimand == "Dpp_SA_w+1") %>%
        pull(estimate) %>% first()
    } else NA

    if (!is.na(dpp_placebo_pair) && !is.na(dpp_primary_pair)) {
      log_msg(sprintf("  Raw joint-category comparison (descriptive): Dpp(S+A primary)=%.4f vs Dpp(S+IntlCoop placebo)=%.4f",
                      dpp_primary_pair, dpp_placebo_pair))
      log_msg("  NOTE: Raw joint comparison shares the sanctions dimension across primary/placebo and is descriptive only.")
      dpp_raw_gap <- dpp_primary_pair - dpp_placebo_pair
      log_msg(sprintf("  Raw joint-category gap (primary - placebo): %.4f", dpp_raw_gap))
    }

    # Main specificity test: compare pairing composition within the sanctions margin
    # using predicted probabilities (avoids over-interpreting a shared sanctions shift).
    placebo_cond_grid <- datagrid(warmth_z = 1, blame_china = c(0, 1), model = mpool_placebo)
    placebo_cond_preds <- safe_mnl_predictions(mpool_placebo, placebo_cond_grid, "Placebo-CondSpec")

    if (!is.null(key_estimands) && nrow(placebo_cond_preds) > 0) {
      get_ke <- function(tag) {
        key_estimands %>% filter(estimand == tag) %>% pull(estimate) %>% first()
      }

      p_sa_b0 <- get_ke("P_SA_w+1_b0")
      p_sa_b1 <- get_ke("P_SA_w+1_b1")
      p_so_b0 <- get_ke("P_Sanctionsonly_w+1_b0")
      p_so_b1 <- get_ke("P_Sanctionsonly_w+1_b1")

      p_pl_pair_b0 <- placebo_cond_preds %>%
        filter(group == "Sanctions+IntlCoop", blame_china == 0) %>%
        pull(estimate) %>% first()
      p_pl_pair_b1 <- placebo_cond_preds %>%
        filter(group == "Sanctions+IntlCoop", blame_china == 1) %>%
        pull(estimate) %>% first()
      p_pl_so_b0 <- placebo_cond_preds %>%
        filter(group == "Sanctions-only", blame_china == 0) %>%
        pull(estimate) %>% first()
      p_pl_so_b1 <- placebo_cond_preds %>%
        filter(group == "Sanctions-only", blame_china == 1) %>%
        pull(estimate) %>% first()

      denom_primary_b0 <- p_sa_b0 + p_so_b0
      denom_primary_b1 <- p_sa_b1 + p_so_b1
      denom_placebo_b0 <- p_pl_pair_b0 + p_pl_so_b0
      denom_placebo_b1 <- p_pl_pair_b1 + p_pl_so_b1

      vals_needed <- c(p_sa_b0, p_sa_b1, p_so_b0, p_so_b1,
                       p_pl_pair_b0, p_pl_pair_b1, p_pl_so_b0, p_pl_so_b1,
                       denom_primary_b0, denom_primary_b1,
                       denom_placebo_b0, denom_placebo_b1)

      if (all(!is.na(vals_needed)) &&
          all(c(denom_primary_b0, denom_primary_b1,
                denom_placebo_b0, denom_placebo_b1) > 0)) {
        primary_cond_b0 <- p_sa_b0 / denom_primary_b0
        primary_cond_b1 <- p_sa_b1 / denom_primary_b1
        placebo_cond_b0 <- p_pl_pair_b0 / denom_placebo_b0
        placebo_cond_b1 <- p_pl_pair_b1 / denom_placebo_b1

        dpp_primary_conditional <- primary_cond_b1 - primary_cond_b0
        dpp_placebo_conditional <- placebo_cond_b1 - placebo_cond_b0

        log_msg(sprintf(
          paste0("  Composition specificity check (within sanctions, warmth=+1): ",
                 "Dpp[P(Action|Sanctions)] primary=%.4f vs ",
                 "Dpp[P(IntlCoop|Sanctions)] placebo=%.4f"),
          dpp_primary_conditional, dpp_placebo_conditional
        ))

        dpp_conditional_gap <- dpp_primary_conditional - dpp_placebo_conditional
        log_msg(sprintf("  Conditional specificity gap (primary - placebo): %.4f", dpp_conditional_gap))
        log_msg("  Interpretation: report as comparative magnitude (no arbitrary PASS/FAIL threshold).")
      } else {
        dpp_conditional_gap <- NA_real_
        log_msg("  Placebo specificity (within sanctions): could not compute due to missing/degenerate predicted probabilities")
      }
    } else {
      dpp_conditional_gap <- NA_real_
      log_msg("  Placebo specificity (within sanctions): could not compute (missing key_estimands or placebo predictions)")
    }
  }

  # Placebo figure
  grid_placebo <- build_mnl_grid(pooled, include_year = TRUE)
  preds_placebo <- safe_mnl_predictions(mpool_placebo, grid_placebo, "Placebo")

  if (nrow(preds_placebo) > 0) {
    placebo_colors <- c(
      "Neither"            = neutral_gray,
      "Sanctions-only"     = pax_highlight,
      "IntlCoop-only"      = pax_accent,
      "Sanctions+IntlCoop" = positive_green
    )

    p_placebo <- ggplot(preds_placebo %>%
                          mutate(blame_label = ifelse(blame_china == 1,
                                                     "Blame = 1", "Blame = 0")),
                        aes(x = warmth_z, y = estimate,
                            color = group, fill = group)) +
      geom_line(linewidth = 1) +
      facet_wrap(~ blame_label) +
      scale_color_manual(values = placebo_colors, name = "Tool Config (Placebo)") +
      scale_fill_manual(values = placebo_colors, name = "Tool Config (Placebo)") +
      scale_y_continuous(labels = percent_format(), limits = c(0, NA)) +
      labs(
        title = "Placebo: Sanctions x Intl Cooperation (Appendix)",
        subtitle = "Multinomial logit; pooled + year FE; controls at medians/modes",
        x = "Warmth toward China (z-score)",
        y = "Predicted Probability"
      ) +
      theme_pax()

    has_ci_placebo <- any(!is.na(preds_placebo$conf.low))
    if (has_ci_placebo) {
      p_placebo <- p_placebo +
        geom_ribbon(aes(ymin = conf.low, ymax = conf.high),
                    alpha = 0.12, color = NA)
    }

    save_figure(p_placebo, "app_fig_placebo_sanctionsXintlcoop", w = 6.5, h = 4.5)
  }

  # Placebo coefficient table
  tryCatch({
    tex_pl <- here("output", "tables", "app_tab_placebo_models.tex")
    texreg(list(mpool_placebo),
           custom.model.names = "Placebo (Pooled)",
           caption = "Placebo Pairing: Sanctions x International Cooperation (Appendix)",
           label = "tab:phase4_placebo",
           float.pos = "htbp",
           use.packages = FALSE,
           file = tex_pl)
    log_msg("  Saved: app_tab_placebo_models.tex")

    tidy_pl <- broom::tidy(mpool_placebo, conf.int = TRUE) %>%
      mutate(model = "Placebo (Pooled)")
    write_csv(tidy_pl, here("output", "tables", "app_tab_placebo_models.csv"))
    log_msg("  Saved: app_tab_placebo_models.csv")
  }, error = function(e) log_msg(sprintf("  Placebo table export failed: %s", e$message)))

} else {
  log_msg("  Placebo model FAILED; skipping figure and table")
}


# --- Section 11: Interpretation -----------------------------------------------

log_msg("")
log_msg("--- Section 11: Interpretation ---")

# Build interpretation block
interp <- character()
interp <- c(interp,
  "=== PHASE 4 RESULTS INTERPRETATION ===",
  "",
  "Framing: Complementary descriptive analysis of tool-configuration preferences.",
  "Phase 3 establishes the continuous posture result (warmth x blame -> posture_z).",
  "Phase 4 asks a follow-up: among the discrete tool pairings respondents endorse,",
  "does blame shift the MIX of tools, conditional on the same posture level?",
  "",
  "Key design choices:",
  "  - posture_z included as a control to hold constant the cooperative orientation",
  "    established in Phase 3, isolating tool-composition effects.",
  "    NOTE: posture_z is a post-treatment variable (blame affects posture via Phase 3).",
  "    The total effect of blame on tool composition (without posture control) should",
  "    also be reported for comparison. This model estimates a conditional association,",
  "    not a causal effect.",
  "  - DV is 4-category tool_config (Neither / Sanctions-only / Action-only / S+A),",
  "    a categorical decomposition of the posture index's components.",
  "  - SEs are model-based (Hessian from nnet::multinom), not design-based.",
  ""
)

# Numeric anchors
if (!is.null(key_estimands)) {
  interp <- c(interp, "Key estimands (Dpp = P(cat|blame=1) - P(cat|blame=0)):")
  for (tag in c("Dpp_SA_w-1", "Dpp_SA_w+1", "Dpp_Sonly_w+1")) {
    row <- key_estimands %>% filter(estimand == tag)
    if (nrow(row) == 1) {
      interp <- c(interp, sprintf("  %s = %.4f (SE=%.4f)", tag, row$estimate, row$std.error))
    }
  }
  interp <- c(interp, "")
}

# ROBUSTNESS ASSESSMENT (foregrounded per reviewer recommendation)
interp <- c(interp, "--- ROBUSTNESS ASSESSMENT ---", "")

# Acquiescence summary (full results in appendix)
if (!is.na(acq_pct_change) && !is.na(acq_baseline_wp1) && !is.na(acq_ctrl_wp1)) {
  interp <- c(interp,
    sprintf(
      paste0("Acquiescence summary (appendix): Dpp_SA(w=+1) moves from %.3f to %.3f ",
             "(attenuation %.0f%% after adding endorsement controls)."),
      acq_baseline_wp1, acq_ctrl_wp1, acq_pct_change
    ),
    "Interpretation: attenuation is material; treat this as sensitivity evidence rather than a binary robustness verdict.")
} else {
  interp <- c(interp, "Acquiescence summary: NOT EVALUATED.")
}

# Placebo summary (full results in appendix)
if (!is.null(mpool_placebo) &&
    !is.na(dpp_primary_conditional) &&
    !is.na(dpp_placebo_conditional)) {
  interp <- c(interp,
    sprintf(
      paste0("Placebo summary (appendix): within-sanctions Dpp is %.3f for primary ",
             "Action|Sanctions and %.3f for placebo IntlCoop|Sanctions; gap = %.3f."),
      dpp_primary_conditional, dpp_placebo_conditional, dpp_conditional_gap
    ),
    "Interpretation: report as comparative magnitude; no arbitrary threshold is used for a pass/fail specificity claim.")
} else {
  interp <- c(interp, "Placebo summary: NOT EVALUATED.")
}

interp <- c(interp, "")

# Overall descriptive synthesis
interp <- c(interp,
  "OVERALL: Baseline estimates indicate a sizable S+A shift under blame,",
  "  but robustness checks show meaningful attenuation and a non-trivial placebo-pairing shift.",
  "  Taken together, Phase 4 evidence is suggestive and should be interpreted cautiously.",
  "")

# Phase 3 -> Phase 4 bridge (after robustness, so reader has context)
interp <- c(interp,
  "--- THEORETICAL INTERPRETATION (conditional on robustness caveats above) ---",
  "",
  "Phase 3 -> Phase 4 bridge:",
  "  Phase 3: Warmth predicts dovish posture; blame amplifies this.",
  "  Phase 4: Conditional on posture_z, blame shifts the tool mix toward S+A",
  "  (leveraged cooperation), not toward Sanctions-only (punishment).",
  "  This is a within-posture composition effect, not a net posture shift.",
  ""
)

for (line in interp) log_msg(paste("  ", line))


# --- Section 12: Sanity Checks ------------------------------------------------

log_msg("")
log_msg("--- Section 12: Sanity Checks ---")

# Weighted crosstab
log_msg("  Weighted P(tool_config) by blame x warmth tercile (pooled):")
pooled_check <- pooled %>%
  filter(!is.na(warmth_z) & !is.na(blame_china) & !is.na(tool_config)) %>%
  mutate(warmth_tercile = cut(warmth_z, breaks = c(-Inf, -0.5, 0.5, Inf),
                               labels = c("Low", "Mid", "High")))

xt_check <- pooled_check %>%
  group_by(warmth_tercile, blame_china, tool_config) %>%
  summarise(wt_n = sum(weight, na.rm = TRUE), .groups = "drop") %>%
  group_by(warmth_tercile, blame_china) %>%
  mutate(pct = 100 * wt_n / sum(wt_n)) %>%
  ungroup()

for (wt in c("Low", "Mid", "High")) {
  for (bl in c(0, 1)) {
    sub <- xt_check %>% filter(warmth_tercile == wt, blame_china == bl)
    vals <- paste(sprintf("%s:%.0f%%", sub$tool_config, sub$pct), collapse = ", ")
    log_msg(sprintf("    warmth=%s, blame=%d: %s", wt, bl, vals))
  }
}

# Convergence summary
log_msg("")
log_msg("  Model convergence summary:")
for (mn in c("mpool", "m24", "m25")) {
  mod <- get(mn)
  if (is.null(mod)) {
    log_msg(sprintf("    %s: NULL (not fitted)", mn))
  } else {
    log_msg(sprintf("    %s: converged=%s", mn,
                    ifelse(mod$convergence %in% c(0L, 0), "YES", "NO/UNCLEAR")))
  }
}


# --- Section 13: Save Objects & Log -------------------------------------------

log_msg("")
log_msg("--- Section 13: Save Objects & Log ---")

phase4_objects <- list(
  mpool = mpool, m24 = m24, m25 = m25,
  mpool_acq1 = mpool_acq1, mpool_acq2 = mpool_acq2,
  mpool_placebo = mpool_placebo,
  fig3_data = fig3_data,
  key_estimands = key_estimands,
  interpretation = interp,
  warmth_moments = c(mu = warmth_mu, sd = warmth_sd),
  posture_moments = c(mu = posture_mu, sd = posture_sd),
  tool_config_dist = table(pooled$tool_config, pooled$year)
)
saveRDS(phase4_objects, here("output", "tables", "phase4_objects.rds"))
log_msg("  Saved: phase4_objects.rds")

# File manifest
output_files <- c(
  "output/tables/tab3_phase4_estimands.tex",
  "output/tables/tab3_phase4_estimands.csv",
  "output/tables/app_tab_phase4_mnl_coefficients.tex",
  "output/tables/app_tab_phase4_mnl_coefficients.csv",
  "output/tables/app_tab_phase4_acquiescence.tex",
  "output/tables/app_tab_phase4_acquiescence.csv",
  "output/figures/fig3_tool_config_warmthXblame.pdf",
  "output/figures/app_fig_placebo_sanctionsXintlcoop.pdf",
  "output/tables/app_tab_placebo_models.tex",
  "output/tables/app_tab_placebo_models.csv",
  "output/tables/phase4_objects.rds",
  "output/tables/phase4_log.txt"
)

log_msg("  File manifest:")
for (f in output_files) {
  exists_flag <- ifelse(file.exists(here(f)), "EXISTS", "MISSING")
  if (grepl("phase4_log", f)) exists_flag <- "PENDING"
  log_msg(sprintf("    %s [%s]", f, exists_flag))
}

# sessionInfo
log_msg("")
log_msg("  sessionInfo():")
si <- capture.output(sessionInfo())
for (line in si) log_msg(paste("    ", line))

log_msg("")
log_msg("========================================================================")
log_msg("PHASE 4 COMPLETE")
log_msg("========================================================================")

writeLines(log_env$entries, here("output", "tables", "phase4_log.txt"))
message(sprintf("Phase 4 log saved to: %s",
                here("output", "tables", "phase4_log.txt")))
