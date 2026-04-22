#' ============================================================================
#' 02_descriptives.R — Weighted Descriptive Statistics
#' Poll 2025: Fentanyl Policy Survey Analysis
#' PAX sapiens Foundation
#'
#' @description Produces weighted descriptive statistics, publication-ready
#'   tables (LaTeX + CSV), and figures (PDF) for the 2024/2025 fentanyl
#'   policy survey. Covers core analysis variables, the full China module,
#'   and demographic subgroup breakdowns with year-over-year comparisons.
#'
#' @input  data/clean/pooled.rds, data/clean/clean_2024.rds,
#'         data/clean/clean_2025.rds
#' @output prism/tables/tab1–tab7 (.tex + .csv), prism/figures/fig1–fig5
#'         (.pdf), prism/tables/descriptives_log.txt
#'
#' @depends here, tidyverse, survey, srvyr, kableExtra, scales
#' ============================================================================

# --- Setup -------------------------------------------------------------------

library(here)
library(tidyverse)
library(survey)
library(srvyr)
library(kableExtra)
library(scales)

set.seed(20250217)

# Ensure output directories exist (for fresh-clone reproducibility)
dir.create(here("prism", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("prism", "figures"), recursive = TRUE, showWarnings = FALSE)

# Environment-based logging (same pattern as 01_clean.R)
log_env <- new.env(parent = emptyenv())
log_env$entries <- character()

#' Log a message to console and internal log
#' @param msg Character string to log
log_msg <- function(msg) {
  log_env$entries <- c(log_env$entries, msg)
  message(msg)
}

log_msg("========================================================================")
log_msg("DESCRIPTIVES LOG: Poll 2025 — Weighted Descriptive Statistics")
log_msg(sprintf("Timestamp: %s", Sys.time()))
log_msg("========================================================================")

# PAX sapiens color palette (provisional)
pax_dark       <- "#1a1a2e"
pax_blue       <- "#16213e"
pax_accent     <- "#0f3460"
pax_highlight  <- "#e94560"
neutral_gray   <- "#6c757d"
positive_green <- "#198754"
negative_red   <- "#dc3545"

year_colors <- c("2024" = pax_accent, "2025" = pax_highlight)

#' Custom ggplot2 theme for PAX sapiens publications
#' @param base_size Base font size (default 14)
theme_pax <- function(base_size = 14) {
  theme_minimal(base_size = base_size, base_family = "serif") +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold")
    )
}

# Load cleaned data
log_msg("")
log_msg("--- Loading Data ---")
pooled    <- readRDS(here("data", "clean", "pooled.rds"))
clean_2024 <- readRDS(here("data", "clean", "clean_2024.rds"))
clean_2025 <- readRDS(here("data", "clean", "clean_2025.rds"))
log_msg(sprintf("  Pooled: %d rows, 2024: %d rows, 2025: %d rows",
                nrow(pooled), nrow(clean_2024), nrow(clean_2025)))


# --- Step 1b: Value-Audit Checks --------------------------------------------

log_msg("")
log_msg("--- Value-Audit Checks ---")

#' Audit binary battery items for expected coding
#' @param df Data frame to check
#' @param vars Character vector of variable names
#' @param expected Numeric vector of expected values (default c(1, 2))
#' @param label Description for logging
audit_binary <- function(df, vars, expected = c(1, 2), label = "battery") {
  for (v in vars) {
    if (!v %in% names(df)) {
      log_msg(sprintf("  AUDIT SKIP: %s not in data", v))
      next
    }
    vals <- sort(unique(na.omit(as.numeric(df[[v]]))))
    if (length(vals) == 0) {
      log_msg(sprintf("  AUDIT WARN: %s is all NA", v))
      next
    }
    if (!all(vals %in% expected)) {
      stop(sprintf("VALUE AUDIT FAIL [%s]: %s has values {%s}, expected subset of {%s}",
                   label, v, paste(vals, collapse = ","),
                   paste(expected, collapse = ",")))
    }
  }
  log_msg(sprintf("  AUDIT PASS [%s]: %d items, all values in {%s}",
                  label, length(vars), paste(expected, collapse = ",")))
}

#' Audit a continuous/ordinal variable for expected range
#' @param df Data frame
#' @param var Variable name
#' @param lo Expected minimum
#' @param hi Expected maximum
#' @param label Description
audit_range <- function(df, var, lo, hi, label = var) {
  if (!var %in% names(df)) {
    log_msg(sprintf("  AUDIT SKIP: %s not in data", var))
    return(invisible(NULL))
  }
  vals <- na.omit(as.numeric(df[[var]]))
  actual_lo <- min(vals)
  actual_hi <- max(vals)
  if (actual_lo < lo || actual_hi > hi) {
    stop(sprintf("VALUE AUDIT FAIL [%s]: range [%s, %s], expected [%s, %s]",
                 label, actual_lo, actual_hi, lo, hi))
  }
  log_msg(sprintf("  AUDIT PASS [%s]: range [%s, %s] within [%s, %s]",
                  label, actual_lo, actual_hi, lo, hi))
}

# Responsibility battery (q7_1–q7_8): expect {1, 2}
resp_vars <- c("responsible_drug_users", "responsible_doctors",
               "responsible_pharma", "responsible_cartels",
               "responsible_state_local", "responsible_federal",
               "responsible_china", "responsible_mexico")
audit_binary(pooled, resp_vars, expected = c(1, 2), label = "responsibility q7")

# Approaches battery (q10_1–q10_12): expect {1, 2}
approach_vars <- c("approach_intl_cooperation", "approach_enforce_sales",
                   "approach_enforce_users", "approach_legalization",
                   "approach_health_justice", "approach_addiction_resources",
                   "approach_blocking_drugs", "approach_social_cultural",
                   "approach_sanctions", "approach_china_action",
                   "approach_mexico_action", "approach_penalties")
audit_binary(pooled, approach_vars, expected = c(1, 2), label = "approaches q10")

# Most responsible (q8): expect 1–8
audit_range(pooled, "most_responsible", 1, 8, label = "most_responsible q8")

# Warmth: expect 1–7
audit_range(pooled, "warmth", 1, 7, label = "warmth")

# China module items
audit_range(pooled, "china_role", 1, 7, label = "china_role")

china_behavior_vars <- paste0("china_behavior_", letters[1:4])
china_us_behavior_vars <- paste0("us_behavior_", letters[1:4])
china_us_preferred_vars <- paste0("us_preferred_", letters[1:4])

# China behavior/preference items: all 7-point Likert scales per codebook
for (v in c(china_behavior_vars, china_us_behavior_vars, china_us_preferred_vars)) {
  if (v %in% names(pooled)) {
    audit_range(pooled, v, 1, 7, label = v)
  }
}

# Demographics for derivation
# ideo5: 1=Very liberal, 2=Liberal, 3=Moderate, 4=Conservative,
# 5=Very conservative, 6=Not sure
audit_range(pooled, "ideo5", 1, 6, label = "ideo5")
audit_range(pooled, "faminc_new", 1, 97, label = "faminc_new")  # CES codes up to 97

# Audit blame_china (created in 01_clean.R as 0/1 binary)
audit_binary(pooled, "blame_china", expected = c(0, 1), label = "blame_china")

# Audit binary demographics (must be 0/1 for svymean to return proportions)
stopifnot(
  "educ_college must be 0/1" = all(na.omit(pooled$educ_college) %in% c(0, 1)),
  "female must be 0/1"       = all(na.omit(pooled$female) %in% c(0, 1))
)
log_msg("  AUDIT PASS [binary demographics]: educ_college and female are 0/1")

log_msg("  All value audits passed.")


# --- Step 2: Derive Additional Subgroup Variables ----------------------------

log_msg("")
log_msg("--- Deriving Additional Subgroup Variables ---")

#' Derive ideo3 and income_cat for demographic subgroup analysis
#' @param df Data frame with ideo5 and faminc_new columns
#' @return Data frame with ideo3 and income_cat appended
derive_subgroups <- function(df) {
  df %>%
    mutate(
      # Ideology: 3-category
      # ideo5: 1=Very liberal, 2=Liberal, 3=Moderate, 4=Conservative,
      #        5=Very conservative, 6=Not sure → NA
      ideo3 = case_when(
        as.numeric(ideo5) %in% c(1, 2) ~ "Liberal",
        as.numeric(ideo5) == 3          ~ "Moderate",
        as.numeric(ideo5) %in% c(4, 5) ~ "Conservative",
        as.numeric(ideo5) == 6          ~ NA_character_,
        TRUE ~ NA_character_
      ) %>% factor(levels = c("Liberal", "Moderate", "Conservative")),

      # Income: 3-category from CES faminc_new
      # 1=<$10k, 2=$10-20k, 3=$20-30k, 4=$30-40k, 5=$40-50k,
      # 6=$50-60k, 7=$60-70k, 8=$70-80k, 9=$80-100k,
      # 10=$100-120k, 11=$120-150k, 12=$150-200k, 13=$200-250k,
      # 14=$250-350k, 15=$350-500k, 16=$500k+, 97=Prefer not to say
      income_cat = case_when(
        as.numeric(faminc_new) %in% 1:5   ~ "Under $50k",
        as.numeric(faminc_new) %in% 6:9   ~ "$50k-$100k",
        as.numeric(faminc_new) %in% 10:16 ~ "Over $100k",
        TRUE ~ NA_character_
      ) %>% factor(levels = c("Under $50k", "$50k-$100k", "Over $100k")),

      # Warmth tercile for Figure 3
      warmth_tercile = case_when(
        warmth %in% 1:2 ~ "Low (1-2)",
        warmth %in% 3:4 ~ "Medium (3-4)",
        warmth %in% 5:7 ~ "High (5-7)",
        TRUE ~ NA_character_
      ) %>% factor(levels = c("Low (1-2)", "Medium (3-4)", "High (5-7)"))
    )
}

pooled     <- derive_subgroups(pooled)
clean_2024 <- derive_subgroups(clean_2024)
clean_2025 <- derive_subgroups(clean_2025)

# Log derivation counts
for (v in c("ideo3", "income_cat", "warmth_tercile")) {
  n_na <- sum(is.na(pooled[[v]]))
  log_msg(sprintf("  %s: %d valid, %d NA (%.1f%%)",
                  v, nrow(pooled) - n_na, n_na, 100 * n_na / nrow(pooled)))
}


# --- Step 3: Survey Design Objects -------------------------------------------

log_msg("")
log_msg("--- Creating Survey Design Objects ---")

des_pooled <- svydesign(ids = ~1, weights = ~weight, data = pooled)
des_2024   <- svydesign(ids = ~1, weights = ~weight, data = clean_2024)
des_2025   <- svydesign(ids = ~1, weights = ~weight, data = clean_2025)

log_msg("  Survey designs created (ids = ~1, weights = ~weight)")


# --- Step 4: Helper Functions ------------------------------------------------

#' Compute weighted proportions for a factor variable, by year
#' @param des_24 Survey design for 2024
#' @param des_25 Survey design for 2025
#' @param var_name Character name of factor variable
#' @return Tibble with level, prop/CI/N by year
wtd_prop_by_year <- function(des_24, des_25, var_name) {
  get_props <- function(des, year_label) {
    fmla <- as.formula(paste0("~", var_name))
    sv <- svymean(fmla, des, na.rm = TRUE)
    ci <- confint(sv)
    n_tab <- table(des$variables[[var_name]], useNA = "no")
    tibble(
      level     = gsub(paste0("^", var_name), "", names(coef(sv))),
      prop      = as.numeric(coef(sv)),
      ci_lo     = ci[, 1],
      ci_hi     = ci[, 2],
      n_unwtd   = as.integer(n_tab),
      year      = year_label
    )
  }
  bind_rows(get_props(des_24, "2024"), get_props(des_25, "2025"))
}

#' Compute weighted mean + SD for a numeric variable, by year
#' @param des_24 Survey design for 2024
#' @param des_25 Survey design for 2025
#' @param var_name Character name of numeric variable
#' @return Tibble with mean, sd, n by year
wtd_mean_by_year <- function(des_24, des_25, var_name) {
  get_stats <- function(des, year_label) {
    fmla <- as.formula(paste0("~", var_name))
    sv_mean <- svymean(fmla, des, na.rm = TRUE)
    sv_var  <- svyvar(fmla, des, na.rm = TRUE)
    n_valid <- sum(!is.na(des$variables[[var_name]]))
    tibble(
      variable = var_name,
      mean     = as.numeric(coef(sv_mean)),
      se       = as.numeric(SE(sv_mean)),
      sd       = sqrt(as.numeric(coef(sv_var))),
      n_unwtd  = n_valid,
      year     = year_label
    )
  }
  bind_rows(get_stats(des_24, "2024"), get_stats(des_25, "2025"))
}

#' Standardized year-over-year test on stacked (pooled) data
#' @param des_pool Pooled survey design
#' @param outcome_var Character name of 0/1 outcome variable
#' @param binary Logical: TRUE for quasibinomial, FALSE for gaussian
#' @param subset_des Optional pre-subsetted survey design object (default NULL)
#' @return List with diff estimate, SE, and p-value
yoy_test <- function(des_pool, outcome_var, binary = TRUE, subset_des = NULL) {
  des <- if (!is.null(subset_des)) subset_des else des_pool
  fmla <- as.formula(paste0(outcome_var, " ~ factor(year)"))
  fam <- if (binary) quasibinomial() else gaussian()
  fit <- svyglm(fmla, design = des, family = fam)
  coefs <- summary(fit)$coefficients
  # Year coefficient is row 2 (factor(year)2025)
  if (nrow(coefs) < 2) {
    return(list(diff = NA_real_, se = NA_real_, p = NA_real_))
  }
  list(
    diff = coefs[2, "Estimate"],
    se   = coefs[2, "Std. Error"],
    p    = coefs[2, "Pr(>|t|)"]
  )
}

#' Save a table as LaTeX (.tex) and CSV (.csv)
#' @param tbl_df Tibble or data frame
#' @param name File stem (no extension)
#' @param caption LaTeX table caption
save_table <- function(tbl_df, name) {
  # CSV
  csv_path <- here("prism", "tables", paste0(name, ".csv"))
  write_csv(tbl_df, csv_path)

  # LaTeX
  tex_path <- here("prism", "tables", paste0(name, ".tex"))
  latex_out <- tbl_df %>%
    kbl(format = "latex", booktabs = TRUE, caption = NULL, linesep = "") %>%
    kable_styling(latex_options = c("scale_down"))
  writeLines(as.character(latex_out), tex_path)

  log_msg(sprintf("  Saved: %s.tex + %s.csv", name, name))
}

#' Save a ggplot figure as PDF
#' @param plot ggplot object
#' @param name File stem (no extension)
#' @param w Width in inches (default 6.5)
#' @param h Height in inches (default 4.5)
save_figure <- function(plot, name, w = 6.5, h = 4.5) {
  pdf_path <- here("prism", "figures", paste0(name, ".pdf"))
  ggsave(pdf_path, plot = plot, width = w, height = h, dpi = 300,
         bg = "transparent")
  log_msg(sprintf("  Saved: %s.pdf (%s x %s in)", name, w, h))
}

#' Format p-value with significance stars
#' @param p Numeric p-value
#' @return Character string
fmt_p <- function(p) {
  stars <- case_when(
    is.na(p)  ~ "",
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    TRUE      ~ ""
  )
  ifelse(is.na(p), "—",
         paste0(formatC(p, format = "f", digits = 3), stars))
}


# =============================================================================
# TABLES
# =============================================================================

log_msg("")
log_msg("--- Generating Tables ---")

# --- Table 1: Sample Demographics --------------------------------------------

log_msg("")
log_msg("  Table 1: Sample Demographics")

demo_vars <- c("party3", "race_eth", "educ_college", "female",
               "age_group", "ideo3", "income_cat")
demo_labels <- c("Party ID", "Race/Ethnicity", "College Degree", "Female",
                 "Age Group", "Ideology", "Household Income")

tab1_rows <- list()
for (i in seq_along(demo_vars)) {
  v <- demo_vars[i]
  lbl <- demo_labels[i]

  # Handle binary variables differently
  if (v %in% c("educ_college", "female")) {
    get_binary <- function(des, yr) {
      fmla <- as.formula(paste0("~", v))
      sv <- svymean(fmla, des, na.rm = TRUE)
      ci <- confint(sv)
      n_1 <- sum(des$variables[[v]] == 1, na.rm = TRUE)
      n_tot <- sum(!is.na(des$variables[[v]]))
      tibble(
        variable = lbl,
        level = ifelse(v == "female", "Female", "College+"),
        prop = as.numeric(coef(sv)),
        ci_lo = ci[1, 1], ci_hi = ci[1, 2],
        n_unwtd = n_1, n_total = n_tot, year = yr
      )
    }
    tab1_rows[[i]] <- bind_rows(
      get_binary(des_2024, "2024"), get_binary(des_2025, "2025")
    )
  } else {
    tab1_rows[[i]] <- wtd_prop_by_year(des_2024, des_2025, v) %>%
      mutate(variable = lbl)
  }
}

tab1_long <- bind_rows(tab1_rows)

# Pivot to wide for display
tab1_wide <- tab1_long %>%
  mutate(cell = sprintf("%.1f%% (%d)", prop * 100, n_unwtd)) %>%
  select(variable, level, year, cell) %>%
  pivot_wider(names_from = year, values_from = cell, names_prefix = "y") %>%
  rename(Variable = variable, Category = level, `2024` = y2024, `2025` = y2025)

save_table(tab1_wide, "tab1_demographics")


# --- Table 2: Responsibility Battery -----------------------------------------

log_msg("  Table 2: Responsibility Battery (q7)")

resp_labels <- c("Drug Users", "Doctors", "Pharma Companies", "Drug Cartels",
                 "State/Local Gov.", "Federal Gov.", "China", "Mexico")

# Recode 1/2 → 1/0 for each item (audit already confirmed {1,2})
for (v in resp_vars) {
  pooled[[paste0(v, "_bin")]]     <- ifelse(as.numeric(pooled[[v]]) == 1, 1L, 0L)
  clean_2024[[paste0(v, "_bin")]] <- ifelse(as.numeric(clean_2024[[v]]) == 1, 1L, 0L)
  clean_2025[[paste0(v, "_bin")]] <- ifelse(as.numeric(clean_2025[[v]]) == 1, 1L, 0L)
}

# Rebuild designs after adding binary columns
des_pooled <- svydesign(ids = ~1, weights = ~weight, data = pooled)
des_2024   <- svydesign(ids = ~1, weights = ~weight, data = clean_2024)
des_2025   <- svydesign(ids = ~1, weights = ~weight, data = clean_2025)

tab2_rows <- list()
for (i in seq_along(resp_vars)) {
  bin_v <- paste0(resp_vars[i], "_bin")
  stats_24 <- svymean(as.formula(paste0("~", bin_v)), des_2024, na.rm = TRUE)
  ci_24    <- confint(stats_24)
  stats_25 <- svymean(as.formula(paste0("~", bin_v)), des_2025, na.rm = TRUE)
  ci_25    <- confint(stats_25)

  # YoY test on pooled stacked data
  yoy <- yoy_test(des_pooled, bin_v, binary = TRUE)

  tab2_rows[[i]] <- tibble(
    Group       = resp_labels[i],
    `2024 %`    = sprintf("%.1f", as.numeric(coef(stats_24)) * 100),
    `2024 CI`   = sprintf("[%.1f, %.1f]", ci_24[1, 1] * 100, ci_24[1, 2] * 100),
    `2025 %`    = sprintf("%.1f", as.numeric(coef(stats_25)) * 100),
    `2025 CI`   = sprintf("[%.1f, %.1f]", ci_25[1, 1] * 100, ci_25[1, 2] * 100),
    `YoY p`     = fmt_p(yoy$p)
  )
}

tab2 <- bind_rows(tab2_rows) %>%
  mutate(.avg = (as.numeric(`2024 %`) + as.numeric(`2025 %`)) / 2) %>%
  arrange(desc(.avg)) %>%
  select(-.avg)

save_table(tab2, "tab2_responsibility")


# --- Table 3: Most Responsible (q8) ------------------------------------------

log_msg("  Table 3: Most Responsible (q8)")

most_resp_labels <- c("Drug Users", "Doctors", "Pharma Companies", "Drug Cartels",
                      "State/Local Gov.", "Federal Gov.", "China", "Mexico")

tab3_rows <- list()
for (yr_label in c("2024", "2025")) {
  des <- if (yr_label == "2024") des_2024 else des_2025
  d   <- if (yr_label == "2024") clean_2024 else clean_2025

  # Factor with all 8 levels
  des$variables$most_resp_f <- factor(des$variables$most_responsible,
                                       levels = 1:8, labels = most_resp_labels)
  sv <- svymean(~most_resp_f, des, na.rm = TRUE)
  n_tab <- table(d$most_responsible, useNA = "no")

  tab3_rows[[yr_label]] <- tibble(
    Group   = most_resp_labels,
    pct     = sprintf("%.1f", as.numeric(coef(sv)) * 100),
    n_unwtd = as.integer(n_tab[as.character(1:8)]),
    year    = yr_label
  )
}

tab3 <- bind_rows(tab3_rows) %>%
  mutate(cell = sprintf("%s%% (%d)", pct, n_unwtd)) %>%
  select(Group, year, cell) %>%
  pivot_wider(names_from = year, values_from = cell)

# Add N missing
n_miss_24 <- sum(is.na(clean_2024$most_responsible))
n_miss_25 <- sum(is.na(clean_2025$most_responsible))
log_msg(sprintf("    q8 missing: 2024=%d, 2025=%d", n_miss_24, n_miss_25))

save_table(tab3, "tab3_most_responsible")


# --- Table 4: Policy Approaches ----------------------------------------------

log_msg("  Table 4: Policy Approaches (q10)")

approach_labels <- c("International Cooperation", "Enforce Against Sellers",
                     "Enforce Against Users", "Legalization",
                     "Health/Justice Reform", "Addiction Resources",
                     "Block Drug Supply", "Social/Cultural Change",
                     "Sanctions", "Press China to Act",
                     "Press Mexico to Act", "Harsher Penalties")

# Recode 1/2 → 1/0
for (v in approach_vars) {
  pooled[[paste0(v, "_bin")]]     <- ifelse(as.numeric(pooled[[v]]) == 1, 1L, 0L)
  clean_2024[[paste0(v, "_bin")]] <- ifelse(as.numeric(clean_2024[[v]]) == 1, 1L, 0L)
  clean_2025[[paste0(v, "_bin")]] <- ifelse(as.numeric(clean_2025[[v]]) == 1, 1L, 0L)
}

# Rebuild designs after adding approach binary columns
des_pooled <- svydesign(ids = ~1, weights = ~weight, data = pooled)
des_2024   <- svydesign(ids = ~1, weights = ~weight, data = clean_2024)
des_2025   <- svydesign(ids = ~1, weights = ~weight, data = clean_2025)

tab4_rows <- list()
for (i in seq_along(approach_vars)) {
  bin_v <- paste0(approach_vars[i], "_bin")
  stats_24 <- svymean(as.formula(paste0("~", bin_v)), des_2024, na.rm = TRUE)
  ci_24    <- confint(stats_24)
  stats_25 <- svymean(as.formula(paste0("~", bin_v)), des_2025, na.rm = TRUE)
  ci_25    <- confint(stats_25)
  yoy <- yoy_test(des_pooled, bin_v, binary = TRUE)

  tab4_rows[[i]] <- tibble(
    Approach    = approach_labels[i],
    `2024 %`    = sprintf("%.1f", as.numeric(coef(stats_24)) * 100),
    `2024 CI`   = sprintf("[%.1f, %.1f]", ci_24[1, 1] * 100, ci_24[1, 2] * 100),
    `2025 %`    = sprintf("%.1f", as.numeric(coef(stats_25)) * 100),
    `2025 CI`   = sprintf("[%.1f, %.1f]", ci_25[1, 1] * 100, ci_25[1, 2] * 100),
    `YoY p`     = fmt_p(yoy$p)
  )
}

tab4 <- bind_rows(tab4_rows) %>%
  mutate(.avg = (as.numeric(`2024 %`) + as.numeric(`2025 %`)) / 2) %>%
  arrange(desc(.avg)) %>%
  select(-.avg)

save_table(tab4, "tab4_approaches")


# --- Table 5: China Warmth ---------------------------------------------------

log_msg("  Table 5: China Warmth")

# Summary stats by year
warmth_stats <- wtd_mean_by_year(des_2024, des_2025, "warmth")

# Distribution: % at each level 1–7
warmth_dist_rows <- list()
for (yr_label in c("2024", "2025")) {
  des <- if (yr_label == "2024") des_2024 else des_2025
  des$variables$warmth_f <- factor(des$variables$warmth, levels = 1:7)
  sv <- svymean(~warmth_f, des, na.rm = TRUE)
  warmth_dist_rows[[yr_label]] <- tibble(
    level = as.character(1:7),
    pct   = as.numeric(coef(sv)) * 100,
    year  = yr_label
  )
}

warmth_dist <- bind_rows(warmth_dist_rows)
warmth_dist_wide <- warmth_dist %>%
  mutate(pct_fmt = sprintf("%.1f%%", pct)) %>%
  select(level, year, pct_fmt) %>%
  pivot_wider(names_from = year, values_from = pct_fmt) %>%
  rename(`Warmth Level` = level)

# Combine stats + distribution
tab5_stats <- warmth_stats %>%
  mutate(across(c(mean, se, sd), ~sprintf("%.2f", .))) %>%
  select(year, mean, sd, se, n_unwtd)

save_table(warmth_dist_wide, "tab5_warmth")
log_msg(sprintf("    2024: mean=%.2f (SD=%.2f), 2025: mean=%.2f (SD=%.2f)",
                warmth_stats$mean[1], warmth_stats$sd[1],
                warmth_stats$mean[2], warmth_stats$sd[2]))


# --- Table 6: Subgroup Blame Cross-tabs --------------------------------------

log_msg("  Table 6: Subgroup Blame by Demographics")

subgroup_vars  <- c("party3", "race_eth", "educ_college", "female",
                    "age_group", "ideo3", "income_cat")
subgroup_labels <- c("Party ID", "Race/Ethnicity", "College", "Gender",
                     "Age Group", "Ideology", "Income")

tab6_rows <- list()
row_idx <- 1

for (i in seq_along(subgroup_vars)) {
  sv <- subgroup_vars[i]
  sl <- subgroup_labels[i]
  d <- pooled

  # Get unique non-NA levels
  if (is.factor(d[[sv]])) {
    levs <- levels(d[[sv]])
  } else {
    levs <- sort(unique(na.omit(d[[sv]])))
  }

  for (lv in levs) {
    lv_label <- if (sv == "educ_college") {
      ifelse(lv == 1 | lv == "1", "College+", "No College")
    } else if (sv == "female") {
      ifelse(lv == 1 | lv == "1", "Female", "Male")
    } else {
      as.character(lv)
    }

    for (yr_label in c("2024", "2025")) {
      des <- if (yr_label == "2024") des_2024 else des_2025
      sub_des <- subset(des, des$variables[[sv]] == lv)
      n_sub <- nrow(sub_des$variables)
      if (n_sub < 10) next

      sv_blame <- svymean(~blame_china, sub_des, na.rm = TRUE, deff = TRUE)
      ci <- confint(sv_blame)
      deff_val <- deff(sv_blame)

      tab6_rows[[row_idx]] <- tibble(
        Variable  = sl,
        Level     = lv_label,
        Year      = yr_label,
        `Blame %` = sprintf("%.1f", as.numeric(coef(sv_blame)) * 100),
        CI        = sprintf("[%.1f, %.1f]", ci[1, 1] * 100, ci[1, 2] * 100),
        DEFF      = sprintf("%.2f", as.numeric(deff_val)),
        N         = n_sub
      )
      row_idx <- row_idx + 1
    }

    # YoY test for this subgroup on pooled data
    sub_pooled <- subset(des_pooled, des_pooled$variables[[sv]] == lv)
    if (nrow(sub_pooled$variables) >= 20) {
      yoy <- yoy_test(des_pooled, "blame_china", binary = TRUE,
                       subset_des = sub_pooled)
      # Add p-value to last 2025 row
      if (length(tab6_rows) > 0) {
        last_row <- tab6_rows[[row_idx - 1]]
        if (!is.null(last_row) && last_row$Year == "2025") {
          tab6_rows[[row_idx - 1]]$`YoY p` <- fmt_p(yoy$p)
        }
      }
    }
  }
}

tab6 <- bind_rows(tab6_rows) %>%
  mutate(`YoY p` = ifelse(is.na(`YoY p`), "", `YoY p`))

save_table(tab6, "tab6_subgroup_blame")


# --- Table 7: China Module Summary -------------------------------------------

log_msg("  Table 7: China Module Summary")

china_module_vars <- c("china_role", china_behavior_vars,
                       china_us_behavior_vars, china_us_preferred_vars)
china_module_vars <- china_module_vars[china_module_vars %in% names(pooled)]

tab7_rows <- list()
for (v in china_module_vars) {
  stats <- wtd_mean_by_year(des_2024, des_2025, v)
  tab7_rows[[v]] <- stats
}

tab7 <- bind_rows(tab7_rows) %>%
  mutate(cell = sprintf("%.2f (%.2f)", mean, sd)) %>%
  select(variable, year, cell, n_unwtd) %>%
  pivot_wider(names_from = year, values_from = c(cell, n_unwtd),
              names_glue = "{year}_{.value}") %>%
  rename(Variable = variable, `2024` = `2024_cell`, `2025` = `2025_cell`,
         `N 2024` = `2024_n_unwtd`, `N 2025` = `2025_n_unwtd`)

save_table(tab7, "tab7_china_module")


# =============================================================================
# FIGURES
# =============================================================================

log_msg("")
log_msg("--- Generating Figures ---")

# --- Figure 1: Responsibility Bar Chart --------------------------------------

log_msg("  Figure 1: Responsibility Battery")

fig1_data <- list()
for (i in seq_along(resp_vars)) {
  bin_v <- paste0(resp_vars[i], "_bin")
  for (yr in c("2024", "2025")) {
    des <- if (yr == "2024") des_2024 else des_2025
    sv <- svymean(as.formula(paste0("~", bin_v)), des, na.rm = TRUE)
    ci <- confint(sv)
    fig1_data[[length(fig1_data) + 1]] <- tibble(
      group = resp_labels[i], year = yr,
      prop = as.numeric(coef(sv)),
      ci_lo = ci[1, 1], ci_hi = ci[1, 2]
    )
  }
}

fig1_df <- bind_rows(fig1_data) %>%
  mutate(group = fct_reorder(group, prop, .fun = mean))

p1 <- ggplot(fig1_df, aes(x = prop, y = group, fill = year)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(xmin = ci_lo, xmax = ci_hi),
                 position = position_dodge(width = 0.7), width = 0.2,
                 orientation = "y") +
  scale_x_continuous(labels = percent_format(), limits = c(0, 1)) +
  scale_fill_manual(values = year_colors, name = "Year") +
  labs(title = NULL,
       subtitle = NULL,
       x = "Proportion", y = NULL) +
  theme_pax()

save_figure(p1, "fig1_responsibility")


# --- Figure 2: Warmth Distribution -------------------------------------------

log_msg("  Figure 2: Warmth Distribution")

fig2_df <- warmth_dist %>%
  mutate(level = factor(level, levels = 1:7))

p2 <- ggplot(fig2_df, aes(x = level, y = pct / 100, fill = year)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = year_colors, name = "Year") +
  labs(title = NULL,
       subtitle = NULL,
       x = "Warmth Level", y = "Proportion") +
  theme_pax()

save_figure(p2, "fig2_warmth_dist")


# --- Figure 3: P(Blame=1) by Warmth Tercile ----------------------------------

log_msg("  Figure 3: P(Blame) by Warmth Tercile")

fig3_data <- list()
for (yr in c("2024", "2025")) {
  des <- if (yr == "2024") des_2024 else des_2025
  for (terc in levels(pooled$warmth_tercile)) {
    sub_des <- subset(des, des$variables$warmth_tercile == terc)
    if (nrow(sub_des$variables) < 10) next
    sv <- svymean(~blame_china, sub_des, na.rm = TRUE)
    ci <- confint(sv)
    fig3_data[[length(fig3_data) + 1]] <- tibble(
      tercile = terc, year = yr,
      prob    = as.numeric(coef(sv)),
      ci_lo   = ci[1, 1], ci_hi = ci[1, 2]
    )
  }
}

fig3_df <- bind_rows(fig3_data) %>%
  mutate(tercile = factor(tercile, levels = levels(pooled$warmth_tercile)))

p3 <- ggplot(fig3_df, aes(x = tercile, y = prob, fill = year)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                position = position_dodge(width = 0.7), width = 0.2) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  scale_fill_manual(values = year_colors, name = "Year") +
  labs(title = NULL,
       subtitle = NULL,
       x = "China Warmth Tercile", y = "P(Blame China = 1)") +
  theme_pax()

save_figure(p3, "fig3_warmth_blame")


# --- Figure 4: Blame by Party ------------------------------------------------

log_msg("  Figure 4: Blame by Party")

fig4_data <- list()
for (yr in c("2024", "2025")) {
  des <- if (yr == "2024") des_2024 else des_2025
  for (party in levels(pooled$party3)) {
    sub_des <- subset(des, des$variables$party3 == party)
    if (nrow(sub_des$variables) < 10) next
    sv <- svymean(~blame_china, sub_des, na.rm = TRUE)
    ci <- confint(sv)
    fig4_data[[length(fig4_data) + 1]] <- tibble(
      party = party, year = yr,
      prob  = as.numeric(coef(sv)),
      ci_lo = ci[1, 1], ci_hi = ci[1, 2]
    )
  }
}

fig4_df <- bind_rows(fig4_data)

p4 <- ggplot(fig4_df, aes(x = party, y = prob, fill = year)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi),
                position = position_dodge(width = 0.7), width = 0.2) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  scale_fill_manual(values = year_colors, name = "Year") +
  labs(title = NULL,
       subtitle = NULL,
       x = NULL, y = "P(Blame China = 1)") +
  theme_pax()

save_figure(p4, "fig4_blame_party")


# --- Figure 5: Approaches Dot Plot -------------------------------------------

log_msg("  Figure 5: Approaches Dot Plot")

fig5_data <- list()
for (i in seq_along(approach_vars)) {
  bin_v <- paste0(approach_vars[i], "_bin")
  for (yr in c("2024", "2025")) {
    des <- if (yr == "2024") des_2024 else des_2025
    sv <- svymean(as.formula(paste0("~", bin_v)), des, na.rm = TRUE)
    ci <- confint(sv)
    fig5_data[[length(fig5_data) + 1]] <- tibble(
      approach = approach_labels[i], year = yr,
      prop = as.numeric(coef(sv)),
      ci_lo = ci[1, 1], ci_hi = ci[1, 2]
    )
  }
}

fig5_df <- bind_rows(fig5_data) %>%
  mutate(approach = fct_reorder(approach, prop, .fun = mean))

p5 <- ggplot(fig5_df, aes(x = prop, y = approach, color = year)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(xmin = ci_lo, xmax = ci_hi),
                position = position_dodge(width = 0.5), width = 0.3,
                orientation = "y") +
  scale_x_continuous(labels = percent_format(), limits = c(0, 1)) +
  scale_color_manual(values = year_colors, name = "Year") +
  labs(title = NULL,
       subtitle = NULL,
       x = "Proportion Endorsing", y = NULL) +
  theme_pax()

save_figure(p5, "fig5_approaches")


# =============================================================================
# SAVE ALL OBJECTS + LOG
# =============================================================================

log_msg("")
log_msg("--- Saving Objects and Log ---")

# Save all table/figure data objects for downstream use
descriptives_objects <- list(
  tab1 = tab1_wide, tab2 = tab2, tab3 = tab3, tab4 = tab4,
  tab5_dist = warmth_dist_wide, tab5_stats = warmth_stats,
  tab6 = tab6, tab7 = tab7,
  fig1_data = fig1_df, fig2_data = fig2_df, fig3_data = fig3_df,
  fig4_data = fig4_df, fig5_data = fig5_df
)
saveRDS(descriptives_objects,
        here("prism", "tables", "descriptives_objects.rds"))
log_msg("  Saved descriptives_objects.rds")

# Write descriptives log
log_msg("")
log_msg("========================================================================")
log_msg("DESCRIPTIVES COMPLETE")
log_msg("========================================================================")

log_path <- here("prism", "tables", "descriptives_log.txt")
writeLines(log_env$entries, log_path)
message(sprintf("Descriptives log saved to: %s", log_path))
