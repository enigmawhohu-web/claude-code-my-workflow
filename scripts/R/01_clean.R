# ==============================================================================
# 01_clean.R — Harmonized Cleaning Pipeline for Poll 2025
# ==============================================================================
#
# Purpose:
#   Read two SPSS survey waves (PAXS0003: 2024, PAXS0004: 2025), recode
#   missing values, harmonize variable names across waves (China items are
#   renumbered between surveys), derive analysis variables, and produce:
#     (1) clean_2024.rds and clean_2025.rds (identically coded per crosswalk)
#     (2) pooled.rds (stacked with year indicator)
#
# Inputs:
#   data/raw/PAXS0003_OUTPUT.sav  (N = 3,000; 2024 wave)
#   data/raw/PAXS0004_OUTPUT.sav  (N = 2,000; 2025 wave)
#
# Outputs:
#   data/clean/clean_2024.rds
#   data/clean/clean_2025.rds
#   data/clean/pooled.rds
#   output/tables/cleaning_log.txt
#
# Key design decisions:
#   - Descriptive variable names (not q-numbers)
#   - Survey weights unchanged — no rescaling for pooled data
#   - No exclusions by default — speeders flagged but kept
#   - SPSS system-missing already converted to NA by haven::read_sav()
#   - Race/ethnicity uses CES `race` variable directly (Hispanic coded as
#     race=3 per CES convention, not Hispanic-override from `hispanic` var)
#
# Author: PAX sapiens Foundation
# Date:   2026-02-17
# ==============================================================================

# --- Section 0: Setup --------------------------------------------------------

# Seed per project convention (r-code-conventions.md); no randomness in this
# cleaning script, but included for pipeline consistency
set.seed(20250217)

library(here)
library(haven)
library(labelled)
library(tidyverse)

# Create output directories if needed
dir.create(here("data", "clean"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "tables"), recursive = TRUE, showWarnings = FALSE)

# Logging: accumulates messages to vector + prints via message()
log_env <- new.env(parent = emptyenv())
log_env$entries <- character()

#' Append a message to the cleaning log and print to stderr
#'
#' @param msg Character string to log
log_msg <- function(msg) {
  log_env$entries <- c(log_env$entries, msg)
  message(msg)
}

log_msg("=" |> strrep(72))
log_msg("CLEANING LOG: Poll 2025 \u2014 Harmonized Pipeline")
log_msg(paste("Timestamp:", Sys.time()))
log_msg("=" |> strrep(72))


# --- Section 1: Read Raw Data ------------------------------------------------

log_msg("")
log_msg("--- Section 1: Read Raw Data ---")

raw_2024 <- read_sav(here("data", "raw", "PAXS0003_OUTPUT.sav"))
raw_2025 <- read_sav(here("data", "raw", "PAXS0004_OUTPUT.sav"))

log_msg(sprintf("PAXS0003 (2024): %d rows x %d cols", nrow(raw_2024), ncol(raw_2024)))
log_msg(sprintf("PAXS0004 (2025): %d rows x %d cols", nrow(raw_2025), ncol(raw_2025)))

# Verify weight properties
for (wave_name in c("2024", "2025")) {
  d <- if (wave_name == "2024") raw_2024 else raw_2025
  w <- d$weight
  log_msg(sprintf(
    "  %s weights: mean=%.4f, sum=%.0f, range=[%.4f, %.4f], NAs=%d",
    wave_name, mean(w, na.rm = TRUE), sum(w, na.rm = TRUE),
    min(w, na.rm = TRUE), max(w, na.rm = TRUE), sum(is.na(w))
  ))
}


# --- Section 2: Missing Value Recoding ----------------------------------------

log_msg("")
log_msg("--- Section 2: Missing Value Recoding ---")

# Missing value codes used in these surveys:
#   -8, 8, 98  = skipped
#   9, 99      = not asked
#   998        = skipped (open-ended China role)
# In practice, haven::read_sav() already converts SPSS system-missing to R NA.
# The coded skip/not-asked values that remain are valid response categories
# in this dataset. We still scan for them defensively.

#' Recode coded missing values to NA for one survey wave
#'
#' Scans survey items, demographics, and vote variables for skip/not-asked
#' codes. Uses SPSS value labels to identify missing codes dynamically.
#' Skips non-numeric and non-labelled columns safely.
#'
#' @param df Data frame from haven::read_sav() with labelled columns
#' @param wave_label Character string ("2024" or "2025") for log messages
#' @return Data frame with coded missing values replaced by NA
recode_missing_one_wave <- function(df, wave_label) {
  n_recoded_total <- 0

  # Helper: recode missing for a set of variables
  recode_vars <- function(df, vars, extra_codes = numeric(0)) {
    n_recoded <- 0
    for (v in intersect(vars, names(df))) {
      # Skip non-numeric, non-labelled columns (e.g., timestamps, text)
      if (!is.numeric(df[[v]]) && !haven::is.labelled(df[[v]])) next
      x <- suppressWarnings(as.numeric(df[[v]]))
      if (all(is.na(x))) next

      # Detect skip/not-asked from SPSS value labels
      var_labels <- attr(df[[v]], "labels")
      label_codes <- numeric(0)
      if (!is.null(var_labels)) {
        label_names <- tolower(names(var_labels))
        label_codes <- as.numeric(var_labels[grepl("skip|not asked", label_names)])
      }
      all_codes <- unique(c(extra_codes, label_codes))

      n_before <- sum(is.na(x))
      x[x %in% all_codes] <- NA
      n_new_na <- sum(is.na(x)) - n_before
      if (n_new_na > 0) {
        df[[v]] <- x
        n_recoded <- n_recoded + n_new_na
        log_msg(sprintf("  %s %s: %d coded missing -> NA", wave_label, v, n_new_na))
      }
    }
    list(df = df, n = n_recoded)
  }

  # Survey items (q-numbered)
  q_vars <- names(df)[grepl("^q\\d", names(df))]
  res <- recode_vars(df, q_vars, extra_codes = c(-8, 98, 99, 998))
  df <- res$df; n_recoded_total <- n_recoded_total + res$n

  # Demographic variables
  demo_vars <- c("gender", "race", "hispanic", "educ", "marstat", "employ",
                 "faminc_new", "child18", "pid3", "pid7", "ideo5", "newsint",
                 "votereg", "religpew", "pew_churatd", "pew_bornagain",
                 "pew_religimp", "pew_prayer")
  res <- recode_vars(df, demo_vars)
  df <- res$df; n_recoded_total <- n_recoded_total + res$n

  # Presidential vote variables
  vote_vars <- c("presvote16post", "presvote20post", "presvote24post")
  res <- recode_vars(df, vote_vars)
  df <- res$df; n_recoded_total <- n_recoded_total + res$n

  log_msg(sprintf("  %s total recoded: %d values -> NA", wave_label, n_recoded_total))
  return(df)
}

raw_2024 <- recode_missing_one_wave(raw_2024, "2024")
raw_2025 <- recode_missing_one_wave(raw_2025, "2025")

log_msg(paste0(
  "  Note: SPSS system-missing values were already converted to R NA ",
  "by haven::read_sav()."
))
log_msg(paste0(
  "  Coded skip/not-asked values (8, 9, 98, 99) that appear in the data ",
  "are valid response codes, not missing indicators."
))


# --- Section 3: Harmonize Variable Names (Crosswalk) -------------------------

log_msg("")
log_msg("--- Section 3: Harmonize Variable Names ---")

# All China-related items are identical in content across waves — only
# the question numbering shifted. We map both waves to common names.

#' Rename a single column, skipping if source doesn't exist
#'
#' @param df Data frame
#' @param old_name Current column name
#' @param new_name Desired column name
#' @return Data frame with column renamed (or unchanged if old_name absent)
safe_rename <- function(df, old_name, new_name) {
  if (old_name %in% names(df)) {
    names(df)[names(df) == old_name] <- new_name
  }
  return(df)
}

#' Apply the full crosswalk to harmonize variable names for one wave
#'
#' Renames all survey items, China items (wave-specific numbering),
#' and demographics to common descriptive names.
#'
#' @param df Data frame with original SPSS variable names
#' @param wave Integer year (2024 or 2025)
#' @return Data frame with harmonized column names
harmonize_names <- function(df, wave) {

  # --- Shared items (same numbering in both waves) ---
  df <- safe_rename(df, "q1", "consent")
  df <- safe_rename(df, "q2", "overdose_deaths_estimate")
  df <- safe_rename(df, "q3", "know_someone_died")
  df <- safe_rename(df, "q4", "who_died")
  df <- safe_rename(df, "q5", "personal_overdose")
  df <- safe_rename(df, "q6", "blame_open_ended")

  # Responsibility battery (q7_1 through q7_8)
  resp_names <- c("responsible_drug_users", "responsible_doctors",
                  "responsible_pharma", "responsible_cartels",
                  "responsible_state_local", "responsible_federal",
                  "responsible_china", "responsible_mexico")
  for (i in seq_along(resp_names)) {
    df <- safe_rename(df, paste0("q7_", i), resp_names[i])
  }

  df <- safe_rename(df, "q8", "most_responsible")
  df <- safe_rename(df, "q9", "most_responsible_fix")

  # Approaches battery (q10_1 through q10_12)
  approach_names <- c("approach_intl_cooperation", "approach_enforce_sales",
                      "approach_enforce_users", "approach_legalization",
                      "approach_health_justice", "approach_addiction_resources",
                      "approach_blocking_drugs", "approach_social_cultural",
                      "approach_sanctions", "approach_china_action",
                      "approach_mexico_action", "approach_penalties")
  for (i in seq_along(approach_names)) {
    df <- safe_rename(df, paste0("q10_", i), approach_names[i])
  }

  # --- China items (renumbered between waves; content identical) ---
  if (wave == 2024) {
    df <- safe_rename(df, "q12", "china_opinion")
    df <- safe_rename(df, "q13", "china_role")
    for (s in letters[1:4]) {
      df <- safe_rename(df, paste0("q14_", s), paste0("china_behavior_", s))
      df <- safe_rename(df, paste0("q15_", s), paste0("us_behavior_", s))
      df <- safe_rename(df, paste0("q16_", s), paste0("us_preferred_", s))
    }
    for (s in letters[1:8]) {
      df <- safe_rename(df, paste0("q17_", s), paste0("opinion_change_", s))
    }
    for (i in 1:11) {
      df <- safe_rename(df, paste0("q18_", i), paste0("china_experience_", i))
    }
    df <- safe_rename(df, "presvote16post", "presvote_2016")
  } else {
    # wave == 2025
    df <- safe_rename(df, "q11", "china_opinion")
    df <- safe_rename(df, "q12", "china_role")
    for (s in letters[1:4]) {
      df <- safe_rename(df, paste0("q13_", s), paste0("china_behavior_", s))
      df <- safe_rename(df, paste0("q14_", s), paste0("us_behavior_", s))
      df <- safe_rename(df, paste0("q15_", s), paste0("us_preferred_", s))
    }
    for (s in letters[1:8]) {
      df <- safe_rename(df, paste0("q16_", s), paste0("opinion_change_", s))
    }
    for (i in 1:11) {
      df <- safe_rename(df, paste0("q17_", i), paste0("china_experience_", i))
    }
    df <- safe_rename(df, "presvote24post", "presvote_2024")
  }

  # Shared presidential vote
  df <- safe_rename(df, "presvote20post", "presvote_2020")

  return(df)
}

raw_2024 <- harmonize_names(raw_2024, wave = 2024)
raw_2025 <- harmonize_names(raw_2025, wave = 2025)

log_msg(sprintf("  2024 columns after renaming: %d", ncol(raw_2024)))
log_msg(sprintf("  2025 columns after renaming: %d", ncol(raw_2025)))

# Verify crosswalk: key analysis variables must exist in both waves
key_vars <- c("china_opinion", "responsible_china", "most_responsible",
              "weight", "caseid", "birthyr", "gender", "race", "educ",
              "pid3", "pid7", "ideo5")
crosswalk_ok <- TRUE
for (v in key_vars) {
  in_24 <- v %in% names(raw_2024)
  in_25 <- v %in% names(raw_2025)
  if (!in_24 || !in_25) {
    log_msg(sprintf("  FAIL: %s missing in %s", v,
                    paste(c("2024"[!in_24], "2025"[!in_25]), collapse = " and ")))
    crosswalk_ok <- FALSE
  }
}
if (crosswalk_ok) {
  log_msg("  Crosswalk verification: all key analysis variables present in both waves")
} else {
  stop("Crosswalk verification FAILED: key variables missing. See log above.")
}


# --- Section 4: Derive Analysis Variables -------------------------------------

log_msg("")
log_msg("--- Section 4: Derive Analysis Variables ---")

#' Derive analysis-ready variables from harmonized survey data
#'
#' Creates: year, warmth (1-7), blame_china (0/1), blame_china_alt (0/1),
#' age, age_group, educ_college, party3, female, race_eth, survey_duration_min.
#'
#' @param df Data frame with harmonized column names
#' @param year_val Integer year (2024 or 2025)
#' @return Data frame with derived variables appended
derive_variables <- function(df, year_val) {

  df <- df %>%
    mutate(
      # Year indicator
      year = year_val,

      # --- Primary analysis variables ---

      # Warmth: Opinion of China, numeric 1-7
      # 1=Very negative, 7=Very positive (already coded this way)
      warmth = as.numeric(china_opinion),

      # Blame (primary): China responsible for opioid deaths
      # q7_7: 1=selected, 2=not selected -> recode to 1/0
      blame_china = case_when(
        as.numeric(responsible_china) == 1 ~ 1L,
        as.numeric(responsible_china) == 2 ~ 0L,
        TRUE ~ NA_integer_
      ),

      # Blame (robustness): q8 most responsible, China vs others
      # Code 7 = China -> 1, all other valid responses -> 0, NA stays NA
      blame_china_alt = case_when(
        is.na(most_responsible) ~ NA_integer_,
        as.numeric(most_responsible) == 7 ~ 1L,
        TRUE ~ 0L
      ),

      # --- Demographics ---

      # Age (with bounds check: flag implausible values)
      age = year_val - as.numeric(birthyr),
      age = if_else(age >= 18 & age <= 120, age, NA_real_),

      # Age groups
      age_group = case_when(
        age >= 18 & age <= 29 ~ "18-29",
        age >= 30 & age <= 44 ~ "30-44",
        age >= 45 & age <= 64 ~ "45-64",
        age >= 65            ~ "65+",
        TRUE                 ~ NA_character_
      ) %>% factor(levels = c("18-29", "30-44", "45-64", "65+")),

      # Education: college vs no college
      # educ: 1=No HS, 2=HS grad, 3=Some college, 4=2-year, 5=4-year, 6=Post-grad
      educ_college = case_when(
        as.numeric(educ) %in% c(5, 6) ~ 1L,
        as.numeric(educ) %in% c(1, 2, 3, 4) ~ 0L,
        TRUE ~ NA_integer_
      ),

      # Party ID (3-category)
      # pid3: 1=Dem, 2=Rep, 3=Ind, 4=Other, 5=Not sure
      party3 = case_when(
        as.numeric(pid3) == 1 ~ "Democrat",
        as.numeric(pid3) == 2 ~ "Republican",
        as.numeric(pid3) == 3 ~ "Independent",
        as.numeric(pid3) %in% c(4, 5) ~ NA_character_,
        TRUE ~ NA_character_
      ) %>% factor(levels = c("Democrat", "Republican", "Independent")),

      # Female indicator
      # gender: 1=Male, 2=Female
      female = case_when(
        as.numeric(gender) == 2 ~ 1L,
        as.numeric(gender) == 1 ~ 0L,
        TRUE ~ NA_integer_
      ),

      # Race/ethnicity (collapsed)
      # CES/YouGov `race` variable: 1=White, 2=Black, 3=Hispanic, 4=Asian,
      #   5=Native American, 6=Two or more, 7=Other, 8=Middle Eastern
      # NOTE: CES codes Hispanic as a race category in `race`.
      # The separate `hispanic` variable (1=Yes, 2=No) allows alternative
      # Hispanic-override coding. We use `race` directly here, consistent
      # with CES documentation.
      race_eth = case_when(
        as.numeric(race) == 1 ~ "White",
        as.numeric(race) == 2 ~ "Black",
        as.numeric(race) == 3 ~ "Hispanic",
        as.numeric(race) == 4 ~ "Asian",
        as.numeric(race) %in% c(5, 6, 7, 8) ~ "Other",
        TRUE ~ NA_character_
      ) %>% factor(levels = c("White", "Black", "Hispanic", "Asian", "Other"))
    )

  # Survey duration: compute only if timing variables exist
  if (all(c("starttime", "endtime") %in% names(df))) {
    df <- df %>%
      mutate(survey_duration_min = as.numeric(
        difftime(endtime, starttime, units = "mins")
      ))
  } else {
    df$survey_duration_min <- NA_real_
    log_msg(sprintf("  WARNING: %d wave missing starttime/endtime", year_val))
  }

  # Log implausible ages
  n_implausible <- sum(is.na(df$age) & !is.na(df$birthyr), na.rm = TRUE)
  if (n_implausible > 0) {
    log_msg(sprintf("  WARNING: %d has %d implausible ages set to NA",
                    year_val, n_implausible))
  }

  return(df)
}

clean_2024 <- derive_variables(raw_2024, year_val = 2024)
clean_2025 <- derive_variables(raw_2025, year_val = 2025)

# Log derived variable summaries
for (wave_name in c("2024", "2025")) {
  d <- if (wave_name == "2024") clean_2024 else clean_2025
  log_msg(sprintf("  %s warmth: mean=%.2f, sd=%.2f, range=[%d,%d], NAs=%d",
                  wave_name, mean(d$warmth, na.rm = TRUE),
                  sd(d$warmth, na.rm = TRUE),
                  min(d$warmth, na.rm = TRUE), max(d$warmth, na.rm = TRUE),
                  sum(is.na(d$warmth))))
  log_msg(sprintf("  %s blame_china: mean=%.3f (=prop selected), NAs=%d",
                  wave_name, mean(d$blame_china, na.rm = TRUE),
                  sum(is.na(d$blame_china))))
  log_msg(sprintf("  %s blame_china_alt: mean=%.3f, NAs=%d",
                  wave_name, mean(d$blame_china_alt, na.rm = TRUE),
                  sum(is.na(d$blame_china_alt))))
  log_msg(sprintf("  %s age: mean=%.1f, range=[%d,%d]",
                  wave_name, mean(d$age, na.rm = TRUE),
                  min(d$age, na.rm = TRUE), max(d$age, na.rm = TRUE)))
}


# --- Section 5: Quality Checks -----------------------------------------------

log_msg("")
log_msg("--- Section 5: Quality Checks ---")

# Check 1: warmth x blame_china cross-tabulation
log_msg("  Cross-tabulation: warmth x blame_china (pooled)")
warmth_blame_tab <- table(
  warmth = c(clean_2024$warmth, clean_2025$warmth),
  blame = c(clean_2024$blame_china, clean_2025$blame_china),
  useNA = "ifany"
)
log_msg(paste("  ", capture.output(print(warmth_blame_tab)), collapse = "\n"))

# Check 2: Weight means by wave
for (wave_name in c("2024", "2025")) {
  d <- if (wave_name == "2024") clean_2024 else clean_2025
  w_mean <- mean(d$weight, na.rm = TRUE)
  log_msg(sprintf("  %s weight mean: %.6f (should be ~1.0)", wave_name, w_mean))
  if (abs(w_mean - 1.0) > 0.001) {
    log_msg(sprintf("  WARNING: %s weight mean deviates from 1.0!", wave_name))
  }
}

# Check 3: Flag speeders (bottom 5% of duration)
for (wave_name in c("2024", "2025")) {
  d <- if (wave_name == "2024") clean_2024 else clean_2025
  q05 <- quantile(d$survey_duration_min, 0.05, na.rm = TRUE)
  n_fast <- sum(d$survey_duration_min < q05, na.rm = TRUE)
  log_msg(sprintf(
    "  %s speeders (< %.1f min, bottom 5%%%%): %d respondents (NOT excluded)",
    wave_name, q05, n_fast
  ))
}

# Check 4: Key variable completeness
log_msg("  Variable completeness (%% non-missing):")
analysis_vars <- c("warmth", "blame_china", "blame_china_alt", "age",
                   "female", "educ_college", "party3", "race_eth", "weight")
for (v in analysis_vars) {
  vals <- c(clean_2024[[v]], clean_2025[[v]])
  pct <- 100 * (1 - sum(is.na(vals)) / length(vals))
  log_msg(sprintf("    %s: %.1f%% complete (%d NAs of %d)",
                  v, pct, sum(is.na(vals)), length(vals)))
}


# --- Section 6: Export --------------------------------------------------------

log_msg("")
log_msg("--- Section 6: Export ---")

# Strip haven labels from all outputs for consistency
# (Downstream scripts should not depend on SPSS label metadata)
#' Remove haven value labels from all labelled columns
#'
#' Uses haven::zap_labels() which preserves the underlying data type
#' (numeric stays numeric, character stays character) unlike as.numeric()
#' which would corrupt non-numeric labelled columns.
#'
#' @param df Data frame potentially containing haven_labelled columns
#' @return Data frame with all value labels removed
strip_labels <- function(df) {
  haven::zap_labels(df)
}

clean_2024_out <- strip_labels(clean_2024)
clean_2025_out <- strip_labels(clean_2025)

# Save wave-specific datasets
saveRDS(clean_2024_out, here("data", "clean", "clean_2024.rds"))
saveRDS(clean_2025_out, here("data", "clean", "clean_2025.rds"))
log_msg(sprintf("  Saved clean_2024.rds (%d rows x %d cols)",
                nrow(clean_2024_out), ncol(clean_2024_out)))
log_msg(sprintf("  Saved clean_2025.rds (%d rows x %d cols)",
                nrow(clean_2025_out), ncol(clean_2025_out)))

# Stack into pooled dataset
# bind_rows handles columns unique to one wave (fills with NA)
pooled <- bind_rows(clean_2024_out, clean_2025_out)
saveRDS(pooled, here("data", "clean", "pooled.rds"))
log_msg(sprintf("  Saved pooled.rds (%d rows x %d cols)",
                nrow(pooled), ncol(pooled)))

# Verify pooled N
stopifnot(nrow(pooled) == nrow(clean_2024) + nrow(clean_2025))
log_msg(sprintf("  Pooled N verified: %d = %d + %d",
                nrow(pooled), nrow(clean_2024), nrow(clean_2025)))

# Verify year indicator
log_msg("  Year distribution in pooled:")
log_msg(sprintf("    2024: %d", sum(pooled$year == 2024)))
log_msg(sprintf("    2025: %d", sum(pooled$year == 2025)))

# Write cleaning log to file
log_msg("")
log_msg("=" |> strrep(72))
log_msg("CLEANING COMPLETE")
log_msg("=" |> strrep(72))

writeLines(log_env$entries, here("output", "tables", "cleaning_log.txt"))
message("Cleaning log saved to: ", here("output", "tables", "cleaning_log.txt"))
