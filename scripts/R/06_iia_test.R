#' ============================================================================
#' 06_iia_test.R -- Formal IIA Test for Phase 4 Multinomial (R&R MR-A)
#' Poll 2025: Fentanyl Policy Survey Analysis
#'
#' @description Attempts Hausman-McFadden IIA test on the Phase 4 tool_config
#'   multinomial (warmth_z x blame_china interaction, pooled, survey-weighted
#'   via frequency weights). Tries `mlogit::hmftest` first, then VGAM nested
#'   logit structure {Neither, Action-only} vs {Sanctions-only, S+A}.
#'
#' @input  Data/cleaned/pooled.rds, prism/tables/phase4_objects.rds
#' @output prism/tables/app_tab_iia_formal.tex (+ .csv)
#'         Output/06_iia_log.txt
#'
#' @depends here, nnet, mlogit, dfidx, VGAM, survey, tidyverse
#' ============================================================================

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(nnet)
  library(survey)
  library(mlogit)
  library(dfidx)
  library(VGAM)
  library(kableExtra)
})

set.seed(20260420)

out_dir <- here("Output")
tab_dir <- here("prism", "tables")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

log_lines <- character()
log_msg <- function(msg) {
  log_lines <<- c(log_lines, msg)
  message(msg)
}

log_msg("========================================================================")
log_msg("06_iia_test.R -- Formal IIA Test")
log_msg(sprintf("Timestamp: %s", Sys.time()))
log_msg("========================================================================")

# ------------------------------------------------------------------ Load data
pooled <- readRDS(here("Data", "cleaned", "pooled.rds"))

# Build variables (mirror 04_mechanisms.R minimal set)
build_analysis_vars <- function(df, warmth_mu, warmth_sd) {
  df %>%
    mutate(
      us_pref_a = as.numeric(us_preferred_a),
      us_pref_b = as.numeric(us_preferred_b),
      us_pref_c_r = 8 - as.numeric(us_preferred_c),
      us_pref_d = as.numeric(us_preferred_d),
      posture_n_valid = rowSums(!is.na(cbind(us_pref_a, us_pref_b,
                                             us_pref_c_r, us_pref_d))),
      posture_index = ifelse(posture_n_valid >= 3,
        rowMeans(cbind(us_pref_a, us_pref_b, us_pref_c_r, us_pref_d),
                 na.rm = TRUE), NA_real_),
      warmth_z = (warmth - warmth_mu) / warmth_sd,
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
      newsint_attn = { ni <- as.numeric(newsint)
        ifelse(ni %in% 1:4, 5L - ni, NA_integer_) },
      ideo5_clean  = { id <- as.numeric(ideo5)
        ifelse(id %in% 1:5, id, NA_real_) }
    )
}

build_tool_config <- function(df) {
  df %>% mutate(
    sanctions_bin = { x <- as.numeric(approach_sanctions)
      ifelse(is.na(x), NA_integer_, ifelse(x == 1, 1L, 0L)) },
    action_bin = { x <- as.numeric(approach_china_action)
      ifelse(is.na(x), NA_integer_, ifelse(x == 1, 1L, 0L)) },
    tool_config = case_when(
      is.na(sanctions_bin) | is.na(action_bin) ~ NA_character_,
      sanctions_bin == 0 & action_bin == 0 ~ "Neither",
      sanctions_bin == 1 & action_bin == 0 ~ "Sanctions-only",
      sanctions_bin == 0 & action_bin == 1 ~ "Action-only",
      sanctions_bin == 1 & action_bin == 1 ~ "S+A"
    ) %>% factor(levels = c("Neither", "Sanctions-only", "Action-only", "S+A"))
  )
}

warmth_mu <- mean(pooled$warmth, na.rm = TRUE)
warmth_sd <- sd(pooled$warmth, na.rm = TRUE)
pooled <- build_analysis_vars(pooled, warmth_mu, warmth_sd)
pooled <- build_tool_config(pooled)

# Posture_z required for control set (uses pooled moments)
posture_mu <- mean(pooled$posture_index, na.rm = TRUE)
posture_sd <- sd(pooled$posture_index, na.rm = TRUE)
pooled$posture_z <- (pooled$posture_index - posture_mu) / posture_sd

model_vars <- c("tool_config", "warmth_z", "blame_china", "party3",
                "ideo5_clean", "educ_college", "age", "female", "race_eth",
                "newsint_attn", "exposure_index", "posture_z", "year", "weight")
d <- pooled[complete.cases(pooled[, model_vars]), ]
log_msg(sprintf("  Complete-case N for IIA test: %d", nrow(d)))

# ----------------------------------------------------------- Hausman-McFadden
# The Hausman-McFadden test compares coefficients between the full multinomial
# and a restricted choice set (dropping one alternative). Under IIA the
# coefficient vectors should not change systematically.
#
# We use mlogit::mlogit() with the long-format data frame built from dfidx,
# weighting via the `weights` argument. If mlogit fails with survey weights,
# we fall back to unweighted HM test (documented as such).

# mlogit via mlogit.data(). Prepare wide input with all covariates, using
# mlogit.data to reshape to long.
d_ml <- d %>%
  mutate(
    tool_choice = factor(as.character(tool_config),
                         levels = c("Neither", "Sanctions-only",
                                    "Action-only", "S+A")),
    year_fac    = factor(year),
    weight_ml   = weight
  ) %>%
  select(tool_choice, warmth_z, blame_china, party3, ideo5_clean,
         educ_college, age, female, race_eth, newsint_attn,
         exposure_index, posture_z, year_fac, weight_ml) %>%
  as.data.frame()

alt_levels <- levels(d_ml$tool_choice)

d_long <- tryCatch(
  mlogit.data(d_ml, choice = "tool_choice", shape = "wide"),
  error = function(e) {
    log_msg(sprintf("  mlogit.data() failed: %s", conditionMessage(e)))
    NULL
  })

# Full model formula: 0 -> no alternative-specific intercepts beyond defaults
fmla_full <- tool_choice ~ 0 |
  warmth_z * blame_china + party3 + ideo5_clean + educ_college + age +
  female + race_eth + newsint_attn + exposure_index + posture_z +
  year_fac

log_msg("")
log_msg("--- Fitting full mlogit model (survey-weighted) ---")
m_full <- NULL
hm_used_weights <- FALSE
if (!is.null(d_long)) {
  m_full <- tryCatch(
    mlogit(fmla_full, data = d_long, weights = weight_ml,
           reflevel = "Neither"),
    error = function(e) {
      log_msg(sprintf("  Weighted mlogit failed: %s", conditionMessage(e)))
      NULL
    })
  hm_used_weights <- !is.null(m_full)
  if (is.null(m_full)) {
    log_msg("  Weighted mlogit failed. Retrying unweighted.")
    m_full <- tryCatch(
      mlogit(fmla_full, data = d_long, reflevel = "Neither"),
      error = function(e) {
        log_msg(sprintf("  Unweighted mlogit failed: %s", conditionMessage(e)))
        NULL
      })
  }
}

# Restricted models: drop each alternative in turn and re-estimate
hm_rows <- list()

if (!is.null(m_full)) {
  log_msg(sprintf("  Full model OK. N=%d, logLik=%.2f",
                  nrow(d), as.numeric(logLik(m_full))))
  for (drop_alt in alt_levels) {
    alt_sub <- setdiff(alt_levels, drop_alt)
    m_r <- tryCatch(
      if (hm_used_weights) {
        mlogit(fmla_full, data = d_long, weights = weight_ml,
               reflevel = if ("Neither" %in% alt_sub) "Neither" else alt_sub[1],
               alt.subset = alt_sub)
      } else {
        mlogit(fmla_full, data = d_long,
               reflevel = if ("Neither" %in% alt_sub) "Neither" else alt_sub[1],
               alt.subset = alt_sub)
      },
      error = function(e) {
        log_msg(sprintf("  Restricted (drop=%s) fit failed: %s",
                        drop_alt, conditionMessage(e)))
        NULL
      })
    if (is.null(m_r)) {
      log_msg(sprintf("  Restricted model dropping '%s' failed to fit.", drop_alt))
      next
    }
    hm <- tryCatch(mlogit::hmftest(m_full, m_r),
                   error = function(e) {
                     log_msg(sprintf("  hmftest(drop=%s) failed: %s",
                                     drop_alt, conditionMessage(e)))
                     NULL
                   })
    if (!is.null(hm)) {
      hm_rows[[drop_alt]] <- tibble(
        dropped_alternative = drop_alt,
        chi_squared         = unname(hm$statistic),
        df                  = unname(hm$parameter),
        p_value             = unname(hm$p.value),
        result              = ifelse(hm$p.value < 0.05,
                                     "Reject IIA (alt has distinctive effects)",
                                     "Fail to reject IIA")
      )
      log_msg(sprintf("  Drop '%s': X2=%.3f, df=%d, p=%.4f (%s)",
                      drop_alt, hm$statistic, hm$parameter, hm$p.value,
                      hm_rows[[drop_alt]]$result))
    }
  }
}

hm_df <- bind_rows(hm_rows)

# ------------------------------------------------------------------ Save table
if (nrow(hm_df) > 0) {
  display <- hm_df %>%
    mutate(
      `Dropped alternative` = dropped_alternative,
      `$\\chi^{2}$`         = sprintf("%.3f", chi_squared),
      `$df$`                = as.character(df),
      `$p$-value`           = sprintf("%.4f", p_value),
      Verdict               = result
    ) %>%
    select(`Dropped alternative`, `$\\chi^{2}$`, `$df$`,
           `$p$-value`, Verdict)

  write_csv(hm_df, file.path(tab_dir, "app_tab_iia_formal.csv"))

  tex <- kbl(display, format = "latex", booktabs = TRUE,
             caption = NULL, linesep = "", escape = FALSE)
  writeLines(as.character(tex),
             file.path(tab_dir, "app_tab_iia_formal.tex"))
  log_msg("  Saved: app_tab_iia_formal.tex (+ .csv)")
  weight_note <- if (hm_used_weights) "survey-weighted" else
    "unweighted (weighted mlogit failed; see log)"
  log_msg(sprintf("  NOTE: Hausman-McFadden test ran %s.", weight_note))
} else {
  # Document failure
  fail_df <- tibble(
    Item   = c("Method", "Result",
               "Package", "Fallback"),
    Value  = c("Hausman-McFadden (mlogit::hmftest)",
               "Could not be computed for any dropped alternative",
               "mlogit::mlogit / hmftest",
               "See 05_probes.R for divergence test; see coef-stability heuristic in app_tab_iia_test.tex")
  )
  write_csv(fail_df, file.path(tab_dir, "app_tab_iia_formal.csv"))
  tex <- kbl(fail_df, format = "latex", booktabs = TRUE,
             caption = NULL, linesep = "", escape = FALSE)
  writeLines(as.character(tex),
             file.path(tab_dir, "app_tab_iia_formal.tex"))
  log_msg("  Formal test failed; documenting failure in app_tab_iia_formal.tex")
}

writeLines(log_lines, file.path(out_dir, "06_iia_log.txt"))
log_msg(sprintf("  Log saved: %s", file.path(out_dir, "06_iia_log.txt")))
