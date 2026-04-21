#' 07_mi_robustness.R — Multiple Imputation robustness for Phase 4 MNL and Phase 5 probes
#'
#' @description Addresses referee comments M-3 and MOD-1. The existing MI
#'   infrastructure in `03_models.R` only pools the Phase 3 OLS interaction
#'   (warmth_z x blame_china on posture_z). This script re-imputes the pooled
#'   sample with an *extended* variable set that covers:
#'     - Phase 4 MNL (tool_config, constructed post-imputation from raw items)
#'     - Phase 5 probe contrasts (eight `probe_*` + `is_SA` derived post-imputation)
#'   and pools parameter estimates using Rubin's rules, explicitly reporting
#'   V_W (within-imputation variance), V_B (between-imputation variance),
#'   T (total variance), FMI (fraction of missing information), and
#'   Barnard-Rubin degrees of freedom for each key quantity.
#'
#'   DOES NOT modify any existing script. DOES NOT overwrite any existing table.
#'
#' @input  Data/cleaned/pooled.rds
#' @output prism/tables/app_tab_mi_phase4.{tex,csv}
#'         prism/tables/app_tab_mi_phase5.{tex,csv}
#'         prism/tables/app_tab_mi_diagnostics.{tex,csv}
#'         prism/tables/phase7_log.txt
#'         prism/tables/phase7_objects.rds
#' @depends tidyverse, survey, srvyr, nnet, mice, here
#' @seed    2024 (same as 03_models.R for Phase 3 MI consistency)

# ---------------------------------------------------------------------------
# Setup
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(survey)
  library(srvyr)
  library(nnet)
  library(mice)
})

set.seed(2024)

# --- Logging ---------------------------------------------------------------
log_env <- new.env()
log_env$lines <- character()
log_msg  <- function(msg) { cat(msg, "\n", sep = ""); log_env$lines <- c(log_env$lines, msg) }
log_only <- function(msg) { log_env$lines <- c(log_env$lines, msg) }

log_msg("=====================================================================")
log_msg("  07_mi_robustness.R — MI for Phase 4 MNL and Phase 5 probes")
log_msg(sprintf("  Started: %s", format(Sys.time())))
log_msg("=====================================================================")

t0 <- Sys.time()

# Output dir
out_dir <- here("prism", "tables")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---------------------------------------------------------------------------
# Constants (mirror 05_probes.R)
# ---------------------------------------------------------------------------

TARGET_PROBE     <- "d"
BOUNDARY_PROBES  <- c("a", "b", "c", "e", "f", "g", "h")
ALL_PROBES       <- letters[1:8]
M_IMP            <- 10

APPROACH_VARS <- c(
  "approach_intl_cooperation", "approach_enforce_sales",
  "approach_enforce_users", "approach_legalization",
  "approach_health_justice", "approach_addiction_resources",
  "approach_blocking_drugs", "approach_social_cultural",
  "approach_sanctions", "approach_china_action",
  "approach_mexico_action", "approach_penalties"
)
APPROACH_EXCL <- setdiff(APPROACH_VARS,
                         c("approach_sanctions", "approach_china_action"))

# ---------------------------------------------------------------------------
# Load and prepare pooled data
# ---------------------------------------------------------------------------

log_msg("")
log_msg("--- Section 1: Load and prepare pooled data ---")

pooled <- readRDS(here("Data", "cleaned", "pooled.rds"))
log_msg(sprintf("  pooled: N=%d, vars=%d", nrow(pooled), ncol(pooled)))

# --- Phase 3 constructed vars (mirrors 03_models.R build_analysis_vars) ----

warmth_mu <- mean(pooled$warmth, na.rm = TRUE)
warmth_sd <- sd(pooled$warmth,   na.rm = TRUE)

pooled <- pooled %>%
  mutate(
    us_pref_a   = as.numeric(us_preferred_a),
    us_pref_b   = as.numeric(us_preferred_b),
    us_pref_c_r = 8 - as.numeric(us_preferred_c),
    us_pref_d   = as.numeric(us_preferred_d),
    posture_n_valid = rowSums(!is.na(cbind(us_pref_a, us_pref_b,
                                           us_pref_c_r, us_pref_d))),
    posture_index = ifelse(
      posture_n_valid >= 3,
      rowMeans(cbind(us_pref_a, us_pref_b, us_pref_c_r, us_pref_d), na.rm = TRUE),
      NA_real_
    )
  )
posture_mu <- mean(pooled$posture_index, na.rm = TRUE)
posture_sd <- sd(pooled$posture_index,   na.rm = TRUE)

pooled <- pooled %>%
  mutate(
    posture_z = (posture_index - posture_mu) / posture_sd,
    warmth_z  = (warmth - warmth_mu) / warmth_sd,
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

# --- Build raw probe variables (numeric) ----------------------------------
for (s in ALL_PROBES) {
  var_name   <- paste0("opinion_change_", s)
  probe_name <- paste0("probe_", s)
  if (var_name %in% names(pooled)) {
    pooled[[probe_name]] <- as.numeric(pooled[[var_name]])
  } else {
    log_msg(sprintf("  WARNING: %s not found; probe_%s set to NA", var_name, s))
    pooled[[probe_name]] <- NA_real_
  }
}

# Coerce raw approach_* to 0/1 numeric (1 = endorsed)
# Original coding in pooled.rds is typically character/labelled; we standardise
# to integer {0, 1, NA} before handing to mice.
for (v in APPROACH_VARS) {
  x <- pooled[[v]]
  if (!is.numeric(x)) x <- as.numeric(x)
  pooled[[v]] <- ifelse(is.na(x), NA_integer_,
                        ifelse(x == 1, 1L, 0L))
}

# Ensure key categorical vars are factors with consistent levels
pooled$party3     <- factor(pooled$party3)
pooled$race_eth   <- factor(pooled$race_eth)
pooled$year       <- as.integer(pooled$year)
pooled$female     <- as.integer(pooled$female)
pooled$educ_college <- as.integer(pooled$educ_college)
pooled$blame_china     <- as.integer(pooled$blame_china)
pooled$blame_china_alt <- as.integer(pooled$blame_china_alt)

# ---------------------------------------------------------------------------
# Section 2: Build extended MI variable set
# ---------------------------------------------------------------------------

log_msg("")
log_msg("--- Section 2: Build extended MI variable set ---")

probe_cols <- paste0("probe_", ALL_PROBES)
probe_cols_avail <- intersect(probe_cols, names(pooled))

mi_vars_ext <- unique(c(
  # Phase 3
  "posture_z", "warmth_z", "blame_china", "blame_china_alt",
  "party3", "ideo5_clean", "educ_college", "age", "female",
  "race_eth", "newsint_attn", "exposure_index",
  # Phase 4 raw items (tool_config and n_approaches_10 derived post-imputation)
  APPROACH_VARS,
  # Phase 5 probes
  probe_cols_avail,
  # Auxiliary / stratification
  "year", "weight"
))
mi_vars_ext <- intersect(mi_vars_ext, names(pooled))

pooled_mi_ext <- pooled[, mi_vars_ext, drop = FALSE]

log_msg(sprintf("  MI variable set: %d variables", length(mi_vars_ext)))
log_msg("  Missingness (% NA) for each variable:")
for (v in mi_vars_ext) {
  n_miss <- sum(is.na(pooled_mi_ext[[v]]))
  if (n_miss > 0) {
    log_only(sprintf("    %-30s %6d (%5.2f%%)",
                     v, n_miss, 100 * n_miss / nrow(pooled_mi_ext)))
  }
}

# ---------------------------------------------------------------------------
# Section 3: Auto-detect imputation methods and run mice()
# ---------------------------------------------------------------------------

log_msg("")
log_msg("--- Section 3: Run mice() (m=10, seed=2024) ---")

mi_method <- character(ncol(pooled_mi_ext))
names(mi_method) <- names(pooled_mi_ext)

for (v in names(mi_method)) {
  vals <- pooled_mi_ext[[v]]
  if (is.factor(vals)) {
    mi_method[v] <- if (nlevels(vals) == 2) "logreg" else "polyreg"
  } else if (is.integer(vals) || is.numeric(vals)) {
    uniq <- unique(na.omit(vals))
    if (length(uniq) == 2 && all(uniq %in% c(0, 1))) {
      mi_method[v] <- "logreg"
    } else {
      mi_method[v] <- "pmm"
    }
  } else {
    mi_method[v] <- ""
  }
}
mi_method["year"]   <- ""
mi_method["weight"] <- ""

# probe items are mostly 1-7 Likert; pmm is appropriate
log_msg("  Imputation method counts:")
for (m in unique(mi_method)) {
  if (nzchar(m)) log_only(sprintf("    %-10s n=%d", m,
                                   sum(mi_method == m)))
}

t_mi0 <- Sys.time()
mi_out_ext <- tryCatch(
  mice(pooled_mi_ext,
       m       = M_IMP,
       method  = mi_method,
       seed    = 2024,
       printFlag = FALSE),
  error = function(e) {
    log_msg(sprintf("  mice() FAILED: %s", e$message))
    NULL
  }
)
t_mi <- as.numeric(difftime(Sys.time(), t_mi0, units = "secs"))
log_msg(sprintf("  mice() runtime: %.1f sec", t_mi))

if (is.null(mi_out_ext)) {
  stop("mice() failed — cannot proceed")
}

# Logged events (collinearity, constants etc.)
le <- mi_out_ext$loggedEvents
if (!is.null(le) && nrow(le) > 0) {
  log_msg(sprintf("  mice loggedEvents: %d entries (first 10 shown)",
                  nrow(le)))
  for (i in seq_len(min(10, nrow(le)))) {
    log_only(sprintf("    it=%d im=%d meth=%s out=%s",
                     le$it[i], le$im[i], le$meth[i], le$out[i]))
  }
} else {
  log_msg("  mice loggedEvents: none")
}

# ---------------------------------------------------------------------------
# Section 4: Rubin's rules helpers
# ---------------------------------------------------------------------------

#' Pool a scalar estimate across m imputations via Rubin's rules.
#' @param thetas m-vector of point estimates
#' @param vars   m-vector of within-imputation variances (SE^2)
#' @return list with pooled estimate, variance decomposition, df, t, p
rubin_pool <- function(thetas, vars) {
  m <- length(thetas)
  theta_bar <- mean(thetas)
  V_W <- mean(vars)
  V_B <- if (m > 1) sum((thetas - theta_bar)^2) / (m - 1) else 0
  Tvar <- V_W + (1 + 1 / m) * V_B
  se   <- sqrt(Tvar)
  # FMI (Rubin 1987, eq 3.1.10)
  fmi <- ((1 + 1 / m) * V_B) / Tvar
  # Barnard-Rubin df
  if (V_B > 0 && is.finite(V_B)) {
    df_old <- (m - 1) * (1 + V_W / ((1 + 1 / m) * V_B))^2
  } else {
    df_old <- Inf
  }
  tstat <- theta_bar / se
  pval  <- 2 * pt(-abs(tstat), df = df_old)
  list(est = theta_bar, se = se, V_W = V_W, V_B = V_B, T = Tvar,
       fmi = fmi, df = df_old, t = tstat, p = pval, m = m)
}

#' Pool a linear combination c'beta across imputations.
#' @param coefs list of coefficient vectors
#' @param vcovs list of variance-covariance matrices
#' @param cvec  contrast vector named by coefficient name
rubin_pool_linear <- function(coefs, vcovs, cvec) {
  # Align cvec to each fit (some may have dropped levels -> pad with 0)
  thetas <- numeric(length(coefs))
  vars   <- numeric(length(coefs))
  for (k in seq_along(coefs)) {
    b <- coefs[[k]]
    V <- vcovs[[k]]
    names_b <- names(b)
    c_aligned <- setNames(rep(0, length(b)), names_b)
    nm <- intersect(names(cvec), names_b)
    c_aligned[nm] <- cvec[nm]
    thetas[k] <- sum(c_aligned * b)
    vars[k]   <- as.numeric(t(c_aligned) %*% V %*% c_aligned)
  }
  rubin_pool(thetas, vars)
}

fmt_p <- function(p) {
  if (is.na(p)) return("--")
  if (p < 0.001) "<0.001" else sprintf("%.3f", p)
}
fmt4 <- function(x) ifelse(is.na(x), "--", sprintf("%.4f", x))

# ---------------------------------------------------------------------------
# Section 5: Phase 3 replication (sanity check vs existing app_tab_mi_robustness)
# ---------------------------------------------------------------------------

log_msg("")
log_msg("--- Section 5: Phase 3 replication (warmth_z:blame_china on posture_z) ---")

fmla_p3 <- posture_z ~ warmth_z * blame_china + factor(year) + party3 +
  ideo5_clean + educ_college + age + female + race_eth + newsint_attn +
  exposure_index

p3_coefs <- vector("list", M_IMP)
p3_vcovs <- vector("list", M_IMP)

for (i in seq_len(M_IMP)) {
  d_i <- complete(mi_out_ext, i)
  fit <- lm(fmla_p3, data = d_i, weights = weight)
  p3_coefs[[i]] <- coef(fit)
  p3_vcovs[[i]] <- vcov(fit)
}

cvec_p3 <- c("warmth_z:blame_china" = 1)
pool_p3 <- rubin_pool_linear(p3_coefs, p3_vcovs, cvec_p3)
log_msg(sprintf("  Phase 3 MI pooled: b=%.4f SE=%.4f (V_W=%.5f V_B=%.5f FMI=%.3f df=%.1f p=%s)",
                pool_p3$est, pool_p3$se, pool_p3$V_W, pool_p3$V_B,
                pool_p3$fmi, pool_p3$df, fmt_p(pool_p3$p)))
log_msg("  Reference (03_models.R na4_mi_result): b=0.1232 SE=0.0258")

# ---------------------------------------------------------------------------
# Section 6: Phase 4 MNL — fit multinom on each imputed dataset, pool
# ---------------------------------------------------------------------------

log_msg("")
log_msg("--- Section 6: Phase 4 MNL (tool_config) ---")

build_tool_config_vec <- function(san, act) {
  # 4-level factor ordered: Neither, Sanctions-only, Action-only, S+A
  tc <- case_when(
    san == 0 & act == 0 ~ "Neither",
    san == 1 & act == 0 ~ "Sanctions-only",
    san == 0 & act == 1 ~ "Action-only",
    san == 1 & act == 1 ~ "S+A",
    TRUE ~ NA_character_
  )
  factor(tc, levels = c("Neither", "Sanctions-only", "Action-only", "S+A"))
}

build_n_excl_vec <- function(dfi) {
  bins <- vapply(APPROACH_EXCL, function(v) as.integer(dfi[[v]] == 1),
                 integer(nrow(dfi)))
  rowSums(bins, na.rm = FALSE)
}

fmla_p4 <- tool_config ~ warmth_z * blame_china + party3 + ideo5_clean +
  educ_college + age + female + race_eth + newsint_attn + exposure_index +
  factor(year)

p4_coefs <- vector("list", M_IMP)
p4_vcovs <- vector("list", M_IMP)
p4_n     <- integer(M_IMP)
p4_convergence <- logical(M_IMP)

t_p40 <- Sys.time()
for (i in seq_len(M_IMP)) {
  d_i <- complete(mi_out_ext, i)
  d_i$tool_config <- build_tool_config_vec(d_i$approach_sanctions,
                                           d_i$approach_china_action)
  d_i$n_approaches_10 <- build_n_excl_vec(d_i)

  fit <- tryCatch(
    multinom(fmla_p4, data = d_i, weights = weight,
             trace = FALSE, Hess = TRUE, maxit = 300),
    error = function(e) { log_msg(sprintf("  multinom i=%d FAILED: %s", i, e$message)); NULL }
  )
  if (is.null(fit)) {
    p4_convergence[i] <- FALSE
    next
  }
  p4_convergence[i] <- fit$convergence == 0
  cmat <- coef(fit)                  # 3 equations x p params
  b    <- as.numeric(t(cmat))        # stack row by row
  names(b) <- paste(rep(rownames(cmat), each = ncol(cmat)),
                    rep(colnames(cmat), times = nrow(cmat)), sep = ":")
  p4_coefs[[i]] <- b
  p4_vcovs[[i]] <- vcov(fit)          # naming: "Equation:Param"
  # Align names: vcov uses "Sanctions-only:(Intercept)" etc. multinom::vcov
  # names are "Equation:coefname" which matches our manual build above.
  p4_n[i] <- nrow(d_i)
}
t_p4 <- as.numeric(difftime(Sys.time(), t_p40, units = "secs"))
log_msg(sprintf("  Phase 4 MNL runtime: %.1f sec  converged=%d/%d",
                t_p4, sum(p4_convergence), M_IMP))

# Focal contrasts on LOG-ODDS scale (ln-odds of equation j vs reference Neither):
#   (a) warmth_z:blame_china interaction in S+A equation
#   (b) blame effect at warmth=+1 in S+A eq:    b_blame + 1 * b_interaction
#   (c) same pair for Sanctions-only eq
p4_contrasts <- list(
  `SA_warmth:blame`       = c("S+A:warmth_z:blame_china" = 1),
  `SA_blame_at_warmth+1`  = c("S+A:blame_china" = 1, "S+A:warmth_z:blame_china" = 1),
  `SA_warmth_main`        = c("S+A:warmth_z" = 1),
  `Sonly_warmth:blame`    = c("Sanctions-only:warmth_z:blame_china" = 1),
  `Sonly_blame_at_warmth+1` = c("Sanctions-only:blame_china" = 1,
                                 "Sanctions-only:warmth_z:blame_china" = 1)
)

# Only use imputations that produced a fit
keep_p4 <- !vapply(p4_coefs, is.null, logical(1))

p4_pooled <- lapply(p4_contrasts, function(cv) {
  rubin_pool_linear(p4_coefs[keep_p4], p4_vcovs[keep_p4], cv)
})

for (nm in names(p4_pooled)) {
  pp <- p4_pooled[[nm]]
  log_msg(sprintf("  %-28s b=%.4f SE=%.4f FMI=%.3f df=%.1f p=%s",
                  nm, pp$est, pp$se, pp$fmi, pp$df, fmt_p(pp$p)))
}

# ---------------------------------------------------------------------------
# Section 7: Phase 5 probe contrasts
# ---------------------------------------------------------------------------

log_msg("")
log_msg("--- Section 7: Phase 5 probe contrasts (is_SA effect) ---")

# For each imputed dataset i:
#   subset to is_SA-defined rows (tool_config in {Sanctions-only, S+A})
#   fit lm per probe: probe_s ~ is_SA + n_excl + n_excl_miss + controls + year FE
#   store coef/vcov for is_SA

fmla_p5 <- function(probe_var) {
  as.formula(paste0(probe_var,
    " ~ is_SA + n_approaches_10 + n_approaches_10_miss + party3 + ",
    "ideo5_clean + educ_college + age + female + race_eth + newsint_attn + ",
    "exposure_index + factor(year)"))
}

# Storage: for each probe, list of coef vectors and vcov matrices across imps
p5_store <- setNames(vector("list", length(ALL_PROBES)), ALL_PROBES)
for (s in ALL_PROBES) {
  p5_store[[s]] <- list(coefs = vector("list", M_IMP),
                        vcovs = vector("list", M_IMP),
                        n     = integer(M_IMP))
}

t_p50 <- Sys.time()
for (i in seq_len(M_IMP)) {
  d_i <- complete(mi_out_ext, i)
  d_i$tool_config <- build_tool_config_vec(d_i$approach_sanctions,
                                           d_i$approach_china_action)
  d_i$n_approaches_10      <- build_n_excl_vec(d_i)
  d_i$n_approaches_10_miss <- 0L   # after MI, no missingness in approach_*

  d_sa <- d_i[d_i$tool_config %in% c("Sanctions-only", "S+A"), , drop = FALSE]
  d_sa$is_SA <- as.integer(d_sa$tool_config == "S+A")

  for (s in ALL_PROBES) {
    pv <- paste0("probe_", s)
    if (!pv %in% names(d_sa)) next
    fit <- tryCatch(
      lm(fmla_p5(pv), data = d_sa, weights = weight),
      error = function(e) NULL
    )
    if (is.null(fit)) next
    p5_store[[s]]$coefs[[i]] <- coef(fit)
    p5_store[[s]]$vcovs[[i]] <- vcov(fit)
    p5_store[[s]]$n[i]       <- nobs(fit)
  }
}
t_p5 <- as.numeric(difftime(Sys.time(), t_p50, units = "secs"))
log_msg(sprintf("  Phase 5 per-probe fits runtime: %.1f sec", t_p5))

# Pool is_SA coefficient for each probe
p5_pooled <- setNames(vector("list", length(ALL_PROBES)), ALL_PROBES)
for (s in ALL_PROBES) {
  keep <- !vapply(p5_store[[s]]$coefs, is.null, logical(1))
  cvec <- c("is_SA" = 1)
  p5_pooled[[s]] <- rubin_pool_linear(
    p5_store[[s]]$coefs[keep],
    p5_store[[s]]$vcovs[keep],
    cvec
  )
  pp <- p5_pooled[[s]]
  log_msg(sprintf("  probe_%s: Delta=%.4f SE=%.4f FMI=%.3f p=%s",
                  s, pp$est, pp$se, pp$fmi, fmt_p(pp$p)))
}

# Composite boundary: average of is_SA coefficients across 7 non-target probes,
# pooled by creating a per-imputation composite statistic.
# Within each imputation i: theta_i = mean(b_is_SA_s for s in BOUNDARY_PROBES)
# Var_W within i: SEs not independent across probes (same respondents), but
#   a sensible design-neutral approximation is
#   V_W_i = (1/k^2) * sum(Var(b_is_SA_s) within i)   (ignores covariance --
#   documented in the log; referees asking for MI on probe contrasts care
#   primarily about the between-imputation component.)
k_b <- length(BOUNDARY_PROBES)
composite_thetas <- numeric(M_IMP)
composite_vars   <- numeric(M_IMP)
for (i in seq_len(M_IMP)) {
  bs  <- numeric(k_b)
  vs  <- numeric(k_b)
  ok  <- TRUE
  for (j in seq_along(BOUNDARY_PROBES)) {
    s <- BOUNDARY_PROBES[j]
    b <- p5_store[[s]]$coefs[[i]]
    V <- p5_store[[s]]$vcovs[[i]]
    if (is.null(b) || !("is_SA" %in% names(b))) { ok <- FALSE; break }
    bs[j] <- b["is_SA"]
    vs[j] <- V["is_SA", "is_SA"]
  }
  if (!ok) { composite_thetas[i] <- NA_real_; composite_vars[i] <- NA_real_; next }
  composite_thetas[i] <- mean(bs)
  composite_vars[i]   <- sum(vs) / (k_b^2)   # independence approx; see note above
}
keep_comp <- !is.na(composite_thetas)
pool_composite <- rubin_pool(composite_thetas[keep_comp],
                              composite_vars[keep_comp])
log_msg(sprintf("  Composite boundary: Delta=%.4f SE=%.4f FMI=%.3f p=%s",
                pool_composite$est, pool_composite$se, pool_composite$fmi,
                fmt_p(pool_composite$p)))

# Omnibus divergence: Delta_d - composite_boundary, pooled
divergence_thetas <- numeric(M_IMP)
divergence_vars   <- numeric(M_IMP)
for (i in seq_len(M_IMP)) {
  b_d <- p5_store[[TARGET_PROBE]]$coefs[[i]]
  V_d <- p5_store[[TARGET_PROBE]]$vcovs[[i]]
  if (is.null(b_d) || is.na(composite_thetas[i])) {
    divergence_thetas[i] <- NA_real_; divergence_vars[i] <- NA_real_; next
  }
  divergence_thetas[i] <- b_d["is_SA"] - composite_thetas[i]
  # approx variance = Var(b_d) + Var(composite) (ignoring cross-probe covariance)
  divergence_vars[i]   <- V_d["is_SA", "is_SA"] + composite_vars[i]
}
keep_div <- !is.na(divergence_thetas)
pool_divergence <- rubin_pool(divergence_thetas[keep_div],
                               divergence_vars[keep_div])
log_msg(sprintf("  Omnibus divergence (Delta_d - composite): %.4f SE=%.4f FMI=%.3f p=%s",
                pool_divergence$est, pool_divergence$se, pool_divergence$fmi,
                fmt_p(pool_divergence$p)))

# ---------------------------------------------------------------------------
# Section 8: Complete-case reference values (for side-by-side tables)
# ---------------------------------------------------------------------------

log_msg("")
log_msg("--- Section 8: Complete-case reference estimates ---")

# Phase 3 CC: pull from phase3_objects.rds
cc_p3 <- tryCatch({
  po3 <- readRDS(here("prism", "tables", "phase3_objects.rds"))
  list(est = po3$na4_mi_result$cc_est,
       se  = po3$na4_mi_result$cc_se,
       p   = po3$na4_mi_result$cc_p,
       n   = po3$na4_mi_result$cc_n)
}, error = function(e) list(est = NA, se = NA, p = NA, n = NA))

# Phase 4 CC: refit on complete cases
pooled$tool_config <- build_tool_config_vec(pooled$approach_sanctions,
                                            pooled$approach_china_action)
pooled$n_approaches_10 <- build_n_excl_vec(pooled)

cc_p4_fit <- tryCatch(
  multinom(fmla_p4, data = pooled, weights = weight,
           trace = FALSE, Hess = TRUE, maxit = 300),
  error = function(e) { log_msg(sprintf("  CC multinom failed: %s", e$message)); NULL }
)

extract_mnl_contrast_cc <- function(fit, cvec) {
  if (is.null(fit)) return(list(est = NA, se = NA, p = NA, n = NA))
  cmat <- coef(fit)
  b <- as.numeric(t(cmat))
  names(b) <- paste(rep(rownames(cmat), each = ncol(cmat)),
                    rep(colnames(cmat), times = nrow(cmat)), sep = ":")
  V <- vcov(fit)
  nm <- intersect(names(cvec), names(b))
  c_al <- setNames(rep(0, length(b)), names(b))
  c_al[nm] <- cvec[nm]
  est <- sum(c_al * b)
  se  <- sqrt(as.numeric(t(c_al) %*% V %*% c_al))
  z   <- est / se
  p   <- 2 * pnorm(-abs(z))
  list(est = est, se = se, p = p, n = nobs(fit))
}

cc_p4 <- lapply(p4_contrasts, extract_mnl_contrast_cc, fit = cc_p4_fit)

# Phase 5 CC: refit per probe
pooled$n_approaches_10_miss <- as.integer(
  is.na(pooled$n_approaches_10) |
    rowSums(is.na(pooled[, APPROACH_EXCL, drop = FALSE])) > 0
)
pooled_sa_cc <- pooled[pooled$tool_config %in% c("Sanctions-only", "S+A"), ]
pooled_sa_cc$is_SA <- as.integer(pooled_sa_cc$tool_config == "S+A")

cc_p5 <- setNames(vector("list", length(ALL_PROBES)), ALL_PROBES)
for (s in ALL_PROBES) {
  pv <- paste0("probe_", s)
  if (!pv %in% names(pooled_sa_cc)) {
    cc_p5[[s]] <- list(est = NA, se = NA, p = NA, n = NA); next
  }
  fit <- tryCatch(lm(fmla_p5(pv), data = pooled_sa_cc, weights = weight),
                  error = function(e) NULL)
  if (is.null(fit)) {
    cc_p5[[s]] <- list(est = NA, se = NA, p = NA, n = NA); next
  }
  sm <- coef(summary(fit))
  cc_p5[[s]] <- list(
    est = sm["is_SA", "Estimate"],
    se  = sm["is_SA", "Std. Error"],
    p   = sm["is_SA", "Pr(>|t|)"],
    n   = nobs(fit)
  )
}

# ---------------------------------------------------------------------------
# Section 9: Build output tables
# ---------------------------------------------------------------------------

log_msg("")
log_msg("--- Section 9: Build output tables ---")

# --- Table A: Diagnostics (V_W, V_B, T, FMI, df, pooled est, SE, t, p, N) ---

diag_rows <- list()
add_diag <- function(label, pp, n) {
  diag_rows[[length(diag_rows) + 1]] <<- data.frame(
    Parameter = label,
    m         = pp$m,
    V_W       = pp$V_W,
    V_B       = pp$V_B,
    T_total   = pp$T,
    FMI       = pp$fmi,
    df        = pp$df,
    estimate  = pp$est,
    SE        = pp$se,
    t         = pp$t,
    p         = pp$p,
    N         = n,
    stringsAsFactors = FALSE
  )
}

add_diag("Phase 3: warmth_z x blame_china (posture_z)", pool_p3,
        round(mean(vapply(seq_len(M_IMP), function(i)
          sum(complete.cases(complete(mi_out_ext, i)[,
            c("posture_z", "warmth_z", "blame_china")])), integer(1)))))
add_diag("Phase 4: S+A warmth_z x blame_china (ln-odds)",
        p4_pooled$`SA_warmth:blame`, round(mean(p4_n[p4_n > 0])))
add_diag("Phase 4: blame effect at warmth=+1 on S+A ln-odds",
        p4_pooled$`SA_blame_at_warmth+1`, round(mean(p4_n[p4_n > 0])))
add_diag("Phase 4: S-only warmth_z x blame_china (ln-odds)",
        p4_pooled$`Sonly_warmth:blame`, round(mean(p4_n[p4_n > 0])))
add_diag("Phase 4: S-only blame effect at warmth=+1",
        p4_pooled$`Sonly_blame_at_warmth+1`, round(mean(p4_n[p4_n > 0])))
add_diag(sprintf("Phase 5: Delta_%s (target probe)", TARGET_PROBE),
        p5_pooled[[TARGET_PROBE]],
        round(mean(p5_store[[TARGET_PROBE]]$n[p5_store[[TARGET_PROBE]]$n > 0])))
add_diag("Phase 5: Composite boundary contrast",
        pool_composite,
        round(mean(p5_store[[BOUNDARY_PROBES[1]]]$n[p5_store[[BOUNDARY_PROBES[1]]]$n > 0])))
add_diag("Phase 5: Omnibus divergence (Delta_d - composite)",
        pool_divergence,
        round(mean(p5_store[[TARGET_PROBE]]$n[p5_store[[TARGET_PROBE]]$n > 0])))

diag_df <- do.call(rbind, diag_rows)

# Write CSV
write.csv(diag_df, file.path(out_dir, "app_tab_mi_diagnostics.csv"),
          row.names = FALSE)

# Write LaTeX (bare tabular + threeparttable)
render_diag_tex <- function(df) {
  hdr <- paste(
    "\\begin{threeparttable}",
    "\\begin{tabular}{lrrrrrrrrrrr}",
    "\\toprule",
    paste("Parameter & $m$ & $V_W$ & $V_B$ & $T$ & FMI & df &",
          "Est. & SE & $t$ & $p$ & $N$ \\\\"),
    "\\midrule",
    sep = "\n"
  )
  rows <- apply(df, 1, function(r) {
    sprintf("%s & %s & %.5f & %.5f & %.5f & %.3f & %.1f & %.4f & %.4f & %.2f & %s & %s \\\\",
            r["Parameter"], r["m"],
            as.numeric(r["V_W"]), as.numeric(r["V_B"]),
            as.numeric(r["T_total"]), as.numeric(r["FMI"]),
            as.numeric(r["df"]), as.numeric(r["estimate"]),
            as.numeric(r["SE"]), as.numeric(r["t"]),
            fmt_p(as.numeric(r["p"])), r["N"])
  })
  ftr <- paste("\\bottomrule", "\\end{tabular}",
               "\\end{threeparttable}", sep = "\n")
  paste(hdr, paste(rows, collapse = "\n"), ftr, sep = "\n")
}
writeLines(render_diag_tex(diag_df),
           file.path(out_dir, "app_tab_mi_diagnostics.tex"))

# --- Table B: Phase 4 MI vs CC comparison ---

phase4_rows <- list()
p4_labels <- c(
  `SA_warmth:blame`        = "S+A: warmth_z x blame_china",
  `SA_blame_at_warmth+1`   = "S+A: blame effect at warmth=+1",
  `SA_warmth_main`         = "S+A: warmth_z main effect",
  `Sonly_warmth:blame`     = "S-only: warmth_z x blame_china",
  `Sonly_blame_at_warmth+1`= "S-only: blame effect at warmth=+1"
)
for (nm in names(p4_labels)) {
  cc <- cc_p4[[nm]]; mi <- p4_pooled[[nm]]
  phase4_rows[[length(phase4_rows) + 1]] <- data.frame(
    Parameter = p4_labels[[nm]],
    CC_est = cc$est, CC_SE = cc$se, CC_p = cc$p, CC_N = cc$n,
    MI_est = mi$est, MI_SE = mi$se, MI_p = mi$p, MI_FMI = mi$fmi,
    stringsAsFactors = FALSE
  )
}
phase4_df <- do.call(rbind, phase4_rows)
write.csv(phase4_df, file.path(out_dir, "app_tab_mi_phase4.csv"),
          row.names = FALSE)

render_p4_tex <- function(df) {
  hdr <- paste(
    "\\begin{threeparttable}",
    "\\begin{tabular}{lrrrrrrrr}",
    "\\toprule",
    "& \\multicolumn{4}{c}{Complete case} & \\multicolumn{4}{c}{Multiple imputation (m=10)} \\\\",
    "\\cmidrule(lr){2-5} \\cmidrule(lr){6-9}",
    "Parameter & Est. & SE & $p$ & $N$ & Est. & SE & $p$ & FMI \\\\",
    "\\midrule",
    sep = "\n"
  )
  rows <- apply(df, 1, function(r) {
    sprintf("%s & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
            r["Parameter"],
            fmt4(as.numeric(r["CC_est"])),
            fmt4(as.numeric(r["CC_SE"])),
            fmt_p(as.numeric(r["CC_p"])),
            r["CC_N"],
            fmt4(as.numeric(r["MI_est"])),
            fmt4(as.numeric(r["MI_SE"])),
            fmt_p(as.numeric(r["MI_p"])),
            sprintf("%.3f", as.numeric(r["MI_FMI"])))
  })
  ftr <- paste("\\bottomrule", "\\end{tabular}",
               "\\end{threeparttable}", sep = "\n")
  paste(hdr, paste(rows, collapse = "\n"), ftr, sep = "\n")
}
writeLines(render_p4_tex(phase4_df),
           file.path(out_dir, "app_tab_mi_phase4.tex"))

# --- Table C: Phase 5 MI vs CC comparison ---

phase5_rows <- list()
for (s in ALL_PROBES) {
  cc <- cc_p5[[s]]; mi <- p5_pooled[[s]]
  phase5_rows[[length(phase5_rows) + 1]] <- data.frame(
    Parameter = sprintf("Delta_%s (probe %s)", s, s),
    CC_est = cc$est, CC_SE = cc$se, CC_p = cc$p, CC_N = cc$n,
    MI_est = mi$est, MI_SE = mi$se, MI_p = mi$p, MI_FMI = mi$fmi,
    stringsAsFactors = FALSE
  )
}
# Composite and divergence rows (no CC reference -- leave blank)
phase5_rows[[length(phase5_rows) + 1]] <- data.frame(
  Parameter = "Composite boundary (mean of 7 non-target)",
  CC_est = NA, CC_SE = NA, CC_p = NA, CC_N = NA,
  MI_est = pool_composite$est, MI_SE = pool_composite$se,
  MI_p = pool_composite$p, MI_FMI = pool_composite$fmi,
  stringsAsFactors = FALSE
)
phase5_rows[[length(phase5_rows) + 1]] <- data.frame(
  Parameter = "Omnibus divergence (Delta_d - composite)",
  CC_est = NA, CC_SE = NA, CC_p = NA, CC_N = NA,
  MI_est = pool_divergence$est, MI_SE = pool_divergence$se,
  MI_p = pool_divergence$p, MI_FMI = pool_divergence$fmi,
  stringsAsFactors = FALSE
)
phase5_df <- do.call(rbind, phase5_rows)
write.csv(phase5_df, file.path(out_dir, "app_tab_mi_phase5.csv"),
          row.names = FALSE)

render_p5_tex <- function(df) {
  hdr <- paste(
    "\\begin{threeparttable}",
    "\\begin{tabular}{lrrrrrrrr}",
    "\\toprule",
    "& \\multicolumn{4}{c}{Complete case} & \\multicolumn{4}{c}{Multiple imputation (m=10)} \\\\",
    "\\cmidrule(lr){2-5} \\cmidrule(lr){6-9}",
    "Parameter & Est. & SE & $p$ & $N$ & Est. & SE & $p$ & FMI \\\\",
    "\\midrule",
    sep = "\n"
  )
  fmt_or_dash <- function(x, fn) if (is.na(x)) "--" else fn(x)
  rows <- apply(df, 1, function(r) {
    sprintf("%s & %s & %s & %s & %s & %s & %s & %s & %s \\\\",
            r["Parameter"],
            fmt_or_dash(as.numeric(r["CC_est"]), function(x) sprintf("%.4f", x)),
            fmt_or_dash(as.numeric(r["CC_SE"]),  function(x) sprintf("%.4f", x)),
            fmt_or_dash(as.numeric(r["CC_p"]),   fmt_p),
            if (is.na(r["CC_N"])) "--" else r["CC_N"],
            fmt4(as.numeric(r["MI_est"])),
            fmt4(as.numeric(r["MI_SE"])),
            fmt_p(as.numeric(r["MI_p"])),
            sprintf("%.3f", as.numeric(r["MI_FMI"])))
  })
  ftr <- paste("\\bottomrule", "\\end{tabular}",
               "\\end{threeparttable}", sep = "\n")
  paste(hdr, paste(rows, collapse = "\n"), ftr, sep = "\n")
}
writeLines(render_p5_tex(phase5_df),
           file.path(out_dir, "app_tab_mi_phase5.tex"))

# ---------------------------------------------------------------------------
# Section 10: Save objects and log
# ---------------------------------------------------------------------------

phase7_objects <- list(
  mi_out_ext      = mi_out_ext,
  mi_method       = mi_method,
  mi_vars_ext     = mi_vars_ext,
  pool_p3         = pool_p3,
  p4_pooled       = p4_pooled,
  p4_convergence  = p4_convergence,
  p5_pooled       = p5_pooled,
  pool_composite  = pool_composite,
  pool_divergence = pool_divergence,
  cc_p3           = cc_p3,
  cc_p4           = cc_p4,
  cc_p5           = cc_p5,
  diag_df         = diag_df,
  phase4_df       = phase4_df,
  phase5_df       = phase5_df,
  session_info    = sessionInfo()
)
saveRDS(phase7_objects, file.path(out_dir, "phase7_objects.rds"))

t_total <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
log_msg("")
log_msg("=====================================================================")
log_msg(sprintf("  Total runtime: %.1f sec", t_total))
log_msg(sprintf("  Finished: %s", format(Sys.time())))
log_msg("=====================================================================")

writeLines(log_env$lines, file.path(out_dir, "phase7_log.txt"))
