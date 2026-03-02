# ── Backend contract ───────────────────────────────────────────────────────────
#
# Every backend pair must implement:
#
#   .fixed_<name>(model, conf_level, prep) → besd_tidy_result
#   .re_<name>(model, conf_level, prep)    → besd_tidy_result (0-row ok)
#
# All backend functions must return via .as_tidy_result(), which stamps the
# class and validates the schema. Errors surface at the backend that broke the
# contract, not silently downstream.
#
# Currently supported:
#   glm       → .fixed_profile() / no random effects
#   glmerMod  → .fixed_glmer()   / .re_lme4()
#   clm       → .fixed_profile() / no random effects
#   clmm      → .fixed_profile() / .re_ordinal()
#   brmsfit   → .fixed_brms()    / .re_brms()
#

.TIDY_EFFECT_TYPES <- c("fixed", "random")
.TIDY_PARAM_TYPES  <- c("slope", "intercept", "cutpoint", 
                        "sd", "cor", "ranef", "ranef_deviation")

# Validate and class-stamp a completed tidy tibble. Called as the final expression
# in every .fixed_*() and .re_*() backend. Errors immediately if contract broken
# rather than propagating bad outputs silently.
.as_tidy_result <- function(x) {
  required <- c("variable", "level", "country", "estimate", "std.error",
                "lower", "upper", "rhat", "ess", "effect_type", "param_type")
  chr_cols <- c("variable", "level", "country", "effect_type", "param_type")
  num_cols <- c("estimate", "std.error", "lower", "upper", "rhat", "ess")
  
  miss <- setdiff(required, names(x))
  if (length(miss)) .stopf("Tidy backend missing column(s): %s", .pastec(miss))
  for (col in chr_cols) {
    if (!is.character(x[[col]])) {
      .stopf("Tidy backend column `%s` must be character, got %s", col, class(x[[col]]))
    }
  }
  for (col in num_cols) {
    if (!is.numeric(x[[col]])) {
      .stopf("Tidy backend column `%s` must be numeric, got %s", col, class(x[[col]]))
    }
  }
  
  bad_et <- setdiff(unique(x$effect_type[!is.na(x$effect_type)]), .TIDY_EFFECT_TYPES)
  bad_pt <- setdiff(unique(x$param_type[!is.na(x$param_type)]),   .TIDY_PARAM_TYPES)
  if (length(bad_et)) .stopf("Unknown effect_type value(s): %s", .pastec(bad_et))
  if (length(bad_pt)) .stopf("Unknown param_type value(s): %s",  .pastec(bad_pt))
  
  structure(x, class = c("besd_tidy_result", class(x)))
}


# ── tidy_model() ───────────────────────────────────────────────────────────────

#' Tidy BeSD model outputs
#'
#' @param fit A `besd_regress()` output.
#' @param conf_level CI/CrI level.
#' @param include_random If TRUE, include SD/cor and group-level effects.
#' @param include_cor If TRUE (and include_random=TRUE), include correlation
#'   parameters (cor_*). Set FALSE to drop cor parameters from the tidy output.
#' @param exponentiate If TRUE, exponentiate slopes/intercepts (OR scale).
#' @param return_baseline If TRUE, attach baseline (reference) category info.
#'   Adds column `baseline` and attaches a `baseline` attribute with one
#'   one row per (variable, country).
#' @export
tidy_model <- function(fit, conf_level = 0.95, include_random = FALSE,
                       include_cor = FALSE, exponentiate = FALSE,
                       return_baseline = TRUE) {
  
  if (!inherits(fit, c("besd_fit", "besd_fit_by_country"))) {
    .stopf("`fit` must be a besd_fit or besd_fit_by_country object.")
  }
  
  # Guard: prep must be present.
  if (is.null(fit$prep)) .stopf("This fit object does not contain a `prep` field.")
  
  .assert_is_scalar_number(conf_level, "conf_level")
  
  prep          <- fit$prep
  is_by_country <- inherits(fit, "besd_fit_by_country")
  countries     <- if (is_by_country) prep$countries else ""
  outcomes      <- prep$outcomes
  
  rows <- list()
  for (cc in countries %||% "") {
    fits_cc <- if (is_by_country) fit$fits[[cc]] else fit$fits
    if (is.null(fits_cc) || !length(fits_cc)) next
    
    for (yy in outcomes) {
      m <- fits_cc[[yy]]
      if (is.null(m)) next
      
      td <- .tidy_one_model(model = m, conf_level = conf_level,
                            include_random = include_random, prep = prep, 
                            outcome_col = yy)
      if (!nrow(td)) next
      
      if (is_by_country) {
        td$country <- ifelse(is.na(td$country) | td$country == "", cc, td$country)
      }

      rows[[length(rows) + 1]] <- td
    }
  }
  
  if (!length(rows)) {
    out <- .empty_tidy(include_baseline = isTRUE(return_baseline))
    if (isTRUE(return_baseline)) attr(out, "baseline") <- .besd_baselines(fit)
    return(out)
  }
  
  out <- dplyr::bind_rows(rows)
  
  if (isTRUE(include_random) && !isTRUE(include_cor) && nrow(out)) {
    out <- out[out$param_type != "cor", , drop = FALSE]
  }
  
  out <- dplyr::select(
    out, outcome, item, variable, level, country,
    estimate, std.error, lower, upper, rhat, ess, effect_type, param_type
  )
  
  if (isTRUE(return_baseline)) {
    base_tbl <- .besd_baselines(fit)
    out <- .attach_baselines(out, base_tbl)
    attr(out, "baseline") <- base_tbl
  }
  
  # Note: exponentiation should not be used if the package is ever extended to
  # include normal-likelihood outcomes or continuous level-2 covariates.
  .apply_exp(out, exponentiate)
}


# ── Orchestrator ───────────────────────────────────────────────────────────────

# Orchestrate fixed + random tidying for one model object. Dispatch is by
# model class inside .backend_fixed() and .backend_re();
.tidy_one_model <- function(model, conf_level, include_random, prep, outcome_col) {
  fitted_terms <- attr(model, "besd_fitted_terms")
  if (!is.null(fitted_terms)) {
    prep$coef_lookup <- .build_coef_lookup(prep, fitted_terms)
  }
  out <- .backend_fixed(model, conf_level, prep)
  .warn_undecoded_terms(out, model)
  if (isTRUE(include_random)) {
    out <- dplyr::bind_rows(out, .backend_re(model, conf_level, prep))
  }
  
  mc_map  <- prep$mc_label_map %||% list()
  mc_lev  <- mc_map[[outcome_col]] %||% NA_character_
  base_id <- if (!is.na(mc_lev)) sub("_mc_.*$", "", outcome_col) else outcome_col
  out$outcome <- base_id
  out$item    <- if (!is.na(mc_lev)) mc_lev else .outcome_item_label(base_id, prep$dict)
  out
}


# ── Dispatchers ────────────────────────────────────────────────────────────────

# Dispatch fixed-effect extraction to the appropriate backend by model class.
.backend_fixed <- function(model, conf_level, prep) {
  if (inherits(model, "brmsfit"))  return(.fixed_brms(model, conf_level, prep))
  if (inherits(model, "glmerMod")) return(.fixed_glmer(model, conf_level, prep))
  # glm, clm, and clmm all share the same coef(summary()) / confint() interface.
  if (inherits(model, c("glm", "clm", "clmm"))) {
    return(.fixed_profile(model, conf_level, prep))
  }
  .stopf(
    "No fixed-effect tidying backend for model class: %s",
    paste(class(model), collapse = ", ")
  )
}

# Dispatch random-effect extraction to the appropriate backend by model class.
# For models with no random effects (glm, clm), return a valid 0-row result.
.backend_re <- function(model, conf_level, prep) {
  if (inherits(model, "brmsfit"))  return(.re_brms(model, conf_level, prep))
  if (inherits(model, "merMod"))   return(.re_lme4(model, conf_level, prep))
  if (inherits(model, "clmm"))     return(.re_ordinal(model, conf_level, prep))
  # glm and clm have no random structure: return a valid empty result.
  .as_tidy_result(.make_tidy_row()[0L, ])
}


# ── CI helpers ─────────────────────────────────────────────────────────────────

# Wald CI: z * SE. Used for glmerMod (profile CIs prohibitively slow) and as
# the fallback when .ci_profile() fails or is not applicable.
.ci_wald <- function(estimate, std_error, conf_level) {
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  list(lower = estimate - z * std_error, upper = estimate + z * std_error)
}

# Profile-likelihood CI via confint(). Falls back silently to Wald on failure.
# Used for glm, clm, and clmm, where profile CIs are practical to compute.
.ci_profile <- function(model, estimate, std_error, conf_level) {
  ci <- tryCatch(
    suppressMessages(
      stats::confint(model, level = conf_level, method = "profile")
    ),
    error   = function(e) NULL,
    warning = function(w) NULL
  )
  if (!is.null(ci) && is.matrix(ci) && nrow(ci) == length(estimate)) {
    return(list(lower = as.numeric(ci[, 1]), upper = as.numeric(ci[, 2])))
  }
  .ci_wald(estimate, std_error, conf_level)
}


# ── Fixed-effect backends ──────────────────────────────────────────────────────

# glm / clm / clmm: coef(summary()) extraction with profile-likelihood CIs
# (Wald fallback). All three use the same coef layout and confint() interface.
.fixed_profile <- function(model, conf_level, prep) {
  coefs     <- stats::coef(summary(model))
  term      <- rownames(coefs)
  estimate  <- as.numeric(coefs[, "Estimate"])
  std_error <- as.numeric(coefs[, "Std. Error"])
  ci        <- .ci_profile(model, estimate, std_error, conf_level)
  info      <- .parse_terms(term, prep)
  .as_tidy_result(.make_tidy_row(
    variable   = info$variable,
    level      = info$level,
    country    = info$country,
    estimate   = estimate,
    std.error  = std_error,
    lower      = ci$lower,
    upper      = ci$upper,
    param_type = info$param_type
  ))
}

# glmerMod: lme4::fixef() extraction with Wald CIs. Profile CIs are
# prohibitively slow for multilevel models and are not used here.
.fixed_glmer <- function(model, conf_level, prep) {
  estimate  <- as.numeric(lme4::fixef(model))
  term      <- names(lme4::fixef(model))
  std_error <- sqrt(diag(as.matrix(stats::vcov(model))))
  ci        <- .ci_wald(estimate, std_error, conf_level)
  info      <- .parse_terms(term, prep)
  .as_tidy_result(.make_tidy_row(
    variable   = info$variable,
    level      = info$level,
    country    = info$country,
    estimate   = estimate,
    std.error  = std_error,
    lower      = ci$lower,
    upper      = ci$upper,
    param_type = info$param_type
  ))
}

# brmsfit: posterior means/SDs and equal-tailed CrIs via brms::fixef().
# Also extracts ordinal cutpoints from posterior_summary(), which fixef() omits.
.fixed_brms <- function(model, conf_level, prep) {
  .require_pkg("brms",      "for Bayesian tidying")
  .require_pkg("posterior", "for diagnostics")
  
  probs    <- c((1 - conf_level) / 2, 1 - (1 - conf_level) / 2)
  fixed    <- brms::fixef(model, probs = probs)
  qc       <- .qcols(colnames(fixed))
  term     <- rownames(fixed)
  term[term == "(Intercept)"] <- "Intercept"
  diag_tbl <- .get_diag(model)
  bvars    <- .brms_fixef_draw_names(term)
  info     <- .parse_terms(term, prep)
  
  out <- .make_tidy_row(
    variable   = info$variable,
    level      = info$level,
    country    = info$country,
    estimate   = as.numeric(fixed[, "Estimate"]),
    std.error  = as.numeric(fixed[, "Est.Error"]),
    lower      = as.numeric(fixed[, qc$lower]),
    upper      = as.numeric(fixed[, qc$upper]),
    rhat       = .lookup_diag(diag_tbl, bvars, "rhat"),
    ess        = .lookup_diag(diag_tbl, bvars, "ess"),
    param_type = info$param_type
  )
  
  # Ordinal cutpoints live in posterior_summary() but not fixef().
  post <- brms::posterior_summary(model, probs = probs)
  cp   <- rownames(post)[grepl("^Intercept\\[[0-9]+\\]$", rownames(post))]
  if (length(cp)) {
    q2  <- .qcols(colnames(post))
    out <- dplyr::bind_rows(out, .make_tidy_row(
      estimate   = as.numeric(post[cp, "Estimate"]),
      std.error  = as.numeric(post[cp, "Est.Error"]),
      lower      = as.numeric(post[cp, q2$lower]),
      upper      = as.numeric(post[cp, q2$upper]),
      rhat       = .lookup_diag(diag_tbl, cp, "rhat"),
      ess        = .lookup_diag(diag_tbl, cp, "ess"),
      param_type = "cutpoint"
    ))
  }
  .as_tidy_result(out)
}

# Placeholder for INLA fixed-effect tidying.
.fixed_inla <- function(model, conf_level, prep) {
  .stopf("INLA fixed-effect tidying is not yet implemented. ")
}


# ── Random-effect backends ─────────────────────────────────────────────────────

# merMod (glmerMod): extract variance components and conditional modes via lme4.
.re_lme4 <- function(model, conf_level, prep) {
  vc      <- lme4::VarCorr(model)
  re_list <- lme4::ranef(model, condVar = TRUE)
  .as_tidy_result(.re_freq_from_parts(vc, re_list, "lme4", conf_level, prep))
}

# clmm: extract variance components and conditional modes via ordinal.
# Structure is identical to lme4 except for the extraction calls.
.re_ordinal <- function(model, conf_level, prep) {
  .require_pkg("ordinal", "for random effects in clmm models")
  vc      <- ordinal::VarCorr(model)
  re_list <- ordinal::ranef(model, condVar = TRUE)
  .as_tidy_result(.re_freq_from_parts(vc, re_list, "ordinal", conf_level, prep))
}

# Shared extraction logic for both lme4 and ordinal mixed models.
# Handles SD/correlation parameters from VarCorr and conditional modes from ranef.
# `pkg` controls whether cor_mat is read from the lme4 correlation attribute or
# the off-diagonal of the ordinal variance matrix.
.re_freq_from_parts <- function(vc, re_list, pkg, conf_level, prep) {
  z    <- stats::qnorm(1 - (1 - conf_level) / 2)
  rows <- list()
  
  # 1) SD and correlation parameters per grouping factor.
  for (grp in names(vc)) {
    m <- vc[[grp]]
    if (is.data.frame(m)) m <- as.matrix(m)
    if (!is.matrix(m) || nrow(m) < 1) next
    
    sds     <- attr(m, "stddev") %||% .safe_sqrt(diag(m))
    tn      <- names(sds) %||% colnames(m) %||% paste0("V", seq_along(sds))
    cor_mat <- if (pkg == "lme4") attr(m, "correlation") else NULL
    
    for (i in seq_along(sds)) {
      vl <- .coef_var_level(tn[[i]], prep)
      rows[[length(rows) + 1]] <- .make_tidy_row(
        variable    = grp,
        level       = vl$level,
        estimate    = as.numeric(sds[[i]]),
        param_type  = "sd",
        effect_type = "random"
      )
    }
    
    if (length(sds) > 1) {
      for (i in 1:(length(sds) - 1)) {
        for (j in (i + 1):length(sds)) {
          est_cor <- if (pkg == "lme4") cor_mat[i, j] else m[i, j]
          rows[[length(rows) + 1]] <- .make_tidy_row(
            variable    = grp,
            estimate    = as.numeric(est_cor),
            param_type  = "cor",
            effect_type = "random"
          )
        }
      }
    }
  }
  
  # 2) Conditional modes with approximate SEs from postVar / condVar.
  # CIs are z * SE — the conditional variances are themselves Laplace
  # approximations, so profile CIs would not meaningfully improve accuracy.
  for (grp in names(re_list)) {
    df_re <- re_list[[grp]]
    rn    <- rownames(df_re)
    cn    <- colnames(df_re)
    
    # lme4 layout: [coef, coef, level] 3D array; ordinal m.fixeday use condVar.
    pv     <- attr(df_re, "postVar") %||% attr(df_re, "condVar")
    se_mat <- NULL
    if (!is.null(pv)) {
      if (length(dim(pv)) == 3) {
        se_mat <- matrix(NA_real_, nrow = nrow(df_re), ncol = ncol(df_re))
        for (k in seq_len(dim(pv)[3])) {
          m <- matrix(pv[, , k], nrow = dim(pv)[1], ncol = dim(pv)[2])
          se_mat[k, ] <- .safe_sqrt(diag(m))
        }
      } else if (all(dim(as.matrix(pv)) == dim(as.matrix(df_re)))) {
        se_mat <- matrix(.safe_sqrt(as.matrix(pv)), 
                         nrow = nrow(df_re), ncol = ncol(df_re))
      }
      if (!is.null(se_mat)) { rownames(se_mat) <- rn; colnames(se_mat) <- cn }
    }
    
    for (lvl in rn) {
      for (coef in cn) {
        est  <- df_re[lvl, coef]
        se   <- if (!is.null(se_mat)) se_mat[lvl, coef] else NA_real_
        vl   <- if (coef %in% c("(Intercept)", "Intercept")) {
          list(var = "(Intercept)", level = NA_character_)
        } else {
          .coef_var_level(coef, prep)
        }        
        lvl2 <- .country_from_stan(lvl, prep)
        
        rows[[length(rows) + 1]] <- .make_tidy_row(
          variable    = vl$var,
          level       = vl$level,
          country     = lvl2,
          estimate    = as.numeric(est),
          std.error   = as.numeric(se),
          lower       = if (is.finite(se)) as.numeric(est - z * se) else NA_real_,
          upper       = if (is.finite(se)) as.numeric(est + z * se) else NA_real_,
          param_type  = "ranef",
          effect_type = "random"
        )
      }
    }
  }
  
  if (!length(rows)) return(.make_tidy_row()[0L, ])
  dplyr::bind_rows(rows)
}

# brmsfit: SD/correlation parameters from posterior_summary(), total country-level
# effects (fixed + random) from brms::coef(). Falls back to raw r_* deviations
# with a warning if brms::coef() fails.
.re_brms <- function(model, conf_level, prep) {
  .require_pkg("brms",      "for Bayesian random effects")
  .require_pkg("posterior", "for diagnostics")
  
  probs    <- c((1 - conf_level) / 2, 1 - (1 - conf_level) / 2)
  post     <- brms::posterior_summary(model, probs = probs)
  rn       <- rownames(post)
  qcols    <- .qcols(colnames(post))
  diag_tbl <- .get_diag(model)
  rows     <- list()
  
  # 1) SD and correlation parameters (sd_* and cor_* in posterior_summary).
  for (nm in rn[grepl("^(sd_|cor_)", rn)]) {
    typ  <- if (startsWith(nm, "sd_")) "sd" else "cor"
    grp  <- sub("^(sd_|cor_)", "", strsplit(nm, "__", fixed = TRUE)[[1]][[1]])
    rows[[length(rows) + 1]] <- .make_tidy_row(
      variable    = grp,
      estimate    = as.numeric(post[nm, "Estimate"]),
      std.error   = as.numeric(post[nm, "Est.Error"]),
      lower       = as.numeric(post[nm, qcols$lower]),
      upper       = as.numeric(post[nm, qcols$upper]),
      rhat        = .lookup_diag(diag_tbl, nm, "rhat"),
      ess         = .lookup_diag(diag_tbl, nm, "ess"),
      param_type  = typ,
      effect_type = "random"
    )
  }
  
  # 2) Total country-level effects via brms::coef() = fixed + random deviation.
  #    These are the interpretable quantities: the actual predictor effect in
  #    each country, NOT the deviation from the global average.
  coef_list <- tryCatch(
    coef(model, probs = probs, summary = TRUE),
    error = function(e) NULL
  )
  
  if (!is.null(coef_list)) {
    for (grp in names(coef_list)) {
      coef_arr      <- coef_list[[grp]]
      countries_raw <- dimnames(coef_arr)[[1]]
      stat_names    <- dimnames(coef_arr)[[2]]
      term_names    <- dimnames(coef_arr)[[3]]
      qc            <- .qcols(stat_names)
      
      for (tm in term_names) {
        # Ordinal cutpoints are fixed-effect thresholds, not country-varying.
        # Already included in .fixed_brms(); skip here.
        if (grepl("^Intercept\\[[0-9]+\\]$", tm)) next
        
        vl        <- .coef_var_level(tm, prep)
        owner_raw <- .ctx_dummy_owner(tm, prep)
        loop_countries <- if (!is.null(owner_raw)) {
          intersect(owner_raw, countries_raw)
        } else {
          countries_raw
        }
        
        for (cc_raw in loop_countries) {
          cc      <- .country_from_stan(cc_raw, prep)
          tm_diag <- if (tm == "(Intercept)") "Intercept" else tm
          cc_stan <- make.names(cc_raw)
          
          # Context dummy terms are fixed-effect only — diagnostics come from
          # b_* draw variables, not r_*[cc, tm].
          diag_nm <- if (!is.null(owner_raw)) {
            paste0("b_", tm_diag)
          } else {
            paste0("r_", grp, "[", cc_stan, ",", tm_diag, "]")
          }
          
          rows[[length(rows) + 1]] <- .make_tidy_row(
            variable    = vl$var,
            level       = vl$level,
            country     = cc,
            estimate    = as.numeric(coef_arr[cc_raw, "Estimate", tm]),
            std.error   = as.numeric(coef_arr[cc_raw, "Est.Error", tm]),
            lower       = as.numeric(coef_arr[cc_raw, qc$lower,    tm]),
            upper       = as.numeric(coef_arr[cc_raw, qc$upper,    tm]),
            rhat        = .lookup_diag(diag_tbl, diag_nm, "rhat"),
            ess         = .lookup_diag(diag_tbl, diag_nm, "ess"),
            param_type  = "ranef",
            effect_type = "random"
          )
        }
      }
    }
  } else {
    # Fallback: brms::coef() failed. Return raw deviations with an explicit
    # param_type to prevent misinterpretation.
    warning(
      "brms::coef() failed; returning raw group-level deviations (r_* parameters). ",
      "These are centred on zero and should NOT be interpreted as country-level ",
      "effects without adding back the corresponding fixed effects.",
      call. = FALSE
    )
    for (nm in rn[grepl("^r_", rn) & grepl("\\[.+,.+\\]$", rn)]) {
      inside <- sub("^.*\\[", "", sub("\\]$", "", nm))
      sp     <- strsplit(inside, ",", fixed = TRUE)[[1]]
      cc_raw <- trimws(sp[[1]])
      coef   <- trimws(sp[[2]])
      if (coef == "(Intercept)") coef <- "Intercept"
      
      vl <- .coef_var_level(coef, prep)
      rows[[length(rows) + 1]] <- .make_tidy_row(
        variable    = vl$var,
        level       = vl$level,
        country     = .country_from_stan(cc_raw, prep),
        estimate    = as.numeric(post[nm, "Estimate"]),
        std.error   = as.numeric(post[nm, "Est.Error"]),
        lower       = as.numeric(post[nm, qcols$lower]),
        upper       = as.numeric(post[nm, qcols$upper]),
        rhat        = .lookup_diag(diag_tbl, nm, "rhat"),
        ess         = .lookup_diag(diag_tbl, nm, "ess"),
        param_type  = "ranef_deviation",
        effect_type = "random"
      )
    }
  }
  
  if (!length(rows)) return(.as_tidy_result(.make_tidy_row()[0L, ]))
  .as_tidy_result(dplyr::bind_rows(rows))
}

# Placeholder for INLA random-effect tidying.
.re_inla <- function(model, conf_level, prep) {
  .stopf(
    "INLA random-effect tidying is not yet implemented. ",
    "See the backend contract at the top of regress-tidy.R."
  )
}


# ── General helpers ────────────────────────────────────────────────────────────

# Resolve variable and level for a single coefficient name. Intercepts always
# return NA. All other terms look up prep$coef_lookup (built by
# .build_coef_lookup() in .tidy_one_model()). Unrecognised terms return NA and
# are reported by .warn_undecoded_terms().
.coef_var_level <- function(tm, prep) {
  if (tm %in% c("(Intercept)", "Intercept")) {
    return(list(var = NA_character_, level = NA_character_))
  }
  e <- prep$coef_lookup[[tm]] %||% list(variable = NA_character_, level = NA_character_)
  list(var = e$variable, level = e$level)
}

# Decode a vector of internal model term names into (variable, level, country,
# param_type). Intercepts and cutpoints are resolved by pattern; all slope
# terms look up prep$coef_lookup. Any term not in the lookup returns NA and is
# surfaced by .warn_undecoded_terms().
.parse_terms <- function(terms, prep) {
  lookup <- prep$coef_lookup
  
  parse_one <- function(tm) {
    if (tm %in% c("Intercept", "(Intercept)")) {
      return(list(variable = "(Intercept)", level = NA_character_,
                  country = NA_character_, param_type = "intercept"))
    }
    if (grepl("^Intercept\\[[0-9]+\\]$", tm) ||
        grepl("^[0-9]+\\|[0-9]+$", tm) ||
        grepl("^.+\\|.+$", tm)) {
      return(list(variable = "(Cutpoint)", level = tm,
                  country = NA_character_, param_type = "cutpoint"))
    }
    lookup[[tm]] %||% list(variable = NA_character_, level = NA_character_,
                           country = NA_character_, param_type = "slope")
  }
  
  res <- lapply(as.character(terms), parse_one)
  list(
    variable   = vapply(res, `[[`, character(1), "variable"),
    level      = vapply(res, `[[`, character(1), "level"),
    country    = vapply(res, `[[`, character(1), "country"),
    param_type = vapply(res, `[[`, character(1), "param_type")
  )
}

# Pre-build a flat coefficient-name -> label lookup from prep, expanded from
# fitted_terms (the actual column names passed to the fitter). Called once per
# model in .tidy_one_model() and attached as prep$coef_lookup.
#
#  - Columns in prep$term_map (context dummies): entry used directly.
#  - Columns in prep$level_map (factors): one entry per level code,
#    e.g. "dem_gen__02", "dem_gen__03" from column "dem_gen".
#  - All other columns (continuous): single entry keyed by column name.
.build_coef_lookup <- function(prep, fitted_terms) {
  dem_dict  <- prep$dem_dict  %||% NULL
  term_map  <- prep$term_map  %||% list()
  level_map <- prep$level_map %||% list()
  lookup    <- list()
  
  for (col in fitted_terms) {
    if (col %in% names(term_map)) {
      e <- term_map[[col]]
      lookup[[col]] <- list(
        variable   = .var_label(e$var %||% col, dem_dict),
        level      = e$level_label %||% NA_character_,
        country    = e$country     %||% NA_character_,
        param_type = "slope"
      )
    } else if (col %in% names(level_map)) {
      mp      <- level_map[[col]]
      var_lab <- .var_label(col, dem_dict)
      for (i in seq_len(nrow(mp))) {
        lookup[[paste0(col, mp$code[[i]])]] <- list(
          variable   = var_lab,
          level      = as.character(mp$label[[i]]),
          country    = NA_character_,
          param_type = "slope"
        )
      }
    } else {
      lookup[[col]] <- list(
        variable   = .var_label(col, dem_dict),
        level      = NA_character_,
        country    = NA_character_,
        param_type = "slope"
      )
    }
  }
  lookup
}

# Resolve a variable id (e.g. "dem_gen") to a human-readable label by looking
# up prep$dem_dict$question_short. Falls back to de-prefixing and title-casing.
.var_label <- function(var_id, dem_dict = NULL) {
  if (!is.null(dem_dict) && is.data.frame(dem_dict)) {
    cols    <- names(dem_dict)
    lab_col <- if ("question_short" %in% cols) "question_short" else
      if ("variable_label" %in% cols) "variable_label" else NULL
    if (!is.null(lab_col) && "item_id" %in% cols) {
      j <- match(var_id, dem_dict[["item_id"]])
      if (!is.na(j)) {
        val <- dem_dict[[lab_col]][[j]]
        if (!is.na(val) && nzchar(val)) return(val)
      }
    }
  }
  x <- sub("^dem_", "", var_id)
  x <- gsub("_", " ", x)
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

# Resolve a BeSD outcome item_id to its question_short label.
# Falls back to the item_id itself if not found in the dict.
.outcome_item_label <- function(item_id, dict = NULL) {
  if (!is.null(dict) && is.data.frame(dict) &&
      all(c("item_id", "question") %in% names(dict))) {
    j <- match(item_id, dict[["item_id"]])
    if (!is.na(j)) {
      val <- dict[["question"]][[j]]
      if (!is.na(val) && nzchar(val)) return(val)
    }
  }
  item_id
}

# Safe square root that clamps negative values to zero. Used when extracting
# SDs from variance-covariance matrices that may have tiny negative diagonals
# due to numerical imprecision.
.safe_sqrt <- function(x) sqrt(pmax(as.numeric(x), 0))

# Check that every slope coefficient in `out` decoded to a non-NA variable
# label. Fires a warning if any did not, naming the raw coefficient(s) that
# failed so the mismatch is immediately actionable.
.warn_undecoded_terms <- function(out, model) {
  slopes <- out[!is.na(out$param_type) & out$param_type == "slope", , drop = FALSE]
  if (!nrow(slopes)) return(invisible(NULL))
  
  bad <- slopes[is.na(slopes$variable), , drop = FALSE]
  if (!nrow(bad)) return(invisible(NULL))
  
  fitted_terms <- attr(model, "besd_fitted_terms")
  term_hint <- if (!is.null(fitted_terms)) {
    paste0("Fitted predictor columns: ", paste(fitted_terms, collapse = ", "), ". ")
  } else ""
  
  warning(
    sprintf("%d slope coefficient(s) could not be decoded to human-readable labels. ", nrow(bad)),
    term_hint,
    "Check that prep$term_map and prep$level_map are consistent with prep$df.",
    call. = FALSE
  )
}

# Map brms fixed-effect term names to their Stan draw variable names (b_*).
.brms_fixef_draw_names <- function(terms) {
  vapply(terms, function(t) {
    if (startsWith(t, "b_")) return(t)
    if (t == "Intercept")    return("b_Intercept")
    paste0("b_", t)
  }, character(1))
}

# Return the raw country label that owns a context dummy term, or NULL for
# common predictors and the intercept. Context dummy columns are named
# ctx_<var>_<Cxx>_<levelcode>; the country code is used to look up the owner
# from prep$country_code_of.
.ctx_dummy_owner <- function(tm, prep) {
  m <- regmatches(tm, regexpr("_(C[0-9]+)_", tm))
  if (!length(m)) return(NULL)
  ccode <- sub("^_", "", sub("_$", "", m))
  owner <- names(prep$country_code_of)[prep$country_code_of == ccode]
  if (!length(owner)) return(NULL)
  owner[[1]]
}

# Reverse Stan's make.names() sanitisation of country labels using the exact
# raw country labels stored in prep$countries.
.country_from_stan <- function(stan_name, prep) {
  hits <- prep$countries[make.names(prep$countries) == stan_name]
  if (length(hits)) hits[[1]] else stan_name
}

# Identify lower and upper quantile column names in a brms summary matrix.
.qcols <- function(cols) {
  q    <- cols[grepl("^Q", cols)]
  if (length(q) < 2) .stopf("Could not find quantile columns in brms output.")
  qnum <- suppressWarnings(as.numeric(sub("^Q", "", q)))
  ord  <- order(qnum)
  list(lower = q[[ord[1]]], upper = q[[ord[length(ord)]]])
}

# Extract posterior rhat and ess_bulk diagnostics from a brmsfit.
# Returns an empty tibble on failure.
.get_diag <- function(model) {
  empty <- tibble::tibble(variable = character(), rhat = numeric(), ess = numeric())
  
  draws <- tryCatch(posterior::as_draws_array(model), error = function(e) NULL)
  if (is.null(draws)) return(empty)
  
  dt <- tryCatch(
    posterior::summarise_draws(draws, rhat = posterior::rhat, ess = posterior::ess_bulk),
    error = function(e) NULL
  )
  if (is.null(dt)) return(empty)
  
  vcol <- if (".variable" %in% names(dt)) ".variable" else
    if ("variable"  %in% names(dt)) "variable"  else names(dt)[1]
  tibble::tibble(
    variable = as.character(dt[[vcol]]),
    rhat     = as.numeric(dt[["rhat"]]),
    ess      = as.numeric(dt[["ess"]])
  )
}

# Look up a diagnostic statistic (rhat or ess) for a vector of draw variable
# names. Returns NA for any name not found.
.lookup_diag <- function(diag_tbl, varnames, stat = c("rhat", "ess")) {
  stat <- match.arg(stat)
  if (is.null(diag_tbl) || !nrow(diag_tbl) || !(stat %in% names(diag_tbl))) {
    return(rep(NA_real_, length(varnames)))
  }
  idx <- match(varnames, diag_tbl$variable)
  out <- rep(NA_real_, length(varnames))
  ok  <- !is.na(idx)
  out[ok] <- diag_tbl[[stat]][idx[ok]]
  out
}

# Central constructor for consistent output columns across all backends.
.make_tidy_row <- function(variable = NA_character_, level = NA_character_,
                           country = NA_character_, estimate = NA_real_,
                           std.error = NA_real_, lower = NA_real_,
                           upper = NA_real_, rhat = NA_real_,
                           ess = NA_real_, effect_type = "fixed",
                           param_type = "slope") {
  tibble::tibble(
    variable    = variable,
    level       = level,
    country     = country,
    estimate    = estimate,
    std.error   = std.error,
    lower       = lower,
    upper       = upper,
    rhat        = rhat,
    ess         = ess,
    effect_type = effect_type,
    param_type  = param_type
  )
}

# Empty tidy tibble with correct schema.
.empty_tidy <- function(include_baseline = FALSE) {
  tb <- tibble::tibble(
    outcome     = character(),
    item        = character(),
    variable    = character(),
    level       = character(),
    country     = character(),
    estimate    = numeric(),
    std.error   = numeric(),
    lower       = numeric(),
    upper       = numeric(),
    rhat        = numeric(),
    ess         = numeric(),
    effect_type = character(),
    param_type  = character()
  )
  if (isTRUE(include_baseline)) {
    tb$baseline      <- character()
  }
  tb
}

# Exponentiate estimates/CIs for slope, intercept, and ranef rows only.
.apply_exp <- function(out, exponentiate) {
  if (!isTRUE(exponentiate) || !nrow(out)) return(out)
  idx <- out$param_type %in% c("slope", "intercept", "ranef")
  out$estimate[idx] <- exp(out$estimate[idx])
  out$lower[idx]    <- exp(out$lower[idx])
  out$upper[idx]    <- exp(out$upper[idx])
  out
}


# ── Baseline helpers ───────────────────────────────────────────────────────────
# Initialise empty table for baselines
.empty_baseline_tbl <- function() {
  tibble::tibble(
    variable_id   = character(),
    variable      = character(),
    country       = character(),
    baseline      = character()
  )
}

# Build a tibble of reference category labels for each predictor.
# By-country fits: one row per (variable, country).
# Multilevel fits: common predictors get a global row (country = NA);
# context predictors get per-country rows.
.besd_baselines <- function(fit) {
  if (!inherits(fit, c("besd_fit", "besd_fit_by_country"))) return(tibble::tibble())
  
  prep <- fit$prep
  if (is.null(prep)) return(.empty_baseline_tbl())
  
  is_bc <- prep$scope == "by_country"
  rows  <- c(
    .baselines_common(prep, prep$dem_dict, is_bc),
    if (!is_bc) .baselines_context(prep, prep$dem_dict) else list()
  )
  if (!length(rows)) return(.empty_baseline_tbl())
  dplyr::bind_rows(rows)
}

# Build baseline rows for common predictors.
.baselines_common <- function(prep, dem_dict, is_bc) {
  rows <- list()
  for (v in prep$preds_common) {
    var_lab <- .var_label(v, dem_dict)
    c2l     <- .code_to_label(prep$level_map[[v]])
    
    if (is_bc) {
      for (cc in prep$countries) {
        bcode <- prep$ref_code_by_group[[v]][[cc]] %||% NA_character_
        blab  <- if (!is.null(c2l) && !is.na(bcode)) .map_get(c2l, bcode) else NA_character_
        rows[[length(rows) + 1]] <- tibble::tibble(
          variable_id   = v,
          variable      = var_lab,
          country       = as.character(cc),
          baseline      = as.character(blab)
        )
      }
    } else {
      bcode <- prep$ref_code[[v]] %||% NA_character_
      blab  <- if (!is.null(c2l) && !is.na(bcode)) .map_get(c2l, bcode) else NA_character_
      rows[[length(rows) + 1]] <- tibble::tibble(
        variable_id   = v,
        variable      = var_lab,
        country       = NA_character_,
        baseline      = as.character(blab)
      )
    }
  }
  rows
}

# Build baseline rows for context predictors (multilevel only).
.baselines_context <- function(prep, dem_dict) {
  rows      <- list()
  countries <- names(prep$country_code_of)
  
  for (v in prep$preds_context) {
    var_lab <- .var_label(v, dem_dict)
    c2l     <- .code_to_label(prep$level_map_context[[v]])
    
    for (cc in countries) {
      tm_entry <- Filter(
        function(e) identical(e$var, v) && identical(e$country, cc),
        prep$term_map
      )
      
      if (length(tm_entry)) {
        blab  <- tm_entry[[1]]$baseline_label %||% NA_character_
      } else {
        bcode <- prep$ref_code_by_group_context[[v]][[cc]] %||% NA_character_
        blab  <- if (!is.null(c2l)) .map_get(c2l, bcode) else NA_character_
      }
      
      rows[[length(rows) + 1]] <- tibble::tibble(
        variable_id   = v,
        variable      = var_lab,
        country       = as.character(cc),
        baseline      = as.character(blab)
      )
    }
  }
  rows
}

# Build a code-to-label lookup from a level_map entry. Returns NULL if malformed.
.code_to_label <- function(mp) {
  if (is.null(mp) || !all(c("code", "label") %in% names(mp))) return(NULL)
  stats::setNames(as.character(mp$label), as.character(mp$code))
}

# Look up a single value from a named character vector. Returns NA for misses.
.map_get <- function(mp, key) {
  if (is.null(mp) || is.na(key)) return(NA_character_)
  val <- unname(mp[[as.character(key)]])
  if (is.null(val) || !length(val)) return(NA_character_)
  as.character(val[[1]])
}

# Join baseline labels onto the tidy output tibble. Matches on (variable,
# country); falls back to the global baseline for multilevel common predictors.
.attach_baselines <- function(out, base_tbl) {
  if (is.null(out) || !nrow(out) || is.null(base_tbl) || !nrow(base_tbl)) {
    if (!("baseline"      %in% names(out))) out$baseline      <- NA_character_
    return(out)
  }
  
  key <- function(x) ifelse(is.na(x) | x == "", ".GLOBAL", as.character(x))
  out2  <- out;       out2$`.__ck__`  <- key(out2$country)
  base2 <- base_tbl;  base2$`.__ck__` <- key(base2$country)
  base2 <- dplyr::select(base2, variable, `.__ck__`, baseline)
  
  out2 <- dplyr::left_join(out2, base2, by = c("variable", ".__ck__"))
  
  base_global <- dplyr::select(
    dplyr::filter(base2, `.__ck__` == ".GLOBAL"),
    variable, baseline_g = baseline
  )
  out2 <- dplyr::left_join(out2, base_global, by = "variable")
  out2$baseline      <- dplyr::coalesce(out2$baseline,      out2$baseline_g)
  out2[c("baseline_g", ".__ck__")] <- NULL
  
  if ("level" %in% names(out2)) {
    out2 <- dplyr::relocate(out2, baseline, .after = level)
  }
  out2
}