#' Tidy BeSD model outputs
#'
#' @param fit A `besd_regress()` output.
#' @param conf_level CI/CrI level.
#' @param include_random If TRUE, include SD/cor and group-level effects.
#' @param include_cor If TRUE (and include_random=TRUE), include correlation
#'   parameters (cor_*). Set FALSE to drop cor parameters from the tidy output.
#' @param exponentiate If TRUE, exponentiate slopes/intercepts (OR scale).
#' @param return_baseline If TRUE, attach baseline (reference) category info.
#'   Adds columns `baseline` and `baseline_code` and attaches a `baseline`
#'   attribute with one row per (variable, country).
#' @export
tidy_model <- function(fit, conf_level = 0.95, include_random = FALSE, 
                       include_cor = FALSE, exponentiate = FALSE, 
                       return_baseline = FALSE) {
  
  # Validate object class early
  if (!inherits(fit, c("besd_fit", "besd_fit_by_country"))) {
    .stopf("`fit` must be a besd_fit or besd_fit_by_country object.")
  }
  
  meta <- fit$meta %||% list()
  is_by_country <- inherits(fit, "besd_fit_by_country")
  
  # Countries/outcomes are driven by metadata; fallback to names if needed.
  countries <- if (is_by_country) meta$countries %||% names(fit$fits) else ""
  outcomes <- meta$outcomes %||% names(fit$fits) %||% character()
  
  rows <- list()
  
  # Unify by-country and non-by-country flows by iterating over countries.
  # For non-by-country fits, the "countries" loop runs once with "".
  for (cc in countries %||% "") {
    fits_cc <- if (is_by_country) fit$fits[[cc]] else fit$fits
    if (is.null(fits_cc) || !length(fits_cc)) next
    
    # Each outcome has its own model object; we tidy each and bind later.
    for (yy in outcomes) {
      m <- fits_cc[[yy]]
      if (is.null(m)) next
      
      td <- .tidy_model(model = m, engine = meta$engine, conf_level = conf_level,
        include_random = include_random, meta = meta)
      
      if (!nrow(td)) next
      
      # Ensure country col is populated for by-country fits even if the model
      # itself didn't carry it through term parsing (common with random effects).
      if (is_by_country) {
        td$country <- ifelse(is.na(td$country) | td$country == "", cc, td$country)
      }
      
      # Outcome is always explicit in the returned tidy tibble.
      td$outcome <- yy
      rows[[length(rows) + 1]] <- td
    }
  }
  
  # If nothing produced (e.g., all models missing), return empty tibble. Attach baselines.
  if (!length(rows)) {
    out <- .empty_tidy(include_baseline = isTRUE(return_baseline))
    if (isTRUE(return_baseline)) attr(out, "baseline") <- .besd_baselines(fit)
    return(out)
  }
  
  out <- dplyr::bind_rows(rows)
  
  # Optionally drop correlation parameters when random effects are included.
  if (isTRUE(include_random) && !isTRUE(include_cor) && nrow(out)) {
    out <- out[out$param_type != "cor", , drop = FALSE]
  }
  
  # Standardise column order (differs slightly for by-country vs pooled).
  out <- if (is_by_country) {
    dplyr::select(
      out, country, outcome, variable, level,
      estimate, std.error, lower, upper, rhat, ess, effect_type, param_type
    )
  } else {
    dplyr::select(
      out, outcome, variable, level, country,
      estimate, std.error, lower, upper, rhat, ess, effect_type, param_type
    )
  }
  
  # Attach baseline labels/codes for each predictor (country-specific when needed).
  if (isTRUE(return_baseline)) {
    base_tbl <- .besd_baselines(fit)
    out <- .attach_baselines(out, base_tbl)
    attr(out, "baseline") <- base_tbl
  }
  
  # Optional exponentiation is applied to slopes/intercepts/ranef only.
  # ** Note: exponentiation should *not* be used if package ever updated to include:
  # -- normal likelihood 
  # -- and / or continuous level-2 covariates .
  .apply_exp(out, exponentiate)
}

# ---- Model tidying ---------------------------------------------------------

#' Tidy models
#' @keywords internal
.tidy_model <- function(model, engine, conf_level, include_random, meta) {
  
  # Tidy fixed-effect terms
  out <- if (engine == "frequentist") {
    .tidy_freq(model, conf_level, meta)
  } else {
    .tidy_bayes(model, conf_level, meta)
  }
  
  # Random effects are optionally appended
  if (!isTRUE(include_random)) return(out)
  
  re <- if (engine == "frequentist") {
    .tidy_re_freq(model, conf_level, meta)
  } else {
    .tidy_re_bayes(model, conf_level, meta)
  }
  dplyr::bind_rows(out, re)
}

.tidy_freq <- function(model, conf_level, meta) {
  # Pull estimates/SEs and CIs.
  #
  # CI strategy by model type:
  #  - glm / clm: confint() gives profile-likelihood CIs (more accurate near boundaries 
  #    than Wald). Falls back to Wald if confint() fails.
  #  - glmerMod: profile confint() is prohibitively slow for multilevel models;
  #    Wald CI from vcov() is the practical standard here.
  #  - clmm: confint() available and used.
  #
  # Random-effect CIs (.tidy_re_freq) remain z * SE — the conditional variances
  # from postVar/condVar are themselves Laplace approximations, so profile CIs
  # would not meaningfully improve accuracy.
  
  if (inherits(model, "glmerMod")) {
    estimate  <- as.numeric(lme4::fixef(model))
    term      <- names(lme4::fixef(model))
    std.error <- sqrt(diag(as.matrix(stats::vcov(model))))
    z         <- stats::qnorm(1 - (1 - conf_level) / 2)
    lower     <- estimate - z * std.error
    upper     <- estimate + z * std.error
  } else {
    coefs     <- stats::coef(summary(model))
    term      <- rownames(coefs)
    estimate  <- as.numeric(coefs[, "Estimate"])
    std.error <- as.numeric(coefs[, "Std. Error"])
    
    # Try profile-likelihood CIs; fall back to Wald on failure.
    ci <- tryCatch(
      stats::confint(model, level = conf_level, method = "profile"),
      error = function(e) NULL,
      warning = function(w) NULL
    )
    if (!is.null(ci) && is.matrix(ci) && nrow(ci) == length(term)) {
      lower <- as.numeric(ci[, 1])
      upper <- as.numeric(ci[, 2])
    } else {
      z     <- stats::qnorm(1 - (1 - conf_level) / 2)
      lower <- estimate - z * std.error
      upper <- estimate + z * std.error
    }
  }
  
  info <- .parse_terms(term, meta)
  .make_tidy_row(
    variable  = info$variable,
    level     = info$level,
    country   = info$country,
    estimate  = estimate,
    std.error = std.error,
    lower     = lower,
    upper     = upper,
    param_type = info$param_type
  )
}

.tidy_bayes <- function(model, conf_level, meta) {
  # Pull estimates/SEs and CrIs for Bayesian models

  .require_pkg("brms", "for Bayesian tidying")
  .require_pkg("posterior", "for diagnostics")
  
  # We compute an equal-tailed interval based on conf_level.
  probs <- c((1 - conf_level) / 2, 1 - (1 - conf_level) / 2)
  
  fixed <- brms::fixef(model, probs = probs)
  qcols <- .qcols(colnames(fixed))
  
  term <- rownames(fixed)
  term[term == "(Intercept)"] <- "Intercept"
  
  # Diagnostics are computed on all draw variables (rhat + ess_bulk).
  diag_tbl <- .get_diag(model)
  
  # brms draw variables are usually prefixed with b_; map fixed-effect term names
  # to the corresponding draw variable names for diagnostics lookup.
  bvars <- vapply(
    term,
    function(t) {
      if (startsWith(t, "b_")) return(t)
      if (t == "Intercept") return("b_Intercept")
      paste0("b_", t)
    },
    character(1)
  )
  
  info <- .parse_terms(term, meta)
  out <- .make_tidy_row(
    variable = info$variable,
    level = info$level,
    country = info$country,
    estimate = as.numeric(fixed[, "Estimate"]),
    std.error = as.numeric(fixed[, "Est.Error"]),
    lower = as.numeric(fixed[, qcols$lower]),
    upper = as.numeric(fixed[, qcols$upper]),
    rhat = .lookup_diag(diag_tbl, bvars, "rhat"),
    ess = .lookup_diag(diag_tbl, bvars, "ess"),
    param_type = info$param_type
  )
  
  # Ordinal models can have cutpoints (Intercept[k]) that aren't part of fixef().
  # We include them if present in posterior_summary().
  post <- brms::posterior_summary(model, probs = probs)
  rn <- rownames(post)
  cp <- rn[grepl("^Intercept\\[[0-9]+\\]$", rn)]
  if (!length(cp)) return(out)
  
  q2 <- .qcols(colnames(post))
  dplyr::bind_rows(
    out,
    .make_tidy_row(
      estimate = as.numeric(post[cp, "Estimate"]),
      std.error = as.numeric(post[cp, "Est.Error"]),
      lower = as.numeric(post[cp, q2$lower]),
      upper = as.numeric(post[cp, q2$upper]),
      rhat = .lookup_diag(diag_tbl, cp, "rhat"),
      ess = .lookup_diag(diag_tbl, cp, "ess"),
      param_type = "cutpoint"
    )
  )
}

# ---- Random effects --------------------------------------------------------

.tidy_re_freq <- function(model, conf_level = 0.95, meta = NULL) {
  alpha <- 1 - conf_level
  z <- stats::qnorm(1 - alpha / 2)
  
  .safe_sqrt <- function(x) sqrt(pmax(as.numeric(x), 0))
  
  # Best-effort mapping from a coefficient name to a level label. Mostly useful for 
  # SD rows where the coefficient corresponds to a predictor (e.g. random slope)
  .coef_level <- function(coef_name) {
    if (is.null(meta)) return(NA_character_)
    coef1 <- if (coef_name %in% c("(Intercept)", "Intercept")) "Intercept" else
      coef_name
    info <- .parse_terms(coef1, meta)
    info$level[[1]] %||% NA_character_
  }
  
  rows <- list()
  
  # Identify engine and pull variance components + conditional modes.
  if (inherits(model, "merMod")) {
    vc <- lme4::VarCorr(model)
    re_list <- lme4::ranef(model, condVar = TRUE)
    pkg <- "lme4"
  } else if (inherits(model, "clmm")) {
    .require_pkg("ordinal", "for random effects in clmm models")
    vc <- ordinal::VarCorr(model)
    re_list <- ordinal::ranef(model, condVar = TRUE)
    pkg <- "ordinal"
  } else {
    return(tibble::tibble())
  }
  
  # 1) SD and correlation parameters per grouping factor.
  for (grp in names(vc)) {
    m <- vc[[grp]]
    if (is.data.frame(m)) m <- as.matrix(m)
    if (!is.matrix(m) || nrow(m) < 1) next
    
    sds <- attr(m, "stddev") %||% .safe_sqrt(diag(m))
    tn <- names(sds) %||% colnames(m) %||% paste0("V", seq_along(sds))
    cor_mat <- if (pkg == "lme4") attr(m, "correlation") else NULL
    
    for (i in seq_along(sds)) {
      nm0 <- tn[[i]]
      rows[[length(rows) + 1]] <- .make_tidy_row(
        variable = grp,
        level = .coef_level(nm0),
        estimate = as.numeric(sds[[i]]),
        param_type = "sd",
        effect_type = "random"
      )
    }
    
    if (length(sds) > 1) {
      for (i in 1:(length(sds) - 1)) {
        for (j in (i + 1):length(sds)) {
          est_cor <- if (pkg == "lme4") cor_mat[i, j] else m[i, j]
          rows[[length(rows) + 1]] <- .make_tidy_row(
            variable = grp,
            estimate = as.numeric(est_cor),
            param_type = "cor",
            effect_type = "random"
          )
        }
      }
    }
  }
  
  # 2) Group-level deviations (conditional modes) with optional SEs.
  for (grp in names(re_list)) {
    df_re <- re_list[[grp]]
    rn <- rownames(df_re)
    cn <- colnames(df_re)
    
    # lme4 stores conditional variances in postVar; ordinal may use condVar.
    pv <- attr(df_re, "postVar") %||% attr(df_re, "condVar")
    se_mat <- NULL
    if (!is.null(pv)) {
      # Common lme4 layout: [coef, coef, level] 3D array of variances/covariances.
      if (length(dim(pv)) == 3) {
        se_mat <- t(vapply(
          seq_len(dim(pv)[3]),
          function(k) .safe_sqrt(diag(pv[, , k, drop = FALSE])),
          numeric(ncol(df_re))
        ))
      } else if (all(dim(as.matrix(pv)) == dim(as.matrix(df_re)))) {
        # Some models may return a matrix of variances.
        se_mat <- matrix(
          .safe_sqrt(as.matrix(pv)),
          nrow = nrow(df_re),
          ncol = ncol(df_re)
        )
      }
      if (!is.null(se_mat)) {
        rownames(se_mat) <- rn
        colnames(se_mat) <- cn
      }
    }
    
    for (lvl in rn) {
      for (coef in cn) {
        est <- df_re[lvl, coef]
        se <- if (!is.null(se_mat)) se_mat[lvl, coef] else NA_real_
        
        coef_show <- if (coef == "(Intercept)") "Intercept" else coef
        
        # Parse the coefficient as a predictor term to recover variable/level.
        coef_info <- if (!is.null(meta)) .parse_terms(coef_show, meta) else NULL
        var_join <- if (!is.null(coef_info) && coef_show != "Intercept") {
          coef_info$variable[[1]]
        } else {
          NA_character_
        }
        lev_join <- if (!is.null(coef_info) && coef_show != "Intercept") {
          coef_info$level[[1]]
        } else {
          NA_character_
        }
        
        # Unsanitize group labels
        lvl2 <- if (!is.null(meta)) .unsanitize_country(lvl, meta) else lvl
        
        rows[[length(rows) + 1]] <- .make_tidy_row(
          variable = var_join,
          level = lev_join,
          country = lvl2,
          estimate = as.numeric(est),
          std.error = as.numeric(se),
          lower = if (is.finite(se)) as.numeric(est - z * se) else NA_real_,
          upper = if (is.finite(se)) as.numeric(est + z * se) else NA_real_,
          param_type = "ranef",
          effect_type = "random"
        )
      }
    }
  }
  
  if (!length(rows)) return(tibble::tibble())
  dplyr::bind_rows(rows)
}

.tidy_re_bayes <- function(model, conf_level, meta) {
  .require_pkg("brms", "for Bayesian random effects")
  .require_pkg("posterior", "for diagnostics")
  
  probs <- c((1 - conf_level) / 2, 1 - (1 - conf_level) / 2)
  post <- brms::posterior_summary(model, probs = probs)
  rn <- rownames(post)
  qcols <- .qcols(colnames(post))
  
  diag_tbl <- .get_diag(model)
  rows <- list()
  
  # 1) Random-effect SD/correlation parameters (sd_* and cor_*).
  for (nm in rn[grepl("^(sd_|cor_)", rn)]) {
    typ <- if (startsWith(nm, "sd_")) "sd" else "cor"
    parts <- strsplit(nm, "__", fixed = TRUE)[[1]]
    grp <- sub("^(sd_|cor_)", "", parts[[1]])
    
    rows[[length(rows) + 1]] <- .make_tidy_row(
      variable = grp,
      estimate = as.numeric(post[nm, "Estimate"]),
      std.error = as.numeric(post[nm, "Est.Error"]),
      lower = as.numeric(post[nm, qcols$lower]),
      upper = as.numeric(post[nm, qcols$upper]),
      rhat = .lookup_diag(diag_tbl, nm, "rhat"),
      ess = .lookup_diag(diag_tbl, nm, "ess"),
      param_type = typ,
      effect_type = "random"
    )
  }
  
  # 2) Total country-level effects via brms::coef() = fixed + random deviation.
  #    These are the interpretable quantities: the actual effect of a predictor
  #    in each country, NOT the deviation from the global average.
  coef_list <- tryCatch(
    coef(model, probs = probs, summary = TRUE),
    error = function(e) NULL
  )
  
  if (!is.null(coef_list)) {
    
    # Build a reverse lookup: country_code ("C01") -> raw country label
    # as it appears in coef() dimnames (e.g. "The UK", "France").
    country_code_of <- meta$country_code_of %||% list()
    code_to_raw <- setNames(names(country_code_of), as.character(country_code_of))
    
    # Returns the raw country label that owns a context dummy term, or NULL
    # for common predictors / Intercept (which apply to all countries).
    # Context dummies are named: ctx_<var>_<Cxx>_<levelcode>
    ctx_owner_raw <- function(tm) {
      m <- regmatches(tm, regexpr("_(C[0-9]+)_", tm))
      if (!length(m)) return(NULL)
      ccode <- sub("^_", "", sub("_$", "", m))
      owner <- code_to_raw[[ccode]]
      if (is.null(owner) || is.na(owner)) return(NULL)
      owner
    }
    
    for (grp in names(coef_list)) {
      coef_arr      <- coef_list[[grp]]
      countries_raw <- dimnames(coef_arr)[[1]]
      stat_names    <- dimnames(coef_arr)[[2]]
      term_names    <- dimnames(coef_arr)[[3]]
      qc            <- .qcols(stat_names)
      
      for (tm in term_names) {
        # Ordinal cutpoints (Intercept[1], Intercept[2], ...) are fixed-effect
        # thresholds, not country-varying. Skip here — already in .tidy_bayes().
        if (grepl("^Intercept\\[[0-9]+\\]$", tm)) next
        
        coef_show <- if (tm == "(Intercept)") "Intercept" else tm
        coef_info <- .parse_terms(coef_show, meta)
        var_join  <- if (coef_show != "Intercept") coef_info$variable[[1]] else NA_character_
        lev_join  <- if (coef_show != "Intercept") coef_info$level[[1]]    else NA_character_
        
        # Context dummies: only emit the owning country's row.
        # Common predictors and Intercept: emit all countries.
        owner_raw     <- ctx_owner_raw(tm)
        loop_countries <- if (!is.null(owner_raw)) {
          intersect(owner_raw, countries_raw)
        } else {
          countries_raw
        }
        
        for (cc_raw in loop_countries) {
          cc      <- .unsanitize_country(cc_raw, meta)
          tm_diag <- if (tm == "(Intercept)") "Intercept" else tm
          
          # Stan sanitises country names to syntactic R identifiers (spaces -> dots
          # etc.), so the draw variable uses make.names(cc_raw), not cc_raw itself.
          cc_stan <- make.names(cc_raw)
          
          # Context dummy terms are fixed-effect only (no random slope per country),
          # so their diagnostics come from the b_* draw variable, not r_*[cc, tm].
          diag_nm <- if (!is.null(owner_raw)) {
            paste0("b_", tm_diag)
          } else {
            paste0("r_", grp, "[", cc_stan, ",", tm_diag, "]")
          }
          
          rows[[length(rows) + 1]] <- .make_tidy_row(
            variable    = var_join,
            level       = lev_join,
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
    # Fallback: if brms::coef() fails for any reason, return raw deviations and flag.
    warning(
      "brms::coef() failed; returning raw group-level deviations (r_* parameters). ",
      "These are centred on zero and should NOT be interpreted as country-level effects ",
      "without adding back the corresponding fixed effects.",
      call. = FALSE
    )
    for (nm in rn[grepl("^r_", rn) & grepl("\\[.+,.+\\]$", rn)]) {
      grp    <- sub("^r_([^\\[]+)\\[.*$", "\\1", nm)
      inside <- sub("^.*\\[", "", sub("\\]$", "", nm))
      sp     <- strsplit(inside, ",", fixed = TRUE)[[1]]
      
      cc_raw <- trimws(sp[[1]])
      cc     <- .unsanitize_country(cc_raw, meta)
      coef   <- trimws(sp[[2]])
      if (coef == "(Intercept)") coef <- "Intercept"
      
      coef_info <- .parse_terms(coef, meta)
      var_join  <- if (coef != "Intercept") coef_info$variable[[1]] else NA_character_
      lev_join  <- if (coef != "Intercept") coef_info$level[[1]] else NA_character_
      
      rows[[length(rows) + 1]] <- .make_tidy_row(
        variable  = var_join,
        level     = lev_join,
        country   = cc,
        estimate  = as.numeric(post[nm, "Estimate"]),
        std.error = as.numeric(post[nm, "Est.Error"]),
        lower     = as.numeric(post[nm, qcols$lower]),
        upper     = as.numeric(post[nm, qcols$upper]),
        rhat      = .lookup_diag(diag_tbl, nm, "rhat"),
        ess       = .lookup_diag(diag_tbl, nm, "ess"),
        param_type  = "ranef_deviation",  # explicit: NOT total country effect
        effect_type = "random"
      )
    }
  }
  
  if (!length(rows)) return(tibble::tibble())
  dplyr::bind_rows(rows)
}

# ---- Term parsing ----------------------------------------------------------

.parse_terms <- function(terms, meta) {
  term_map <- meta$term_map %||% NULL
  level_map <- meta$level_map %||% list()
  dem_dict <- meta$dem_dict %||% NULL
  
  # Parse one term into (parameter, variable, level, country, param_type).
  # Order of precedence:
  #  (1) intercept/cutpoints special cases
  #  (2) explicit meta$term_map entry (most authoritative)
  #  (3) "__" coded terms (variable__levelCode)
  #  (4) fallback: treat term as variable id and label it
  parse_one <- function(tm) {
    variable <- level <- country <- NA_character_
    param_type <- "slope"
    
    if (tm %in% c("Intercept", "(Intercept)")) {
      param_type <- "intercept"
    } else if (grepl("^Intercept\\[[0-9]+\\]$", tm) ||
               grepl("^[0-9]+\\|[0-9]+$", tm)) {
      # Ordinal cutpoints: Intercept[k] or i|j encodings.
      param_type <- "cutpoint"
    } else if (!is.null(term_map) && tm %in% names(term_map)) {
      # term_map is most authoritative — use it directly.
      mp <- term_map[[tm]]
      variable <- .var_label(mp$var %||% tm, dem_dict)
      level    <- mp$level_label %||% NA_character_
      country  <- mp$country     %||% NA_character_
    } else if (grepl("__", tm, fixed = TRUE)) {
      # "__" encoding: varId__levelCode -> look up level label.
      parts   <- strsplit(tm, "__", fixed = TRUE)[[1]]
      var_id  <- parts[[1]]
      lvl     <- paste0("__", parts[[2]])
      variable <- .var_label(var_id, dem_dict)
      mp <- level_map[[var_id]]
      if (!is.null(mp)) level <- mp$label[match(lvl, mp$code)]
    } else {
      variable <- .var_label(tm, dem_dict)
    }
    
    list(variable = variable, level = level, country = country,
         param_type = param_type)
  }
  
  res <- lapply(as.character(terms), parse_one)
  list(
    variable = vapply(res, `[[`, character(1), "variable"),
    level = vapply(res, `[[`, character(1), "level"),
    country = vapply(res, `[[`, character(1), "country"),
    param_type = vapply(res, `[[`, character(1), "param_type")
  )
}

.var_label <- function(var_id, dem_dict = NULL) {
  # Prefer metadata dictionary labels when available.
  if (!is.null(dem_dict) && is.data.frame(dem_dict)) {
    cols <- names(dem_dict)
    id_col <- "item_id"
    lab_col <- if ("question_short" %in% cols) {
      "question_short"
    } else if ("variable_label" %in% cols) {
      "variable_label"
    } else {
      NULL
    }
    
    if (!is.null(lab_col) && id_col %in% cols) {
      j <- match(var_id, dem_dict[[id_col]])
      if (!is.na(j)) {
        val <- dem_dict[[lab_col]][[j]]
        if (!is.na(val) && nzchar(val)) return(val)
      }
    }
  }
  
  # Fallback: de-prefix and title-case a readable label.
  x <- sub("^dem_", "", var_id)
  x <- gsub("_", " ", x)
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

# ---- Utilities -------------------------------------------------------------

.empty_tidy <- function(include_baseline = FALSE) {
  # Keep a stable schema for downstream dplyr/purrr consumers.
  tb <- tibble::tibble(
    country = character(),
    outcome = character(),
    variable = character(),
    level = character(),
    estimate = numeric(),
    std.error = numeric(),
    lower = numeric(),
    upper = numeric(),
    rhat = numeric(),
    ess = numeric(),
    effect_type = character(),
    param_type = character()
  )
  
  if (isTRUE(include_baseline)) {
    tb$baseline <- character()
    tb$baseline_code <- character()
  }
  tb
}

.apply_exp <- function(out, exponentiate) {
  # Only exponentiate model-scale parameters (not SD/cor/cutpoints).
  if (!isTRUE(exponentiate) || !nrow(out)) return(out)
  
  idx <- out$param_type %in% c("slope", "intercept", "ranef")
  out$estimate[idx] <- exp(out$estimate[idx])
  out$lower[idx] <- exp(out$lower[idx])
  out$upper[idx] <- exp(out$upper[idx])
  out
}

.get_diag <- function(model) {
  empty <- tibble::tibble(variable = character(), rhat = numeric(), ess = numeric())
  
  # Attempt to get draws, stripping path__ if present (Pathfinder produces
  # path__ instead of chain__; posterior::summarise_draws() chokes on it).
  draws <- tryCatch({
    d <- posterior::as_draws_array(model)
    pvars <- posterior::variables(d)
    if ("path__" %in% pvars) {
      d <- posterior::subset_draws(d, variable = setdiff(pvars, "path__"))
    }
    d
  }, error = function(e) NULL)
  
  if (is.null(draws)) return(empty)
  
  # For Pathfinder fits there is only one "chain" so rhat is not meaningful,
  # but ess_bulk still gives a useful effective-sample-size estimate.
  dt <- tryCatch(
    posterior::summarise_draws(draws, rhat = posterior::rhat, ess = posterior::ess_bulk),
    error = function(e) NULL
  )
  if (is.null(dt)) return(empty)
  
  vcol <- if (".variable" %in% names(dt)) ".variable" else
    if ("variable" %in% names(dt)) "variable" else names(dt)[1]
  tibble::tibble(
    variable = as.character(dt[[vcol]]),
    rhat     = as.numeric(dt[["rhat"]]),
    ess      = as.numeric(dt[["ess"]])
  )
}

.lookup_diag <- function(diag_tbl, varnames, stat = c("rhat", "ess")) {
  stat <- match.arg(stat)
  if (is.null(diag_tbl) || !nrow(diag_tbl) || !(stat %in% names(diag_tbl))) {
    return(rep(NA_real_, length(varnames)))
  }
  
  idx <- match(varnames, diag_tbl$variable)
  out <- rep(NA_real_, length(varnames))
  ok <- !is.na(idx)
  out[ok] <- diag_tbl[[stat]][idx[ok]]
  out
}

.qcols <- function(cols) {
  # brms quantile columns are typically like Q2.5 and Q97.5.
  q <- cols[grepl("^Q", cols)]
  if (length(q) < 2) .stopf("Could not find quantile columns.")
  qnum <- suppressWarnings(as.numeric(sub("^Q", "", q)))
  ord <- order(qnum)
  list(lower = q[[ord[1]]], upper = q[[ord[length(ord)]]])
}

.unsanitize_country <- function(x, meta) {
  # If countries were made syntactic names, try to map back.
  x <- as.character(x)
  cands <- unique(c(
    meta$countries %||% character(),
    names(meta$country_code_of %||% list())
  ))
  if (!length(cands)) return(x)
  
  mp <- stats::setNames(cands, make.names(cands))
  out <- unname(mp[x])
  ifelse(is.na(out), x, out)
}

.make_tidy_row <- function(variable = NA_character_, level = NA_character_,
                           country = NA_character_, estimate = NA_real_,
                           std.error = NA_real_, lower = NA_real_,
                           upper = NA_real_, rhat = NA_real_,
                           ess = NA_real_, effect_type = "fixed",
                           param_type = "slope") {
  # Central constructor for consistent output columns across engines.
  tibble::tibble(
    variable = variable,
    level = level,
    country = country,
    estimate = estimate,
    std.error = std.error,
    lower = lower,
    upper = upper,
    rhat = rhat,
    ess = ess,
    effect_type = effect_type,
    param_type = param_type
  )
}

# ---- Baselines (reference categories) -------------------------------------

.empty_baseline_tbl <- function() {
  tibble::tibble(
    variable_id = character(), variable = character(),
    country = character(), baseline_code = character(), baseline = character()
  )
}

.besd_baselines <- function(fit) {
  if (!inherits(fit, c("besd_fit", "besd_fit_by_country"))) {
    return(tibble::tibble())
  }
  
  meta <- fit$meta %||% list()
  dem_dict <- meta$dem_dict %||% NULL
  
  .map_get <- function(mp, key) {
    if (is.null(mp) || is.na(key)) return(NA_character_)
    val <- unname(mp[[as.character(key)]])
    if (is.null(val) || !length(val)) return(NA_character_)
    as.character(val[[1]])
  }
  
  # --- by-country fits ------------------------------------------------------
  if (inherits(fit, "besd_fit_by_country")) {
    preds <- meta$predictors %||% character()
    countries <- meta$countries %||% names(fit$fits) %||% character()
    lvl_map <- meta$level_map %||% list()
    refs <- meta$ref_code_by_group %||% list()
    
    rows <- list()
    for (v in preds) {
      var_lab <- .var_label(v, dem_dict)
      
      mp <- lvl_map[[v]]
      code_to_label <- if (!is.null(mp) && all(c("code", "label") %in% names(mp))) {
        stats::setNames(as.character(mp$label), as.character(mp$code))
      } else {
        NULL
      }
      
      for (cc in countries) {
        bcode <- refs[[v]][[cc]] %||% NA_character_
        blab <- if (!is.null(code_to_label) && !is.na(bcode)) {
          .map_get(code_to_label, bcode)
        } else {
          NA_character_
        }
        
        rows[[length(rows) + 1]] <- tibble::tibble(
          variable_id = v,
          variable = var_lab,
          country = as.character(cc),
          baseline_code = as.character(bcode),
          baseline = as.character(blab)
        )
      }
    }
    
    if (!length(rows)) return(.empty_baseline_tbl())
    return(dplyr::bind_rows(rows))
  }
  
  # --- multilevel fits ------------------------------------------------------
  preds_common <- meta$predictors_common %||% character()
  preds_ctx <- meta$predictors_context %||% character()
  countries <- names(meta$country_code_of %||% list())
  
  lvl_common <- meta$level_map %||% list()
  lvl_ctx <- meta$level_map_context %||% list()
  
  # Older saved fits may not store ref_code_common; infer best-effort from model.
  ref_common <- meta$ref_code_common %||% list()
  if (!length(ref_common) && length(preds_common)) {
    ref_common <- .infer_common_ref_from_model(fit, preds_common)
  }
  
  # Context baselines may be stored in meta or embedded in term_map.
  ref_ctx <- meta$ref_code_by_group_context %||% list()
  tm_tbl <- .term_map_baselines(meta$term_map %||% list())
  
  rows <- list()
  
  # Common predictors have a single global baseline (country = NA).
  for (v in preds_common) {
    var_lab <- .var_label(v, dem_dict)
    bcode <- ref_common[[v]] %||% NA_character_
    blab <- NA_character_
    
    mp <- lvl_common[[v]]
    if (!is.null(mp) && all(c("code", "label") %in% names(mp)) && !is.na(bcode)) {
      blab <- as.character(mp$label[match(bcode, mp$code)][[1]])
    }
    
    rows[[length(rows) + 1]] <- tibble::tibble(
      variable_id = v,
      variable = var_lab,
      country = NA_character_,
      baseline_code = as.character(bcode),
      baseline = as.character(blab)
    )
  }
  
  # Context predictors have country-specific baselines.
  for (v in preds_ctx) {
    var_lab <- .var_label(v, dem_dict)
    mp <- lvl_ctx[[v]]
    
    code_to_label <- if (!is.null(mp) && all(c("code", "label") %in% names(mp))) {
      stats::setNames(as.character(mp$label), as.character(mp$code))
    } else {
      NULL
    }
    
    for (cc in countries %||% character()) {
      # Prefer term_map-embedded baselines because they reflect fallback logic.
      hit <- tm_tbl[tm_tbl$variable_id == v & tm_tbl$country == cc, , drop = FALSE]
      if (nrow(hit)) {
        bcode <- hit$baseline_code[[1]]
        blab <- hit$baseline[[1]]
      } else {
        bcode <- ref_ctx[[v]][[cc]] %||% NA_character_
        blab <- if (!is.null(code_to_label) && !is.na(bcode)) {
          .map_get(code_to_label, bcode)
        } else {
          NA_character_
        }
      }
      
      rows[[length(rows) + 1]] <- tibble::tibble(
        variable_id = v,
        variable = var_lab,
        country = as.character(cc),
        baseline_code = as.character(bcode),
        baseline = as.character(blab)
      )
    }
  }
  
  if (!length(rows)) return(.empty_baseline_tbl())
  dplyr::bind_rows(rows)
}

.term_map_baselines <- function(term_map) {
  # Extract baseline info stored in meta$term_map entries (if any).
  if (!length(term_map)) {
    return(tibble::tibble(
      variable_id = character(),
      country = character(),
      baseline_code = character(),
      baseline = character()
    ))
  }
  
  rows <- list()
  for (nm in names(term_map)) {
    mp <- term_map[[nm]]
    if (is.null(mp) || is.null(mp$var) || is.null(mp$country)) next
    if (is.null(mp$baseline_code) && is.null(mp$baseline_label)) next
    
    rows[[length(rows) + 1]] <- tibble::tibble(
      variable_id = as.character(mp$var),
      country = as.character(mp$country),
      baseline_code = as.character(mp$baseline_code %||% NA_character_),
      baseline = as.character(mp$baseline_label %||% NA_character_)
    )
  }
  
  if (!length(rows)) {
    return(tibble::tibble(
      variable_id = character(), country = character(),
      baseline_code = character(), baseline = character()
    ))
  }
  
  tmp <- dplyr::bind_rows(rows)
  tmp <- dplyr::filter(tmp, !is.na(.data$variable_id), !is.na(.data$country))
  dplyr::distinct(
    tmp,
    .data$variable_id,
    .data$country,
    .data$baseline_code,
    .data$baseline,
    .keep_all = TRUE
  )
}

.infer_common_ref_from_model <- function(fit, vars) {
  # Best-effort fallback for older saved fits without meta$ref_code_common.
  # We inspect the first available model and use factor level ordering.
  m <- NULL
  if (!is.null(fit$fits) && length(fit$fits) && is.list(fit$fits)) {
    for (yy in names(fit$fits)) {
      if (!is.null(fit$fits[[yy]])) {
        m <- fit$fits[[yy]]
        break
      }
    }
  }
  
  out <- setNames(as.list(rep(NA_character_, length(vars))), vars)
  if (is.null(m)) return(out)
  
  # glm/clm often store xlevels (factor levels per predictor).
  xlv <- tryCatch(m$xlevels, error = function(e) NULL)
  if (is.list(xlv) && length(xlv)) {
    for (v in intersect(vars, names(xlv))) {
      levs <- xlv[[v]]
      if (is.character(levs) && length(levs)) out[[v]] <- levs[[1]]
    }
    return(out)
  }
  
  # merMod/brmsfit: try model.frame if available.
  mf <- tryCatch(stats::model.frame(m), error = function(e) NULL)
  if (is.data.frame(mf)) {
    for (v in intersect(vars, names(mf))) {
      vv <- mf[[v]]
      if (is.factor(vv) && !is.ordered(vv) && length(levels(vv))) {
        out[[v]] <- levels(vv)[[1]]
      }
    }
  }
  out
}

.attach_baselines <- function(out, base_tbl) {
  # Robust no-op behavior: always return out with baseline columns present.
  if (is.null(out) || !nrow(out) || is.null(base_tbl) || !nrow(base_tbl)) {
    if (!("baseline" %in% names(out))) out$baseline <- NA_character_
    if (!("baseline_code" %in% names(out))) out$baseline_code <- NA_character_
    return(out)
  }
  
  out2 <- out
  
  # Use a normalized country key to allow "global" baselines to join cleanly.
  out2$`.__country_key__` <- ifelse(
    is.na(out2$country) | out2$country == "",
    ".GLOBAL",
    as.character(out2$country)
  )
  
  base2 <- base_tbl
  base2$`.__country_key__` <- ifelse(
    is.na(base2$country) | base2$country == "",
    ".GLOBAL",
    as.character(base2$country)
  )
  
  base2 <- dplyr::select(base2, variable, `.__country_key__`,
                         baseline, baseline_code)
  
  # 1) Exact (variable, country) match handles country-specific predictors.
  out2 <- dplyr::left_join(out2, base2, by = c("variable", ".__country_key__"))
  
  # 2) Fallback: if missing, use global baseline for that variable.
  base_global <- dplyr::filter(base2, `.__country_key__` == ".GLOBAL")
  base_global <- dplyr::select(
    base_global,
    variable,
    baseline_g = baseline,
    baseline_code_g = baseline_code
  )
  
  out2 <- dplyr::left_join(out2, base_global, by = "variable")
  out2$baseline <- dplyr::coalesce(out2$baseline, out2$baseline_g)
  out2$baseline_code <- dplyr::coalesce(out2$baseline_code, out2$baseline_code_g)
  
  out2$baseline_g <- NULL
  out2$baseline_code_g <- NULL
  out2$`.__country_key__` <- NULL
  
  # Keep baselines close to the "level" column for readability.
  if ("level" %in% names(out2)) {
    out2 <- dplyr::relocate(out2, baseline, baseline_code, .after = level)
  }
  out2
}