
# ── besd_dictionary() ──────────────────────────────────────────────────────────
#' BeSD item dictionary
#'
#' Built-in harmonised dictionary of core quantitative BeSD items. Questions and responses
#' align precisely with the BeSD quantitative toolkit see:
#' \url{https://www.who.int/publications/i/item/9789240049680} (accessed 24 Feb 2026)
#'
#' @return A tibble with item metadata.
#' @export
besd_dictionary <- function() {
  
  items <- list(
    # Thinking and Feeling
    tf_benefits = list(
      domain = "thinking and feeling",
      item_type = "ordinal",
      levels = c("Not at all important", "A little important", "Moderately important", 
                 "Very important"),
      toplevs = c("Moderately important", "Very important"),
      reverse = FALSE,
      question = "How important do you think vaccines are for your child's health?",
      question_short = "Confidence in vaccine benefits"
    ),
    tf_safety = list(
      domain = "thinking and feeling",
      item_type = "ordinal",
      levels = c("Not at all safe", "A little safe", "Moderately safe", "Very safe"),
      toplevs = c("Moderately safe", "Very safe"),
      reverse = FALSE,
      question = "How safe do you think vaccines are for your child?",
      question_short = "Confidence in vaccine safety"
    ),
    tf_hcws = list(
      domain = "thinking and feeling",
      item_type = "ordinal",
      levels = c("Not at all", "A little", "Moderately", "Very much"),
      toplevs = c("Moderately", "Very much"),
      reverse = FALSE,
      question = "How much do you trust the health workers who give children vaccines?",
      question_short = "Confidence in health workers"
    ),
    # Social Processes
    so_peer = list(
      domain = "social processes",
      item_type = "binary",
      levels = c("No", "Yes"),
      toplevs = c("Yes"),
      reverse = FALSE,
      question = "Do you think most parents you know get their children vaccinated?",
      question_short = "Peer norms"
    ),
    so_family = list(
      domain = "social processes",
      item_type = "binary",
      levels = c("No", "Yes"),
      toplevs = c("Yes"),
      reverse = FALSE,
      question = paste0(
        "Do you think most of your close family and friends want ", 
        "you to get your child vaccinated?"
      ),
      question_short = "Family norms"
    ),
    so_religious = list(
      domain = "social processes",
      item_type = "binary",
      levels = c("No", "Yes"),
      toplevs = c("Yes"),
      reverse = FALSE,
      question = paste0(
        "Do you think your religious leaders want you to get your child vaccinated?"
      ),
      question_short = "Religious leader norms"
    ),
    so_community = list(
      domain = "social processes",
      item_type = "binary",
      levels = c("No", "Yes"),
      toplevs = c("Yes"),
      reverse = FALSE,
      question = paste0(
        "Do you think your community leaders want you to get your child vaccinated?"
      ),
      question_short = "Community leader norms"
    ),
    so_hcw_rec = list(
      domain = "social processes",
      item_type = "binary",
      levels = c("No", "Yes"),
      toplevs = c("Yes"),
      reverse = FALSE,
      question = "Has a health worker recommended your child be vaccinated?",
      question_short = "Health worker recommendation"
    ),
    so_travel_autonomy = list(
      domain = "social processes",
      item_type = "binary",
      levels = c("No", "Yes"),
      toplevs = c("No"),
      reverse = TRUE,
      question = "Would the mother need permission to take your child to the clinic?",
      question_short = "Mother's travel autonomy"
    ),
    # Practical Issues
    pr_recall = list(
      domain = "practical issues",
      item_type = "binary",
      levels = c("No", "Yes"),
      toplevs = c("Yes"),
      reverse = FALSE,
      question = paste0(
        "Have you ever been contacted about your child being due for vaccination?"
      ),
      question_short = "Received recall"
    ),
    pr_know_where = list(
      domain = "practical issues",
      item_type = "binary",
      levels = c("No", "Yes"),
      toplevs = c("Yes"),
      reverse = FALSE,
      question = "Do you know where to go to get your child vaccinated?",
      question_short = "Know where to go to get vaccination"
    ),
    pr_took_child = list(
      domain = "practical issues",
      item_type = "binary",
      levels = c("No", "Yes"),
      toplevs = c("Yes"),
      reverse = FALSE,
      question = "Have you personally ever taken your youngest child to get vaccinated?",
      question_short = "Took child for vaccination"
    ),
    pr_turned_away = list(
      domain = "practical issues",
      item_type = "binary",
      levels = c("No", "Yes"),
      toplevs = c("No"),
      reverse = TRUE,
      question = paste0(
        "Have you ever been turned away when you tried to get your child vaccinated?"
      ),
      question_short = "Vaccination availability"
    ),
    pr_ease_access = list(
      domain = "practical issues",
      item_type = "ordinal",
      levels = c("Not at all easy", "A little easy", "Moderately easy", "Very easy"),
      toplevs = c("Moderately easy", "Very easy"),
      reverse = FALSE,
      question = "How easy is it to get vaccination services for your child?",
      question_short = "Ease of access"
    ),
    pr_afford = list(
      domain = "practical issues",
      item_type = "ordinal",
      levels = c("Not at all easy", "A little easy", "Moderately easy", "Very easy"),
      toplevs = c("Moderately easy", "Very easy"),
      reverse = FALSE,
      question = paste0(
        "How easy is it to pay for vaccination? (payments, cost of travel, ", 
        "cost of time, etc)"
      ),
      question_short = "Affordability"
    ),
    pr_service_satisfaction = list(
      domain = "practical issues",
      item_type = "ordinal",
      levels = c(
        "Not at all satisfied", 
        "A little satisfied", 
        "Moderately satisfied", 
        "Very satisfied"
      ),
      toplevs = c("Moderately satisfied", "Very satisfied"),
      reverse = FALSE,
      question = "How satisfied are you with the vaccination services?",
      question_short = "Service satisfaction"
    ),
    
    # Practical Issues (Reasons)
    pr_reasons_ease_access = list(
      domain = "practical issues",
      item_type = "multichoice",
      levels = c(
        "Nothing, it's not hard",
        "Getting to the clinic is hard",
        "The clinic opening times are inconvenient",
        "The clinic sometimes turns people away without vaccinating",
        "The waiting time in the clinic takes too long",
        "I cannot leave my job for long enough",
        "My family and household responsibilities make it difficult for me to find time",
        "I am not allowed to visit the clinic on my own",
        "I am made to feel unwelcome at the clinic",
        "I cannot afford to pay for transport or clinic services",
        "The clinic only had health workers of the opposite sex",
        "Is there something else?"
      ),
      toplevs = NULL,
      reverse = NA,
      question = "What makes it hard to get vaccination services for your child?",
      question_short = "Reasons for low ease of access"
    ),
    pr_service_quality = list(
      domain = "practical issues",
      item_type = "multichoice",
      levels = c(
        "Nothing, you are satisfied",
        "Vaccine is not always available",
        "The clinic does not open on time",
        "Waiting times are long",
        "The clinic is not clean",
        "Staff are poorly trained",
        "Staff are not respectful",
        "Staff do not spend enough time with people",
        "Is there something else?"
      ),
      toplevs = NULL,
      reverse = NA,
      question = "What is not satisfactory about the vaccination services?",
      question_short = "Service quality"
    )
  )
  
  .build_dictionary(items)
}


# ── dem_dictionary() ───────────────────────────────────────────────────────────
#' Socio-demographic item dictionary
#'
#' @return A tibble with item metadata.
#' @export
dem_dictionary <- function() {
  
  items <- list(
    dem_gen = list(
      domain = "predictor",
      item_type = "categorical",
      levels = c("Man", "Woman", "Nonbinary", "Other"),
      toplevs = NA,
      reverse = NA,
      question = "What is your gender?",
      question_short = "Gender"
    ),
    dem_age = list(
      domain = "predictor",
      item_type = "categorical",
      levels = c(
        "18-24 years old",
        "25-34 years old",
        "35-44 years old",
        "45-54 years old",
        "55+ years"
      ),
      toplevs = NA,
      reverse = NA,
      question = "How old are you?",
      question_short = "Age"
    )
  )
  
  .build_dictionary(items)
}


# ── modify_dictionary() ────────────────────────────────────────────────────────
#' Add / remove items from dictionary
#'
#' Items being added must include `item_id` and all required columns.
#'
#' @param df Data frame/tibble of new/updated items. Must include dictionary columns
#'  including `item_id`.
#' @param id_out Character vector of item_id values to remove.
#' @param dict Existing dictionary.
#' @param replace If TRUE, overwrite existing rows with same item_id.
#' @export
modify_dictionary <- function(df = NULL,
                              id_out = NULL,
                              dict = besd_dictionary(),
                              replace = TRUE) {
  # Input arg checks
  .assert_valid_dict(dict) # validate dictionary
  if (is.null(df) && is.null(id_out)) {
    .stopf("One of `df` and `id_out` needs to be non-NULL.")
  }
  
  # Remove items
  if (!is.null(id_out) && length(id_out)) {
    if (!is.character(id_out)) {
      .stopf("`id_out` must be a character vector of item_id values.")
    }
    if (anyNA(id_out) || any(id_out == "")) {
      .stopf("Empty/NA values found in `id_out`.")
    }
    
    id_out <- unique(id_out)
    
    missing_ids <- setdiff(id_out, dict$item_id)
    if (length(missing_ids)) {
      .stopf("`id_out` contains item_id not in `dict`: %s", .pastec(missing_ids))
    }
    
    dict <- dict[!(dict$item_id %in% id_out), , drop = FALSE]
  }
  
  # Add / update items
  if (!is.null(df)) {
    req <- .dictionary_cols(TRUE)  # required dictionary columns
    if (!is.data.frame(df)) .stopf("`df` must be a data.frame/tibble.")
    if (!("item_id" %in% names(df))) .stopf("`df` must include an `item_id` column.")
    
    .assert_has_cols(df, req, "df")
    df <- df[, req, drop = FALSE]
    
    # Coerce flat character `levels` to list-column (one entry per item_id)
    if (is.character(df$levels)) {
      df <- df |>
        dplyr::reframe(
          dplyr::across(dplyr::all_of(setdiff(req, c("item_id", "levels"))), dplyr::first),
          levels = list(levels),
          .by = item_id
        )
      df <- df[, req, drop = FALSE]
    }
    # Coerce levels list-column entries to character vectors. Guards against
    # factor columns being passed in via sort(unique(factor_col)), which would
    # store factor integer codes instead of labels after any c(factor, character)
    # operation downstream.
    if (is.list(df$levels)) {
      df$levels <- lapply(df$levels, function(x) {
        if (is.factor(x)) as.character(x) else x
      })
    }
    
    if (replace) {
      dict <- dict[!(dict$item_id %in% df$item_id), , drop = FALSE]
    } else {
      overlap <- intersect(dict$item_id, df$item_id)
      if (length(overlap)) {
        .stopf("`df` contains item_id already in `dict`: %s", .pastec(overlap))
      }
    }
    
    dict <- dplyr::bind_rows(dict, df)
  }
  
  .assert_valid_dict(dict)
  dict
}



# ── Helpers ────────────────────────────────────────────────────────────────────

# Update levels for a single column inside the dicts embedded in a besd_info
# list. Removes `remove` labels and appends `add` labels (NA values in `add`
# are silently dropped, so NA-target recodes in besd_recode_levels() are safe).
# Returns the modified info list. Used by besd_recode_levels() and
# besd_recode_missing() to keep dict$levels consistent with the data.
.update_dict_levels <- function(info, col, remove, add = character(0)) {
  add <- add[!is.na(add)]
  .do_update <- function(d) {
    if (is.null(d)) return(NULL)
    d   <- tibble::as_tibble(d)
    idx <- match(col, d$item_id)
    if (is.na(idx)) return(d)
    levs            <- d$levels[[idx]]
    levs            <- levs[!levs %in% remove]
    levs            <- c(levs, setdiff(add, levs))
    d$levels[[idx]] <- levs
    d
  }
  info$besd_dict <- .do_update(info$besd_dict)
  info$dem_dict  <- .do_update(info$dem_dict)
  info
}

# Return the required dictionary column names in the correct order. When
# `with_id = TRUE`, prepends "item_id" for contexts that need the full schema.
.dictionary_cols <- function(with_id = FALSE) {
  cols <- c("domain", "item_type", "levels", "toplevs", "reverse",
            "question", "question_short")
  if (isTRUE(with_id)) cols <- c("item_id", cols)
  cols
}

# Construct a validated dictionary tibble from a named list of item definitions.
# Names become `item_id`. Each element must contain exactly the fields returned by 
# .dictionary_cols(); missing or unknown fields stop with an informative message.
.build_dictionary <- function(items) {
  req <- .dictionary_cols(FALSE)
  
  if (!is.list(items) || is.null(names(items)) || any(names(items) == "")) {
    .stopf("`items` must be a *named* list; names are `item_id`.")
  }
  
  ids <- names(items)
  
  rows <- lapply(ids, function(item_id) {
    x <- items[[item_id]]
    if (!is.list(x)) .stopf("Item `%s` must be a list.", item_id)
    
    missing <- setdiff(req, names(x))
    extra   <- setdiff(names(x), req)
    if (length(missing)) {
      .stopf("Item `%s` missing field(s): %s", item_id, paste(missing, collapse = ", "))
    }
    if (length(extra)) {
      .stopf("Item `%s` has unknown field(s): %s", item_id, paste(extra, collapse = ", "))
    }
    if (!is.character(x$domain) || length(x$domain) != 1) {
      .stopf("`domain` must be length-1 character for `%s`.", item_id)
    }
    if (!is.character(x$item_type) || length(x$item_type) != 1) {
      .stopf("`item_type` must be length-1 character for `%s`.", item_id)
    }
    if (!is.character(x$levels)) {
      .stopf("`levels` must be a character vector for `%s`.", item_id)
    }
    if (!(is.null(x$toplevs) ||
          is.character(x$toplevs) ||
          (is.logical(x$toplevs) && all(is.na(x$toplevs))))) {
      .stopf("`toplevs` must be NULL, a character vector, or NA for `%s`.", item_id)
    }
    if (is.character(x$toplevs)) {
      bad_tl <- setdiff(x$toplevs, as.character(x$levels))
      if (length(bad_tl)) {
        .stopf("`toplevs` for `%s` contains value(s) not in `levels`: %s",
               item_id, paste(bad_tl, collapse = ", "))
      }
    }
    if (!(is.logical(x$reverse) && length(x$reverse) == 1) && !is.na(x$reverse)) {
      .stopf("`reverse` must be TRUE/FALSE/NA for `%s`.", item_id)
    }
    if (!is.character(x$question) || length(x$question) != 1) {
      .stopf("`question` must be length-1 character for `%s`.", item_id)
    }
    if (!is.character(x$question_short) || length(x$question_short) != 1) {
      .stopf("`question_short` must be length-1 character for `%s`.", item_id)
    }

    toplevs_stored <- if (is.character(x$toplevs)) {
      list(as.character(x$toplevs))
    } else {
      list(NA_character_)
    }

    tibble::tibble(
      item_id        = item_id,
      domain         = x$domain,
      item_type      = x$item_type,
      levels         = list(as.character(x$levels)),
      toplevs        = toplevs_stored,
      reverse        = x$reverse,
      question       = x$question,
      question_short = x$question_short
    )
  })
  
  out <- dplyr::bind_rows(rows)
  if (anyDuplicated(out$item_id)) .stopf("Duplicate `item_id` found in dictionary.")
  out
}

# Look up the `item_type` of a single item in `dict`. Stops if the item not found.
.item_type <- function(dict, item_id) {
  m <- dict[dict$item_id == item_id, , drop = FALSE]
  if (nrow(m) == 0) .stopf("Item not in dictionary: %s", item_id)
  m$item_type[[1]]
}

# Validate a dictionary data frame and return a character vector of problem
# descriptions (empty if valid). Checks required columns, types, allowed `item_type` 
# values, `reverse` logic, and that `levels` is a non-empty list-column of character 
# vectors. Returns problems rather than stopping so callers can choose how to handle.
.validate_dict <- function(dict,
                           with_id = TRUE,
                           allowed_item_types = c(
                             "binary", 
                             "ordinal", 
                             "categorical", 
                             "multichoice"
                           )) {
  probs <- character()
  
  if (!is.data.frame(dict)) {
    return("`dict` must be a data.frame/tibble.")
  }
  
  req <- .dictionary_cols(with_id)
  miss <- setdiff(req, names(dict))
  extra   <- setdiff(names(dict), req)
  
  if (length(miss)) probs <- c(probs, sprintf("Missing column(s): %s", .pastec(miss)))
  if (length(extra))   probs <- c(probs, sprintf("Unknown column(s): %s", .pastec(extra)))
  if (length(probs)) return(probs)
  
  n <- nrow(dict)
  
  # item_id
  if (with_id) {
    if (!is.character(dict$item_id)) {
      probs <- c(probs, "`item_id` must be character.")
    } else {
      if (anyNA(dict$item_id) || any(dict$item_id == "")) {
        probs <- c(probs, "Empty/NA `item_id` found.")
      }
      if (anyDuplicated(dict$item_id)) {
        probs <- c(probs, "Duplicate `item_id` found.")
      }
    }
  }
  
  # scalar character columns
  for (nm in c("domain", "item_type", "question", "question_short")) {
    if (!is.character(dict[[nm]])) {
      probs <- c(probs, sprintf("`%s` must be character.", nm))
    }
    if (anyNA(dict[[nm]]) || any(dict[[nm]] == "")) {
      probs <- c(probs, sprintf("Empty/NA `%s` found.", nm))
    } 
  }
  
  # item_type values
  if (is.character(dict$item_type)) {
    bad <- setdiff(unique(dict$item_type), allowed_item_types)
    if (length(bad)) probs <- c(probs, sprintf("Unknown `item_type`: %s", .pastec(bad)))
  }
  
  # reverse
  if (!is.logical(dict$reverse) || length(dict$reverse) != n) {
    probs <- c(probs, "`reverse` must be a logical vector with length nrow(dict).")
  }
  
  # levels: must be a list-column of character vectors
  if (!is.list(dict$levels) || length(dict$levels) != n) {
    probs <- c(probs, "`levels` must be a list-column with length nrow(dict).")
  } else {
    bad_levels <- vapply(dict$levels, function(x) {
      x <- if (is.factor(x)) as.character(x) else x
      !is.character(x) || length(x) < 1 || anyNA(x) || any(x == "")
    }, logical(1))
    if (any(bad_levels)) {
      probs <- c(
        probs,
        sprintf(
          "`levels` must contain non-empty character vectors; bad row(s): %s",
          .pastec(which(bad_levels))
        )
      )
    }
  }

  # toplevs: list-column of NA or character vectors that are subsets of levels
  if (!is.list(dict$toplevs) || length(dict$toplevs) != n) {
    probs <- c(probs, "`toplevs` must be a list-column with length nrow(dict).")
  } else if (is.list(dict$levels)) {
    bad_tl <- vapply(seq_len(n), function(i) {
      tl <- dict$toplevs[[i]]
      if (is.null(tl) || (length(tl) == 1 && is.na(tl[[1]]))) return(FALSE)
      if (!is.character(tl) || anyNA(tl) || any(tl == "")) return(TRUE)
      lv <- dict$levels[[i]]
      length(setdiff(tl, as.character(lv))) > 0
    }, logical(1))
    if (any(bad_tl)) {
      probs <- c(
        probs,
        sprintf(
          "`toplevs` must be NA or a character subset of `levels`; bad row(s): %s",
          .pastec(which(bad_tl))
        )
      )
    }
  }
  
  # Warn if any duplicates
  if (anyDuplicated(dict$item_id)) {
    dup <- unique(dict$item_id[duplicated(dict$item_id)])
    .stopf("Duplicate item_id in combined dictionary: %s", paste(dup, collapse = ", "))
  }
  
  probs
}