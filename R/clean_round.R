#' Clean a Single Afrobarometer Round
#'
#' Loads a single Afrobarometer SPSS file (or accepts a pre-loaded data frame),
#' harmonises variable names to conceptual names, replaces codebook sentinel
#' values with NA, recodes education, scales 0-4 items to 0-1, computes
#' composite indices, and optionally binarizes selected output variables.
#'
#' @param data Either a file path to an SPSS \code{.sav} file (character), or
#'   a data frame already read with \code{haven::read_spss()}.
#' @param round Integer (5-9). Specifies which Afrobarometer round the data
#'   belongs to.
#' @param sentinels Numeric vector of codebook sentinel values to replace with
#'   NA. Defaults to \code{AB_SENTINEL_VALUES}.
#' @param binarize_vars Character vector of output variable names to binarize
#'   (convert to 0/1 based on median). Typical choice:
#'   \code{c("local_gvt_performance")}. Set to \code{NULL} to skip.
#' @param binarize_threshold Optional numeric threshold for binarization. If
#'   NULL, each variable's median is used.
#' @param var_map Named list of variable mappings. Defaults to
#'   \code{AB_VAR_MAP}.
#' @param extra_vars Optional named character vector of additional raw columns
#'   to keep, e.g. \code{c(my_var = "Q99")}. Names become the output column
#'   names; values are the raw column names in the SPSS file.
#'
#' @return A tibble with harmonised, cleaned variables ready for analysis or
#'   row-binding across rounds with \code{ab_merge_rounds()}.
#'
#' @details
#' **Output columns always present:**
#' \itemize{
#'   \item \code{respondent}, \code{round}, \code{region}, \code{rural}
#'   \item \code{education} (ordered factor)
#'   \item News: \code{news_radio}, \code{news_tv}, \code{news_newspaper},
#'     \code{news_internet}, \code{news_sm} (scaled 0-1), \code{news_weekly}
#'     (internet share), \code{internet_news_weekly} (binary)
#'   \item Participation: \code{discuss_politics}, \code{raise_issue},
#'     \code{demonstrated}, \code{voted}, \code{attend_rally},
#'     \code{persuade_others}, \code{worked_campaign}
#'   \item Infrastructure (binary): \code{electricity}, \code{water},
#'     \code{sewage}, \code{phone_service}, \code{school}, \code{police_station},
#'     \code{clinic}, \code{police}, \code{soldiers}, \code{roadblocks},
#'     \code{paved_road}
#'   \item Contact: \code{contact_local_gvt}, \code{contact_mp},
#'     \code{contact_official}, \code{contact_traditional}
#'   \item Bribes: \code{bribe_for_permit}, \code{bribe_for_clinic},
#'     \code{bribe_for_police}, \code{bribe_for_school}
#'   \item Difficulty: \code{difficulty_id}, \code{difficulty_help_police},
#'     \code{dififculty_schooling}, \code{difficulty_treatment}
#'   \item Performance: \code{local_gvt_performance}, \code{often_use_internet}
#'   \item Composites: \code{services}, \code{facilities}, \code{security},
#'     \code{contact}, \code{bribes}, \code{difficulty_services},
#'     \code{civic_participation}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' ab5 <- ab_clean_round("path/to/tan_r5_data_july_2015.sav", round = 5)
#' ab6 <- ab_clean_round("path/to/tan_r6_data_eng.sav",       round = 6,
#'                        binarize_vars = "local_gvt_performance")
#' }
ab_clean_round <- function(data,
                            round,
                            sentinels        = AB_SENTINEL_VALUES,
                            binarize_vars    = NULL,
                            binarize_threshold = NULL,
                            var_map          = AB_VAR_MAP,
                            extra_vars       = NULL) {

  #  1. Load 
  if (is.character(data)) {
    if (!requireNamespace("haven", quietly = TRUE))
      stop("Package 'haven' is required. Install with install.packages('haven').")
    df <- haven::read_spss(data)
  } else {
    df <- as.data.frame(data)
  }

  # Strip haven labels so numeric comparisons work cleanly
  df <- as.data.frame(lapply(df, function(x) {
    if (inherits(x, "haven_labelled")) haven::zap_labels(x) else x
  }))

  #  2. Replace sentinel values across all numeric columns 
  df <- ab_replace_sentinels(df, sentinels = sentinels)

  #  3. Helper: pull raw column value, return NA vector if missing 
  pull_raw <- function(concept, max_val = NULL) {
    raw <- ab_lookup_var(concept, round, var_map)
    if (is.na(raw) || !raw %in% names(df)) {
      return(rep(NA_real_, nrow(df)))
    }
    v <- df[[raw]]
    if (!is.null(max_val)) v <- ab_scale_item(v, max_val = max_val)
    v
  }

  pull_bin <- function(raw_col, valid_min = 0, valid_max = 1) {
    # For 0/1 EA_ infrastructure variables already binary
    if (!raw_col %in% names(df)) return(rep(NA_real_, nrow(df)))
    v <- df[[raw_col]]
    ifelse(v == 1, 1L, ifelse(v == 0, 0L, NA_integer_))
  }

  pull_range <- function(concept, lo, hi) {
    raw <- ab_lookup_var(concept, round, var_map)
    if (is.na(raw) || !raw %in% names(df)) return(rep(NA_real_, nrow(df)))
    v <- df[[raw]]
    ifelse(v < lo | v > hi, NA_real_, as.numeric(v))
  }

  #  4. Build output data frame 
  edu_raw_col <- ab_lookup_var("education_raw", round, var_map)
  edu_raw     <- if (!is.na(edu_raw_col) && edu_raw_col %in% names(df))
                   df[[edu_raw_col]] else rep(NA_real_, nrow(df))

  # Road variable differs by round
  road_col <- switch(as.character(round),
    "5"  = "EA_ROAD",
    "EA_ROAD_B")
  road_col <- if (road_col %in% names(df)) road_col else NA_character_

  out <- data.frame(
    respondent = if ("RESPNO" %in% names(df)) df[["RESPNO"]] else NA_integer_,
    round      = as.integer(round),
    region     = if ("REGION" %in% names(df)) df[["REGION"]] else NA_character_,
    rural      = ifelse(df[["URBRUR"]] == 2, 1L, 0L),
    education  = ab_recode_education(edu_raw),

    # News media (scaled 0-1)
    news_radio      = pull_raw("news_radio",     max_val = 4),
    news_tv         = pull_raw("news_tv",         max_val = 4),
    news_newspaper  = pull_raw("news_newspaper",  max_val = 4),
    news_internet   = pull_raw("news_internet",   max_val = 4),
    news_sm         = pull_raw("news_sm",          max_val = 4),

    # Political participation
    discuss_politics = pull_range("discuss_politics", 0, 3),
    raise_issue      = pull_range("raise_issue",      0, 5),
    demonstrated     = pull_range("demonstrated",     0, 5),
    voted            = pull_range("voted",             0, 8),
    attend_rally     = pull_range("attend_rally",      0, 1),
    persuade_others  = pull_range("persuade_others",   0, 1),
    worked_campaign  = pull_range("worked_campaign",   0, 1),

    # Infrastructure (binary)
    electricity   = pull_bin("EA_SVC_A"),
    water         = pull_bin("EA_SVC_B"),
    sewage        = pull_bin("EA_SVC_C"),
    phone_service = pull_bin("EA_SVC_D"),
    school        = pull_bin("EA_FAC_B"),
    police_station = pull_bin("EA_FAC_C"),
    clinic        = pull_bin("EA_FAC_D"),
    police        = pull_bin("EA_SEC_A"),
    soldiers      = pull_bin("EA_SEC_B"),
    roadblocks    = pull_bin("EA_SEC_C"),
    paved_road    = if (!is.na(road_col)) pull_bin(road_col) else NA_integer_,

    # Contact with leaders
    contact_local_gvt   = pull_range("contact_local_gvt",   0, 3),
    contact_mp          = pull_range("contact_mp",           0, 3),
    contact_official    = pull_range("contact_official",     0, 3),
    contact_traditional = pull_range("contact_traditional",  0, 3),

    # Bribes
    bribe_for_permit = pull_range("bribe_for_permit", 0, 4),
    bribe_for_clinic = pull_range("bribe_for_clinic", 0, 4),
    bribe_for_police = pull_range("bribe_for_police", 0, 4),
    bribe_for_school = pull_range("bribe_for_school", 0, 4),

    # Difficulty accessing services
    difficulty_id          = pull_range("difficulty_id",           0, 4),
    difficulty_help_police = pull_range("difficulty_help_police",  0, 4),
    dififculty_schooling   = pull_range("dififculty_schooling",    0, 4),
    difficulty_treatment   = pull_range("difficulty_treatment",    0, 4),

    # Performance & internet
    local_gvt_performance = pull_range("local_gvt_performance", 0, 4),
    often_use_internet    = pull_range("often_use_internet",     0, 4),

    stringsAsFactors = FALSE
  )

  #  5. Derived news variables 
  out$news <- rowSums(out[, c("news_radio","news_tv","news_newspaper",
                               "news_internet","news_sm")], na.rm = FALSE)
  out$news_weekly <- with(out,
    ifelse(news == 0, NA_real_, (news_internet + news_sm) / news))

  # Binary: watched internet/SM news at least weekly (codes 3 or 4 in raw)
  ni_raw <- ab_lookup_var("news_internet", round, var_map)
  ns_raw <- ab_lookup_var("news_sm",       round, var_map)
  ni_vec <- if (!is.na(ni_raw) && ni_raw %in% names(df)) df[[ni_raw]] else rep(NA_real_, nrow(df))
  ns_vec <- if (!is.na(ns_raw) && ns_raw %in% names(df)) df[[ns_raw]] else rep(NA_real_, nrow(df))
  out$internet_news_weekly <- as.integer(
    (ni_vec %in% c(3, 4)) | (ns_vec %in% c(3, 4))
  )

  #  6. Composite indices (row-wise means, ignoring NA) 
  services_cols     <- c("electricity","water","sewage","phone_service")
  facilities_cols   <- c("school","police_station","clinic")
  security_cols     <- c("police","soldiers","roadblocks")
  contact_cols      <- c("contact_local_gvt","contact_mp",
                         "contact_official","contact_traditional")
  bribes_cols       <- c("bribe_for_permit","bribe_for_clinic",
                         "bribe_for_police","bribe_for_school")
  difficulty_cols   <- c("difficulty_id","difficulty_help_police",
                         "dififculty_schooling","difficulty_treatment")
  civic_cols        <- c("discuss_politics","raise_issue","demonstrated",
                         "voted","attend_rally","persuade_others","worked_campaign")

  row_mean <- function(df_sub, cols) {
    m <- df_sub[, intersect(cols, names(df_sub)), drop = FALSE]
    rowMeans(m, na.rm = TRUE)
  }

  out$services           <- row_mean(out, services_cols)
  out$facilities         <- row_mean(out, facilities_cols)
  out$security           <- row_mean(out, security_cols)
  out$contact            <- row_mean(out, contact_cols)
  out$bribes             <- row_mean(out, bribes_cols)
  out$difficulty_services <- row_mean(out, difficulty_cols)
  out$civic_participation <- row_mean(out, civic_cols)

  #  7. Extra user-specified raw columns 
  if (!is.null(extra_vars)) {
    for (nm in names(extra_vars)) {
      raw_col <- extra_vars[[nm]]
      out[[nm]] <- if (raw_col %in% names(df)) df[[raw_col]] else NA
    }
  }

  #  8. Binarize requested output variables 
  if (!is.null(binarize_vars)) {
    out <- ab_binarize(out,
                       cols      = binarize_vars,
                       threshold = binarize_threshold,
                       use_median = is.null(binarize_threshold))
  }

  dplyr::as_tibble(out)
}
