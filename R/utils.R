#' Replace Afrobarometer Codebook Sentinel Values with NA
#'
#' Scans every numeric column in a data frame and replaces any value found
#' in \code{sentinels} with \code{NA}. By default uses \code{AB_SENTINEL_VALUES}
#' which covers the standard Afrobarometer "Not Applicable / Refused / Missing"
#' codes: -1, 7, 8, 9, 98, 99.
#'
#' @param df A data frame, typically a raw Afrobarometer SPSS import.
#' @param sentinels Numeric vector of values to replace with NA.
#'   Defaults to \code{AB_SENTINEL_VALUES}.
#' @param cols Optional character vector of column names to restrict cleaning
#'   to. If NULL (default), all numeric columns are cleaned.
#'
#' @return The data frame with sentinel values replaced by \code{NA}.
#' @export
#'
#' @examples
#' \dontrun{
#' ab5 <- haven::read_spss("tan_r5_data_july_2015.sav")
#' ab5 <- ab_replace_sentinels(ab5)
#' }
ab_replace_sentinels <- function(df,
                                 sentinels = AB_SENTINEL_VALUES,
                                 cols = NULL) {
  if (is.null(cols)) {
    cols <- names(df)[sapply(df, is.numeric)]
  }
  df[cols] <- lapply(df[cols], function(x) {
    x[x %in% sentinels] <- NA
    x
  })
  df
}


#' Binarize Selected Columns in a Data Frame
#'
#' Converts one or more columns to binary (0/1) based on a threshold. Values
#' strictly greater than \code{threshold} become 1; values at or below become
#' 0; NAs are preserved.
#'
#' @param df A data frame.
#' @param cols Character vector of column names to binarize.
#' @param threshold Numeric scalar. Values \emph{above} this threshold become 1.
#'   Default is the column median (computed per column, ignoring NA).
#' @param use_median Logical. If TRUE (default) and \code{threshold} is not
#'   explicitly provided, uses each column's median as its threshold.
#'
#' @return The data frame with the specified columns binarized.
#' @export
#'
#' @examples
#' df <- data.frame(score = c(1, 2, 3, 4, NA), rating = c(3, 1, 4, 2, 3))
#' ab_binarize(df, cols = c("score", "rating"), threshold = 2)
ab_binarize <- function(df, cols, threshold = NULL, use_median = TRUE) {
  for (col in cols) {
    if (!col %in% names(df)) {
      warning(sprintf("Column '%s' not found in data frame; skipping.", col))
      next
    }
    thresh <- if (!is.null(threshold)) {
      threshold
    } else if (use_median) {
      stats::median(df[[col]], na.rm = TRUE)
    } else {
      stop("Provide either `threshold` or set `use_median = TRUE`.")
    }
    df[[col]] <- ifelse(is.na(df[[col]]), NA,
                        ifelse(df[[col]] > thresh, 1L, 0L))
  }
  df
}


#' Recode Education Variable
#'
#' Converts the raw Afrobarometer education numeric code to an ordered factor
#' using the standard Afrobarometer codebook categories.
#'
#' @param x Numeric vector of raw education codes.
#'
#' @return An ordered factor with levels:
#'   No schooling < Informal or primary < Secondary < University or tech < Advanced.
#' @export
#'
#' @examples
#' ab_recode_education(c(0, 2, 5, 7, 9, NA))
ab_recode_education <- function(x) {
  edu <- dplyr::case_when(
    x == 0             ~ "No schooling",
    x > 1  & x < 4    ~ "Informal or primary",
    x > 3  & x < 6    ~ "Secondary",
    x > 5  & x < 9    ~ "University or tech",
    x > 8  & x < 10   ~ "Advanced",
    TRUE               ~ NA_character_
  )
  factor(edu,
         levels = AB_EDUCATION_LEVELS,
         ordered = TRUE)
}


#' Look Up a Raw Variable Name for a Given Round
#'
#' A helper to retrieve the raw survey column name for a conceptual variable
#' in a specific Afrobarometer round, using \code{AB_VAR_MAP}.
#'
#' @param concept_var Character. Name of the conceptual variable
#'   (e.g. \code{"news_radio"}).
#' @param round Integer. Survey round (5-9).
#' @param var_map Named list. Defaults to \code{AB_VAR_MAP}.
#'
#' @return Character scalar raw variable name, or \code{NA_character_} if not
#'   available in that round.
#' @export
#'
#' @examples
#' ab_lookup_var("news_radio", round = 5)   # "Q13A"
#' ab_lookup_var("news_sm",    round = 5)   # NA - not in round 5
ab_lookup_var <- function(concept_var, round, var_map = AB_VAR_MAP) {
  if (!concept_var %in% names(var_map)) {
    stop(sprintf("'%s' is not a recognised conceptual variable in AB_VAR_MAP.",
                 concept_var))
  }
  mapping <- var_map[[concept_var]]
  row <- mapping[mapping$round == round, ]
  if (nrow(row) == 0) return(NA_character_)
  as.character(row$raw_var[1])
}


#' Scale a 0-4 Afrobarometer Item to 0-1
#'
#' Divides values by 4 after setting out-of-range values (negative or > 4) to
#' NA. This matches the normalisation used in the original dissertation script.
#'
#' @param x Numeric vector with raw Afrobarometer 0-4 response scale.
#' @param max_val Maximum valid value. Default 4.
#'
#' @return Numeric vector scaled to [0, 1].
#' @export
ab_scale_item <- function(x, max_val = 4) {
  x[x < 0 | x > max_val] <- NA
  x / max_val
}
