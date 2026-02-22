#' afrobarometer: Clean and Harmonize Afrobarometer Survey Data
#'
#' The \pkg{afrobarometer.cleaning} package provides tools to load, clean, and
#' harmonise Afrobarometer survey data across rounds 5-9. It handles
#' round-specific variable name differences via a built-in mapping table,
#' replaces codebook "not applicable" sentinel values with \code{NA},
#' scales 0-4 Likert items to 0-1, computes composite indices, and
#' optionally binarizes output variables.
#'
#' @section Main functions:
#' \describe{
#'   \item{\code{\link{ab_clean_round}}}{Clean a single Afrobarometer round.}
#'   \item{\code{\link{ab_merge_rounds}}}{Clean and merge multiple rounds.}
#'   \item{\code{\link{ab_replace_sentinels}}}{Replace codebook sentinel values with NA.}
#'   \item{\code{\link{ab_binarize}}}{Binarize selected variables.}
#'   \item{\code{\link{ab_recode_education}}}{Recode education to ordered factor.}
#'   \item{\code{\link{ab_media_trends}}}{Summarise media consumption trends.}
#'   \item{\code{\link{ab_lookup_var}}}{Look up raw variable name for a given round.}
#' }
#'
#' @section Key data objects:
#' \describe{
#'   \item{\code{\link{AB_SENTINEL_VALUES}}}{Standard codebook NA sentinel codes.}
#'   \item{\code{\link{AB_VAR_MAP}}}{Cross-round variable name mapping.}
#' }
#'
#' @docType package
#' @name afrobarometer.cleaning-package
#' @aliases afrobarometer.cleaning
"_PACKAGE"

#' @importFrom dplyr %>% group_by summarise mutate select filter bind_rows
#'   as_tibble case_when rowwise c_across
#' @importFrom rlang .data
#' @importFrom stats median
NULL
