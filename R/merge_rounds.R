#' Load and Clean Multiple Afrobarometer Rounds
#'
#' A convenience wrapper around \code{\link{ab_clean_round}} that accepts a
#' named list of file paths (or pre-loaded data frames), cleans each round in
#' turn, and row-binds them into a single harmonised tibble.
#'
#' @param sources Named list where \emph{names} are round numbers (as
#'   characters or integers) and \emph{values} are either file paths to SPSS
#'   \code{.sav} files or pre-loaded data frames. Example:
#'   \code{list("5" = "path/r5.sav", "6" = "path/r6.sav")}.
#' @param sentinels Numeric vector of sentinel values to replace with NA.
#'   Defaults to \code{AB_SENTINEL_VALUES}.
#' @param binarize_vars Character vector of output variable names to binarize
#'   across all rounds. Defaults to \code{NULL}.
#' @param binarize_threshold Optional numeric threshold for binarization.
#' @param var_map Variable mapping list. Defaults to \code{AB_VAR_MAP}.
#' @param extra_vars Optional named list of extra raw columns per round.
#'   Either a single named character vector applied to all rounds, or a named
#'   list of named character vectors keyed by round number.
#' @param verbose Logical. Print progress messages. Default TRUE.
#'
#' @return A single tibble with all rounds row-bound together. A column
#'   \code{round} (integer) identifies the source round.
#' @export
#'
#' @examples
#' \dontrun{
#' sources <- list(
#'   "5" = "data/tan_r5_data_july_2015.sav",
#'   "6" = "data/tan_r6_data_eng.sav",
#'   "7" = "data/tan_r7_data.sav",
#'   "8" = "data/afrobarometer_release-data_tan_r8_en_2021-06-10.sav",
#'   "9" = "data/TAN_R9.data_.final_.wtd_release.30May23.sav"
#' )
#'
#' ab <- ab_merge_rounds(sources,
#'                        binarize_vars = "local_gvt_performance")
#' }
ab_merge_rounds <- function(sources,
                             sentinels          = AB_SENTINEL_VALUES,
                             binarize_vars      = NULL,
                             binarize_threshold = NULL,
                             var_map            = AB_VAR_MAP,
                             extra_vars         = NULL,
                             verbose            = TRUE) {

  rounds <- as.integer(names(sources))
  if (any(is.na(rounds))) {
    stop("Names of `sources` must be coercible to integers (e.g. '5', '6', ...).")
  }

  cleaned <- vector("list", length(sources))

  for (i in seq_along(sources)) {
    r <- rounds[i]
    if (verbose) message(sprintf("Cleaning round %d ...", r))

    # Resolve per-round extra_vars
    ev <- if (is.null(extra_vars)) {
      NULL
    } else if (is.list(extra_vars) && !is.null(names(extra_vars)) &&
               all(names(extra_vars) %in% as.character(rounds))) {
      extra_vars[[as.character(r)]]
    } else {
      extra_vars  # same vector applied to every round
    }

    cleaned[[i]] <- tryCatch(
      ab_clean_round(
        data               = sources[[i]],
        round              = r,
        sentinels          = sentinels,
        binarize_vars      = binarize_vars,
        binarize_threshold = binarize_threshold,
        var_map            = var_map,
        extra_vars         = ev
      ),
      error = function(e) {
        warning(sprintf("Round %d failed: %s", r, conditionMessage(e)))
        NULL
      }
    )
  }

  # Drop failed rounds
  cleaned <- Filter(Negate(is.null), cleaned)

  if (length(cleaned) == 0) stop("All rounds failed to clean.")

  if (verbose) message(sprintf("Merging %d rounds ...", length(cleaned)))
  dplyr::bind_rows(cleaned)
}


#' Summarise Media Consumption Trends Across Rounds
#'
#' Computes the percentage of respondents who consumed traditional or online
#' news at least weekly in each round, ready for plotting.
#'
#' @param ab A merged Afrobarometer tibble from \code{ab_merge_rounds()}.
#'
#' @return A long tibble with columns \code{round}, \code{source} (one of
#'   \code{"Traditional"} or \code{"Online"}), and \code{percent}.
#' @export
#'
#' @examples
#' \dontrun{
#' trends <- ab_media_trends(ab)
#' ggplot(trends, aes(round, percent, color = source)) + geom_line()
#' }
ab_media_trends <- function(ab) {
  trad_cols   <- c("news_radio","news_tv","news_newspaper")
  online_cols <- c("news_internet","news_sm")

  ab$weekly_trad   <- as.integer(
    rowSums(ab[, intersect(trad_cols,   names(ab))] >= 0.75, na.rm = TRUE) > 0
  )
  ab$weekly_online <- as.integer(
    rowSums(ab[, intersect(online_cols, names(ab))] >= 0.75, na.rm = TRUE) > 0
  )

  summ <- ab |>
    dplyr::group_by(.data$round) |>
    dplyr::summarise(
      Traditional = mean(.data$weekly_trad,   na.rm = TRUE) * 100,
      Online      = mean(.data$weekly_online, na.rm = TRUE) * 100,
      .groups     = "drop"
    )

  tidyr::pivot_longer(summ,
    cols      = c("Traditional", "Online"),
    names_to  = "source",
    values_to = "percent"
  )
}
