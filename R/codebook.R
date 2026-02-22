#' Afrobarometer Codebook Sentinel Values
#'
#' These are the standard "not applicable" / "missing" sentinel codes used
#' across Afrobarometer rounds. Any column value matching one of these will
#' be replaced with NA during cleaning.
#'
#' From the Afrobarometer codebook:
#' \itemize{
#'   \item -1 = Missing
#'   \item  8 = Refused to answer
#'   \item  9 = Don't know (where applicable)
#'   \item 98 = Refused to answer (2-digit scales)
#'   \item 99 = Don't know (2-digit scales)
#'   \item  7 = Not applicable (used in some bribery questions)
#' }
#'
#' @export
AB_SENTINEL_VALUES <- c(-1, 7, 8, 9, 98, 99)

#' Variable name mapping across Afrobarometer rounds 5-9
#'
#' A named list where each element describes how a conceptual variable maps
#' to the raw survey column name in each round.
#'
#' @format A named list of data frames, one per conceptual variable.
#'   Each data frame has columns \code{round} (integer) and \code{raw_var}
#'   (character).
#' @export
AB_VAR_MAP <- list(

  discuss_politics = data.frame(
    round   = c(5,    6,    7,    8,    9),
    raw_var = c("Q15","Q14","Q13","Q9", "Q8")
  ),

  voted = data.frame(
    round   = c(5,    6,    7,    8,    9),
    raw_var = c("Q27","Q21","Q22","Q13","Q13")
  ),

  # raise_issue - "contacted official / raised issue"
  raise_issue = data.frame(
    round   = c(5,     6,     7,     8,     9),
    raw_var = c("Q26B","Q27A","Q26A","Q11B","Q10B")
  ),

  demonstrated = data.frame(
    round   = c(5,     6,     7,     8,     9),
    raw_var = c("Q26D","Q27E","Q26E","Q11C","Q10C")
  ),

  attend_rally = data.frame(
    round   = c(5,     6,     7,     8,     9),
    raw_var = c("Q29A","Q23A","Q24A","Q15A","Q10A")
  ),

  persuade_others = data.frame(
    round   = c(5,     6,     7,    8,    9),
    raw_var = c("Q29B","Q23C","Q24B","Q15C", NA)   # not in R9
  ),

  worked_campaign = data.frame(
    round   = c(5,     6,     7,     8,     9),
    raw_var = c("Q29C","Q23D","Q24B","Q15B", NA)   # not in R9
  ),

  # News media variables
  news_radio = data.frame(
    round   = c(5,     6,     7,     8,     9),
    raw_var = c("Q13A","Q12A","Q12A","Q55A","Q74A")
  ),
  news_tv = data.frame(
    round   = c(5,     6,     7,     8,     9),
    raw_var = c("Q13B","Q12B","Q12B","Q55B","Q74B")
  ),
  news_newspaper = data.frame(
    round   = c(5,     6,     7,     8,     9),
    raw_var = c("Q13C","Q12C","Q12C","Q55C","Q74C")
  ),
  news_internet = data.frame(
    round   = c(5,     6,     7,     8,     9),
    raw_var = c("Q13D","Q12D","Q12D","Q55D","Q74D")
  ),
  news_sm = data.frame(
    round   = c(5,     6,     7,     8,     9),
    raw_var = c(NA,   "Q12E","Q12E","Q55E","Q74E")  # not in R5
  ),

  often_use_internet = data.frame(
    round   = c(5,     6,     7,     8,     9),
    raw_var = c("Q91B","Q92B","Q91B","Q92I","Q90I")
  ),

  # Contact with leaders
  contact_local_gvt = data.frame(
    round   = c(5,     6,     7,     8,     9),
    raw_var = c("Q30A","Q24A","Q25A","Q12A","Q11B")
  ),
  contact_mp = data.frame(
    round   = c(5,     6,     7,     8,     9),
    raw_var = c("Q30B","Q24B","Q25B","Q12B","Q11C")
  ),
  contact_official = data.frame(
    round   = c(5,     6,     7,     8,     9),
    raw_var = c("Q30C","Q24C","Q25C","Q12C","Q11D")
  ),
  contact_traditional = data.frame(
    round   = c(5,     6,     7,     8,     9),
    raw_var = c(NA,   "Q24E","Q25E","Q12D","Q11E")  # not in R5
  ),

  # Bribes
  bribe_for_permit = data.frame(
    round   = c(5,     6,     7,     8,     9),
    raw_var = c("Q61A","Q55F","Q49K","Q44I","Q42C")
  ),
  bribe_for_clinic = data.frame(
    round   = c(5,     6,     7,     8,     9),
    raw_var = c("Q61C","Q55D","Q49G","Q44F","Q41C")
  ),
  bribe_for_police = data.frame(
    round   = c(5,     6,     7,     8,     9),
    raw_var = c("Q61D","Q55J","Q49R","Q44L","Q43C")
  ),
  bribe_for_school = data.frame(
    round   = c(5,     6,     7,     8,     9),
    raw_var = c("Q61E","Q55B","Q49C","Q44C","Q40C")
  ),

  # Difficulty accessing services
  difficulty_id = data.frame(
    round   = c(5,     6,     7,     8,     9),
    raw_var = c("Q67A","Q55E","Q49I","Q44H","Q42B")
  ),
  difficulty_help_police = data.frame(
    round   = c(5,     6,     7,     8,     9),
    raw_var = c("Q67C","Q55I","Q49P","Q44K","Q43B")
  ),
  dififculty_schooling = data.frame(
    round   = c(5,     6,     7,     8,     9),
    raw_var = c("Q67D","Q55A","Q49B","Q44B","Q40B")
  ),
  difficulty_treatment = data.frame(
    round   = c(5,     6,     7,     8,     9),
    raw_var = c("Q67E","Q55C","Q49E","Q44E","Q41B")
  ),

  # Local government performance
  local_gvt_performance = data.frame(
    round   = c(5,     6,     7,     8,     9),
    raw_var = c("Q66A","Q67A","Q58C","Q51C","Q47C")
  ),

  # Education
  education_raw = data.frame(
    round   = c(5,    6,    7,    8,    9),
    raw_var = c("Q97","Q97","Q97","Q97","Q94")
  ),

  # EA (enumeration area) infrastructure
  EA_SVC_A = data.frame(round = 5:9, raw_var = rep("EA_SVC_A", 5)),
  EA_SVC_B = data.frame(round = 5:9, raw_var = rep("EA_SVC_B", 5)),
  EA_SVC_C = data.frame(round = 5:9, raw_var = rep("EA_SVC_C", 5)),
  EA_SVC_D = data.frame(round = 5:9, raw_var = rep("EA_SVC_D", 5)),
  EA_FAC_B = data.frame(round = 5:9, raw_var = rep("EA_FAC_B", 5)),
  EA_FAC_C = data.frame(round = 5:9, raw_var = rep("EA_FAC_C", 5)),
  EA_FAC_D = data.frame(round = 5:9, raw_var = rep("EA_FAC_D", 5)),
  EA_SEC_A = data.frame(round = 5:9, raw_var = rep("EA_SEC_A", 5)),
  EA_SEC_B = data.frame(round = 5:9, raw_var = rep("EA_SEC_B", 5)),
  EA_SEC_C = data.frame(round = 5:9, raw_var = rep("EA_SEC_C", 5))
)

#' Education level labels
#'
#' Maps raw education codes to descriptive labels following Afrobarometer
#' codebook conventions.
#' @export
AB_EDUCATION_LEVELS <- c(
  "No schooling",
  "Informal or primary",
  "Secondary",
  "University or tech",
  "Advanced"
)
