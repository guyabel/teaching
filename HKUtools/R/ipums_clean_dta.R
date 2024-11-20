#' Clean IPUMS International Data Imported from Stata File
#'
#' @param d Data frame with labels, as typically obtained when imported using haven::read_dta()
#' @param numeric_convert Vector of character strings for columns to convert to numeric types
#' @param string_as_factors Covert any factors to strings.
#' @param clean_labels Clean labels. Extra step, often not required.
#' @param mis_lab Vector of character strings to convert labels for missing values to `NA`
#'
#' @return Cleaned data frame.
#' @export
ipums_clean_dta <- function(
  d = NULL, numeric_convert = NULL, string_as_factors = TRUE, clean_labels = FALSE,
  mis_lab = NULL){
  mis_lab <- c(
    mis_lab,
    "unknown", "niu (not in universe)", "not reported/missing",
    "unknown/missing", "response suppressed", "unclassifiable",
    # in cambodia 2013 nativity
     "niu (not universe)",
    # dhs
    "missing", "don't know", "not weighed at birth", "flagged cases", "not present",
    "refused", "other", "out of plausible range", "inconsistent"
  )
  d %>%
    {if(clean_labels) dplyr::mutate_if(haven::is.labelled, ipumsr::lbl_clean) else .} %>%
    dplyr::mutate_if(haven::is.labelled, ~ipumsr::lbl_na_if(., ~.lbl %in% mis_lab)) %>%
    dplyr::mutate_at({numeric_convert}, haven::zap_labels) %>%
    dplyr::mutate_at({numeric_convert}, as.numeric) %>%
    dplyr::mutate_if(haven::is.labelled, haven::as_factor) %>%
    dplyr::mutate_if(is.factor, as.character) %>%
    {if(string_as_factors) dplyr::mutate_if(., is.character, as.factor) else .}
}
