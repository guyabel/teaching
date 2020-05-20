ipums_clean_dta <- function(
  d = NULL, numeric_convert = NULL, string_as_factors = TRUE, clean_labels = FALSE,
  mis_lab = c("unknown", "niu (not in universe)", "not reported/missing", 
              "unknown/missing", "response suppressed", "unclassifiable")
  ){
  d %>% 
    {if(clean_labels) dplyr::mutate_if(is.labelled, ipumsr::lbl_clean) else .} %>%
    dplyr::mutate_if(is.labelled, ~ipumsr::lbl_na_if(., ~.lbl %in% mis_labs)) %>%
    dplyr::mutate_at({numeric_convert}, haven::zap_labels) %>%
    dplyr::mutate_if(is.labelled, haven::as_factor) %>%
    {if(string_as_factors) dplyr::mutate_if(., is.factor, as.character) else .}
}
# library(devtools)
# source_url("https://raw.github.com/guyabel/teaching/master/ipums_clean_dta.R")
# ipums_clean_dta(d0)
