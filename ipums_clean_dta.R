ipums_clean_dta <- function(d, numeric_convert = NULL, drop_factors = FALSE, 
                              mis_lab = c("unknown", 
                                          "niu (not in universe)",
                                          "not reported/missing", 
                                          "unknown/missing", 
                                          "response suppressed", 
                                          "unclassifiable")){
  d %>% 
    # dplyr::mutate_if(is.labelled, ipumsr::lbl_clean) %>%
    dplyr::mutate_if(is.labelled, ~ipumsr::lbl_na_if(., ~.lbl %in% mis_labs)) %>%
    dplyr::mutate_at({numeric_convert}, haven::zap_labels) %>%
    dplyr::mutate_if(is.labelled, haven::as_factor) %>%
    {if(drop_factors) dplyr::mutate_if(., is.factor, as.character) else .}
}
# ipums_stata_clean(d2, numeric_convert = c("age", "migyrs1"), drop_factors = TRUE)
# library(devtools)
# source_url("https://raw.github.com/guyabel/teaching/master/ipums_clean_dta.R")
