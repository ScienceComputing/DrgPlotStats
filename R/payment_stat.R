#' Payment Calculation
#'
#' This function calculates \code{statistics} over all of the DRG codes for average Medicare payments.
#'
#' @param data a dataframe.
#' @param statistics a string name for either the mean, median or standard deviation.
#'
#' @return A dataframe of either the mean, median or standard deviation for average Medicare payments by DRG code.
#' @export
#'
#' @import dplyr
#' @importFrom purrr map
#'
#' @examples
#' payment_calculate(data = sample_data, statistics = 'mean')
#'
payment_calculate <- function(data, statistics){
  data_clean <- data %>%
    mutate(drg_code = unlist(
      map(
        strsplit(
          x = `DRG Definition`,
          split = " "),
        1))) %>%
    group_by(drg_code)
  if(statistics == 'mean'){
    data_clean %>%
      summarise(mean = mean(`Average Medicare Payments`))
  }
  else if(statistics == 'median'){
    data_clean %>%
      summarise(median = median(`Average Medicare Payments`))}
  else {
    data_clean %>%
      summarise(standard_deviation = sd(`Average Medicare Payments`))}
}

