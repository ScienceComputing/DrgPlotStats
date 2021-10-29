#' Payment Calculation
#'
#' This function calculates \code{statistics} over all of the DRG codes for average Medicare payments.
#'
#' @param data a dataframe
#' @param statistics a string name for either the mean, median or standard deviation
#'
#' @return A dataframe of either the mean, median or standard deviation for average Medicare payments by DRG code
#' @export
#'
#' @import dplyr
#' @importFrom purrr map
#'
#' @examples
#' payment_calculate(data = sample_data, statistics = 'mean') ## return mean of average Medicare payments by DRG code
#' payment_calculate(data = sample_data, statistics = 'median') ## return median of average Medicare payments by DRG code
#' payment_calculate(data = sample_data, statistics = 'standard deviation') ## return standard deviation of average Medicare payments by DRG code
#'
payment_calculate <- function(data, statistics){
  data_clean <- data %>%
    mutate(drg_code = unlist( ## create a variable for DRG code
                        map( ## apply string split function to each element of `DRG Definition` column
                        strsplit( ## extract numerical part of DRG definition
                          x = `DRG Definition`,
                          split = ' '),
                        1))) %>%
    group_by(drg_code) ## group data by DRG code
  if(statistics == 'mean'){ ## set condition
    data_clean %>%
      summarise(mean = mean(`Average Medicare Payments`)) ## calculate mean of average Medicare payments by DRG code
  } else if(statistics == 'median'){ ## set condition
    data_clean %>%
      summarise(median = median(`Average Medicare Payments`)) ## calculate median of average Medicare payments by DRG code
  } else { ## set condition
    data_clean %>%
      summarise(standard_deviation = sd(`Average Medicare Payments`)) ## calculate standard deviation of average Medicare payments by DRG code
  }
}

