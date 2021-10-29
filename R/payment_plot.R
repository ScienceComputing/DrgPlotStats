#' Payment Plot
#'
#' This function creates a boxplot of payments for each \code{pay_type} by DRG code.
#'
#' @param data a dataframe
#' @param pay_type a string name for either the Average Medicare Payments, the Average Total Payments, or the Average Covered Charges
#'
#' @return A boxplot of payments for \code{pay_type} by DRG code
#' @export
#'
#' @import dplyr
#' @importFrom purrr map
#' @import ggplot2
#'
#' @examples
#' payment_plot(data = sample_data, pay_type = 'Average Medicare Payments') ## return a boxplot of average Medicare payments (log transformed) by DRG code
#' payment_plot(data = sample_data, pay_type = 'Average Total Payments') ## return a boxplot of average total payments (log transformed) by DRG code
#' payment_plot(data = sample_data, pay_type = 'Average Covered Charges') ## return a boxplot of average covered payments (log transformed) by DRG code
payment_plot <- function(data, pay_type){
  data %>%
    mutate(drg_code = unlist( ## create a variable for DRG code
                        map( ## apply string split function to each element of `DRG Definition` column
                          strsplit( ## extract numerical part of DRG definition
                            x = `DRG Definition`,
                            split = ' '),
                          1))) %>%
    ggplot(aes( ## initialize ggplot object
            x = drg_code,
            y = get(pay_type), ## return value of a named object
            fill = drg_code)) + ## color boxplots by DRG code
      geom_boxplot() + ## make boxplots
      guides(fill = 'none') + ## remove legend
      scale_y_continuous(trans = 'log10', ## use log scale of y-axis
                         labels = format_format(scientific = FALSE)) + ## avoid scientific notation
      labs(
        x = "DRG code", ## relabel x-axis
        y = paste0('Log (', pay_type, ' (USD))'), ## relabel y-axis
      title = paste(pay_type, 'by DRG Code')) + ## relabel title
      theme_classic() + ## use classic plotting style
      theme(axis.text.x = element_text(
                            angle = 90, ## rotate x-axis label
                            vjust = 0.5, ## adjust location
                            hjust=1)) ## adjust location
}

