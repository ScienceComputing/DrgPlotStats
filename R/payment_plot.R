#' Payment Plot
#'
#' This function creates a boxplot of payments of \code{pay_type} by DRG code.
#'
#' @param data a dataframe.
#' @param pay_type a string name for either the Average Medicare Payments, the Average Total Payments, or the Average Covered Charges.
#'
#' @return A boxplot of payments of \code{pay_type} by DRG code.
#' @export
#'
#' @import dplyr
#' @importFrom purrr map
#' @import ggplot2
#'
#' @examples
#' payment_plot(data = sample_data, pay_type = 'Average Medicare Payments')
#'
payment_plot <- function(data, pay_type){
  data %>%
    mutate(drg_code = unlist(
                        map(
                          strsplit(
                            x = `DRG Definition`,
                            split = ' '),
                          1))) %>%
    ggplot(aes(
      x = drg_code,
      y = get(pay_type),
      fill = drg_code)) +
    geom_boxplot() +
    guides(fill = 'none') +
    labs(
      x = "DRG code",
      y = pay_type,
      title = paste(pay_type, ' by DRG Code')) +
    theme_classic() +
    theme(axis.text.x = element_text(
                          angle = 45,
                          vjust = 0.5,
                          hjust=1))
}
