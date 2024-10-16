#' Counts Percentage Distribution of Group Variable
#'
#' @details
#' Shows the user the percentage distribution of a chosen group variable as well as the raw n counts for each.
#'
#' @param data A tibble or data frame object
#' @param group_var The group in which one wishes to get the % and raw counts for, this could be something like sentiment, topic or brand.
#'
#' @param rows_n State how many rows of data in total for if the number of rows to divide the group variable by differs from the number of rows in the `data` object provided.
#' @return Prints the percentage distribution of any given variable
#' @export
#'
#' @examples
#' percentage_distribution(data = data,
#'                         group_var = group_var)
percentage_distribution <- function(data, group_var, rows_n = NULL) {

  if (!is.null(rows_n)) {
    percent_count <- data %>%
      dplyr::group_by({{ group_var }}) %>%
      dplyr::count() %>%
      dplyr::mutate(percentage = n / rows_n * 100)  %>%
      dplyr::arrange(dplyr::desc(percentage))
  } else {
    percent_count <- data %>%
      dplyr::group_by({{ group_var }}) %>%
      dplyr::count() %>%
      dplyr::mutate(percentage = n / nrow(data) * 100) %>%
      dplyr::arrange(dplyr::desc(percentage))
  }

  print(percent_count)
}
