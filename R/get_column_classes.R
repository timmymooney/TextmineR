#' Shows all Column Classes in a Data frame
#'
#' @details
#' A function to quickly check all column/variable classes of a chosen data-frame or tibble
#'
#' @param data A tibble or data frame object
#'
#' @return A data frame that displays each of the variables inside of the data object and their respective class
#' @export
#'
#' @examples
#' TextmineR::get_column_classes(data = data)
get_column_classes <- function(data) {
  t(as.data.frame(lapply(data, function(x) paste(class(x), collapse = ','))))
}
