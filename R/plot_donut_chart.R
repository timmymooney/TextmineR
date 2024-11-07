#' Plot a Donut Showing Group Distribution
#'
#' @param data A dataframe or tibble object
#' @param group_variable The group in which the user wishes to display distribution
#' @param custom_colours For if the user has a customised colour pallatte to apply to the group variables. This should be supplied as a vector, if not the default colour palette is "Set3" of `scale_fill_brewer()`
#' @param legend_position The options are; "right", "left", "bottom" or "none", the default is "right".
#' @param legend_text_size For if the users wishes to change the size of the legend text, the default is set to size = 12.
#' @param legend_title The user may amend the title by upplying this as a character string.
#'
#' @return A ggplot2 visual, showing the distribution of a group variable byway of a donut chart.
#' @export
#'
#' @examples
#' theme_colours <- c("theme_1" = "#004d00",
#'                    "theme_2" = "#009900",
#'                    "theme_3" = "#ccffcc")
#' data %>%
#'  TextmineR::plot_donut_chart(group_variable = theme,
#'                             custom_colours = theme_colours)
plot_donut_chart <- function(data,
                             group_variable,
                             custom_colours = NULL,
                             legend_position = c("right", "left", "bottom", "none")[1],
                             legend_text_size = 12,
                             legend_title = "Group Distribution Donut Chart") {

  # ensure that the legend_position is a single value
  legend_position <- match.arg(legend_position, choices = c("right", "left", "bottom", "none"))

  # capture group_variable as a symbol
  group_sym <- rlang::ensym(group_variable)

  # count group percentages and round to 1 decimal place
  groups_counted <- data %>%
    dplyr::count(!!group_sym) %>%
    dplyr::mutate(percentage = n / nrow(data) * 100) %>%
    dplyr::arrange(dplyr::desc(percentage)) %>%
    dplyr::rename(!!rlang::as_name(group_sym) := !!group_sym)

  # arrange the data in order by setting factor levels of the group variable
  groups_counted <- groups_counted %>%
    dplyr::mutate(!!group_sym := factor(!!group_sym, levels = groups_counted[[rlang::as_name(group_sym)]]))

  # quick check if custom_colours are provided and apply them if present
  if (!is.null(custom_colours)) {
    if (length(custom_colours) != dplyr::n_distinct(groups_counted[[rlang::as_name(group_sym)]])) {
      stop("The length of custom_colours must match the number of unique groups.")
    }
    plot <- groups_counted %>%
      ggplot2::ggplot(ggplot2::aes(x = 2, y = percentage, fill = !!group_sym)) +
      ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
      ggplot2::coord_polar(theta = "y") +
      ggplot2::xlim(0.5, 2.5) + # this creates the 'hole' in the donut chart
      ggplot2::theme_void() +
      ggplot2::scale_fill_manual(values = custom_colours) +
      ggplot2::theme(
        legend.position = legend_position,
        legend.text = ggplot2::element_text(size = legend_text_size, face = "bold"),
        legend.title = ggplot2::element_text(size = 15, face = "bold", color = "grey30")
      ) +
      ggplot2::labs(fill = legend_title)
  } else {
    plot <- groups_counted %>%
      ggplot2::ggplot(ggplot2::aes(x = 2, y = percentage, fill = !!group_sym)) +
      ggplot2::geom_bar(stat = "identity", width = 1, color = "white") +
      ggplot2::coord_polar(theta = "y") +
      ggplot2::xlim(0.5, 2.5) + # this creates the 'hole' in the donut chart
      ggplot2::theme_void() +
      ggplot2::scale_fill_brewer(palette = "Set3") +
      ggplot2::theme(
        legend.position = legend_position,
        legend.text = ggplot2::element_text(size = legend_text_size, face = "bold"),
        legend.title = ggplot2::element_text(size = 15, face = "bold", color = "grey30")
      ) +
      ggplot2::labs(fill = legend_title)
  }

  print(plot)
}
