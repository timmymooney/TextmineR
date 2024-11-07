#' Plot Data Volumes over Time per Group
#'
#'#' @details
#' A function to render a simple volume over time chart but faceted for each level in a group variable, alike `plot_volume_over_time`, the user can change the unit, axis text breaks by unit, colours for each group and how many rows they wish to plot each group across. The user may also wish to fix the y axis limit, the default is TRUE to show volumes in comparison to each group more clearly.
#'
#' @param data A tibble or data frame object
#' @param date_variable The column containing dates for each observation or row of data, must be of class Date
#' @param group_variable The group variable that the user wishes to compare volumes
#' @param group_colour A custom colour vector for if the user wishes to apply, the default pallet is ggplot2::scale_fill_brewer(palette = "Set3")
#' @param unit ow often should the dates be grouped when being summarised, the options are but not limited to; "1 day", "1 week", "1 month", "1 quarter", "1 year", for example; one may wish to call unit = "2 weeks"
#' @param axis_text_unit How often should the dates be plotted onto the x axis, the options are but not limited to; "1 day", "1 week", "1 month", "1 quarter", "1 year", for example; one may wish to call unit = "2 weeks"
#' @param rows How many rows the user wishes to plot the group volume over time charts across, the default is 2 rows
#' @param fix_y_axis A logical argument for if the user wishes to fix the y axis limit for each group based on the max value, the default is TRUE to display the comparison in group sizes.
#'
#' @return A ggplot2 visual showing the volumes over time of a given group within a dataset
#' @export
#'
#' @examples
#' data %>%
#'  plot_group_volume_over_time(date_variable = date,
#'                              group_variable = brand,
#'                              group_colour = custom_colours,
#'                              unit = "1 month",
#'                              axis_text_unit = "1 year",
#'                              rows = 3,
#'                              fix_y_axis = TRUE)
plot_group_volume_over_time <- function(data,
                                        date_variable,
                                        group_variable,
                                        group_colour = NULL,
                                        unit = c("1 day", "1 week", "1 month", "1 quarter", "1 year"),
                                        axis_text_unit = "1 month",
                                        rows = 2,
                                        fix_y_axis = TRUE) {

  # ensure unit is valid and convert date and group variables to symbols
  time <- match.arg(unit)
  date_sym <- rlang::ensym(date_variable)
  group_sym <- rlang::ensym(group_variable)

  # create plot_date column based on the specified time unit
  data <- data %>%
    dplyr::mutate(plot_date = lubridate::floor_date(!!date_sym, unit = time))

  # count group percentages and round to 1 decimal place
  groups_counted <- data %>%
    dplyr::count(!!group_sym) %>%
    dplyr::mutate(percentage = n / nrow(data) * 100) %>%
    dplyr::arrange(dplyr::desc(percentage)) %>%
    dplyr::rename(!!rlang::as_name(group_sym) := !!group_sym)

  # count occurrences by date and group
  data_count <- data %>%
    dplyr::count(plot_date, !!group_sym) %>%
    dplyr::mutate(!!group_sym := factor(!!group_sym, levels = groups_counted[[rlang::as_name(group_sym)]]))

  # define the ylimit based on largest value for counts
  ylimit_value <- max(data_count$n)

  # initialize the plot with grouping by color and add faceting
  plot <- data_count %>%
    ggplot2::ggplot(ggplot2::aes(x = plot_date, y = n, fill = !!group_sym)) +
    ggplot2::geom_col() +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                   plot.title.position = "plot",
                   axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(size = 0.5)) +
    ggplot2::labs(x = NULL, y = "", title = "Volume over Time by Group", fill = "") +
    ggplot2::scale_x_date(date_breaks = axis_text_unit, date_labels = "%b %Y") +  # Set axis text breaks
    ggplot2::facet_wrap(rlang::as_label(group_sym), scales = "free_y", nrow = rows)  # Facet by group variable

  # apply custom group colors if provided, else use default colors
  if (!is.null(group_colour)) {
    plot <- plot + ggplot2::scale_fill_manual(values = group_colour)
  } else {
    plot <- plot + ggplot2::scale_fill_brewer(palette = "Set3")  # Default palette
  }
 # if fix_y_axis == TRUE set limit to maximum value in data counts
  if (fix_y_axis == TRUE) {
    plot <- plot + ggplot2::scale_y_continuous(limits = c(0, ylimit_value))
  }

  return(plot)
}

