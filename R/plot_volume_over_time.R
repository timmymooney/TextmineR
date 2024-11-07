#' Plot Data Volumes over Time
#'
#' @details
#' A function to render a simple volume over time chart, with the option to change the unit, axis text breaks by unit, colour and trend lend options. The user may also include a dashed vertical line at a given date for if they wish.
#'
#' @param data A tibble or data frame object
#' @param date_variable The column containing dates for each observation or row of data, must be of class Date
#' @param smooth A logical argument stating whether to include a smooth trend line, the default is TRUE
#' @param vline_date For if the user wishes to plot a vertical line at any given date to point toward any changes or notable moments within the volume over time plot, argument expects a date given as a string, default option is NULL, no vertical line
#' @param bar_colour What colour should the volume bars be, given as a string and the default is "black"
#' @param line_colour What colour should the trend line be if smooth = TRUE, given as a string and the default is "yellow"
#' @param unit How often should the dates be grouped when being summarised, the options are but not limited to; "1 day", "1 week", "1 month", "1 quarter", "1 year", for example; one may wish to call unit = "2 weeks"
#' @param axis_text_unit How often should the dates be plotted onto the x axis, the options are but not limited to; "1 day", "1 week", "1 month", "1 quarter", "1 year", for example; one may wish to call unit = "2 weeks"
#'
#' @return A ggplot2 visual showing the volumes over time of a given dataset
#' @export
#'
#' @examples
#' data %>%
#'  plot_volume_over_time(date_variable = date,
#'                        smooth = TRUE,
#'                        vline_date = NULL,
#'                        bar_colour = "black",
#'                        line_colour = "gold",
#'                        unit = "1 month",
#'                        axis_text_unit = "1 year")
plot_volume_over_time <- function(data,
                                  date_variable = date,
                                  smooth = TRUE,
                                  vline_date = NULL,
                                  bar_colour = "black",
                                  line_colour = "gold",
                                  unit = c("1 day", "1 week", "1 month", "1 quarter", "1 year"),
                                  axis_text_unit = "1 month"){

  # ensure unit is valid and turn date to symbol
  time <- match.arg(unit)
  date_sym <- rlang::ensym(date_variable)

  # create plot_date column based on the specified time unit
  data <- data %>%
    dplyr::mutate(plot_date = lubridate::floor_date(!!date_sym, unit = time))

  # initial plot with geom_col
  plot <- data %>%
    dplyr::count(plot_date) %>%
    ggplot2::ggplot(ggplot2::aes(x = plot_date, y = n)) +
    ggplot2::geom_col(fill = bar_colour) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold"),
                   plot.title.position = "plot",
                   axis.text.x = ggplot2::element_text(angle = 0, vjust = 0.1),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   axis.line = ggplot2::element_line(size = 0.5)) +
    ggplot2::labs(x = NULL, y = "", title = "Volume over Time") +
    ggplot2::scale_x_date(date_breaks = axis_text_unit, date_labels = "%b %Y")  # set axis text breaks

  # add smoothing line if specified
  if (smooth == TRUE) {
    plot <- plot + ggplot2::geom_smooth(method = "lm", colour = line_colour)
  }

  # add vertical line if vline_date is provided
  if (!is.null(vline_date)) {
    plot <- plot + ggplot2::geom_vline(xintercept = as.Date(vline_date),
                                       linetype = "dashed",
                                       size = 1,
                                       colour = "#6B695E")
  }

  return(plot)
}
