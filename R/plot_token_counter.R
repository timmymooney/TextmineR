#' Plot Token Frequencies of a Text Variable
#'
#' @details
#' A function to render a simple bar chart showing the most frequent words in a text variable. Along with being able to choose how many of the top terms to visualize, the user has the ability to change some aesthetics, such as y axis text size and fill color.
#'
#' @param data A tibble or data frame object
#' @param text_var The text or message variable to analyse
#' @param n Amount of top terms to visualize
#' @param text_size How large should the y axis text be, this should also depend on the number of top terms selected
#' @param fill What color should the bar chart plot be
#'
#' @return Returns a simple bar chart displaying the top terms mentioned throughout any one message variable
#' @export
#'
#' @examples
#' plot_token_counter(data = data,
#'                    text_var = review_comment,
#'                    n = 10,
#'                    y_axis_text_size = 15,
#'                    fill = "gold")
plot_token_counter <- function(data,
                               text_var,
                               n,
                               y_axis_text_size,
                               fill = "gold") {
  text_var <- data %>%
    dplyr::select({{text_var}})

  stop_words <- tm::stopwords(kind = "smart")

  tokens <- strsplit(stringr::str_to_upper({{text_var}}), "\\W+")

  freq_data <- data.frame(token = unlist(tokens), stringsAsFactors = FALSE)
  freq_data <- transform(freq_data, token = gsub("[[:punct:]]", "", token))
  freq_data <- data.frame(table(freq_data$token))

  freq_data <- freq_data[order(-freq_data$Freq), ]

  freq_data <- freq_data %>%
    dplyr::filter(!Var1 %in% stop_words) %>%
    dplyr::rename(token = Var1)

  # create the tokens plot
  plot <-   freq_data %>%
    dplyr::slice_head(n = {{n}}) %>%
    ggplot2::ggplot(ggplot2::aes(x = stats::reorder(token, Freq), y = Freq)) +
    ggplot2::geom_bar(stat = "identity", fill = fill) +
    ggplot2::labs(x = "Token", y = "Frequency") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(text = element_text(colour = "black", size = {{text_size}}))

  return(plot)
}

