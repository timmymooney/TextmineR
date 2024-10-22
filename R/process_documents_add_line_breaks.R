#' Add Line Breaks to Documents
#'
#' @param text_variable The column containing documents or texts the user wishes to add line breaks()
#' @param word_interval An integer expressing how many words after should each line break appear
#'
#' @return Additional line breaks to the chosen text variable.
#' @export
#'
#' @examples
#' data <- data %>% dplyr::mutate(texts_with_breaks = sapply(texts, process_documents_add_line_breaks))
process_documents_add_line_breaks <- function(text_variable,
                                              word_interval = 10) {

  words <- strsplit(text_variable, " ")[[1]]
  paste(sapply(seq(1, length(words), word_interval),
               function(i)
               { paste(words[i:min(i + word_interval - 1,
                                   length(words))], collapse = " ") }), collapse = "<br>")
}

