#' Break Documents out by Sentence
#'
#' @details
#' Often in NLP, it's necessary to break documents up into sentences to perform lower-level analyses upon. This function serves as a means for doing so, outputting a dataframe containing each sentence beloning to documents.
#'
#' @param data A dataframe of tibble object containing documents
#' @param text_variable The column containing documents that the user wishes to unnest into sentences
#'
#' @return A dataframe with documents broken out by sentence, where each row represents a sentence.
#' @export
#'
#' @examples
#' sentences_df <- process_documents_to_sentences(data = data,
#'                                                text_variable = texts)
process_documents_to_sentences <- function(data,
                                           text_variable){

  # some handling of text variable
  text_sym <- rlang::ensym(text_variable)

  # unnest documents, breaking them out by sentence
  sentences_df <- data %>%
    dplyr::mutate(sentence = stringr::str_split(!!text_sym, "(?<=\\.)\\s+")) %>%
    tidyr::unnest(sentence)

  return(sentences_df)
}
