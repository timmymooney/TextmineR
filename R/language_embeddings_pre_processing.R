#' Clean Text Variable Ready for Embedding
#'
#' @details By performing various cleaning steps, the `language_embeddings_pre_processing` function is designed to prepare text data for embedding. It is particularly useful for pre-processing texts before applying them to any natural language processing (NLP) or transformer model such as those hosted on Hugging Face and similar. The key cleaning steps include:
#'
#'-  URLs: Removes most forms of URL link.
#' - Hashtags: Removes hashtags.
#' - Mentions: Removes user mentions or handle tags.
#' - Emojis: Removes any emojis or non-ASCII characters from text.
#' - Punctuation: Removes all punctuation (optional).
#' - Cleaning Digits: Removes any digits found at the very start of documents but not any others.
#' - Removing Non-English Symbols: Excludes non-English symbols, preserving only alphanumeric characters, punctuation, and spaces.
#' - Handling Multiple Spaces: Reduces consecutive spaces to a single space, addressing any excess spacing, wile also cleaning any spaces that may start documents.
#'
#'The function provides the option to make all texts lower-case by calling to_lower = TRUE, as well as execute all of the above cleaning steps in parallel, by calling in_parallel = TRUE. This is typically performed to leverage multiple CPU cores for efficiency when working with slightly larger data sets with many documents. Overall, this serves as a versatile tool to enhance the quality of textual data before any further analysis or embedding is conducted.
#'
#' @param data A tibble or data frame object
#' @param text_var The text variable or documents to be cleaned for the embedding process
#' @param to_lower Whether to make text lowercase, the default option is FALSE
#' @param remove_punctuation Whether to remove all forms of punctuation from the text variable, the default option is FALSE
#' @param in_parallel Whether to utilize parallel processing and split the job across cores, in order to process faster. The default option is FALSE
#'
#' @return Returns the data object with the provided text_var cleaned and ready to begin language embedding
#' @export
#'
#' @examples
#' output <- data %>%
#'  TextmineR::language_embeddings_pre_processing(text_var = texts)
#'
#' output <- data %>%
#'  TextmineR::language_embeddings_pre_processing(text_var = texts,
#'                                                to_lower = TRUE,
#'                                                remove_punctuation = TRUE,
#'                                                in_parallel = TRUE)
text_embeddings_pre_processing <- function(data,
                                           text_var,
                                           to_lower = FALSE,
                                           remove_punctuation = FALSE,
                                           in_parallel = FALSE) {

  # sort text variable as symbol
  text_sym <- rlang::ensym(text_var)

  # REGEX patterns for each cleaning step
  hashtags_regex <- c("(?<=#)[:graph:]*(?![:graph:])|(?<=#)[:graph:]*$", "#") # hashtags
  mentions_regex <- c("(?<=@)[:graph:]*(?![:graph:])|(?<=@)[:graph:]*$", "@") # mentions of users
  emojis_regex <- "[^\001-\177]" # emojis
  punctuation_regex <- "[:punct:]" # punctuation
  non_english_regex <- "[^A-Za-z0-9[:punct:][:space:]]" # non-English symbols
  spaces_starting_docs_regex <- "^\\s+" # documents with a space at the start
  digits_starting_docs_regex <- "^[0-9]+\\s*" # documents with digits starting them

  if (remove_punctuation == TRUE) {
  all_regex_names <- c(hashtags_regex, # join all above REGEX patterns
                       mentions_regex,
                       emojis_regex,
                       punctuation_regex,
                       non_english_regex,
                       spaces_starting_docs_regex,
                       digits_starting_docs_regex)
  } else {
    all_regex_names <- c(hashtags_regex, # join all above REGEX patterns
                         mentions_regex,
                         emojis_regex,
                         non_english_regex,
                         spaces_starting_docs_regex,
                         digits_starting_docs_regex)
  }
  all_regex <- character(length(all_regex_names))
  names(all_regex) <- all_regex_names

  # REGEX for any bits of text with multiple spaces in a row
  multiple_spaces_regex <- "\\s{2,}"

  # REGEX for URLs and similar
  urls_regex <- "htt(p|ps)\\S+|[w]{3}\\.\\S+|\\S+\\.[a-z]+\\S+"

  # if to_lower is called, which it is by default, make all text lowercase
  if (to_lower == TRUE) {
    data <- data %>%
      dplyr::mutate(!!text_sym := tolower(!!text_sym))
  }

  # if in_parallel is called, split processing across all cores except one
  if (in_parallel == TRUE) {
    options(future.rng.onMisuse = "ignore")
    message("Beginning parallel sessions")
    future::plan(future::multisession(workers = future::availableCores() - 1))

    data <- data %>%
      dplyr::mutate(cuts_id = dplyr::row_number(),cuts = cut(cuts_id, future::availableCores() - 1))
    data <- data %>%
      dplyr::group_split(cuts) %>%
      furrr::future_map_dfr(~.x %>%
                              dplyr::mutate({{text_sym}} := stringr::str_replace_all({{text_sym}}, urls_regex, ""),
                                            {{text_sym}} := stringr::str_remove_all({{text_sym}}, all_regex),
                                            {{text_sym}} := stringr::str_replace_all({{text_sym}}, multiple_spaces_regex," ")))

    # remove those bits where we cut the data before and end parallel session
    data <- data %>% dplyr::select(-c(cuts_id, cuts))
    message("Ending parallel sessions")
    future::plan(future::sequential())
  } else {
    data <- data %>%
      dplyr::mutate({{text_sym}} := stringr::str_replace_all({{text_sym}}, urls_regex, ""),
                    {{text_sym}} := stringr::str_remove_all({{text_sym}}, all_regex),
                    {{text_sym}} := stringr::str_replace_all({{text_sym}}, multiple_spaces_regex," "))

  }

  return(data)
}
