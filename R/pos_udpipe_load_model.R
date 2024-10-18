#' Load a Pre-trained Parts of Speech model from UDPipe
#'
#' @details
#' A function that wraps around the UDPipe package and retrieves/downloads pre-built models previously trained by the community, covering 65 different languages. For more information on what models are available, visit:[UDPipe Documentation](https://bnosac.github.io/udpipe/docs/doc1.html#pre-trained-models)
#'
#' @param model The chosen model indicating what language the user wishes to select from udpipe's pre-trained parts of speech models. There are 65 options to choose from
#'
#' @return Loads the chosen UDPipe model into memory, ready for the annotation steps in the parts of speech workflow.
#' @export
#'
#' @examples
#' pos_model_en <- TextmineR::pos_udpipe_load_model(model = "english")
pos_udpipe_load_model <- function(model) {

  # create a stop for if the user doesn't use a string input for model
  stopifnot(rlang::is_string(model))

  # save model to a temporary directory don't overwrite by default
  model <- udpipe::udpipe_download_model(language = model,
                                         model_dir = tempdir(),
                                         overwrite = FALSE)

  # load model from the temp directory
  retrieved_model <- udpipe::udpipe_load_model(file = model$file)

  return(retrieved_model)
}
