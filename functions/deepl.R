library(quanteda)
library(httr)
library(jsonlite)
library(pbapply)

deepl <- function(dataset = NULL,
                  content.col = NULL,
                  content.vec = NULL,
                  to_lang = NULL,
                  from_lang = NULL) {
  # check if input is valid
  LANGUAGES <- c("auto","DE","EN","FR","ES","IT","NL","PL")
  if (missing(from_lang))
    from_lang <- "auto"
  if (from_lang %in% LANGUAGES == FALSE)
    stop("Source language not available.")
  if (to_lang %in% LANGUAGES == FALSE)
    stop("Target language not available.")
  if((is.null(dataset) | is.null(content.col)) & is.null(content.vec)) {
    stop("You must either provide a dataset and a content column or a single vector of content.")
  }
  if(!(is.null(dataset))) {
    if(!(is.data.frame(dataset))) {
      stop("Dataset must be a data frame.")}
  }
  if(!(is.null(content.col))) {
    if(!(is.character(content.col))) {
      stop("Content column must be a character string.")}
  }
  if(!(is.null(content.col))) {
    if(!(is.character(dataset[[content.col]]))) {
      stop("The column containing the content must be of type character.")}
  }
  if(!(is.null(content.vec))) {
    if(!(is.character(content.vec))){
      stop("Content vector must be a character vector.")}
  }
  # prepare input for translation (tokenization)
  if (!(is.null(dataset))) {
    to.translate <- dataset[[content.col]]
  }
  if (!(is.null(content.vec))) {
    to.translate <- content.vec
  }
  sentences <- as.list(tokens(to.translate, what = "sentence"))
  # do translation
  translation <- pblapply(sentences, function(x) {
    vec <- vector()
    for (i in 1:length(x)) {
      sentence <- x[i]
      BODY <- list(
        jsonrpc = "2.0",
        method =  "LMT_handle_jobs",
        params =  list(
          jobs =  list(
            data.frame(
              kind =  "default",
              raw_en_sentence = sentence
            )
          ),
          lang = list(
            user_preferred_langs =  list(
              from_lang,
              to_lang
            ),
            source_lang_user_selected =  from_lang,
            target_lang = to_lang
          ),
          priority = -1
        )
      )
      answer <- httr::POST(url = "https://www.deepl.com/jsonrpc",
                           body = rjson::toJSON(BODY),
                           httr::content_type_json())
      result <- fromJSON(content(answer, as = "text", encoding="UTF-8"))
      # check if result is valid
      if ("error" %in% names(result)) {
        stop(paste("An error occured:", result$error$message, sep = " "))
      }
      if (!"result" %in% names(result)) {
        stop("An error occured.")
      }
      # prepare correct output
      if (is.null(result$result$translations$beams[[1]]$postprocessed_sentence)) {
        print(paste("No translation available for sentence:", sentence, "Sentence will be removed from output.", sep = " "))
        transl <- ""
      } else {
        transl <- result$result$translations$beams[[1]]$postprocessed_sentence[1]
      }
      vec <- append(vec, transl)
    }
    transl.txt <- paste(vec, collapse = " ")
    return(transl.txt)
  })
  # prepare correct output (final)
  if (!(is.null(content.vec))) {
    translated <- unname(unlist(translation))
    return(translated)
  }
  if (!(is.null(dataset) & is.null(content.col))) {
    translated <- Reduce(rbind, translation)
    dataset$translated <- translated
    return(dataset)
  }
}
