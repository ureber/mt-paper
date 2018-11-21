library(quanteda)
library(pbapply)
library(translateR)

google <- function(dataset = NULL,
                  content.col = NULL,
                  content.vec = NULL,
                  to_lang = NULL,
                  from_lang = NULL) {
  # check if input is valid
  LANGUAGES <- c("de","en")
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
      stop("deeplDataset must be a data frame.")}
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
      result <- translate(content.vec = sentence,
                          google.api.key = "2F4WzIkfzQRpLeU2etkiuxVjJsbHoLll", # random API key (not working)
                          source.lang = from_lang,
                          target.lang= to_lang)
      vec <- append(vec, result)
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
