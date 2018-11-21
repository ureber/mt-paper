library(quanteda)

# function to split and duplicate counts in features containing the concatenator character "_"

dfm_splitgrams <- function(x, concatenator = "_") {
  # separate the unigrams
  x_unigrams <-  dfm_remove(x, concatenator, valuetype = "regex")
  # separate the ngrams
  x_ngrams <- dfm_select(x, concatenator, valuetype = "regex")
  # split into components
  split_ngrams <- stringi::stri_split_regex(featnames(x_ngrams), concatenator)
  # get a repeated index for the ngram feature names
  index_split_ngrams <- rep(featnames(x_ngrams), lengths(split_ngrams))
  # subset the ngram matrix using the (repeated) ngram feature names
  x_split_ngrams <- x_ngrams[, index_split_ngrams]
  # assign the ngram dfm the feature names of the split ngrams
  colnames(x_split_ngrams) <- unlist(split_ngrams, use.names = FALSE)
  # return the column concatenation of unigrams and split ngrams
  suppressWarnings(cbind(x_unigrams, x_split_ngrams))
}
