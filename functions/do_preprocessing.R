library(quanteda)

do_preprocessing <- function(dfm) {
  #Lowercase-Reduktion
  dfm <- dfm_tolower(dfm)
  #Mindestwortlänge 3
  dfm <- dfm_select(dfm, min_nchar = 3)
  #Lemmatisierung
  baseforms <- read.csv('functions/do_preprocessing/baseforms.csv', header=F, encoding = 'UTF-8')
  colnames(baseforms) <- c('flektierte_form', 'lemma_form')
  baseforms.1 <- baseforms[!duplicated(baseforms$flektierte_form), ]
  features <- as.data.frame(dfm@Dimnames$features)
  features$order_id <- as.numeric(rownames(features))
  colnames(features) <- c('features', 'order_id')
  features.base <- merge(features, baseforms.1, by.x='features', by.y='flektierte_form', all.x=TRUE)
  features.base$lemmfeat[is.na(features.base$lemma_form)] <- features.base$features[is.na(features.base$lemma_form)]
  features.base$lemmfeat[!is.na(features.base$lemma_form)] <- features.base$lemma_form[!is.na(features.base$lemma_form)]
  features.base <- features.base[order(features.base$order_id),]
  dfm@Dimnames$features <- features.base$lemmfeat
  dfm <- dfm_compress(dfm, margin = "features")
  #Stoppwort-Entfernung
  blacklist.climate <- read.csv("functions/do_preprocessing/blacklist_climate.csv", header=F, quote = "", encoding = 'UTF-8')
  dfm <- dfm_remove(dfm, blacklist.climate$V1, verbose='TRUE')
  stopwords <- read.csv("functions/do_preprocessing/stopwords_en_de.txt", header=F, quote = "", encoding = 'UTF-8')
  dfm <- dfm_remove(dfm, stopwords$V1, verbose='TRUE')
  #Relatives Pruning (Terme, die in weniger als 0.5% aller Dokumente und in mehr als 99 %  aller Dokumente vorkommen, werden gelöscht)
  dfm <- dfm_trim(dfm, min_docfreq = 0.005, max_docfreq = 0.99, verbose = TRUE)
  return(dfm)
}
