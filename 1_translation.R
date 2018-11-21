#####################################
## Translation of German Web Pages ##
#####################################

# Ueli Reber, University of Bern
# ueli.reber@ikmb.unibe.ch
# https://github.com/ureber/

################################
## load packages and functions

options(stringsAsFactors = F)

library(dplyr)
library(quanteda)

source("functions/make_corpus.R", encoding = "UTF-8")
source("functions/do_preprocessing.R", encoding = "UTF-8")

#####################################
## full-text translation with DeepL

source("functions/deepl.R", encoding = "UTF-8")

# translation of texts with time lag (due to API restrictions)
load(file = "data/subset_D.RData", .GlobalEnv)

int <- 3
i <- 1

while (i < nrow(subset.D.de)) {
  rows <- i:(i + int)
  translated <- deepl(dataset = subset.D.de[rows,],
                      content.col = "text",
                      to_lang = "EN", 
                      from_lang = "DE")
  df.deepl <- rbind(df.deepl, translated)
  save(df.deepl, file = "data/subset_D_translated_DeepL.RData")
  i <- i + int + 1
  Sys.sleep(600)
}

# make DTM with translated documents (raw)
load(file = "data/subset_D_translated_DeepL.RData", .GlobalEnv)

df.deepl.translated <- data.frame(d_id = df.deepl$d_id, text = as.data.frame(df.deepl$translated)[,1])
df.deepl.slim <- df.deepl %>%
  select(-text, -translated) %>%
  full_join(df.deepl.translated, by = "d_id") 

corp.D.de.deepl <- make_corpus(df.deepl.slim)
tokens.D.de.deepl <- tokens(corp.D.de.deepl, what = 'word', remove_punct = TRUE, remove_symbols = TRUE, 
                            remove_numbers = TRUE, remove_hyphens = FALSE, include_docvars = TRUE, verbose=TRUE)
dfm.D.de.deepl.raw <- dfm(tokens.D.de.deepl, tolower = FALSE, stem = FALSE, verbose=TRUE)

# do preprocessing
dfm.D.de.deepl.preprocessed <- do_preprocessing(dfm.D.de.deepl.raw)

dfm.D.de.deepl.preprocessed@Dimnames$docs <- paste0(dfm.D.de.deepl.preprocessed@Dimnames$docs, "_deepl")
save(dfm.D.de.deepl.preprocessed, file = "data/dfm_D_de_DeepL_preprocessed.RData")

################################################
## full-text translation with Google Translate

source("functions/google.R", encoding = "UTF-8")

# translation of texts
load(file = "data/subset_D.RData", .GlobalEnv)

translated <- google(dataset = subset.D.de,
                     content.col = "text",
                     to_lang = "en", 
                     from_lang = "de")
df.google <- rbind(df.google, translated)

save(df.google, file = "data/subset_D_translated_Google.RData")

# make DTM with translated documents (raw)
load(file = "data/subset_D_translated_Google.RData", .GlobalEnv)

df.google.translated <- data.frame(d_id = df.google$d_id, text = as.data.frame(df.google$translated)[,1])
df.google.slim <- df.google %>%
  select(-text, -translated) %>%
  full_join(df.google.translated, by = "d_id")

corp.D.de.google <- make_corpus(df.google.slim)
tokens.D.de.google <- tokens(corp.D.de.google, what = 'word', remove_punct = TRUE, remove_symbols = TRUE, 
                             remove_numbers = TRUE, remove_hyphens = FALSE, include_docvars = TRUE, verbose=TRUE)
dfm.D.de.google.raw <- dfm(tokens.D.de.google, tolower = FALSE, stem = FALSE, verbose=TRUE)

# do preprocessing
dfm.D.de.google.preprocessed <- do_preprocessing(dfm.D.de.google.raw)

dfm.D.de.google.preprocessed@Dimnames$docs <- paste0(dfm.D.de.google.preprocessed@Dimnames$docs, "_google")
save(dfm.D.de.google.preprocessed, file = "data/dfm_D_de_Google_preprocessed.RData")

##########################################
## DTM translation with Google Translate

source("functions/dfm_splitgrams.R", encoding = "UTF-8")

# make DTM for translation
load(file = "data/subset_D.RData", .GlobalEnv)

corp.D.de.DFM <- make_corpus(subset.D.de)
tokens.D.de.DFM <- tokens(corp.D.de.DFM, what = 'word', remove_punct = TRUE, remove_symbols = TRUE, 
                          remove_numbers = TRUE, remove_hyphens = FALSE, include_docvars = TRUE, verbose=TRUE)
dfm.D.de.DFM <- dfm(tokens.D.de.DFM, tolower = FALSE, stem = FALSE, verbose=TRUE)

# create a data frame with the vocabulary for translation
trans.de <- as.data.frame(dfm.D.de.DFM@Dimnames$features)
colnames(trans.de) <- 'de_voc'

# translation of individual terms
trans.en <- translate(dataset = trans.de,
                      content.field = "de_voc",
                      google.api.key = "Zrw66pJ0YEs4qyXHwuhMoipqLlL7-e4o", # key is fake
                      source.lang = "de",
                      target.lang= "en")

save(trans.en, file = "data/vocab_DTM_translated.RData")

# replace spaces in compounds (ngrams) with '_' (compounds originate from translation)
trans.en$translatedContent <- gsub(" ", "_", trans.en$translatedContent, fixed = TRUE)

# replace de vocab in DTM with translated vocab (all lower case)
dfm.D.de.trans <- dfm.D.de.DFM
dfm.D.de.trans@Dimnames$features <- char_tolower(trans.en$translatedContent) 

# split and duplicate counts in features containing the concatenator character "_"
dfm.D.de.DFM.raw <- dfm_splitgrams(dfm.D.de.trans)

# do preprocessing
dfm.D.de.DFM.preprocessed <- do_preprocessing(dfm.D.de.DFM.raw)

dfm.D.de.DFM.preprocessed@Dimnames$docs <- paste0(dfm.D.de.DFM.preprocessed@Dimnames$docs, "_dfm")
save(dfm.D.de.DFM.preprocessed, file = "data/dfm_D_de_DFM_preprocessed.RData")
