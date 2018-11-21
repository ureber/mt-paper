library(quanteda)

make_corpus <- function(data){
  corp <- corpus(data$text)
  docnames(corp) <- data$d_id
  docvars(corp, "d_id") <- data$d_id
  docvars(corp, "actor_country") <- data$actor_country
  docvars(corp, "language") <- data$language
  docvars(corp, "organisation") <- data$organisation
  return(corp)
}
