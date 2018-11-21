#########################
## Study 1: Robustness ##
#########################

# Ueli Reber, University of Bern
# ueli.reber@ikmb.unibe.ch
# https://github.com/ureber/

################################
## load packages and functions

options(stringsAsFactors = F)

library(tidyverse)
library(quanteda)
library(ggpubr)
library(scales)
library(stm)

##########################################
## cosine similarities per document pair

load(file = "data/subset_D.RData", .GlobalEnv)
load(file = "data/dfm_D_de_DeepL_preprocessed.RData", .GlobalEnv)
load(file = "data/dfm_D_de_Google_preprocessed.RData", .GlobalEnv)
load(file = "data/dfm_D_de_DFM_preprocessed.RData", .GlobalEnv)

# DeepL and Google
dfm.compare.deepl.google <- rbind(dfm.D.de.deepl.preprocessed, dfm.D.de.google.preprocessed)

sim.deepl.google <- data.frame(d_id = as.numeric(),
                               similarity = as.numeric())

for (i in 1:nrow(subset.D.de)) {
  doc <- subset.D.de[i,1]
  d1 <- paste0(doc, "_deepl")
  d2 <- paste0(doc, "_google")
  sim <- as.matrix(textstat_simil(dfm.compare.deepl.google, d1, method = "cosine", margin = "documents"))
  sim <- sim[d2,1]
  sim.df <- data.frame(d_id = doc, similarity = sim)
  sim.deepl.google <- rbind(sim.deepl.google, sim.df)
}

hist(sim.deepl.google$similarity)

# Google and DFM
dfm.compare.google.DFM <- rbind(dfm.D.de.google.preprocessed, dfm.D.de.DFM.preprocessed)

sim.google.DFM <- data.frame(d_id = as.numeric(),
                             similarity = as.numeric())

for (i in 1:nrow(subset.D.de)) {
  doc <- subset.D.de[i,1]
  d1 <- paste0(doc, "_google")
  d2 <- paste0(doc, "_dfm")
  sim <- as.matrix(textstat_simil(dfm.compare.google.DFM, d1, method = "cosine", margin = "documents"))
  sim <- sim[d2,1]
  sim.df <- data.frame(d_id = doc, similarity = sim)
  sim.google.DFM <- rbind(sim.google.DFM, sim.df)
}

hist(sim.google.DFM$similarity)

# DeepL and DTM
dfm.compare.deepl.DFM <- rbind(dfm.D.de.deepl.preprocessed, dfm.D.de.DFM.preprocessed)

sim.deepl.DFM <- data.frame(d_id = as.numeric(),
                            similarity = as.numeric())

for (i in 1:nrow(subset.D.de)) {
  doc <- subset.D.de[i,1]
  d1 <- paste0(doc, "_deepl")
  d2 <- paste0(doc, "_dfm")
  sim <- as.matrix(textstat_simil(dfm.compare.deepl.DFM, d1, method = "cosine", margin = "documents"))
  sim <- sim[d2,1]
  sim.df <- data.frame(d_id = doc, similarity = sim)
  sim.deepl.DFM <- rbind(sim.deepl.DFM, sim.df)
}

hist(sim.deepl.DFM$similarity)

sim.deepl.google$facet <- 1
sim.deepl.DFM$facet <- 2
sim.google.DFM$facet <- 3

sim.histo <- rbind(sim.deepl.google, sim.google.DFM, sim.deepl.DFM)

p1 <- gghistogram(sim.histo, x = "similarity",
                  facet.by = "facet", ncol = 1,
                  fill = "lightgray",
                  xlab = "Document cosine similarity",
                  ylab = "Number of documents",
                  add = "mean",
                  panel.labs = list(facet = c("DL/FT vs. GT/FT", 
                                              "DL/FT vs. GT/DTM",
                                              "GT/FT vs. GT/DTM")),
                  binwidth = 0.05,
                  ggtheme = theme_minimal()
                  )

ggsave(file = "results/fig_similarity_documents.tiff", plot = p1, device = "tiff")
ggsave(file = "results/fig_similarity_documents.pdf", plot = p1, device = "pdf")
ggsave(file = "results/fig_similarity_documents.png", plot = p1, device = "png")

# descriptive figures
nrow(sim.deepl.google)
nrow(sim.google.DFM)
nrow(sim.deepl.DFM)

mean(sim.deepl.google$similarity)
mean(sim.google.DFM$similarity)
mean(sim.deepl.DFM$similarity)

sd(sim.deepl.google$similarity)
sd(sim.google.DFM$similarity)
sd(sim.deepl.DFM$similarity)

min(sim.deepl.google$similarity)
min(sim.google.DFM$similarity)
min(sim.deepl.DFM$similarity)

max(sim.deepl.google$similarity)
max(sim.google.DFM$similarity)
max(sim.deepl.DFM$similarity)

sim.anova <- aov(sim.histo$facet ~ sim.histo$similarity)
summary(sim.anova)

###############################
## comparison of unique terms

features.deepl <- featnames(dfm.D.de.deepl.preprocessed)
features.google <- featnames(dfm.D.de.google.preprocessed)
features.DFM <- featnames(dfm.D.de.DFM.preprocessed)

df.barplot <- data.frame(method = c("DL/FT",
                                    "DL/FT",
                                    "GT/FT",
                                    "GT/FT",
                                    "GT/DTM",
                                    "GT/DTM",
                                    "Overlap",
                                    "Overlap",
                                    "Overlap"),
                         nfeatures = c(nfeature(dfm.D.de.deepl.preprocessed),
                                       nfeature(dfm.D.de.deepl.preprocessed),
                                       nfeature(dfm.D.de.google.preprocessed),
                                       nfeature(dfm.D.de.google.preprocessed),
                                       nfeature(dfm.D.de.DFM.preprocessed),
                                       nfeature(dfm.D.de.DFM.preprocessed),
                                       length(intersect(features.deepl, features.google)),
                                       length(intersect(features.google, features.DFM)),
                                       length(intersect(features.deepl, features.DFM))),
                         facet = c(1,2,1,3,2,3,1,3,2))

p2 <- ggbarplot(df.barplot, x = "method", y = "nfeatures",
                facet.by = "facet", ncol = 1,
                fill = "method",
                palette = "grey",
                remove = "legend",
                xlab = FALSE,
                ylab = "Number of unique terms",
                panel.labs = list(facet = c("DL/FT vs. GT/FT", 
                                            "DL/FT vs. GT/DTM",
                                            "GT/FT vs. GT/DTM")),
                ggtheme = theme_minimal()
                ) + theme(legend.position='none')

ggsave(file = "results/fig_similarity_vocab.tiff", plot = p2, device = "tiff")
ggsave(file = "results/fig_similarity_vocab.pdf", plot = p2, device = "pdf")
ggsave(file = "results/fig_similarity_vocab.png", plot = p2, device = "png")

# descriptive figures
nfeature(dfm.D.de.deepl.preprocessed)
nfeature(dfm.D.de.google.preprocessed)
nfeature(dfm.D.de.DFM.preprocessed)

length(intersect(features.deepl, features.google))
length(intersect(features.google, features.DFM))
length(intersect(features.deepl, features.DFM))

###################
## STM estimation

stm.D.de.deepl <- convert(dfm.D.de.deepl.preprocessed, to = "stm", docvars = docvars(dfm.D.de.deepl.preprocessed))
kresult.D.de.deepl <- searchK(documents = stm.D.de.deepl$documents, vocab = stm.D.de.deepl$vocab, data = stm.D.de.deepl$meta,
                              K = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50), init.type = "LDA")
save(kresult.D.de.deepl, file = 'data/kresult_D_de_deepl.RData')
plot.searchK(kresult.D.de.deepl)

stm.D.de.google <- convert(dfm.D.de.google.preprocessed, to = "stm", docvars = docvars(dfm.D.de.google.preprocessed))
kresult.D.de.google <- searchK(documents = stm.D.de.google$documents, vocab = stm.D.de.google$vocab, data = stm.D.de.google$meta,
                               K = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50), init.type = "LDA")
save(kresult.D.de.google, file = 'data/kresult_D_de_google.RData')
plot.searchK(kresult.D.de.google)

stm.D.de.DFM <- convert(dfm.D.de.DFM.preprocessed, to = "stm", docvars = docvars(dfm.D.de.DFM.preprocessed))
kresult.D.de.DFM <- searchK(documents = stm.D.de.DFM$documents, vocab = stm.D.de.DFM$vocab, data = stm.D.de.DFM$meta, 
                            K = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50), init.type = "LDA")
save(kresult.D.de.DFM, file = 'data/kresult_D_de_DFM.RData')
plot.searchK(kresult.D.de.DFM)

# estimate STMs (K = 40)

stm.D.de.deepl.K40 <- selectModel(documents = stm.D.de.deepl$documents, vocab = stm.D.de.deepl$vocab, data = stm.D.de.deepl$meta,
                                  K = 40, runs = 20, max.em.its = 300, init.type = "LDA", seed = 1234, verbose = TRUE)
save(stm.D.de.deepl.K40, file = "data/stm_D_de_deepl_40.RData")

stm.D.de.google.K40 <- selectModel(documents = stm.D.de.google$documents, vocab = stm.D.de.google$vocab, data = stm.D.de.google$meta,
                                   K = 40, runs = 20, max.em.its = 300, init.type = "LDA", seed = 1234, verbose = TRUE)
save(stm.D.de.google.K40, file = "data/stm_D_de_google_40.RData")

stm.D.de.DFM.K40 <- selectModel(documents = stm.D.de.DFM$documents, vocab = stm.D.de.DFM$vocab, data = stm.D.de.DFM$meta,
                                K = 40, runs = 20, max.em.its = 300, init.type = "LDA", seed = 1234, verbose = TRUE)
save(stm.D.de.DFM.K40, file = "data/stm_D_de_DFM_40.RData")

# select best model

plotModels(stm.D.de.deepl.K40)
stm.D.de.deepl.K40.sel <- stm.D.de.deepl.K40$runout[[4]]
plotModels(stm.D.de.google.K40)
stm.D.de.google.K40.sel <- stm.D.de.google.K40$runout[[3]]
plotModels(stm.D.de.DFM.K40)
stm.D.de.DFM.K40.sel <- stm.D.de.DFM.K40$runout[[4]]

save(stm.D.de.deepl.K40.sel, stm.D.de.google.K40.sel, stm.D.de.DFM.K40.sel, file = "data/stm_d_de_40_sel.RData")


#########################
## comparison of topics

load(file = "data/stm_d_de_40_sel.RData", .GlobalEnv)
load(file = "data/subset_D.RData", .GlobalEnv)

# wordcloud of every topic based on marginal highest probability
pdf("results/stm_cloud_deepl.pdf", paper = "a4r")
for(i in 1:40){
  cloud(stm.D.de.deepl.K40.sel, topic = i, max.words = 30)
}
dev.off()

pdf("results/stm_cloud_google.pdf", paper = "a4r")
for(i in 1:40){
  cloud(stm.D.de.google.K40.sel, topic = i, max.words = 30)
}
dev.off()

pdf("results/stm_cloud_DTM.pdf", paper = "a4r")
for(i in 1:40){
  cloud(stm.D.de.DFM.K40.sel, topic = i, max.words = 30)
}
dev.off()

# get documents with high topic proportions
get_docs <- function (model, texts, n.docs, threshold) {
  thoughts.df <- data.frame(topic = integer(),
                            d_id = integer(),
                            prop = integer(),
                            text = as.character())
  for(i in 1:40){
    #get documents fulfilling the requirements (treshhold, n.docs)
    thoughts <- findThoughts(model = model, texts = texts, topics = i, n = 500, thres = threshold)
    df <- as.data.frame(c(thoughts$index, thoughts$docs))
    if(nrow(df) < 11){
      df <- df
    } else {
      df <- sample_n(df, n.docs)
      colnames(df) <- c("d_index", "text")
      # add correct d_id of the documents
      d_id <- as.data.frame(subset.D.de[df$d_index,]$d_id)
      colnames(d_id) <- "d_id"
      df <- cbind(d_id, df)
      # add exact topic proportion
      theta <- as.data.frame(model$theta[df$d_index,])
      theta <- select(theta, prop = paste0("V", i))
      df <- cbind(theta, df)
      #combine everything properly  
      df <- df %>%
        mutate(topic = i) %>%
        select(topic, d_id, prop, text)
      #append documents for every topic
      thoughts.df <- rbind(thoughts.df, df)
    }
  }
  return(thoughts.df)
}

docs.deepl <- get_docs(stm.D.de.deepl.K40.sel, subset.D.de$text, 10, 0.6)
docs.google <- get_docs(stm.D.de.google.K40.sel, subset.D.de$text, 10, 0.6)
docs.DFM <- get_docs(stm.D.de.DFM.K40.sel, subset.D.de$text, 10, 0.6)

write.csv(docs.deepl, file = "results/docs_deepl.csv", row.names = FALSE)
write.csv(docs.google, file = "results/docs_google.csv", row.names = FALSE)
write.csv(docs.DFM, file = "results/docs_dtm.csv", row.names = FALSE)

#get cosine similarity of topics across models
load(file = "data/stm_d_de_40_sel.RData", .GlobalEnv)
labels.deepl <- read.csv(file = "data/labels_deepl.csv")
labels.google <- read.csv(file = "data/labels_google.csv")
labels.DFM <- read.csv(file = "data/labels_dtm.csv")

nrow(filter(labels.deepl, Deepl_Label != "TRASH"))
nrow(filter(labels.google, Google_Label != "TRASH"))
nrow(filter(labels.DFM, DFM_Label != "TRASH"))

cosine_similarity <- function(model1, model2, labels1, labels2, name1, name2, n) {
  sage1 <- sageLabels(model1, n = n)
  prob1 <- as.data.frame(t(sage1$marginal$prob))
  colnames(prob1) <- paste(labels1[,1], labels1[,2], sep = "_")
  prob1 <- select(prob1, -ends_with("TRASH"))
  beta1 <- as.data.frame(exp(1)^(t(as.data.frame(model1$beta))))
  sage2 <- sageLabels(model2, n = n)
  prob2 <- as.data.frame(t(sage2$marginal$prob))
  colnames(prob2) <- paste(labels2[,1], labels2[,2], sep = "_")
  prob2 <- select(prob2, -ends_with("TRASH"))
  beta2 <- as.data.frame(exp(1)^(t(as.data.frame(model2$beta))))
  simil.df <- data.frame(m1 = as.character(),
                         m2 = as.character(),
                         simil = as.numeric())
  for (i in 1:ncol(prob1)) {
    for (j in 1:ncol(prob2)) {
      beta1.n <- head(sort(as.numeric(beta1[,i]), decreasing = TRUE), n = n)
      beta2.n <- head(sort(as.numeric(beta2[,j]), decreasing = TRUE), n = n)
      voc <- unique(c(prob1[,i], prob2[,j]))
      tempdf1 <- expand.grid(1:2, voc)
      colnames(tempdf1) <- c("topic", "feature")
      tempdf1$count <- 0
      tempdf2 <- rbind(tempdf1,
                       data.frame(topic = 1,
                                  feature = as.character(prob1[,i]),
                                  count = beta1.n),
                       data.frame(topic = 2,
                                  feature = as.character(prob2[,j]),
                                  count = beta2.n))
      comp.dfm <-as.dfm(as.data.frame.matrix(xtabs(count ~ topic + feature, aggregate(count ~ topic + feature, tempdf2, sum))))
      simil <- textstat_simil(comp.dfm, margin = "documents", method = "cosine")[1]
      simil.df <- rbind(simil.df,
                        data.frame(m1 = colnames(prob1[i]),
                                   m2 = colnames(prob2[j]),
                                   simil = simil))
    }
  }
  simil.df.notrash <- simil.df %>%
    filter(!str_detect(m1, "TRASH")) %>%
    filter(!str_detect(m2, "TRASH"))
  simil.df.top1 <- simil.df.notrash %>%
    filter(simil >= 0.4) %>%
    group_by(m1) %>%
    filter(simil == max(simil))
  simil.df.top2 <- simil.df.notrash %>%
    filter(simil >= 0.4) %>%
    group_by(m2) %>%
    filter(simil == max(simil))
  simil.df.top <- distinct(rbind(simil.df.top1, simil.df.top2)) %>%
    arrange(desc(simil))
  p1 <- ggplot(data = simil.df, aes(x = m1, y = m2)) +
    geom_tile(aes(fill=simil), colour = "white") +
    scale_fill_gradient(low = "yellow", high = "red") +
    theme(axis.text.x = element_text(angle = 330, hjust = 0)) +
    xlab(name1) + ylab(name2) + guides(fill=guide_legend(title="Cosine similarity"))
  p2 <- ggplot(data = simil.df.notrash, aes(x = m1, y = m2)) +
    geom_tile(aes(fill=simil), colour = "white") +
    scale_fill_gradient(low = "yellow", high = "red") +
    theme(axis.text.x = element_text(angle = 330, hjust = 0)) +
    xlab(name1) + ylab(name2) + guides(fill=guide_legend(title="Cosine similarity"))
  return(list(simil.df, simil.df.notrash, simil.df.top, p1, p2))
}

simil.deepl.DFM <- cosine_similarity(stm.D.de.deepl.K40.sel, stm.D.de.DFM.K40.sel, labels.deepl, labels.DFM, "DeepL", "DTM (GT)", n = 30)
simil.deepl.google <- cosine_similarity(stm.D.de.deepl.K40.sel, stm.D.de.google.K40.sel, labels.deepl, labels.google, "DeepL", "Google Translate", n = 20)
simil.DFM.google <- cosine_similarity(stm.D.de.DFM.K40.sel, stm.D.de.google.K40.sel, labels.DFM, labels.google, "DTM (GT)", "Google Translate", n = 20)

write.csv(simil.deepl.DFM[[3]], file = "results/simil_deepl_dfm.csv", row.names = FALSE)
ggsave(file = "results/simil_deepl_dfm.pdf", simil.deepl.DFM[[5]], width = 297, height = 210, units = "mm")

write.csv(simil.deepl.google[[3]], file = "results/simil_deepl_google.csv", row.names = FALSE)
ggsave(file = "results/simil_deepl_google.pdf", simil.deepl.google[[5]], width = 297, height = 210, units = "mm")

write.csv(simil.DFM.google[[3]], file = "results/simil_dfm_google.csv", row.names = FALSE)
ggsave(file = "results/simil_dfm_google.pdf", simil.DFM.google[[5]], width = 297, height = 210, units = "mm")

### find multi-matches

simil.deepl.DFM <- read.csv(file = "results/simil_deepl_dfm.csv")
simil.deepl.google <- read.csv(file = "results/simil_deepl_google.csv")
simil.DFM.google <- read.csv(file = "results/simil_dfm_google.csv")

find_multimatches <- function(simil.df) {
  simil.df.multi <- simil.df %>%
    filter(match == 1 | match == 2) %>%
    left_join(count(filter(simil.df, match == 1 | match == 2), m1), by = "m1") %>%
    left_join(count(filter(simil.df, match == 1 | match == 2), m2), by = "m2") %>%
    mutate(multi = ifelse((n.x+n.y)>2, 1, 0)) %>%
    select(m1, m2, simil, match, multi)
  return(simil.df.multi)
}

simil.deepl.DFM.multi <- find_multimatches(simil.deepl.DFM)
simil.DFM.google.multi <- find_multimatches(simil.DFM.google)
simil.deepl.google.multi <- find_multimatches(simil.deepl.google)

simil.deepl.google.nd <- filter(simil.deepl.google, match == 1)
simil.deepl.DFM.nd <- filter(simil.deepl.DFM, match == 1)
simil.DFM.google.nd <- filter(simil.DFM.google, match == 1)

simil.bar <- data.frame(height = c(nrow(filter(simil.deepl.google.multi, multi == 0)), nrow(filter(simil.deepl.google.multi, multi == 1)), nrow(simil.deepl.google.nd),
                                   nrow(filter(simil.deepl.DFM.multi, multi == 0)), nrow(filter(simil.deepl.DFM.multi, multi == 1)), nrow(simil.deepl.DFM.nd),
                                   nrow(filter(simil.DFM.google.multi, multi == 0)), nrow(filter(simil.DFM.google.multi, multi == 1)), nrow(simil.DFM.google.nd)),
                        cat1 = factor(c("unique matches","multi-matches", "first/unique matches",
                                       "unique matches","multi-matches", "first/unique matches", 
                                       "unique matches","multi-matches", "first/unique matches"),
                                     levels = c("multi-matches","unique matches", "first/unique matches")),
                        cat2 = factor(c(1,1,2,1,1,2,1,1,2)),
                        comb = factor(c("DL/FT vs. GT/FT", "DL/FT vs. GT/FT", "DL/FT vs. GT/FT",
                                        "DL/FT vs. GT/DTM", "DL/FT vs. GT/DTM", "DL/FT vs. GT/DTM",
                                        "GT/FT vs. GT/DTM", "GT/FT vs. GT/DTM", "GT/FT vs. GT/DTM"),
                                      levels = c("DL/FT vs. GT/FT", "DL/FT vs. GT/DTM", "GT/FT vs. GT/DTM")))

p3 <- ggbarplot(simil.bar, x = "cat2", y = "height",
                fill = "cat1", 
                color = "cat1", 
                palette = c("#bdbdbd", "#636363", "#cccccc"),
                facet.by = "comb",
                label = TRUE, 
                lab.col = "black",
                lab.pos = "in",
                xlab = FALSE,
                ylab = "Number of topic pairs",
                legend = "top",
                ggtheme = theme_minimal()) +
  theme(legend.title=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave(file = "results/fig_matches.tiff", plot = p3, device = "tiff")
ggsave(file = "results/fig_matches.pdf", plot = p3, device = "pdf")
ggsave(file = "results/fig_matches.png", plot = p3, device = "png")

### cosine similarity of matched topics (first matches)

simil.deepl.google.nd <- filter(simil.deepl.google, match == 1)
simil.deepl.DFM.nd <- filter(simil.deepl.DFM, match == 1)
simil.DFM.google.nd <- filter(simil.DFM.google, match == 1)

nrow(simil.deepl.google.nd)
nrow(simil.deepl.DFM.nd)
nrow(simil.DFM.google.nd)

simil.line <- rbind(data.frame(comb = "DL/FT vs. GT/FT",
                               rank = 1:nrow(simil.deepl.google.nd),
                               simil = simil.deepl.google.nd$simil,
                               match = simil.deepl.google.nd$match),
                    data.frame(comb = "DL/FT vs. GT/DTM",
                               rank = 1:nrow(simil.deepl.DFM.nd),
                               simil = simil.deepl.DFM.nd$simil,
                               match = simil.deepl.DFM.nd$match),
                    data.frame(comb = "GT/FT vs. GT/DTM",
                               rank = 1:nrow(simil.DFM.google.nd),
                               simil = simil.DFM.google.nd$simil,
                               match = simil.DFM.google.nd$match))
simil.line$comb <- factor(simil.line$comb, levels = c("DL/FT vs. GT/FT", "DL/FT vs. GT/DTM","GT/FT vs. GT/DTM"))
simil.line$match <- factor(simil.line$match)

p4 <- ggline(simil.line, "rank", "simil",
       linetype = "comb",
       color = "comb",
       palette = "grey",
       xlab = "Rank of topic pair",
       ylab = "Cosine similarity of topic pair",
       legend = "top",
       ggtheme = theme_minimal()) +
  theme(legend.title=element_blank())

ggsave(file = "results/fig_cosine_1st.tiff", plot = p4, device = "tiff")
ggsave(file = "results/fig_cosine_1st.pdf", plot = p4, device = "pdf")
ggsave(file = "results/fig_cosine_1st.png", plot = p4, device = "png")

### Topical prevalence correlations

load(file = "data/stm_d_de_40_sel.RData", .GlobalEnv)

load(file = "data/dfm_D_de_DeepL_preprocessed.RData", .GlobalEnv)
load(file = "data/dfm_D_de_Google_preprocessed.RData", .GlobalEnv)
load(file = "data/dfm_D_de_DFM_preprocessed.RData", .GlobalEnv)

labels.deepl <- read.csv(file = "data/labels_deepl.csv")
labels.google <- read.csv(file = "data/labels_google.csv")
labels.DFM <- read.csv(file = "data/labels_dtm.csv")

deepl.thetas <- as.data.frame(stm.D.de.deepl.K40.sel$theta)
colnames(deepl.thetas) <- paste(labels.deepl[,1], labels.deepl[,2], sep = "_")
deepl.thetas$d_id <- docvars(dfm.D.de.deepl.preprocessed, field = "d_id")
google.thetas <- as.data.frame(stm.D.de.google.K40.sel$theta)
colnames(google.thetas) <- paste(labels.google[,1], labels.google[,2], sep = "_")
google.thetas$d_id <- docvars(dfm.D.de.google.preprocessed, field = "d_id")
DFM.thetas <- as.data.frame(stm.D.de.DFM.K40.sel$theta)
colnames(DFM.thetas) <- paste(labels.DFM[,1], labels.DFM[,2], sep = "_")
DFM.thetas$d_id <- docvars(dfm.D.de.DFM.preprocessed, field = "d_id")

simil.deepl.DFM <- read.csv(file = "results/simil_deepl_dfm.csv")
simil.deepl.google <- read.csv(file = "results/simil_deepl_google.csv")
simil.DFM.google <- read.csv(file = "results/simil_dfm_google.csv")

simil.deepl.DFM.nd <- filter(simil.deepl.DFM, match == 1)
simil.deepl.google.nd <- filter(simil.deepl.google, match == 1)
simil.DFM.google.nd <- filter(simil.DFM.google, match == 1)

get_paircor <- function (theta1, theta2, simil) {
  merged.thetas <- full_join(theta1, theta2, by = "d_id")
  corr.df <- data.frame(top.deepl = as.character(),
                        top.DFM = as.character(),
                        corr = as.numeric())
  for(i in 1:nrow(simil)) {
    top1 <- as.character(simil[i,1])
    top2 <- as.character(simil[i,2])
    match <- as.numeric(simil[i,4])
    corr <- cor(merged.thetas[,top1], merged.thetas[,top2])
    corr.df <- rbind(corr.df, data.frame(top1 = top1,
                                         top2 = top2,
                                         corr = corr,
                                         match = match))
  }
  return(corr.df)
}

cor.deepl.google <- get_paircor(deepl.thetas, google.thetas, simil.deepl.google.nd)
cor.deepl.DFM <- get_paircor(deepl.thetas, DFM.thetas, simil.deepl.DFM.nd)
cor.DFM.google <- get_paircor(DFM.thetas, google.thetas, simil.DFM.google.nd)

cor.deepl.google$facet <- 1
cor.deepl.DFM$facet <- 2
cor.DFM.google$facet <- 3

topic.histo <- rbind(cor.deepl.google, cor.DFM.google, cor.deepl.DFM)
topic.histo$match <- as.factor(topic.histo$match)

p5 <- gghistogram(topic.histo, x = "corr",
                  facet.by = "facet", ncol = 1,
                  fill = "lightgray",
                  add = "mean",
                  xlab = "Correlation",
                  ylab = "Number of topic pairs",
                  panel.labs = list(facet = c("DL/FT vs. GT/FT", 
                                              "DL/FT vs. GT/DTM",
                                              "GT/FT vs. GT/DTM")),
                  binwidth = 0.1,
                  ggtheme = theme_minimal()
                  ) + scale_y_continuous(name = waiver(), breaks = c(0,2,4,6,8,10), minor_breaks = waiver())

ggsave(file = "results/fig_similarity_topicpairs.tiff", plot = p5, device = "tiff")
ggsave(file = "results/fig_similarity_topicpairs.pdf", plot = p5, device = "pdf")
ggsave(file = "results/fig_similarity_topicpairs.png", plot = p5, device = "png")
