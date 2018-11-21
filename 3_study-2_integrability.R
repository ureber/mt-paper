############################
## Study 2: Integrability ##
############################

# Ueli Reber, University of Bern
# ueli.reber@ikmb.unibe.ch
# https://github.com/ureber/

################################
## load packages and functions

options(stringsAsFactors = F)

library(dplyr)
library(quanteda)
library(stm)
library(broom)

source("functions/deepl.R", encoding = "UTF-8")
source("functions/make_corpus.R", encoding = "UTF-8")
source("functions/do_preprocessing.R", encoding = "UTF-8")

###################
## STM estimation

load(file = "data/subset_all.RData", .GlobalEnv)

corpus <- make_corpus(subset.all)
tokens <- tokens(corpus, what = 'word', remove_punct = TRUE, remove_symbols = TRUE, 
                 remove_numbers = TRUE, remove_hyphens = FALSE, include_docvars = TRUE, verbose=TRUE)
dfm.raw <- dfm(tokens, tolower = FALSE, stem = FALSE, verbose=TRUE)

dfm.preprocessed <- do_preprocessing(dfm.raw)

save(dfm.preprocessed, file = "data/dfm_all_preprocessed.RData")

# get basic descriptions about the corpus
docvars.df <- docvars(dfm.preprocessed)
class(docvars.df$d_id)
pages <- docvars.df %>%
  group_by(actor_country) %>%
  summarise(n = n())
orgas <- docvars.df %>%
  group_by(actor_country) %>%
  summarise(n = n_distinct(organisation))
lang <- docvars.df %>%
  group_by(language) %>%
  summarise(n = n())

# convert dfm to stm object
stm.all <- convert(dfm.preprocessed, to = "stm", docvars = docvars(dfm.preprocessed))

# encode variable as factor
stm.all$meta$language <- as.factor(stm.all$meta$language)

# evaluate the right number of topcis (search K)
kresult <- searchK(documents = stm.all$documents, vocab = stm.all$vocab, data = stm.all$meta,
                   K = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50), content = ~ language, init.type = "LDA")

save(kresult, file = 'data/kresult_all.RData')
plot.searchK(kresult)

# estimate STM
stm.all.30 <- stm(documents = stm.all$documents, vocab = stm.all$vocab, data = stm.all$meta,
                  K = 30, content = ~ language, init.type = "LDA", seed = 12345)

save(stm.all.30, file = "data/stm_all_30.RData")

# wordcloud of every topic based on marginal highest probability
pdf("climate/stm_cloud_all_30.pdf", paper = "a4r")
for(i in 1:30){
  cloud(stm.all.30, topic = i, max.words = 80)
}
dev.off()

# get documents with high topic proportions
thoughts.df <- data.frame(topic = integer(),
                          d_id = integer(),
                          prop = integer(),
                          text = as.character())
threshold <- 0.6
n.docs <- 10

for(i in 1:stm.all.30$settings$dim$K){
  #get documents fulfilling the requirements (treshhold, n.docs)
  thoughts <- findThoughts(stm.all.30, texts = subset.all$text, topics = i, n = 500, thres = threshold)
  df <- as.data.frame(c(thoughts$index, thoughts$docs))
  if(nrow(df) < 11){
    df <- df
  } else {
    df <- sample_n(df, n.docs)
  }
  colnames(df) <- c("d_index", "text")
  # add correct d_id of the documents
  d_id <- as.data.frame(subset.all[df$d_index,]$d_id)
  colnames(d_id) <- "d_id"
  df <- cbind(d_id, df)
  # add exact topic proportion
  theta <- as.data.frame(stm.all.30$theta[df$d_index,])
  theta <- select(theta, prop = paste0("V", i))
  df <- cbind(theta, df)
  #combine everything properly  
  df <- df %>%
    mutate(topic = i) %>%
    select(topic, d_id, prop, text)
  #append documents for every topic
  thoughts.df <- rbind(thoughts.df, df)
}

write.csv(thoughts.df, file = "climate/stm_documents_all.csv", row.names = FALSE)

# topwords of selected topics
load(file = "data/stm_all_30.RData", .GlobalEnv)
labels <- read.csv(file = "data/stm_labels_all.csv", header = TRUE, sep = ",", encoding = "UTF-8")

topics.select <- labels %>%
  filter(label != "TRASH") %>%
  mutate(t_id = paste0("t_", t_id))

sage <- sageLabels(stm.all.30, n = 10)

prob <- as.data.frame(t(sage$marginal$prob))
colnames(prob) <- paste0("t_", 1:30)
prob <- select(prob, one_of(topics.select$t_id))

topwords.label <- as.data.frame(t(prob)) %>%
  mutate(t_id = rownames(t(prob))) %>%
  left_join(topics.select, b = "t_id")

write.csv(topwords.label, file = "climate/topwords_all.csv", row.names = FALSE)

########################################
## comparison of mean topic proportion

load(file = "data/dfm_all_preprocessed.RData", .GlobalEnv)
stm.all <- convert(dfm.preprocessed, to = "stm", docvars = docvars(dfm.preprocessed))
load(file = "data/stm_all_30.RData", .GlobalEnv)
labels <- read.csv(file = "data/stm_labels_all.csv", header = TRUE, sep = ",", encoding = "UTF-8")

topics.select <- labels %>%
  filter(label != "TRASH") %>%
  mutate(t_id = paste0("t_", t_id))

topics.v <- as.vector(topics.select$t_id)
labels.v <- as.vector(topics.select$label)

# get topic probabilities per page with country information
theta <- as.data.frame(stm.all.30$theta)
country <- as.data.frame(as.factor(stm.all$meta$actor_country))
colnames(country) <- "country"
levels(country$country) <- c("D","USA", "UK")
organisation <- as.data.frame(stm.all$meta$organisation)
theta.info <- cbind(country, organisation, theta)
colnames(theta.info) <- c("country", "organisation", paste0("t_", 1:30))
theta.info <- select(theta.info, country, organisation, topics.v)

theta.info.labels <- theta.info
colnames(theta.info.labels)  <- c("country", "organisation", labels.v)

# check number of distinct organisations per country
theta.orga.count <- theta.info %>%
  select(country, organisation) %>%
  group_by(country) %>%
  summarise(n_distinct(organisation))

# reshape and filter data with first aggregation on organisation level
theta.orga.mean.bar <- theta.info.labels %>%
  group_by(country, organisation) %>%
  summarise_all(mean) %>%
  select(-organisation) %>%
  group_by(country) %>%
  summarise_all(mean) %>%
  melt(id.vars = "country") %>%
  select(Country = country, Topic = variable, Probability = value) %>%
  mutate(Topic = as.character(Topic)) %>%
  arrange(Topic)

p6 <- ggbarplot(theta.orga.mean.bar, x = "Topic", y = "Probability",
                facet.by = "Country", ncol = 1,
                fill = "Country",
                palette = "grey",
                x.text.angle = 90,
                remove = "legend",
                xlab = FALSE,
                ylab = "Mean topic proportion",
                panel.labs = list(Country = c("Germany", "United States", "United Kingdom")),
                ggtheme = theme_minimal()
) + theme(legend.position='none')

ggsave(file = "figures/fig_climate_topicprop.tiff", plot = p6, device = "tiff")
ggsave(file = "figures/fig_climate_topicprop.pdf", plot = p6, device = "pdf")
ggsave(file = "figures/fig_climate_topicprop.png", plot = p6, device = "png")

theta.orga.mean.bar %>%
  select(-Topic) %>%
  group_by(Country) %>%
  summarise_all(mean)

# check for significant differences using ANOVA

theta.orga.mean.aov <- theta.info %>%
  group_by(country, organisation) %>%
  summarise_all(mean)
theta.orga.mean.aov$country <- as.numeric(theta.orga.mean.aov$country)

anova <- tidy(aov(theta.orga.mean.aov$country ~ theta.orga.mean.aov$t_1))
anova <- rbind(anova, tidy(aov(theta.orga.mean.aov$country ~ theta.orga.mean.aov$t_2)))
anova <- rbind(anova, tidy(aov(theta.orga.mean.aov$country ~ theta.orga.mean.aov$t_3)))
anova <- rbind(anova, tidy(aov(theta.orga.mean.aov$country ~ theta.orga.mean.aov$t_4)))
anova <- rbind(anova, tidy(aov(theta.orga.mean.aov$country ~ theta.orga.mean.aov$t_6)))
anova <- rbind(anova, tidy(aov(theta.orga.mean.aov$country ~ theta.orga.mean.aov$t_7)))
anova <- rbind(anova, tidy(aov(theta.orga.mean.aov$country ~ theta.orga.mean.aov$t_11)))
anova <- rbind(anova, tidy(aov(theta.orga.mean.aov$country ~ theta.orga.mean.aov$t_12)))
anova <- rbind(anova, tidy(aov(theta.orga.mean.aov$country ~ theta.orga.mean.aov$t_13)))
anova <- rbind(anova, tidy(aov(theta.orga.mean.aov$country ~ theta.orga.mean.aov$t_14)))
anova <- rbind(anova, tidy(aov(theta.orga.mean.aov$country ~ theta.orga.mean.aov$t_16)))
anova <- rbind(anova, tidy(aov(theta.orga.mean.aov$country ~ theta.orga.mean.aov$t_17)))
anova <- rbind(anova, tidy(aov(theta.orga.mean.aov$country ~ theta.orga.mean.aov$t_18)))
anova <- rbind(anova, tidy(aov(theta.orga.mean.aov$country ~ theta.orga.mean.aov$t_19)))
anova <- rbind(anova, tidy(aov(theta.orga.mean.aov$country ~ theta.orga.mean.aov$t_21)))
anova <- rbind(anova, tidy(aov(theta.orga.mean.aov$country ~ theta.orga.mean.aov$t_22)))
anova <- rbind(anova, tidy(aov(theta.orga.mean.aov$country ~ theta.orga.mean.aov$t_23)))
anova <- rbind(anova, tidy(aov(theta.orga.mean.aov$country ~ theta.orga.mean.aov$t_24)))
anova <- rbind(anova, tidy(aov(theta.orga.mean.aov$country ~ theta.orga.mean.aov$t_26)))
anova <- rbind(anova, tidy(aov(theta.orga.mean.aov$country ~ theta.orga.mean.aov$t_27)))
anova <- rbind(anova, tidy(aov(theta.orga.mean.aov$country ~ theta.orga.mean.aov$t_29)))
anova <- rbind(anova, tidy(aov(theta.orga.mean.aov$country ~ theta.orga.mean.aov$t_30)))

anova$p.star[anova$p.value <= 0.05] <- "*"
anova$p.star[anova$p.value <= 0.01] <- "**"
anova$p.star[anova$p.value <= 0.001] <- "***"
anova$p.star[anova$p.value > 0.05] <- ""
anova$p.star[is.na(anova$p.value)] <- NA

write.csv(anova, file = "climate/results_anova_comparison_means.csv", row.names = FALSE)

anova.labels <- anova %>%
  filter(!is.na(p.value))
anova.labels <- cbind(anova.labels, topics.select)

############################
# comparison of top topics

theta.orga.top <- theta.info.labels %>%
  group_by(country, organisation) %>%
  summarise_all(mean)
theta.orga.top$top_top <- colnames(theta.orga.top[,3:24])[apply(theta.orga.top[,3:24],1,which.max)]
theta.orga.top.n <- theta.orga.top %>%
  select(country, top_top) %>%
  group_by(country) %>%
  count(top_top) %>%
  arrange(top_top)

theta.orga.top.n$nprop[theta.orga.top.n$country == "D"] <- theta.orga.top.n$n[theta.orga.top.n$country == "D"]/95
theta.orga.top.n$nprop[theta.orga.top.n$country == "UK"] <- theta.orga.top.n$n[theta.orga.top.n$country == "UK"]/181
theta.orga.top.n$nprop[theta.orga.top.n$country == "USA"] <- theta.orga.top.n$n[theta.orga.top.n$country == "USA"]/539

p7 <- ggbarplot(theta.orga.top.n, x = "top_top", y = "nprop",
                facet.by = "country", ncol = 1,
                fill = "country",
                palette = "grey",
                x.text.angle = 90,
                remove = "legend",
                xlab = FALSE,
                ylab = "Relative frequency of topic as top topic",
                panel.labs = list(country = c("Germany", "United States", "United Kingdom")),
                ggtheme = theme_minimal()
) + theme(legend.position='none')

ggsave(file = "figures/fig_climate_toptopic.tiff", plot = p7, device = "tiff")
ggsave(file = "figures/fig_climate_toptopic.pdf", plot = p7, device = "pdf")
ggsave(file = "figures/fig_climate_toptopic.png", plot = p7, device = "png")

# check for significant differences using Fisher's Exact Test
theta.orga.top <- theta.info %>%
  group_by(country, organisation) %>%
  summarise_all(mean)
theta.orga.top$top_top <- colnames(theta.orga.top[,3:24])[apply(theta.orga.top[,3:24],1,which.max)]

library(broom)

fisher.df <- data.frame(t_id = character(),
                        p.value = numeric(),
                        method = character(),
                        alternative = character())

for (i in 1:length(topics.v)) {
  top_id <- topics.v[i]
  top.filter <- theta.orga.top %>%
    mutate(top = 1) %>%
    select(country, top_top, top) %>%
    filter(top_top == top_id) %>%
    group_by(country) %>%
    summarise_at("top", funs(sum))
  ntop.filter <- theta.orga.top %>%
    mutate(ntop = 1) %>%
    select(country, top_top, ntop) %>%
    filter(top_top != top_id) %>%
    group_by(country) %>%
    summarise_at("ntop", funs(sum))
  top.fisher <- top.filter %>%
    full_join(ntop.filter, by = "country") %>%
    mutate_at(c("top", "ntop"), funs(replace(., is.na(.), 0)))
  top.fisher <- as.data.frame(top.fisher)
  row.names(top.fisher) <- top.fisher$country
  top.fisher$country <- NULL
  table.fisher <- as.table(as.matrix(top.fisher))
  fisher.res <- fisher.test(table.fisher)
  fisher.res.df <- cbind(as.data.frame(top_id), tidy(fisher.res))
  fisher.df <- rbind(fisher.df, fisher.res.df)
}

fisher.df$p.star[fisher.df$p.value <= 0.05] <- "*"
fisher.df$p.star[fisher.df$p.value <= 0.01] <- "**"
fisher.df$p.star[fisher.df$p.value <= 0.001] <- "***"
fisher.df$p.star[fisher.df$p.value > 0.05] <- ""

write.csv(fisher.df, file = "climate/results_fisher_comparison_toptop.csv", row.names = FALSE)
