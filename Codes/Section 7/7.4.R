# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


library(tm)
options(scipen=999)
wiki_docs <- Corpus(DirSource("02. Mastering R/data/lda_sports_politics_long_docs"))
# alt source: 
# https://github.com/selva86/Mastering-R-Programming/tree/master/Datasets/lda_sports_politics_long_docs

# Func to view corpus
to_char <- function(corpus, start=1, end=NULL){
  if(is.null(end)){
    end=length(corpus)  
  }
  sapply(corpus[start:end], function(x){paste(x$content)})
}


# Text Processing ------------------------------------------------------------------------------
stpwords <- readLines("https://raw.githubusercontent.com/selva86/datasets/master/stopwords_long")
wiki_docs <- tm_map(wiki_docs, content_transformer(tolower))
wiki_docs <- tm_map(wiki_docs, removeWords, c(lsa::stopwords_en, stpwords))
wiki_docs <- tm_map(wiki_docs, removeNumbers)
wiki_docs <- tm_map(wiki_docs, removePunctuation)
wiki_docs <- tm_map(wiki_docs, content_transformer(function(x){stringr::str_wrap(x)}) )
wiki_docs <- tm_map(wiki_docs, content_transformer(function(x){stringr::str_replace_all(x, "\n", " ")}) )
to_char(wiki_docs)

# Doc Term Matrix
dtm <- DocumentTermMatrix(wiki_docs, control = list(removePunctuation=TRUE,
                                                    removeNumbers=TRUE,
                                                    stopwords=TRUE))

dim(dtm)
inspect(dtm)[1:4, ]

# LDA ------------------------------------------------------------------------------------------------
library(topicmodels)
# Set parameters
burnin = 4000
iter = 2000
thin = 500
seed = list(2003,5,63,100001,765)
nstart = 5
best = TRUE

# Number of topics
k = 4

# Run LDA
res <-LDA(dtm, k, method="Gibbs", control = list(nstart = nstart, seed = seed, 
                                                 best = best, burnin = burnin, 
                                                 iter = iter, thin = thin))
res

# Number of topics in each document
res_topics = as.matrix(topics(res))
print(res_topics)

# Top 30 terms
res_terms = as.matrix(terms(res, 30))
print(res_terms)


# Show topic probabilities. Rows are documents and Columns are the topics.
res_topicProbs = as.data.frame(res@gamma)
print(cbind(rownames(res_topics), res_topicProbs))

# Check that each term is allocated to all topics
print(rowSums(res_topicProbs))

# Heat Map with Dendrogram ------------------------------------------------------------------------
# install.packages("d3heatmap")
library(d3heatmap)
library(RColorBrewer)
topic_probs <- data.matrix(res_topicProbs)  # convert to matrix

# Set column and row names
colnames(topic_probs) <- c("Sports", "Politics", "Food", "India-Politics")
rownames(topic_probs) <- rownames(res_topics)

# draw heatmap
d3heatmap(topic_probs, colors = brewer.pal(9, "Greens"), scale="column", margins=c(5,5), dendrogram = "row", k_row = 5, cexRow=0.75)



# Answer --------------------------------------------------------------------------------
library(tm)
options(scipen=999)
df <- read.csv("https://raw.githubusercontent.com/selva86/datasets/master/text_classfication.csv", 
               stringsAsFactors = F)
               
# df <- read.csv("https://goo.gl/l8sWCN", stringsAsFactors = F)

wiki_docs <- Corpus(VectorSource(df$content))

stpwords <- readLines("https://raw.githubusercontent.com/selva86/datasets/master/stopwords_long")

wiki_docs <- tm_map(wiki_docs, content_transformer(tolower))
wiki_docs <- tm_map(wiki_docs, removeWords, c(lsa::stopwords_en, stpwords))
wiki_docs <- tm_map(wiki_docs, removeNumbers)
wiki_docs <- tm_map(wiki_docs, removePunctuation)
wiki_docs <- tm_map(wiki_docs, content_transformer(function(x){stringr::str_wrap(x)}) )
wiki_docs <- tm_map(wiki_docs, content_transformer(function(x){stringr::str_replace_all(x, "\n", " ")}) )
dtm <- DocumentTermMatrix(wiki_docs, control = list(removePunctuation=TRUE,
removeNumbers=TRUE,
stopwords=TRUE))

dim(dtm)

# Set parameters
library(topicmodels)
burnin = 4000
iter = 2000
thin = 500
seed = list(2003,5,63,100001,765)
nstart = 5
best = TRUE

# Number of topics
k = 3

# Run LDA
res <-LDA(dtm, k, method="Gibbs", control = list(nstart = nstart, seed = seed, best = best, burnin = burnin, iter = iter, thin = thin))

res

# Number of topics in each document
res_topics = as.matrix(topics(res))
print(res_topics)

# Top 30 terms
res_terms = as.matrix(terms(res, 30))
print(res_terms)

# Show topic probabilities. Rows are documents and Columns are the topics.
res_topicProbs = as.data.frame(res@gamma)
topic_probs <- data.matrix(res_topicProbs)  # convert to matrix

# Set column and row names
colnames(topic_probs) <- c("Sports", "Politics", "Food")
rownames(topic_probs) <- df$articles

# draw heatmap
d3heatmap::d3heatmap(topic_probs, colors = RColorBrewer::brewer.pal(9, "Blues"), 
                    scale="column", margins=c(5,5), dendrogram = "row", k_row = 5, cexRow=0.75)

