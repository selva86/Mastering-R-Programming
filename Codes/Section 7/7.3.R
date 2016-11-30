# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


library(tm) 
library(lsa)

# Create corpus ------------------------------------------------------
wiki_docs <- Corpus(DirSource("02. Mastering R/data/cosine_sim"))
# Download link: https://github.com/selva86/Mastering-R-Programming/tree/master/Datasets
wiki_docs

# Func to view corpus
to_char <- function(corpus, start=1, end=NULL){
  if(is.null(end)){
    end=length(corpus)  
  }
  sapply(corpus[start:end], function(x){paste(x$content)})
}

wiki_docs <- tm_map(wiki_docs, removeWords, c(lsa::stopwords_en))

# a longer list of stop words.
stpwords <- readLines("https://raw.githubusercontent.com/selva86/datasets/master/stopwords_long")

wiki_docs <- tm_map(wiki_docs, removeWords, stpwords)
wiki_docs <- tm_map(wiki_docs, removeNumbers)
wiki_docs <- tm_map(wiki_docs, removePunctuation)
wiki_docs <- tm_map(wiki_docs, content_transformer(function(x){stringr::str_wrap(x)}) )
to_char(wiki_docs)
wiki_docs <- tm_map(wiki_docs, content_transformer(function(x){stringr::str_replace_all(x, "\n", " ")}))
to_char(wiki_docs)

tdm_wiki <- TermDocumentMatrix(wiki_docs, control = list(removePunctuation=TRUE,
                                                         removeNumbers=TRUE,
                                                         stopwords=stpwords))

# Cosine Similarity.
m <- as.matrix(tdm_wiki)
View(m)
csn <- lsa::cosine(m)
csn

# Latent Semantic Analysis.
wiki_docs <- Corpus(DirSource("02. Mastering R/data/lsa_sports_politics_docs"))
# Download Link: https://github.com/selva86/Mastering-R-Programming/tree/master/Datasets

wiki_docs <- tm_map(wiki_docs, content_transformer(removeWords), c(lsa::stopwords_en))
wiki_docs <- tm_map(wiki_docs, content_transformer(removeNumbers))
wiki_docs <- tm_map(wiki_docs, content_transformer(removePunctuation))
wiki_docs <- tm_map(wiki_docs, content_transformer(function(x){stringr::str_wrap(x)}) )
wiki_docs <- tm_map(wiki_docs, content_transformer(function(x){stringr::str_replace_all(x, "\n", " ")}) )
to_char(wiki_docs)


tdm_wiki <- TermDocumentMatrix(wiki_docs, control = list(removePunctuation=TRUE,
                                                         removeNumbers=TRUE,
                                                         stopwords=stpwords,
                                                         weighting=weightTfIdf))


# Run LSA
lsa_out = lsa::lsa(tdm_wiki, dims=lsa::dimcalc_share())
lsa_out

# docs_df
docs_mat <- lsa_out$dk[, c(1:2)]
plotmat_docs_df <- as.data.frame(docs_mat)
colnames(plotmat_docs_df) <- c("Dim1", "Dim2")

# k-means cluster the docs dataframe.
set.seed(101)
clus <- kmeans(plotmat_docs_df, 3)
plotmat_docs_df$cluster <- factor(clus$cluster)


# plot documents in ggplot2
# devtools::install_github("slowkow/ggrepel@0.6.2")
library(ggplot2)
library(ggrepel)
ggplot2::ggplot(plotmat_docs_df, aes(x=Dim1, y=Dim2)) +
  ggplot2::geom_point(size=2, aes(color=cluster)) +
  ggrepel::geom_text_repel(aes(label = rownames(plotmat_docs_df)), 
                           data = plotmat_docs_df, size=3) + 
  ggplot2::theme_bw()






# Answer
# Latent Semantic Analysis.
wiki_docs <- Corpus(DirSource("02. Mastering R/data/lsa_sports_food_docs"))
# Download link: https://github.com/selva86/Mastering-R-Programming/tree/master/Datasets

wiki_docs <- tm_map(wiki_docs, content_transformer(removeWords), c(lsa::stopwords_en))
wiki_docs <- tm_map(wiki_docs, content_transformer(removeNumbers))
wiki_docs <- tm_map(wiki_docs, content_transformer(removePunctuation))
wiki_docs <- tm_map(wiki_docs, content_transformer(function(x){stringr::str_wrap(x)}) )
wiki_docs <- tm_map(wiki_docs, content_transformer(function(x){stringr::str_replace_all(x, "\n", " ")}) )
to_char(wiki_docs)


tdm_wiki <- TermDocumentMatrix(wiki_docs, control = list(removePunctuation=TRUE,
                                                         removeNumbers=TRUE,
                                                         stopwords=TRUE,
                                                         weighting=weightTfIdf))

# Run LSA
lsa_out = lsa::lsa(tdm_wiki, dims=lsa::dimcalc_share())

# docs_df
docs_mat <- lsa_out$dk[, c(1:2)]
plotmat_docs_df <- as.data.frame(docs_mat)
colnames(plotmat_docs_df) <- c("Dim1", "Dim2")

# k-means cluster the docs dataframe.
set.seed(101)
clus <- kmeans(plotmat_docs_df, 3)
plotmat_docs_df$cluster <- factor(clus$cluster)

# plot documents in ggplot2
ggplot(plotmat_docs_df, aes(x=Dim1, y=Dim2)) +
  geom_point(size=2, aes(color=cluster)) +
  ggrepel::geom_text_repel(aes(label = rownames(plotmat_docs_df)), data = plotmat_docs_df, size=3) + theme_bw()

