# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


# install.pacakges("tm")
library(tm)
# cp <- Corpus(DirSource("Path/To/Directory/"))

# Read text
char_vec <- readLines("https://raw.githubusercontent.com/selva86/datasets/master/yoga_wiki.txt")

# Corpus ----------------------------------------------------------------------------
# Create Corpus
cp <- Corpus(VectorSource(char_vec))
cp

inspect(cp)  # inspect each document in corpus

cp[[1]]$content
as.character(cp[[1]])

# Function to view inside the corpus
to_char <- function(corpus, start=1, end=NULL){
  if(is.null(end)){
    end=length(corpus)  
  }
  sapply(corpus[start:end], function(x){paste(x$content)})
}
to_char(cp, 1, 15)

# Meta Data ---------------------------------------------------------------------------
# View and modify Meta data
meta(cp[[1]])
meta(cp[[1]], tag = "source") <- "wiki"
meta(cp[[1]])

# Text Transformations ----------------------------------------------------------------
getTransformations()  # get built-in text transformers

# transformer to remove html
pattern <- "</?\\w+((\\s+\\w+(\\s*=\\s*(?:\".*?\"|'.*?'|[^'\">\\s]+))?)+\\s*|\\s*)/?>"
rmHTML <- function(x) gsub(pattern, "", x)

cp_nohtml <- tm_map(cp, content_transformer(rmHTML))
to_char(cp_nohtml[1:10])

# remove punctuations
cp_noPun <- tm_map(cp_nohtml, content_transformer(removePunctuation))
to_char(cp_noPun[1:10])

# stopwords sources
tm::stopwords()  # tm pkg
lsa::stopwords_en  # lsa pkg
tidytext::stop_words  # tidytext pkg

# Text transformations --------------------------------------------------------------
cp_lower <- tm_map(cp_noPun, content_transformer(tolower))  # convert to lower case
cp_noStpwrds <- tm_map(cp_lower, content_transformer(removeWords), stopwords())  # remove stopwords
cp_noSpace <- tm_map(cp_noStpwrds, content_transformer(stripWhitespace))  # strip whitespace
cp_clean <- tm_map(cp_noSpace, content_transformer(trimws))  # trim trailing and leading whitespace.

# Stemming and Stem completion
cp_clean_copy <- cp_clean
cp_clean_stem <- tm_map(cp_clean, stemDocument)
to_char(cp_clean_stem[1:10])

cp_comp <- tm_map(cp_clean_stem, stemCompletion, dictionary=cp_clean_copy)
to_char(cp_comp[1:10])

# Solution by yanchang zhao:
# http://stackoverflow.com/questions/25206049/stemcompletion-is-not-working
# PDF: https://goo.gl/vueUa3s

stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  # Unexpectedly, stemCompletion completes an empty string to
  # a word in dictionary. Remove empty string to avoid above issue.
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}
myCorpus <- lapply(cp_clean_stem, stemCompletion2, dictionary=cp_clean)
cp_complete <- Corpus(VectorSource(myCorpus))

to_char(cp_complete[1:10])

# Term Document Matrix ---------------------------------------------------------------
# Create term document matrix and find frequent words
tdm <- TermDocumentMatrix(cp_complete)
findFreqTerms(tdm, lowfreq=10) 
m <- as.matrix(tdm)
dim(m)
summary(rowSums(m))

# Remove sparse terms
tdm2 <- removeSparseTerms(tdm, 0.99) # Prepare the data (max 15% empty space)   
m_2 <- as.matrix(tdm2)
summary(rowSums(m_2))


# TF-IDF -------------------------------------------------------------------------
# Normalize with TF-IDF
# Method 1:
tdm_w <- weightTfIdf(tdm, normalize = FALSE)  # weight by TFIDF.

# Method 2:
tdm_w <- TermDocumentMatrix(cp_complete, control = list(removePunctuation=TRUE,
                                                        removeNumbers=TRUE,
                                                        stopwords=TRUE,
                                                        weighting=weightTfIdf))
m_3 <- as.matrix(tdm_w)
dim(m_3)
summary(rowSums(m_3))


# Word Cloud ---------------------------------------------------------------------------
library(wordcloud)
word.freq <- sort(rowSums(m_3), decreasing = T)

# color palette
brewer.pal.info  # show available palettes
pal <- RColorBrewer::brewer.pal(8,"Dark2")  # define colour pallet
# pal <- brewer.pal(8,"Spectral")  # define colour pallet
# pal <- brewer.pal(8,"Set2")  # define colour pallet

# plot word cloud
wordcloud(words = names(word.freq), freq = word.freq, random.order = F, 
          colors = pal, max.words = 700)


# WordCloud2 (htmlWidgets) -------------------------------------------------------------
# html implementation with wordcloud2.js
library(wordcloud2)
word.freq <- sort(rowSums(m_3), decreasing = T)

# color palette
pal <- RColorBrewer::brewer.pal(8,"Dark2")  # define colour pallet

word.freq <- sort(rowSums(m_3), decreasing = T)
df <- as.data.frame(word.freq)
df$word <- rownames(df)
colnames(df) <- c("freq", "word")
df <- df[, c("word", "freq")]  # reorder columns

# draw wordcloud
wordcloud2(df, color = pal)
wordcloud2(df, color = pal, shape = "triangle-forward")




# Answers -----------------------------------------------------------------------------------
tx <- readLines("https://raw.githubusercontent.com/selva86/datasets/master/prideprejudice.txt")

tx <- stringr::str_replace_all(tx, "[^[:alnum:] \\.]", "")  # keep only alpha numeric and space
cpx <- tm::Corpus(VectorSource(tx))

# Create TDM
cpx <- tm::tm_map(cpx, content_transformer(tolower))  # convert to lower case
cpx <- tm::tm_map(cpx, content_transformer(removeWords), lsa::stopwords_en)  # remove stopwords
cpx <- tm::tm_map(cpx, content_transformer(stripWhitespace))  # strip whitespace
cpx <- tm::tm_map(cpx, content_transformer(trimws))  # trim trailing and leading whitespace.

tdm_cp <- tm::TermDocumentMatrix(cpx, control = list(removePunctuation = TRUE))

m <- as.matrix(tdm_cp)


# draw word cloud
pal <- RColorBrewer::brewer.pal(8,"Spectral")  # define colour pallet
word.freq <- sort(rowSums(m), decreasing = T)
wordcloud::wordcloud(words = names(word.freq), freq = word.freq, 
                     random.order = F, colors = pal, max.words = 700)
