# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


# tidytext
library(dplyr)
library(fortunes)
library(tidytext)
library(ggplot2)
theme_set(theme_bw())

df <- read.csv("https://goo.gl/mkDSD7", stringsAsFactors = F)  
# full url: https://raw.githubusercontent.com/selva86/datasets/master/phone_transcripts.csv

# 'the food was not good'
# Score Sentiments ----------------------------------------------------------------------
AFINN <- sentiments %>%
  filter(lexicon == "AFINN")

df_sentiments <- df %>% unnest_tokens(word, Comments) %>%
                      anti_join(stop_words, by = "word") %>%  # remove stopwords
                      inner_join(AFINN, by = "word")  # retain only sentiments.

View(df_sentiments)


df_sentiments_grouped <- group_by(df_sentiments, ID) %>%
  summarize(words = n(),
            recommend=unique(Recommend),
            quotes = n_distinct(ID),
            sentiment = mean(score),
            any_negative_words=if(any(score < 0)){TRUE}else{FALSE})

View(df_sentiments_grouped)


# Plot -------------------------------------------------------------------------
library(ggplot2)
ggplot(df_sentiments_grouped, aes(ID, sentiment, fill = sentiment > 0)) +
  geom_bar(stat = "identity", show.legend = FALSE) + 
  labs(y="Average AFINN sentiment", title="Sentiment by Caller") + 
  coord_flip()



# Syuzhet ----------------------------------------------------------------------
library(syuzhet)
df$syuzhet <- get_sentiment(df$Comments)
df$syuzhet_bing <- get_sentiment(df$Comments, method = "bing")
df$syuzhet_afinn <- get_sentiment(df$Comments, method = "afinn")
df$syuzhet_nrc <- get_sentiment(df$Comments, method = "nrc")
View(df)

# Sentence moods from 'nrc' lexicon
get_nrc_sentiment(df$Comments)

















