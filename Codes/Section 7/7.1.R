# Course Title: Mastering R Programming
# Author: Selva Prabhakaran
# URL: https://www.packtpub.com/big-data-and-business-intelligence/mastering-r-programming-video
# Contact: selva86@gmail.com
# Website: www.r-statistics.co


# Import text file
text = readLines("https://raw.githubusercontent.com/selva86/datasets/master/yoga.txt")
length(text)  # 10 lines
text[1:5]

# Import Web page's source
text = readLines("https://en.wikipedia.org/wiki/Yoga")
head(text)

# selector <- '#installation'

# Use `rvest` to scrape particular portions of webpage. ---------------
library(rvest)
page <- read_html("https://cran.r-project.org/web/packages/rvest/vignettes/selectorgadget.html")
page

selector <- "#use > ol:nth-child(3) > li:nth-child(3) > p"  # css selector

# Extract portion that 'selector' points to
library(dplyr)
# using CSS selector
txt <- page %>% html_node(css=selector) %>% html_text()
txt

node1 <- html_node(page, css=selector)
txt <- html_text(node1)


# using xpath
xpath <- '//*[@id="use"]/ol[1]/li[3]/p'
txt <- page %>% html_node(xpath=xpath) %>% html_text()
txt



# Answer
library(rvest)
txt <- read_html("https://en.wikipedia.org/wiki/Yoga")
txt_body <- txt %>% html_node("body") %>% html_text()
txt_body



# get all <p> tags
txt_p <- txt %>% html_nodes("p") %>% html_text()
txt_p

# html regex
# Remove any remaining HTML.
pattern <- "</?\\w+((\\s+\\w+(\\s*=\\s*(?:\".*?\"|'.*?'|[^'\">\\s]+))?)+\\s*|\\s*)/?>"
txt_m1 = stringr::str_replace_all(txt_p,pattern,"")  # from stringr package.
head(txt_m1)


# Remove extra whitespaces
txt_m2 <- stringr::str_wrap(txt_m1)
head(txt_m2)

# Remove citations
txt_out <- stringr::str_replace_all(txt_m2, "\\[.*\\]", "")
txt_out

# Web URL: http://datacube.wu.ac.at/src/contrib/            
# Package: "openNLPmodels.en_1.5-1.tar.gz"

# Convert to sentences.---------------------------------------------------------------
library(NLP)
library(openNLP)
options(java.parameters = "-Xmx4000m")  # for more memory.

# Create fn to compute sentence annotations with Apache OpenNLP Maxent sentence detector
sentence_token_annotator <- openNLP::Maxent_Sent_Token_Annotator(language = "en")

# Convert text to class String from package NLP
text <- NLP::as.String(txt_out)

sentence.boundaries <- NLP::annotate(text, sentence_token_annotator)  # Sentence boundaries
sentences <- text[sentence.boundaries] # Extract sentences
head(sentences)

# remove newline chars '\n'
sentences_1 <- vapply(sentences, function(x){stringr::str_replace_all(x, "\n", " ")}, 
                      FUN.VALUE = character(1))

# remove the extra double quotes '"' 
sentences_2 <- vapply(sentences_1, function(x){stringr::str_replace_all(x, '[\\"*]', "")},
                      FUN.VALUE = character(1))

names(sentences_2) <- NULL
writeLines(sentences_2, "sentences.txt")  # save in file

