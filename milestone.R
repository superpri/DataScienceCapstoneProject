
library(RCurl)
library(jsonlite)
library(textcat)
library(parallel)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(stringi)
library(ggplot2)
library(openNLP)

#windows
directory <- "C:\\Users\\Home\\Documents\\capstone\\dataset\\en_US\\"

#linux
#directory <- "C:\\Users\\Home\\Documents\\capstone\\dataset\\en_US\\"

blog.file <- paste(directory,"en_US.blogs.txt", sep = "")
twitter.file <- paste(directory,"en_US.twitter.txt", sep = "")
news.file <- paste(directory,"en_US.news.txt", sep = "")

blog <- readLines(blog.file, encoding="UTF-8")
twitter <- readLines(twitter.file, encoding="UTF-8")
news <- readLines(news.file, encoding="UTF-8")

blog.stats <- stri_stats_general(blog)
twitter.stats <- stri_stats_general(twitter)
news.stats <- stri_stats_general(news)

blog.summary <- summary(sapply(blog,FUN=nchar))
twitter.summary <- summary(sapply(twitter,FUN=nchar))
news.summary <- summary(sapply(news,FUN=nchar))

blog.wordCount <- summary(stri_count_words(blog))
twitter.wordCount <- summary(stri_count_words(twitter))
news.wordCount <- summary(stri_count_words(news))

blog.sample <- head(blog, 1000)
twitter.sample <- head(twitter, n=1000)
news.sample <- head(news, 1000)
sample <- iconv(as.String(paste(twitter.sample, news.sample, blog.sample, collapse = " ")))

word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()
sample_annotations <- NLP::annotate(sample, list(sent_ann, word_ann))
sample_doc <- AnnotatedPlainTextDocument(sample, sample_annotations)
wrds <- as.list(words(sample_doc))
isProfanity <- function(w){
    tryCatch({u <- paste(c("http://www.wdyl.com/profanity?q=",w), collapse = '')
              fromJSON(getURLContent(u))$response == "true"},
             error = function(err){return(FALSE)},
             warning = function(war){return(FALSE)});
}
wrds_TF <- mclapply(wrds,isProfanity,mc.cores = 1)
profanity<-unlist(wrds[grep(TRUE,wrds_TF)])

corpus <- Corpus(VectorSource(sample))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(removePunctuation))
corpus <- tm_map(corpus, content_transformer(removeNumbers))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))
corpus <- tm_map(corpus, function(x) removeWords(x, profanity))
