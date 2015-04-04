# Most words should be easy to predict while using a small number of the total unqiue words from the documents.
# Future work will include developing a prediction model based on each of the three sets of documents using only the most frequent words.
# Using these corpora, my next steps will be to use probability tables for word combinations (i.e. “n-grams”“). These probabilities will serve as the basis for prediction in my app. This logic is known as a”markov-chain" approach to language modeling.
# The eventual app will take a user-defined string as input, check to see if it is present/frequent in the highest-order ngram, and if not, successively check lower-order ngrams. This technique is called “backoff.”
# A hashing model is employed to convert the text tokens to hash key values to build the training data set with prediction likelihood estimates. 

setwd("~/coursera/dataScienceSpecialization/DataScienceCapstoneProject/data")

library(bitops)
library(RCurl)
library(jsonlite)
library(textcat)
library(parallel)
library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(stringi)
library(RWeka)
library(ggplot2)
library(openNLP)
library(SnowballC)
library(wordnet)

require(utils.R)

blog.file <- "/home/superpri/coursera/dataScienceSpecialization/DataScienceCapstoneProject/data/final/en_US/en_US.blogs.txt"
twitter.file <- "/home/superpri/coursera/dataScienceSpecialization/DataScienceCapstoneProject/data/final/en_US/en_US.twitter.txt"
news.file <- "/home/superpri/coursera/dataScienceSpecialization/DataScienceCapstoneProject/data/final/en_US/en_US.news.txt"

saveFileAndUniqueWords(blog.file, rdataName = "blog", path="/home/superpri/coursera/dataScienceSpecialization/DataScienceCapstoneProject/data/")
saveFileAndUniqueWords(twitter.file, rdataName = "twitter", path="/home/superpri/coursera/dataScienceSpecialization/DataScienceCapstoneProject/data/")
saveFileAndUniqueWords(news.file, rdataName = "news", path="/home/superpri/coursera/dataScienceSpecialization/DataScienceCapstoneProject/data/", re)

a <- readLines(blog.file, n=2000, encoding="UTF-8")
a <- iconv(a, from="UTF-8", to="latin1", sub=" ")
a_list <- unlist(strsplit(tolower(paste(a, collapse = " "))," "))
a_list <- paste(unique(a_list), collapse=" ")

b <- readLines(twitter.file, n=2000, encoding="UTF-8")
b <- iconv(b, from="UTF-8", to="latin1", sub=" ")
b_list <- unlist(strsplit(tolower(paste(b, collapse = " "))," "))
b_list <- paste(unique(b_list), collapse=" ")

c <- readLines(news.file, n=2000, encoding="UTF-8")
c <- iconv(c, from="UTF-8", to="latin1", sub=" ")
c_list <- unlist(strsplit(tolower(paste(c, collapse = " "))," "))
c_list <- paste(unique(c_list), collapse=" ")

files <- iconv(as.String(paste(a,b,c, collapse = " ")))

word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()

files_annotations <- NLP::annotate(files, list(sent_ann, word_ann))
files_doc <- AnnotatedPlainTextDocument(files, files_annotations)

# wrds <- as.list(words(files_doc))
# 
# isProfanity <- function(w){
#     tryCatch({u <- paste(c("http:/www.wdyl.com/profanity?q=",w), collapse = '')
#               fromJSON(getURLContent(u))$response == "true"},
#              error = function(err){return(FALSE)},
#              warning = function(war){return(FALSE)});
# }
# wrds_TF <- mclapply(wrds,isProfanity,mc.cores = 8)
# profanity<-unlist(wrds[grep(TRUE,wrds_TF)])

corpus <- Corpus(VectorSource(files))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, content_transformer(removePunctuation))
corpus <- tm_map(corpus, content_transformer(removeNumbers))
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, function(x) removeWords(x, stopwords("english")))
#corpus <- tm_map(corpus, function(x) removeWords(x, profanity))

ngram <- function(x,n) NGramTokenizer(x, Weka_control(min = n, max = n))
n1gram <- function(x) ngram(x,1)
n2gram <- function(x) ngram(x,2)
n3gram <- function(x) ngram(x,3)

corpus.tdm <- DocumentTermMatrix(corpus, control = list(tokenize = n1gram))
corpus.tdm.n2 <- DocumentTermMatrix(corpus, control = list(tokenize = n2gram))
corpus.tdm.n3 <- DocumentTermMatrix(corpus, control = list(tokenize = n3gram))

corpus.tdm <- removeSparseTerms(corpus.tdm, 0.7)
unigram <- data.frame(Term = corpus.tdm$dimnames$Terms, Freq = corpus.tdm$v) 
unigram <- unigram[order(unigram$Freq,decreasing = T),]

sumOfFrequencies <- sum(unigram$Freq)

#prob do unigram = seu count/qtos linhas
unigram <- cbind(p = unigram$Freq/sumOfFrequencies, unigram)



# Based on the frequency distribution by word length depicted in the first three charts, 
# the word list is trimmed to words that are less than 30 characters long. The only reason 
# to bring down the threshold to under 21 characters will be to address the size of 
# prediction training data set for speed and publishing server limits for the assignment.
#unigram <- unigram[length(unigram$Term) > 30,]

# After the basic clean up text lines in the base raw data table, 
# we need to compile the list of all 1Gram words that are valid English language words.

corpus.tdm.n2 <- removeSparseTerms(corpus.tdm.n2, 0.7)
bigram <- data.frame(Term = corpus.tdm.n2$dimnames$Terms, Freq = corpus.tdm.n2$v) 
bigram <- bigram[order(bigram$Freq,decreasing = T),] 
bigram <- bigram[bigram$Freq > 1,]

#prob do bigram = seu count/count do primeiro unigram
bigram <- cbind(p = bigram$Freq/unigram[unigram$Term == unlist(strsplit(as.String(bigram$Term), " "))[1],]$Freq, bigram)

corpus.tdm.n3 <- removeSparseTerms(corpus.tdm.n3, 0.7)
trigram <- data.frame(Term = corpus.tdm.n3$dimnames$Terms, Freq = corpus.tdm.n3$v) 
trigram <- trigram[order(trigram$Freq,decreasing = T),] 
trigram <- trigram[trigram$Freq > 1,]

#prob do trigram = seu count/ count dos dois primeiros
trigram <- cbind(p = trigram$Freq/bigram[bigram$Term == paste(unlist(strsplit(as.String(trigram$Term), " "))[1:2],collapse=" "),]$Freq,trigram)



# probabilidade da frase: sum (log (probbilidade de n-gram) )


# all_files <- c(blog, twitter, news)
# train.idx <- files.int(length(all_files), length(all_files)*0.70)
# train.files <- all_files[train.idx]
# test.files <- all_files[-train.idx]
