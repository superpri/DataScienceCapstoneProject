input <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"

#Clean input
input <- tolower(input) 
input <- removePunctuation(input)
input <- removeNumbers(input)
input <- stripWhitespace(input)
input <- stem(input)
input <- stemCompletion(input)

howMany <- unlist(strsplit(input," "))
unigram.size <- nrow(unigram)

trigram[trigram$Term == "the guy in",]$p
bigram[bigram$Term == "guy in",]$p
unigram[unigram$Term == "in",]$p

trigram[trigram$Term == "front of me",]$p
bigram[bigram$Term == "of me",]$p
unigram[unigram$Term == "me",]$p

trigram[trigram$Term == "just bought a",]$p
bigram[bigram$Term == "bought a",]$p
unigram[unigram$Term == "a",]$p

trigram[trigram$Term == "pound of bacon",]$p
bigram[bigram$Term == "of bacon",]$p
unigram[unigram$Term == "bacon",]$p

trigram[trigram$Term == "a bouquet and",]$p
bigram[bigram$Term == "bouquet and",]$p
unigram[unigram$Term == "and",]$p

trigram[trigram$Term == "a case of",]$p
bigram[bigram$Term == "case of",]$p
unigram[unigram$Term == "of",]$p
