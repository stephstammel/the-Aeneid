#####################################################################
# Title: Analysing the Aeneid the Easy Way (e.g. why read great works? kidding.)
# Author: Steph de Silva.
# Email: steph@rex-analytics.com
# Date created: 22/05/16
# Date last altered: 23/05/16
# Attributions and acknowledgment of derivation:
# This script is due to information, tips and advice given in:
# (1) ropenscilabs/gutenbergr from https://github.com/ropenscilabs/gutenbergr
#     Accessed: 20/05/16
# (2)  http://www.rdatamining.com/examples/text-mining
#     Accessed: 21/05/16
# Along with helpful code fixes and tweaks from:
# (1)  http://stackoverflow.com/questions/25069798/r-tm-in-mclapplycontentx-fun-all-scheduled-cores-encountered-errors
# (2)  http://www.inside-r.org/packages/cran/tm/docs/as.TermDocumentMatrix
# (3)  https://stat.ethz.ch/pipermail/r-help/2012-May/313013.html
# (4)  http://stackoverflow.com/questions/29358571/termdocumentmatrix-raises-error-in-r
# All accessed 21/05/16
# Also: 
# (5) http://www.chlt.org/StatisticalMethods/preparing-literary-data.html
#  All accessed 20/05/16
# As well as:
# (6)) Julia Silge is amazing: http://juliasilge.com/blog/You-Must-Allow-Me/
# Accessed 22/05/16
# Handy tute link: http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know#text-transformation
# Also colour and font handling as per:  http://www.r-bloggers.com/word-cloud-in-r/
# this was done with the golden Ass- Apuleius was hilarious .. LOL. http://www.r-bloggers.com/word-clouds-using-text-mining/
# Purpose: This script is intended to download the work by Virgil, the Aeneid
# and then create a word cloud.
#########################################################################
# Data Used: Project Gutenberg copy of the Aeneid. 
# Source: https://www.gutenberg.org/ebooks/22456
# Specifically: http://www.gutenberg.org/cache/epub/22456/pg22456.txt
# Translation by:  Mackail, J. W. (John William), 1859-1945
# Date Accessed: 23/05/16
#########################################################################
#########################################################################
# Script Outline:
# 1. Load Libraries, load data, clean data
# 2. Create term document matrix + word cloud
#########################################################################
# 1. Load libraries, load data
#########################################################################

rm(list=ls(all=TRUE))

library(gutenbergr)
library(tidytext)
library(ggplot2)
library(dplyr)
library(tidyr)
# Download from Project Gutenberg
aeneid <- gutenberg_download(22456, meta_fields = "title") %>%
mutate(linenumber = row_number())

head(aeneid)
x<-count(aeneid)
x<-as.matrix(x)
# Remove gutenberg header
aeneid<-aeneid[76:x,1:4] # the header finish was determined by visual inspection

#########################################################################
# 2. Term Document Matrix
#########################################################################

library(tm)
library(NLP)
# build a corpus, which is a collection of text documents
# VectorSource specifies that the source is character vectors.
myCorpus <- Corpus(VectorSource(aeneid$text))
## Transform text- lower case ##
myCorpus <- tm_map(myCorpus, tolower)
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
# remove stopwords- I added extras due to the antiquated english translation
myStopwords <- c(stopwords('english'), "thou", "thy","thee", "now", "shall", "thine","thus","yet")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
myCorpus<-tm_map(myCorpus, stripWhitespace)

dictCorpus <- myCorpus
# stem words in a text document with the snowball stemmers,
# which requires packages Snowball, RWeka, rJava, RWekajars
library(SnowballC)
library(RWeka)
library(rJava)
library(RWekajars)
myCorpus <- tm_map(myCorpus, stemDocument)
myCorpus <- tm_map(myCorpus, stemDocument,lazy=TRUE)

# Document term matrix
# Fix from: http://stackoverflow.com/questions/29358571/termdocumentmatrix-raises-error-in-r
myCorpus<- tm_map(myCorpus, PlainTextDocument)
mydtm<-TermDocumentMatrix(myCorpus,control=list(minWordLength=1))

library(wordcloud)
set.seed(4567)
m <- as.matrix(mydtm)
# calculate the frequency of words
v <- sort(rowSums(m), decreasing=TRUE)
myNames <- names(v)
d <- data.frame(word=myNames, freq=v)
# colour + font handling as per: http://www.r-bloggers.com/word-cloud-in-r/
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:4)]

wordcloud(d$word, d$freq, min.freq=3,max.words=200, random.order=FALSE, rot.per=0.15, 
          colors=pal, vfont=c("sans serif","plain"))

