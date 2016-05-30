#####################################################################
# Title: Observing social networks in the aeneid
# Author: Steph de Silva.
# Email: steph@rex-analytics.com
# Date created: 31/05/16
# Date last altered: 31/05/16
# Attributions and acknowledgment of derivation:
# This script is due to code, information, tips and advice given in:
# (1) ropenscilabs/gutenbergr from https://github.com/ropenscilabs/gutenbergr
#     Accessed: 20/05/16
# (2)  http://www.rdatamining.com/examples/text-mining
#     Accessed: 21/05/16
# (3) this program was particularly important and much of the code is derived from here
# http://www.rdatamining.com/examples/social-network-analysis accessed 21/05/16
# Along with helpful code fixes and tweaks from:
# (1)  http://stackoverflow.com/questions/25069798/r-tm-in-mclapplycontentx-fun-all-scheduled-cores-encountered-errors
# (2)  http://www.inside-r.org/packages/cran/tm/docs/as.TermDocumentMatrix
# (3)  https://stat.ethz.ch/pipermail/r-help/2012-May/313013.html
# (4)  http://stackoverflow.com/questions/29358571/termdocumentmatrix-raises-error-in-r
# All accessed 21/05/16
# Also: 
# (5) http://www.chlt.org/StatisticalMethods/preparing-literary-data.html
#   accessed 20/05/16
# code help: http://www.inside-r.org/packages/cran/tm/docs/as.TermDocumentMatrix
# https://stat.ethz.ch/pipermail/r-help/2012-May/313013.html
# Size of nodes look here: http://www.shizukalab.com/toolkits/sna/plotting-networks-pt-2
# useful information: http://www.r-bloggers.com/going-viral-with-rs-igraph-package/
# colours in R: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

# Purpose: This script is intended to download the works by Virgil, the Aeneid 
# and then perform a social network analysis.
#########################################################################
# Data Used: Project Gutenberg copy of the Aeneid. 
# Source: https://www.gutenberg.org/ebooks/22456
# Specifically: http://www.gutenberg.org/cache/epub/22456/pg22456.txt
# Translation by:  Mackail, J. W. (John William), 1859-1945
# Date Accessed: 25/05/16
# Gutenberg Number: 22456
#########################################################################
# Script Outline:
# 1. Load Libraries, load data, clean data
# 2. Term document matrix
# 3. Create Social Network Analysis
#########################################################################
# 1. Load libraries, load data
#########################################################################

rm(list=ls(all=TRUE))
library(dplyr)
library(gutenbergr)

# Download from Project Gutenberg
aeneid <- gutenberg_download(22456 , meta_fields = "title") %>%
mutate(linenumber = row_number())
# Remove gutenberg header
head(aeneid)
x<-count(aeneid)
x<-as.matrix(x)
aeneid<-aeneid[76:x,1:4] # the header finish was determined by visual inspection

#########################################################################
# 2. Term Document Matrix
#########################################################################

library(tm)
library(NLP)

myCorpus <- Corpus(VectorSource(aeneid$text))
myCorpus <- tm_map(myCorpus,
                       content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
                       mc.cores=1)
myCorpus <- tm_map(myCorpus, tolower)
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, removeNumbers)
myStopwords <- c(stopwords('english'), "thou", "thy","thee", "now", "shall", "thine","thus","yet", "hath")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
myCorpus<-tm_map(myCorpus, stripWhitespace)
dictCorpus <- myCorpus
    
 myCorpus <- tm_map(myCorpus, stemDocument,lazy=TRUE)
    
# Fix from: http://stackoverflow.com/questions/29358571/termdocumentmatrix-raises-error-in-r
myCorpus<- tm_map(myCorpus, PlainTextDocument)
#character list from http://www.sparknotes.com/lit/aeneid/characters.html
# the purpose here is that we don't care about nouns, we care abuot characters
# some characters were removed from the list in earlier iterations as they were superfluous in the analysis
# that followed. Only characters connected to the social network were kept.
aeneas_dictionary<-c("aeneas", "turnus", "anchises", "dido", "ascanius",  "latinus", "lavinia",  "evander", "pallas", "juturna", "achates", "juno", "venus", "jupiter", "neptune",  "cupid", "allecto",  "saturn", "minerva", "apollo",  "achilles", "hector", "andromache", "paris", "priam", "pyrrhus")
ctrl<-list(minWordLength=1, dictionary=aeneas_dictionary)
mydtm<-TermDocumentMatrix(myCorpus,control=ctrl)
    
    
termDocMatrix <- is.matrix(mydtm)
termDocMatrix <- as.matrix(mydtm)
is.matrix(termDocMatrix)
dim(termDocMatrix)
    
# change it to a Boolean matrix
termDocMatrix[termDocMatrix>=1] <- 1
# transform into a term-term adjacency matrix
termMatrix <- termDocMatrix %*% t(termDocMatrix)

#########################################################################
# 3. social network analysis
#########################################################################


library(igraph)
# build a graph from the above matrix
g <- graph.adjacency(termMatrix, weighted=T, mode = "undirected")
g <- simplify(g)
# set labels and degrees of vertices
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
set.seed(1234)
vertex.label=c(vertex.label.dist=1,
vertex.label.color="black")
V(g)$label.cex <- 2.2 * V(g)$degree / max(V(g)$degree)+ .2
egam <- (log(E(g)$weight)+.4) / max(log(E(g)$weight)+.4)
    
layout1 <- layout.fruchterman.reingold(g)
V(g)$color <- "lightblue"
plot(g, layout=layout1, 
         vertex.color="lightblue",
         vertex.frame.color= "black",
         vertex.label.color = "black",
         vertex.label.family = "sans",
         edge.width=2,  
         edge.color="black")
    
 
