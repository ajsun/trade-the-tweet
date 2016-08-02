###########################################################################
# Script that creates a tdm given a directory of raw_text files named as date
# Example: 
# filename: 2015-01-01.txt
# content: hello stock aapl ..... bullish hello
# 
# Andrew Sun
#
########################################################################### 
library(tm)
directory = 'raw_text/'
texts = DirSource(directory)

corpus = Corpus(texts)
load('dict.Rdata')
tdm = TermDocumentMatrix(corpus, control=list(dictionary=dict))
Y <- as.matrix(tdm)