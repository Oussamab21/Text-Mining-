###################################
# Data Mining for Big Data
# Practical Session 1
# MLDM M2
#
# Subject: Text preprocessing
# and text mining
#
#
# Oussama Bouldjedri 
# Nov 30, 2017
#
###################################


#install.packages('NLP')
#install.packages('tm')
#install.packages('XML')
#install.packages('SnowballC')
#install.packages('ggplot2')
#install.packages('wordcloud')
#install.packages('cluster')
library(NLP)
library(tm)
library(XML)
library(SnowballC)
library(slam)
library(ggplot2)
library(wordcloud)
library(cluster)

#Import data
getReaders()

#Create corpus from directory
doc1 <- Corpus(DirSource("D:/GD/MLDM/Big Data/Text Mining/Data"))
doc1


#Load Ovide from tm package
ovid <- system.file("texts", "txt", package="tm")
ovidCorpus <- Corpus(DirSource(ovid), readerControl = list(reader=readPlain))
inspect(ovidCorpus)

#Create corpus from a vector
docs <- c("This is a text","This is another one")
corpus1 <- Corpus(VectorSource(docs))

#Create corpus from web docs
reut21578 <- system.file("texts", "crude", package = "tm")
reuters <- Corpus(DirSource(reut21578),readerControl = list(reader=readReut21578XML))

#Backup corpus in personal directory
writeCorpus(ovidCorpus,path="D:/GD/MLDM/Big Data/Text Mining/Data/corpusTemp/ovid")
writeCorpus(ovidCorpus,path="D:/GD/MLDM/Big Data/Text Mining/Data/corpusTemp/reuters")
writeCorpus(ovidCorpus,path="D:/GD/MLDM/Big Data/Text Mining/Data/corpusTemp/corpus1")

#Inspect the corpus
inspect(doc1)
class(doc1)
inspect(doc1[[1]])
class(doc1[[1]])
summary(doc1)

writeLines(as.character(doc1[[2]]))

#See full corpus
inspect(ovidCorpus)
#What makes up the corpus
summary(ovidCorpus)
#Output one section
writeLines(as.character(ovidCorpus[[1]]))
writeLines(as.character(reuters[[1]]))
writeLines(as.character(corpus1[[1]]))

#Preprocessing the data
#Focus on preprocessing reuters beause it is the most complicated
#Since it is from XML files

#Transform XML to text
reuters2 <- tm_map(reuters,PlainTextDocument)
writeLines(as.character(reuters2[[1]]))

#Remove whitespace
reuters3 <- tm_map(reuters2,stripWhitespace)
writeLines(as.character(reuters3[[1]]))

#Eliminate stop words
reuters4 <- tm_map(reuters3,removeWords,stopwords("english"))
writeLines(as.character(reuters4[[1]]))

#Inspect stop words
length(stopwords("english"))
stopwords("english")

#Create custom dictionary of additional stop words to remove
stop_dict <- c("crude", "oil")
doc1RW    <- tm_map(reuters3,removeWords,stop_dict)
writeLines(as.character(doc1RW[[1]]))
doc1RW2   <- tm_map(reuters3,removeWords,c("prices"))
writeLines(as.character(doc1RW2[[1]]))

#Stem to roots words
reuters5 <- tm_map(reuters4,stemDocument)
writeLines(as.character(reuters5[[1]]))

#Convert all to lower case
reuters6 <- tm_map(reuters5, content_transformer(tolower))
writeLines(as.character(reuters6[[1]]))

#remove numbers
reuters7 <- tm_map(reuters6, removeNumbers)
writeLines(as.character(reuters7[[1]]))

#remove punctuation
reuters8 <- tm_map(reuters7, removePunctuation)
writeLines(as.character(reuters8[[1]]))

#Inspect term frequency
dtm <- DocumentTermMatrix(reuters8)
inspect(dtm)


#Create dtm matrix from reuters2 using using only dict terms
dtm2 <- DocumentTermMatrix(reuters2,list(dictionary=c("the","said","oil")))
inspect(dtm2)

#Eliminate sparce terms
inspect(removeSparseTerms(dtm,0.05))

dtm <- removeSparseTerms(dtm,0.4)
dtm

#Transform from term doc matrix to plain text matrix
dtm_mat <- as.matrix(dtm)

#write tabe into file
write.table(dtm_mat, file="D:/GD/MLDM/Big Data/Text Mining/Data/dtm_mat.data")

#Load file
dtm_mat2 <- as.matrix(read.table(file="D:/GD/MLDM/Big Data/Text Mining/Data/dtm_mat.data"))

#Write to csv
write.csv(dtm_mat, file="D:/GD/MLDM/Big Data/Text Mining/Data/dtm_mat.data")

#Text Data Mining
freq <- colSums(dtm_mat)

#index size
length(freq)

#list most frequent terms by ordering by frequenc
ord <- order(freq)

#Least frequent terms
freq[head(ord)]

#most frequent terms
freq[tail(ord)]

#Plotting word frequencies
freq <- sort(colSums(dtm_mat), decreasing = TRUE)
head(freq,14)

wf <- data.frame(word=names(freq),freq=freq)
head(wf)

barplot(freq)

#make word cloud
wordcloud(names(freq),freq,max.words=15)

#Frequent itemsets and associations
findFreqTerms(dtm,5)

#Find words having a correlation rate higher of equal to .5
findAssocs(dtm,"oil",0.5)

#Clustering of terms
#heirachial clustering
d   <- dist(t(dtm), method="euclidean")
fit <- hclust(d=d,method="ward.D2")
fit
plot(fit, hang=-1)
