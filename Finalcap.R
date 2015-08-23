
## Load necessary libraries
suppressPackageStartupMessages(c(
        library(tm),
        library(data.table),
        library(NLP),
        library(RWeka),
        library(slam),
        library(plyr),
        library(knitr),
        library(devtools)))
install.packages('rmarkdown')

##Swiftkey.zip file
Url1 <-"https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
if (file.exists(... = "./Coursera-SwiftKey.zip") == FALSE) {
        download.file(Url1, destfile ="./Coursera-SwiftKey.zip", method="curl")
}

## Unzip the downloaded file
unzip("./Coursera-SwiftKey.zip")

## Check the content
list.files("./data/final")
## Read the English files only
list.files(path = "./final/en_US")

## Merge Corpus
## Using 1000 lines
## Merge all in one
merged <- paste(blogs[1:500], news[1:500], twitter[1:500])
getcorpus <- function(merged) {
        corpus <- VCorpus(VectorSource(merged))
        ## Remove unwanted punctuation, words, numbers and whitespace
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, removeWords, stopwords("english"))
        corpus <- tm_map(corpus, tolower)
        corpus <- tm_map(corpus, removeNumbers)
        corpus <- tm_map(corpus, stripWhitespace)
        corpus
}

library(tm)
library(slam)
library(reshape)
library(magrittr)
library(data.table)
library(RWeka)
set.seed(1)
options(mc.cores=3)

# Function to take a 0.05 sample of the dataset
sampler <- function(dataset) 
{ 
        return(dataset[as.logical(rbinom(length(dataset),1,0.06))]) 
} 

tokenize <- function(merged) {
        gsub("([^ ])([.?!&])", "\\1 \\2 ", v)   %>%
                gsub(pattern=" +", replacement=" ")     %>%
                strsplit(split=" ") %>%
                unlist
}

## Create a frequency data.table
tdmToFreq <- function(tdm) {
        freq <- sort(row_sums(tdm, na.rm=TRUE), decreasing=TRUE)
        word <- names(freq)
        data.table(word=word, freq=freq)
}

## Input n-gram data.table
processGram <- function(dt) {
        dt[, c("pre", "cur"):=list(unlist(strsplit(word, "[ ]+?[a-z]+$")), 
                                   unlist(strsplit(word, "^([a-z]+[ ])+"))[2]), 
           by=word]
}


bulk_insert <- function(sql, key_counts)
{
        dbBegin(db)
        dbGetPreparedQuery(db, sql, bind.data = key_counts)
        dbCommit(db)
}

## 5-gram Tokenizer

UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
PentagramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))


