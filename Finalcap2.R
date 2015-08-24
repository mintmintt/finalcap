# Load necessary libraries
library(tm)
library(data.table)
library(NLP)
library(RWeka)
library(slam)
library(plyr)
library(ggplot2)
library(knitr)
library(devtools)

##Swiftkey.zip file
Url1 <-"https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
if (file.exists(... = "./Coursera-SwiftKey.zip") == FALSE) {
        download.file(Url1, destfile ="./Coursera-SwiftKey.zip", method="curl")
}

## Unzip the downloaded file
unzip("./Coursera-SwiftKey.zip")

## Check the content
list.files("./data/final")

There are four files, German, English, Finnish and Russion. For the purposes of this assignment, we only refer to the English files and do not include the German, Finnish and Russian.

## Read the English files only
list.files(path = "./final/en_US")

These three English files; "en_US.blogs.txt"   "en_US.news.txt"    "en_US.twitter.txt", will be the basis to creating the analysis and models.

2. Create a basic report of summary statistics about the data sets.

## Expore the length of blogs, news and twitter
blogs <- readLines(file("./final/en_US/en_US.blogs.txt", encoding = "UTF-8"))
news <- readLines(file("./final/en_US/en_US.news.txt", encoding = "UTF-8"))
twitter <- readLines(file("./final/en_US/en_US.twitter.txt", encoding = "UTF-8"))

print(paste("blog characters = ", length(blogs),
            "news characters = ", length(news),
            "twitter characters = ", length(twitter)
))

Blogs have character of 899288 elements and is 248.5MB, News is 1010242 elements and 249.6MB and Twitter has 2360148 elements and 301.4MB.
List the word count through wc command

system("wc -w ./final/en_US/en_US.blogs.txt", intern=TRUE)
system("wc -w ./final/en_US/en_US.news.txt", intern=TRUE)
system("wc -w ./final/en_US/en_US.twitter.txt", intern=TRUE)

Blogs has 37334690 words, News has 34372720 words and Twitter has 30374206 words.
Due to the very large size of the data, we will only review 500 lines for greater efficiency. 
Merge Corpus

## Using 1000 lines
## Merge all in one
merged <- paste(blogs[1:500], news[1:500], twitter[1:500])
corpus <- VCorpus(VectorSource(merged))
## Remove unwanted punctuation, words, numbers and whitespace
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)

Look at unigram tokenizer and then build an n-gram data frame for word combinations.
3. Report any interesting findings that you amassed so far.

corpus.df <-data.frame(text=unlist(sapply(corpus, `[`, "content")), stringsAsFactors=F)
unigramtoken <-NGramTokenizer(corpus.df, Weka_control(min = 1, max = 1, delimiters = " \\r\\n\\t.,;:\"()?!"))
unigramtoken.1 <- data.frame(table(unigramtoken))
unigramtoken.2 <- unigramtoken.1[order(unigramtoken.1$Freq,decreasing = TRUE),]
unigramtoken.3 <- unigramtoken.2[1:25,]
colnames(unigramtoken.3) <- c("Word", "Frequency")
