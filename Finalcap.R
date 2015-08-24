# Utility Functions
library(magrittr)
library(data.table)
library(RWeka)

options(mc.cores=1)

# Samples from data file
samplefile <- function(filename, fraction) {
        system(paste("perl -ne 'print if (rand() < ",
                     fraction, ")'", filename), intern=TRUE)
}

getCorpus <- function(v) {
        # Processes a vector of documents into a tm Corpus
        corpus <- VCorpus(VectorSource(v))
        corpus <- tm_map(corpus, stripWhitespace)  # remove whitespace
        corpus <- tm_map(corpus, content_transformer(tolower))  # lowercase all
        # corpus <- tm_map(corpus, removeWords, stopwords("english"))  # rm stopwords
        corpus <- tm_map(corpus, removePunctuation)
        corpus <- tm_map(corpus, removeNumbers)
        corpus 
}

tokenize <- function(v) {
        # Add spaces before and after punctuation,
        # remove repeat spaces, and split the strings
        gsub("([^ ])([.?!&])", "\\1 \\2 ", v)   %>%
                gsub(pattern=" +", replacement=" ")     %>%
                strsplit(split=" ") %>%
                unlist
}

tdmToFreq <- function(tdm) {
        # Takes tm TermDocumentMatrix and processes into frequency data.table
        freq <- sort(row_sums(tdm, na.rm=TRUE), decreasing=TRUE)
        word <- names(freq)
        data.table(word=word, freq=freq)
}

processGram <- function(dt) {
        # Add to n-gram data.table pre (before word) and cur (word itself)
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

UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
PentagramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))


# Predict, using N-Grams and Stupid Backoff
library(magrittr)
library(stringr)
library(RSQLite)
library(tm)

ngram_backoff <- function(raw, db) {
        # From Brants et al 2007.
        # Find if n-gram has been seen, if not, multiply by alpha and back off
        # to lower gram model. Alpha unnecessary here, independent backoffs.
        
        max = 3  # max n-gram - 1
        
        # process sentence, don't remove stopwords
        sentence <- tolower(raw) %>%
                removePunctuation %>%
                removeNumbers %>%
                stripWhitespace %>%
                str_trim %>%
                strsplit(split=" ") %>%
                unlist
        
        for (i in min(length(sentence), max):1) {
                gram <- paste(tail(sentence, i), collapse=" ")
                sql <- paste("SELECT word, freq FROM NGram WHERE ", 
                             " pre=='", paste(gram), "'",
                             " AND n==", i + 1, " LIMIT 5", sep="")
                res <- dbSendQuery(conn=db, sql)
                predicted <- dbFetch(res, n=-1)
                names(predicted) <- c("Next Possible Word", "Score (Adjusted Freq)")
                print(predicted)
                
                if (nrow(predicted) > 0) return(predicted)
        }
        
        return("Sorry! You've stumped me, I don't know what would come next.")
}
