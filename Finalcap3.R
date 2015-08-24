# Predict with Backoff for N-grams
library(magrittr)
library(stringr)
library(RSQLite)
library(tm)

backoff <- function(raw, db) {
        
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
        
        return("Oops! Don't know how to predict the next word :(")
}
