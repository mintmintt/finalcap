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
        
        return("I am not sure what comes next :(")
}


library(shiny)
library(RSQLite)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        # input$text and input$action are available
        # output$sentence and output$predicted should be made available
        db <- dbConnect(SQLite(), dbname="train.db")
        dbout <- reactive({backoff(input$text, db)})
        
        output$sentence <- renderText({input$text})
        output$predicted <- renderText({
                out <- dbout()
                if (out[[1]] == "I am not sure what comes next :(") {
                        return(out)
                } else {
                        return(unlist(out)[1])
                }})
        output$alts <- renderTable({dbout()})
})
