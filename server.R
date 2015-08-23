
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
