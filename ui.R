library(shiny)

## Define UI for application that draws a histogram
shinyUI(fluidPage(
        
        ## Application title
        titlePanel("Predictive Word Algorithm"),
        h4("Final Project", style="color:green"),
        hr(),
        
        fluidRow(width=2,
                 p("Type a sequence of words, up to 3 words. You can then select the 'NEXT' button for the prediction"),
                 p("Using backoff model",
                   a(href="http://www.cs.cornell.edu/courses/.../smoothing+backoff.pdf", 
                     "Smoothing and Backoff"), "for unknown ngrams.")),
        hr(),
        
        ## Sidebar with a slider input for the number of bins
        sidebarLayout(
                sidebarPanel(
                        textInput("text", label = h3("Input"), value = "north south east"),
                        helpText("Enter sequence of words, select the 'NEXT' button, a predicted word will appear."),
                        submitButton("NEXT"),
                        hr()
                ),
                
                # Show a plot of the generated distribution
                mainPanel(
                        br(),
                        h2(textOutput("sentence"), align="center"),
                        h1(textOutput("predicted"), align="center", style="color:blue"),
                        hr()
                )
        )
        ))
