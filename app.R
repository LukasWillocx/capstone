#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
source('Functions.R')
load('ngram_data.RData')
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = shinytheme("united"),
  
  # Application title
  titlePanel("Word prediction ngram model"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      textInput('phrase', 'Type an incomplete phrase', value = "", width = NULL, placeholder = NULL),
      submitButton(text='Submit phrase')
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("word")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$word <- renderText({
      predictWord(input$phrase,
                  uni = unigram_DF,
                  bi = bigram_DF,
                  tri = trigram_DF,
                  quad = quadgram_DF)
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
