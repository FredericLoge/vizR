library(shiny)
library(tidyverse)

ui <- fluidPage(
  textInput(inputId = "free_input", label = "Free Input"), 
  selectInput(inputId = "film",label = "Film", choices = unique(unlist(starwars$films))),
  textOutput('output_text')
)

server <- function(input, output) {
  
  output$output_text <- renderText({
    paste0(isolate(input$film), " - ", input$free_input)
  })
  
}

shinyApp(ui, server)