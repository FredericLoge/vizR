library(shiny)
library(tidyverse)

ui <- fluidPage(
  textInput(inputId = "free_input", label = "Free Input"), 
  selectInput(inputId = "film", label = "Film", 
              choices = unique(unlist(starwars$films)))
)

random_code <- function(){
  paste0(sample(letters, 10), collapse = '')
}

server <- function(input, output, session) {
  
  observe({
    print(input$film)
    updateSliderInput(inputId = "free_input",
                      session = session,
                      value = random_code())
  })
  
}

shinyApp(ui, server)