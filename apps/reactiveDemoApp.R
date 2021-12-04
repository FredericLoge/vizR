library(shiny)
library(tidyverse)
library(DT)

ui <- fluidPage(
  selectInput(inputId = "film",label = "Film", choices = unique(unlist(starwars$films))),
  textOutput('genericInfo'),
  br(),
  h5('First 4 characters : '),
  dataTableOutput("myTable")
)

server <- function(input, output, session) {
  
  df <- reactive({
    starwars %>% 
      filter(map_lgl(films, function(x) input$film %in% x))
  })
  
  output$genericInfo <- renderText({
    paste0('Nb of characters : ', nrow(df()))  
  })
  
  output$myTable <- renderDataTable({
    datatable(head(df()[,c('name', 'birth_year', 'species')], 4),
              options = list(dom = 't'))
  })
  
  
}

shinyApp(ui, server)