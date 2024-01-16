library(shiny)
library(tidyverse)
library(ggrepel)

source("plot_shiny_app_1.R")
data("starwars")
FILMS_ENUM <- starwars %>% pull(films) %>% unlist() %>% unique()

"
UI with 2 inputs and 2 outputs
Inputs
* choice of films, multiple choices authorized
* choice of a particular character using regexpr
Outputs
* dataTableOutput of the list of characters within all those movies
* plotOutput, using different alpha levels for all characters within 
    those movies and different color whether or not regexpr match
"

ui <- fluidPage(
  h1("Find the character"),
  selectInput(inputId="films", label="In which film?", choices = FILMS_ENUM, multiple=TRUE),
  textInput(inputId="char", label="Start typing a name..."),

  fluidRow(
    column(width=4,
      div(dataTableOutput(outputId = "table_char"), style = "font-size: 75%; width: 75%; margin: auto; text-align: right;")
    ),
    column(width = 8,
           plotOutput(outputId="plot_mass_height")
           )
  )
)

server <- function(input, output, session){

  env <- reactiveValues(df=starwars, num_max_char=-1)
  
  observe({
    env$df <- starwars %>%
          filter(mass < 1000) %>%
          filter(map_lgl(.x=films, .f=function(x){
            all(input$films %in% x)
          })) %>%
          select(name, birth_year) %>%
          arrange(desc(birth_year))
  })
  
  # df <- reactive({
  #   starwars %>%
  #     filter(mass < 1000) %>%
  #     filter(map_lgl(.x=films, .f=function(x){
  #       all(input$films %in% x)
  #     })) %>%
  #     select(name, birth_year) %>%
  #     arrange(desc(birth_year))
  # })

  output$table_char <- renderDataTable({
      env$df %>% 
      # df() %>%
      slice_head(n=5)
  }, options = list(dom = 't'))
  
  output$plot_mass_height <- renderPlot({
    plot_mass_height(input_films = input$films, input_char = input$char)
  })
}


shinyApp(ui=ui, server=server)