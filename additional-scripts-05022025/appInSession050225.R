library(shiny)
library(tidyverse)

ui <- fluidPage(
  fluidRow(
    column(width=2,
           checkboxInput(inputId = "include_droids", label = "Should we include Droids ?"),
           sliderInput(inputId = "max_birth_year", label="Max Birth Year", min=min(starwars$birth_year, na.rm = TRUE), max=max(starwars$birth_year, na.rm = TRUE), value=50),
           selectizeInput(inputId = "films_to_exclude", label="Films to exclude", choices=unique(unlist(starwars$films)), multiple=TRUE)
    ),
    column(width=5,
           textOutput("information"),
           plotOutput("homeworld_vs_species_plot")
    ),
    column(
      width=5,
      dataTableOutput("datatable")
    )
  )
)

server <- function(input, output) {
  
  # define reactive Values
  df <- reactiveValues(
    filtered_df = starwars,
    index = 0,
    df_intial = starwars
  )
  
  observe({ print(input$max_birth_year) })
  
  # filter tibble based on inputs
  observeEvent(c(input$films_to_exclude, input$include_droids, input$max_birth_year), {
    df$filtered_df <- starwars %>% 
      filter(
        map_lgl(films, function(x) all(!input$films_to_exclude %in% x)),
        is.na(birth_year)|(birth_year<input$max_birth_year)
      )
    print(paste0("Nb lignes 1: ", nrow(df$filtered_df)))
    if(!input$include_droids){
      df$filtered_df <- df$filtered_df %>% filter(species!="Droid")
    }
    print(nrow(df$filtered_df))
  })
  
  # plot 
  output$homeworld_vs_species_plot <- renderPlot({
    tmp <- df$filtered_df %>% 
      count(species, homeworld)
    tmp %>% 
      mutate(
        shared_species = (species %in% (tmp %>% count(species) %>% filter(n>1) %>% pull(species))),
        shared_homeworld = (homeworld %in% (tmp %>% count(homeworld) %>% filter(n>1) %>% pull(homeworld))),
        shared = shared_species | shared_homeworld        
      ) %>%
      ggplot()+
      aes(x=species, y=fct_inorder(homeworld), size=n, color=shared)+
      geom_point()+
      theme(
        axis.text.y = element_text(size = 7),
        axis.text.x = element_text(size = 7, angle=45)
      )
  })
  
  output$datatable <- renderDataTable({
    df$filtered_df %>%
      select(name, height, mass, homeworld, species)
  })
  
  output$information <- renderText({
    n = df$filtered_df %>% nrow()
    paste0("Number of rows selected: ", n, '/', nrow(starwars))
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
