height_vs_mass_graph <- function(filtered_df){
  ggplot(data = filtered_df) +
    aes(x = height, y = mass, label = name, col = gender) +
    geom_point(cex = 2) +
    geom_label_repel() +
    coord_fixed() + 
    labs(title = 'Physical attributes of Star Wars', 
         x = 'Height (cm)', y = 'Mass (kg)') +
    theme(text = element_text(size = 20), legend.position = 'bottom')
}

homeworld_count <- function(filtered_df){

  homeworld_count_cut <- filtered_df %>% 
    filter(is.na(homeworld) == FALSE) %>%
    count(homeworld, gender) %>% 
    group_by(homeworld) %>%
    mutate(nn = sum(n)) %>%
    slice_max(order_by = nn, n = 10)
  
  ggplot(data = homeworld_count_cut) +
    aes(x = reorder(homeworld, -n), y = n, fill = gender) +
    geom_bar(stat = 'identity') +
    labs(x = 'Home World', y = 'People Count', title = 'Home World representation') +
    theme(text = element_text(size = 20),
          axis.text.x = element_text(vjust = 0.5, angle = 45))
  
}


ui = shinyUI(fluidPage(
  
  titlePanel("Star Wars characters"),
  
  selectInput(inputId = "film",label = "Film", 
              choices = unique(unlist(starwars$films))),
  
  mainPanel(
    dataTableOutput("table1"),
    hr(''),
    plotOutput("physic1"),
    hr(''),
    plotOutput("physic2")
  )
)
)

server = shinyServer(function(input, output) {
  
  df <- reactive({ starwars %>%  filter(map_lgl(films, function(x) input$film %in% x)) })
  
  output$table1 <- renderDataTable({
    datatable(head(df()[,c('name', 'birth_year', 'species')], 4),
              options = list(dom = 't'))
  })
  
  output$physic1 <- renderPlot({ height_vs_mass_graph(df()) })

  output$physic2 <- renderPlot({ homeworld_count(df()) })

})

shinyApp(ui = ui, server = server)



