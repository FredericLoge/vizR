#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    df <- reactive({
        starwars %>% 
            filter(map_lgl(films, function(x) input$film %in% x))
    })
    
    output$table1 <- renderDataTable({
        datatable(head(df()[,c('name', 'birth_year', 'species')], 4),
                  options = list(dom = 't'))
    })
    
    output$physic1 <- renderPlot({
        h = df()
        print(h)
        ggplot(data = h) +
            aes(x = height, y = mass, label = name, col = gender) +
            geom_point(cex = 2) +
            geom_label_repel() +
            coord_fixed() + 
            labs(title = 'Physical attributes of Star Wars', 
                 x = 'Height (cm)', y = 'Mass (kg)') +
            theme(text = element_text(size = 20), legend.position = 'bottom')
    })

    output$physic2 <- renderPlot({
        hh = df() %>% 
            filter(is.na(homeworld) == FALSE) %>%
            count(homeworld, gender) %>% 
            group_by(homeworld) %>%
            mutate(nn = sum(n)) %>%
            slice_max(order_by = nn, n = 10)
        ggplot(data = hh) +
            aes(x = reorder(homeworld, -n), y = n, fill = gender) +
            geom_bar(stat = 'identity') +
            labs(x = 'Home World', y = 'People Count', title = 'Home World representation') +
            theme(text = element_text(size = 20),
                  axis.text.x = element_text(vjust = 0.5, angle = 45))
    })
    
})
