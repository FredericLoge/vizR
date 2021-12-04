## app.R ##
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
    dashboardHeader(title = 'Hello LUKE'),
    dashboardSidebar(
        sliderInput("nb_points", "Number of points:", min = 1, max = 50, value = 30),
        actionBttn(inputId = 'act', label = 'ACTION!', style = 'jelly')
    ),
    dashboardBody(
        h2('Hello Luke. This is your Father.'),
        fluidRow(
            column(6, 
                   textOutput("description")
                   ),
            column(6, 
                   dataTableOutput("tableView")
            )
        ),
        plotOutput("somePlot", width = '500px', height = '500px')
    )
)

server <- function(input, output) {
    
    output$description <- renderText({
        paste0('Nb of points selected: ', input$nb_points)
    })
    
    output$tableView <- renderDataTable({ 
        starwars %>% head(n = 5) %>% select(name, height)
    }, options = list(dom = 't'))
    
    observeEvent(input$act, {
        some_random_number <- sample(x = 1:50, size = 1)
        cat('Chose to print ', some_random_number, 'points.')
        output$somePlot <- renderPlot({
            plot_mass_vs_height(starwars %>% slice_sample(n = some_random_number))
        })
    })
}

plot_mass_vs_height <- function(some_df){
    ggplot(data = some_df) +
        aes(x = height, y = mass, label = name) +
        geom_point() +
        geom_text_repel()
}

shinyApp(ui, server)