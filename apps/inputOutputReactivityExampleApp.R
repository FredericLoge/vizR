#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(ggrepel)
data("starwars")

# Define UI for application that draws a histogram
ui <- fluidPage(
    h2('Hello Luke. This is your Father'),
    actionButton('act', 'ACTION!'),
    sliderInput("nb_points", "Number of points:", min = 1, max = 50, value = 30),
    textOutput("description"),
    dataTableOutput("tableView")
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    df <- eventReactive(input$act, { 
        starwars %>% 
            slice_head(n = input$nb_points) %>%
            select(name, homeworld, birth_year, species)})
    output$description <- renderText({ some_text(df()) })
    output$tableView <- renderDataTable({ df() }, options = list(dom = 't'))
}

some_text <- function(some_df){ 
    paste0('Last character :: ', some_df %>% slice_tail(n=1) %>% pull(name)) 
}

# plot_mass_vs_height <- function(some_df){
#     ggplot(data = some_df) +
#         aes(x = height, y = mass, label = name) +
#         geom_point() +
#         geom_text_repel()
# }
# Run the application 
shinyApp(ui = ui, server = server)
