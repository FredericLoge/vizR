library(tidyverse)
library(shiny)
library(ggrepel)
library(shinyWidgets)
library(DT)
data('starwars')

img_path = "https://www.journaldugeek.com/content/uploads/2021/08/template-template-images-jdg-pptx29.jpg"

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    setBackgroundImage(src = img_path),
    
    # Application title
    titlePanel("Star Wars characters"),
    
    # Sidebar with a slider input for number of bins
    selectInput(inputId = "film",label = "Film", 
                choices = unique(unlist(starwars$films))),
    
    # Show a plot of the generated distribution
    mainPanel(
        dataTableOutput("table1"),
        hr(''),
        plotOutput("physic1"),
        hr(''),
        plotOutput("physic2")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
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
            # coord_fixed() + 
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
    
}

shinyApp(ui = ui, server = server)