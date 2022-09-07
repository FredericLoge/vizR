library(tidyverse)
library(shiny)
library(ggrepel)
library(shinyWidgets)
library(DT)
library(shinydashboard)
data('starwars')

img_path = "https://www.journaldugeek.com/content/uploads/2021/08/template-template-images-jdg-pptx29.jpg"
img_path = "https://resize-europe1.lanmedia.fr/r/622,311,forcex,center-middle/img/var/europe1/storage/images/europe1/sciences/decouverte-dun-systeme-a-deux-etoiles-comme-dans-star-wars-2989610/33172193-1-fre-FR/Decouverte-d-un-systeme-a-deux-etoiles-comme-dans-Star-Wars.gif"
img_path = "https://www.francetvinfo.fr/pictures/F6_x2F_s_pg_jouL-dnMvTjPVQ0/752x423/2022/05/27/phppzmPbM.jpg"
img_path_2 = "https://img.freepik.com/free-photo/abstract-grunge-decorative-relief-navy-blue-stucco-wall-texture-wide-angle-rough-colored-background_1258-28311.jpg?w=2000"

# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    dashboardHeader(title = "Some Star Wars app", disable = FALSE),
    dashboardSidebar(collapsed = FALSE, disable = FALSE,
                     sidebarMenu(
                         menuItem("NicestPoster", tabName = "nicePoster", icon = icon("th")),
                         menuItem("SomeViz", icon = icon("th"), tabName = "someViz",
                                  badgeLabel = "click", badgeColor = "green")
                     )),
    dashboardBody(
        tabItems(
            tabItem(tabName = "nicePoster",    
                    setBackgroundImage(src = img_path, shinydashboard = TRUE)
            ),
            tabItem(tabName = "someViz",
                    # h2("Dashboard tab content"),
                    fluidRow(width=12,
                             box(width=3, title = "Film", selectInput(inputId = "film", label="", choices = unique(unlist(starwars$films))))
                    ),
                    hr(), br(),
                    fluidRow(width=12,
                        column(width = 3, 
                               box(width = 10, 
                                   title = 'Featuring:', 
                                   status = 'primary', 
                                   solidHeader = TRUE,
                                   dataTableOutput("table1"))),
                        column(width = 4, 
                               box(width = 10,
                                   title = 'Height vs Mass',
                                   status = 'warning',
                                   solidHeader = TRUE,
                                   plotOutput("physic1")
                               )
                        ),
                        column(width = 4, 
                               box(width = 10,
                                   title = 'Planets',
                                   status = 'warning',
                                   solidHeader = TRUE,
                                   plotOutput("physic2")
                               )
                               )
                    )
            )
        )
    )
    )


    
            
        
        


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    df <- reactive({
        starwars %>% 
            filter(map_lgl(films, function(x) input$film %in% x))
    })
    
    output$table1 <- renderDataTable({
        datatable(head(df()[,c('name', 'birth_year', 'species')], 15), options = list(dom = 't'))
    })
    
    output$physic1 <- renderPlot({
        h = df()
        print(h)
        ggplot(data = h) +
            aes(x = height, y = mass, label = name, col = gender) +
            geom_point(cex = 2) +
            geom_label_repel() +
            coord_equal() + 
            labs(title = 'Physical attributes of Star Wars', x = 'Height (cm)', y = 'Mass (kg)') +
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
            theme(text = element_text(size = 20), axis.text.x = element_text(vjust = 0.5, angle = 45), legend.position = 'bottom')
    })
    
}

shinyApp(ui = ui, server = server)