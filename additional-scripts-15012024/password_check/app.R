library(shiny)
library(tidyverse)

# ui: user interface
# code R genere le code HTML de l'app
# <!> important: les tags HTMLs sont definis dans l'UI
ui <- fluidPage(
  numericInput(inputId = "password", 
               label = "Your password please ? (numerics only)", 
               value = 10),
  plotOutput(outputId = "plot_password")
)

server <- function(input, output, session){
  observe({
    print(input$password)
  })
  output$plot_password <- renderPlot({
    tibble(x=0, y=0, label=input$password) %>%
    ggplot()+
      aes(x=x, y=y, label=label)+
      geom_text()
  })
}

shinyApp(ui = ui, server = server)