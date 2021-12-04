library(shiny)

ui <- fluidPage(
  actionButton("doMagicTrick", "Show something"),
  textOutput("myText")
)

server <- function(input, output) {
  observeEvent(input$doMagicTrick, {
    output$myText <- renderText({ 
      paste0('You are ...', sample(starwars$name, 1), '!')
    })
  })
}

shinyApp(ui, server)

