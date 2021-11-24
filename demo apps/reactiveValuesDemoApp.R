library(shiny)

ui <- fluidPage(
    # input field
    textInput("user_text", label = "Enter some text:", placeholder = "Please enter some text."),
    actionButton("submit", label = "Submit"),
    
    # display text output
    textOutput("text")
)

server <- function(input, output) {
  
  # observe event for updating the reactiveValues
  observeEvent(input$submit,
               {
                 text_reactive$text <- input$user_text
               })
  
  # reactiveValues
  text_reactive <- reactiveValues(
    text = "No text has been submitted yet."
  )
  
  # text output
  output$text <- renderText({
    text_reactive$text
  })
}

shinyApp(ui = ui, server = server)