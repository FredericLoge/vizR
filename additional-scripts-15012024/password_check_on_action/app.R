library(shiny)
SECRET_WORD = "g,dqzgryhef"

# observeEvent: executer des fonctions lorsqu'un bouton est triggered

# textInput: input de l'utilisateur (son mdp)
# textOutput: message renvoye

ui <- fluidPage(
  textInput(inputId = "pwd", label = "Your password pls:"),
  actionButton(inputId = "check_my_pwd", label="Submit pwd"),
  textInput(inputId = "pwd2", label = "Yo"),
  textOutput(outputId = "msg")
)

server <- function(input, output, session) {
  observeEvent(input$pwd2, {
    print("Triggered")
    output$msg <- renderText({
      if(input$pwd == SECRET_WORD){
        "Your password is correct!!"
      }else{
        "NO!"
      }
    })
  })
}

shinyApp(ui, server)