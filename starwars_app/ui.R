#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(ggrepel)
library(DT)
data('starwars')

# Define UI for application that draws a histogram
shinyUI(fluidPage(

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
)
