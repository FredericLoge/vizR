library(tidyverse)
library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(bslib)
library(plotly)
library(ggimage)
library(shinyalert)

data("starwars")

img_luke <- "../../starwars_icon/luke.jpg" # https://images.app.goo.gl/quaR7KeXebQvXkAg6"
img_darth <- "../../starwars_icon/darthVader.jpg" 

starwars <- starwars %>%
  select(-c(films, vehicles, starships)) %>%
  mutate_all(function(x){
    if(class(x)=='character'){
      x[is.na(x)] <- "other"
    }else{
      x[is.na(x)] <- mean(x, na.rm=TRUE)
    }
    x
  })

nice_colname <- function(x){
  x %>% 
    sub('_', ' ', ., fixed=TRUE) %>%
    str_to_title()
}

background_image_fp <- c(
  "Yo"="https://c4.wallpaperflare.com/wallpaper/659/734/329/star-wars-darth-vader-artwork-lightsaber-wallpaper-preview.jpg",
  "Dark Side"="https://r4.wallpaperflare.com/wallpaper/404/780/895/multiple-display-dual-monitors-abstract-digital-art-wallpaper-81c48ada908e25c2e012f324debe503d.jpg",
  "Not a Moon"="https://c4.wallpaperflare.com/wallpaper/1005/822/563/star-wars-death-star-at-at-space-wallpaper-preview.jpg"
)[1]

colnames_variables <- colnames(starwars)
names(colnames_variables) <- sapply(colnames_variables, nice_colname)

colnames_variables_numerical <- c('mass', 'height', 'birth_year')
names(colnames_variables_numerical) <- sapply(colnames_variables_numerical, nice_colname)

# User Interface definition ========

# tab Home ====

tabHome <- tabPanel(
  title="Home", 
  column(width=6, offset=6,
         HTML("<p style='position: relative;float: right;'>\"I find your lack of faith disturbing.<br/><i>Darth Vader</i></p>")
  )
)

# tab Explore ====

tabExplore <- tabPanel(
  title = "Explore",
  fluidRow(
    column(width=6,
           # list of variables included
           # ideally collapsable
           box(width=12, title="Variable List", solidHeader=TRUE,
               verbatimTextOutput(outputId = "variableStr")
           )
    ),
    column(width=6,
           box(width=12,
               title = "Pairplots", solidHeader = FALSE,
               fluidRow(
                 column(width=6, selectInput(inputId='varOnePairplot', label = "Select variable for x:", choices = colnames_variables, selected = "mass")),
                 column(width=6, selectInput(inputId='varTwoPairplot', label = "Select variable for y:", choices = colnames_variables, selected = "height")),
               ),
               plotlyOutput(outputId = 'pairplotOutput')
           )
           # pairplots
    )
  ),
  box(width=12, title="Data Header", solidHeader=TRUE,
      DT::DTOutput(outputId = "dataHeaderOutput")
  )
)

tabExplain <- tabPanel(
  title = "Explain",
  fluidRow(
    column(3, selectInput(inputId='varYModel', label = "Select variable target:", choices = colnames_variables_numerical)),
    column(6, selectInput(inputId='varXModel', label = "Select explicative variables:", choices = colnames_variables, multiple=TRUE)),
    column(3, actionButton(inputId='runModelFit', label = "Run Analysis"))
  ),
  fluidRow(
    column(6, plotOutput(outputId='modelFitGraph')),
    column(6, plotOutput(outputId='explainationGraph'))
  )
)

tabFight <- tabPanel(
    title = 'Fight',
    actionButton(label = "Fight!", inputId="startFight"),
    plotOutput(outputId="fightArena", click="listenClient")
)

ui <- navbarPage(
  title = 'Star Wars Universe',
  setBackgroundImage(
    src = background_image_fp,
    shinydashboard = FALSE
  ),
  tabHome,
  tabExplore,
  tabExplain,
  tabFight,
  theme = bs_theme(bg = "black",
                   fg = "red",
                   primary = "black",
                   base_font = font_google("Montserrat")
  )
)

# Server definition ========
someoneWon <- function(tbl){
  for(v in c("X", "O")){
    for(idx in 1:3){
      cond <- tbl %>% filter(x==idx) %>% summarise(n=sum(label==v)==3) %>% pull(n) 
      if(cond) return(TRUE)
      cond <- tbl %>% filter(y==idx) %>% summarise(n=sum(label==v)==3) %>% pull(n) 
      if(cond) return(TRUE)
    }
    cond <- tbl %>% filter(x==y) %>% summarise(n=sum(label==v)==3) %>% pull(n)
    if(cond) return(TRUE)
    cond <- tbl %>% filter(4-x==y) %>% summarise(n=sum(label==v)==3) %>% pull(n)
    if(cond) return(TRUE)
  }
  return(FALSE)
}

server <- function(input, output, session) {
  
  tmp <- reactiveValues(
    lastModel=NULL,
    formula=NULL,
    lastPredictions=NULL,
    arenaStatus=NULL
  )
  
  observeEvent(input$startFight, {
    tmp$arenaStatus <- expand.grid(x=1:3, y=1:3) %>% as_tibble() %>% mutate(label='.')
    any <- c(sample(x=1:3, size=1), sample(x=1:3, size=1))
    tmp$arenaStatus$label[tmp$arenaStatus$x==any[1] & tmp$arenaStatus$y==any[2]] <- "X"
  })

  observeEvent(input$listenClient, {
    print(input$listenClient)
    tmp$arenaStatus$label[tmp$arenaStatus$x==round(input$listenClient$x) & tmp$arenaStatus$y==round(input$listenClient$y)] <- "O"
    if(someoneWon(tmp$arenaStatus)){
      shinyalert("Nice one!", "It was close... the Force is strong with you", type = "success")
      tmp$arenaStatus <- expand.grid(x=1:3, y=1:3) %>% as_tibble() %>% mutate(label='.')
    }
    # 
    # Sys.sleep(0.25)
    idxs <- which(tmp$arenaStatus$label==".")
    if(length(idxs)==0){
      # do nothing
    }else if(length(idxs)==1){
      tmp$arenaStatus$label[idxs] <- "X"
    }else{
      tmp$arenaStatus$label[sample(x = idxs, size=1)] <- "X"
    }
    if(someoneWon(tmp$arenaStatus)){
      shinyalert("Oops!", "You lost. You were our last hope...", type = "error")
      tmp$arenaStatus <- expand.grid(x=1:3, y=1:3) %>% as_tibble() %>% mutate(label='.')
    }
  })
  
  output$fightArena <- renderPlot(
    tmp$arenaStatus%>%
      mutate(img_src = case_when(
        label == "." ~ "",
        label == "X" ~ img_luke,
        label == "O" ~ img_darth,
        TRUE ~ ""
      )) %>%
    ggplot()+
      aes(x=x, y=y, label=label, image=img_src)+
      geom_text()+
      geom_hline(yintercept = c(1.5, 2.5))+
      geom_vline(xintercept = c(1.5, 2.5))+
      geom_image(data = . %>% filter(img_src != ''), size=0.25, by="height")+
      theme_nothing()+
      xlim(0.5,3.5)+
      ylim(0.5,3.5)
      
      
      # geom_text()
  )
  
  observeEvent(input$runModelFit, {
    tmp$formula <- paste0(input$varYModel, '~', paste0(input$varXModel, collapse="+"))
    tmp$lastModel <- lm(formula = as.formula(tmp$formula), data = starwars)
    tmp$lastPredictions <- predict(tmp$lastModel, starwars)
    print(tmp$formula)
  })
  
  output$explainationGraph <- renderPlot({
    if(is.null(tmp$lastModel)) return(ggplot())
    coeff <- coefficients(tmp$lastModel)
    tibble(
      name=nice_colname(names(coeff)),
      value=as.numeric(coeff)
    ) %>%
      filter(name != '(Intercept)') %>%
      ggplot()+
      aes(y=name, x=value)+
      geom_col()+
      labs(y='Variable', x='Coefficient estimated', title='Coefficients estimated', subtitle=paste0('Intercept=', coefficients(tmp$lastModel)['(Intercept)']))
  })
  
  output$modelFitGraph <- renderPlot({
    if(is.null(tmp$lastModel)) return(ggplot())
    starwars %>%
      mutate(pred = tmp$lastPredictions) %>%
      ggplot()+
      aes(x=mass, y=pred)+
      geom_abline(slope=1, intercept=0, col='red')+
      geom_point()+
      labs(x=input$varYModel, y="Prediction", title="Predicted value versus original:")
  })
  
  output$variableStr <- renderPrint({
    str(starwars, 2)
  })
  
  output$dataHeaderOutput <- DT::renderDataTable({
    DT::datatable(starwars, options = list(
      scrollX = TRUE,
      scrollY = "250px"
    ))
  })
  
  output$pairplotOutput <- renderPlotly({
    c1 <- (starwars %>% pull(input$varOnePairplot) %>% class()) =="character"
    c2 <- (starwars %>% pull(input$varTwoPairplot) %>% class()) =="character"
    if(c1 & c2){
      # bubble chart
      p <- starwars %>%
        ggplot()+
        aes_string(x=input$varOnePairplot, y=input$varTwoPairplot)+
        geom_point()
    }else if(c1 & !c2){
      # boxplot vertical
      p <- starwars %>%
        ggplot()+
        aes_string(x=input$varOnePairplot, y=input$varTwoPairplot)+
        geom_boxplot()
    }else if(!c1 & c2){
      # boxplot horizontal
      p <- starwars %>%
        ggplot()+
        aes_string(y=input$varOnePairplot, x=input$varTwoPairplot)+
        geom_boxplot()+
        coord_flip()
    }else{
      # scatterplot
      p <- starwars %>%
        ggplot()+
        aes_string(x=input$varOnePairplot, y=input$varTwoPairplot)+
        geom_point()
    }
    p <- p+aes(label=name)+labs(
      x=nice_colname(input$varOnePairplot),
      y=nice_colname(input$varTwoPairplot),
      title=paste0(nice_colname(input$varTwoPairplot), ' ~ ', nice_colname(input$varOnePairplot))
    )+theme_dark()
    return(ggplotly(p, tooltip = c("label")))
  })
}

shinyApp(ui = ui, server = server)
