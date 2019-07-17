#load packages
library(shiny)
library(scales)
library(plotrix)
library(tidyverse)
library(gridExtra)
#library(shinydashboard)
#library(shinyjs)
#library(DT)

#shiny part for diplaying data
if (interactive()) {

  ui <- fluidPage(titlePanel("RPS_data"),
                  sidebarLayout(
                    sidebarPanel(
                      #file upload
                      fileInput("file1", "Import CSV File",
                                accept = c(
                                  "text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv"),
                                
                      )
                    ),
                    mainPanel(
                      #tabset for plot, summary and table
                      tabsetPanel(
                        tabPanel("Replay",column(8,actionButton("prevbutton","<")),
                                 actionButton("start","START"),
                                 actionButton("end","END"),
                                 column(2,actionButton("nextbutton",">")),
                                 column(8, align="center",uiOutput('result'))), 
                        tabPanel("Summary", 
                                 splitLayout(cellWidths = c("50%", "50%"), 
                                             plotOutput("plot1"), plotOutput("plot2"),
                                             plotOutput("plot3")
                                             )
                                 ) 
                      )
                      
                      #action buttons
                    )
                  ),
                  #selector for certain round
                  selectInput("round", "Choose a round:",
                              list('#round'=seq(1,100))
                  )
  )
  
  server <- function(input, output,session) {
    
    #function for uploading the file
    data <- reactive({
      #display csv file
      inFile <- input$file1
      if (is.null(inFile))
        return(NULL)
      data <- read.csv(inFile$datapath, header = TRUE)
      updateSelectInput(session, "round",
                        choices = as.list(seq(1, nrow(data), by = 1)),
                        selected = 1)
      data
    })
    
    #replay tabpanel display
    output$result <- renderUI({
      #display csv file
      if(is.null(data())){return ()}
      numround = input$round
    
      #output for certain round
      sub.data <- subset(data(),round_index==numround)
      
      #Winner
      if(sub.data$player1_outcome=="win"){
        winner <- "Player1"
      }
      else if(sub.data$player1_outcome=="tie"){
        winner <- "TIE"
      }
      else {
        winner <- "Player2"
      }
      
      #Summary percent
      player1_win <- length(which(data()[1:numround,]$player1_outcome=="win"))
      player2_win <- length(which(data()[1:numround,]$player2_outcome=="win"))
      player_tie <- length(which(data()[1:numround,]$player1_outcome=="tie"))
      #player1_wpct <- percent(player1_win/as.double(numround))
      #player2_wpct <- percent(player2_win/as.double(numround))
      #tie_pct <- percent(player_tie/as.double(numround))
      
      #result display (text part)
      whitespace2 <- paste(HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'))
      whitespace3 <- paste(HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),
                           HTML('&nbsp;'),HTML('&nbsp;'))
      data.display <- paste(br(),br(),"<b>","Round: ",numround,"/",nrow(data()),br(),
                            "Winner: ",winner,br(),
                            "Player1",whitespace3,"Player2",br(),
                            sub.data$player1_move,whitespace3,sub.data$player2_move,br(),
                            round(as.numeric(sub.data$player1_rt)),whitespace3,
                            round(as.numeric(sub.data$player2_rt)),br(),
                            "Total points: ",sub.data$player1_total,whitespace2,
                            "Total points: ",sub.data$player2_total,br(),
                            "Wins: ",player1_win,"/",numround,whitespace3,
                            "Wins: ",player2_win,"/",numround,br(),
                            "Losses: ",player2_win,"/",numround,whitespace3,
                            "Losses: ",player1_win,"/",numround,br(),
                            "Ties: ",player_tie,"/",numround,whitespace3,
                            "Ties: ",player_tie,"/",numround)
      

      HTML(data.display)
      
      
    })#end of output$result
    
    #set action buttons
    observeEvent(input$prevbutton, {
      total.row <- nrow(data())
      num_round <- seq(1, total.row, by = 1)
      current <- which(num_round == input$round)
      if(current > 1){
        updateSelectInput(session, "round",
                          choices = as.list(num_round),
                          selected = num_round[current - 1])
      }
    })#end of previous button 
    
    observeEvent(input$nextbutton, {
      total.row <- nrow(data())
      num_round <- seq(1, total.row, by = 1)
      current <- which(num_round == input$round)
      if(current < length(num_round)){
        updateSelectInput(session, "round",
                          choices = as.list(num_round),
                          selected = num_round[current + 1])
      }
    })#end of next button
    
    observeEvent(input$end, {
      total.row <- nrow(data())
      num_round <- seq(1, total.row, by = 1)
      updateSelectInput(session, "round",
                          choices = as.list(num_round),
                          selected = num_round[total.row])
    })#end of ending button
    
    observeEvent(input$start, {
      total.row <- nrow(data())
      num_round <- seq(1, total.row, by = 1)
      updateSelectInput(session, "round",
                        choices = as.list(num_round),
                        selected = num_round[1])
    })#end of starting button
    
    #plot display
    output$plot1 <- renderPlot({
      bar1 <- ggplot(data = data(), aes(x = player1_move)) +
        geom_bar(aes(y = (..count..)/sum(..count..)*100),fill = "lightskyblue3")+
        labs(x = "Player1 Move",y = "Percent")
      bar1.1 <- ggplot(data = data(), aes(x = player1_outcome)) +
        geom_bar(aes(y = (..count..)/sum(..count..)*100),fill = "lightskyblue3")+
        labs(x = "Player1 Outcome",y = "Percent")
      grid.arrange(bar1,bar1.1, ncol=1)
    })
    
    output$plot2 <- renderPlot({
      bar2 <- ggplot(data = data(), aes(x = player2_move)) +
        geom_bar(aes(y = (..count..)/sum(..count..)*100))+
        labs(x = "Player2 Move",y = "Percent")
      bar2.1 <- ggplot(data = data(), aes(x = player2_outcome)) +
        geom_bar(aes(y = (..count..)/sum(..count..)*100))+
        labs(x = "Player1 Outcome",y = "Percent")
      grid.arrange(bar2,bar2.1, ncol=1)
    }) # end of bar chart plot
    
  }
  
  shinyApp(ui, server)
}

