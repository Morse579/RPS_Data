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
                      #tabset for replay and summary
                      tabsetPanel(
                        tabPanel("Replay",column(8,actionButton("prevbutton","<")),
                                 actionButton("start","START"),
                                 actionButton("end","END"),
                                 column(2,actionButton("nextbutton",">")),
                                 uiOutput('result'),
                                 splitLayout(cellWidths = c("30%", "35%","35%"),
                                             uiOutput('text1'), uiOutput('text2'),uiOutput('text3')
                                 )
                        ), #end of replay tabpanel
                        tabPanel("Summary",
                                 splitLayout(cellWidths = c("50%", "50%"), 
                                             column(8, offset=3,align="center",uiOutput("title1")),
                                             column(8, offset=3,align="center",uiOutput("title2"))
                                 ),
                                 splitLayout(cellWidths = c("50%", "50%"), 
                                             plotOutput("plot1"), plotOutput("plot2")
                                 ),
                                 splitLayout(cellWidths = c("50%", "50%"), 
                                             uiOutput('matrix1'), uiOutput('matrix2')
                                 ),
                                 splitLayout(cellWidths = c("50%", "50%"), 
                                             uiOutput('matrix3'), uiOutput('matrix4')
                                 )
                        )#end of Summary tabpanel 
                      )#end of tabpanel
                    )#end of main panel
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
    
    #replay tabpanel: #round and winner 
    output$result <- renderUI({
      #user input
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
      
      #result display (text part)
      data.display <- paste(br(),br(),"<b>",
                            "<center>","Round: ",numround,"/",nrow(data()),br(),
                            "Winner: ",winner,br()
      )
      HTML(data.display)
    })#end of output$result
    
    #replay tabpanel: rownames
    output$text1 <- renderUI({
      #user input
      if(is.null(data())){return ()}
      numround = input$round
      row.display <- paste("<b>",br(),"<u>","Move","</u>",br(),
                           "<u>","RT","</u>",br(),
                           "<u>","Total pt","</u>",br(),
                           "<u>","Wins","</u>",br(),
                           "<u>","Losses","</u>",br(),
                           "<u>","Ties","</u>")
      HTML(row.display)
    })
    
    #replay tabpanel: PLAYER1
    output$text2 <- renderUI({
      #user input
      if(is.null(data())){return ()}
      numround = input$round
      
      #output for certain round
      sub.data <- subset(data(),round_index==numround)
      
      #Summary percent
      player1_win <- length(which(data()[1:numround,]$player1_outcome=="win"))
      player2_win <- length(which(data()[1:numround,]$player2_outcome=="win"))
      player_tie <- length(which(data()[1:numround,]$player1_outcome=="tie"))
      player1_wpct <- percent(player1_win/as.double(numround))
      player2_wpct <- percent(player2_win/as.double(numround))
      tie_pct <- percent(player_tie/as.double(numround))
      
      #result display (text part)
      p1.display <- paste("<b>","Player1",br(),
                          sub.data$player1_move,br(),
                          round(as.numeric(sub.data$player1_rt)),br(),
                          sub.data$player1_total,br(),
                          player1_win,"/",numround,"(",player1_wpct,")",br(),
                          player2_win,"/",numround,"(",player2_wpct,")",br(),
                          player_tie,"/",numround,"(",tie_pct,")")
      HTML(p1.display)
    })
    
    #replay tabpanel: PLAYER2
    output$text3 <- renderUI({
      #user input
      if(is.null(data())){return ()}
      numround = input$round
      
      #output for certain round
      sub.data <- subset(data(),round_index==numround)
      
      #Summary percent
      player1_win <- length(which(data()[1:numround,]$player1_outcome=="win"))
      player2_win <- length(which(data()[1:numround,]$player2_outcome=="win"))
      player_tie <- length(which(data()[1:numround,]$player1_outcome=="tie"))
      player1_wpct <- percent(player1_win/as.double(numround))
      player2_wpct <- percent(player2_win/as.double(numround))
      tie_pct <- percent(player_tie/as.double(numround))
      
      #result display (text part)
      p2.display <- paste("<b>","Player2",br(),
                          sub.data$player2_move,br(),
                          round(as.numeric(sub.data$player2_rt)),br(),
                          sub.data$player2_total,br(),
                          player2_win,"/",numround,"(",player2_wpct,")",br(),
                          player1_win,"/",numround,"(",player1_wpct,")",br(),
                          player_tie,"/",numround,"(",tie_pct,")")
      HTML(p2.display)
    })
    
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
      bar1 <- ggplot(data = data(), aes(x = player1_outcome)) +
        geom_bar(aes(y = (..count..)/sum(..count..)*100),fill = "lightskyblue3")+
        labs(x = "Player1 Outcome",y = "Percent")+
        geom_hline(linetype="dashed",color="red",yintercept=33)
      bar1.1 <- ggplot(data = data(), aes(x = player1_move)) +
        geom_bar(aes(y = (..count..)/sum(..count..)*100),fill = "lightskyblue3")+
        labs(x = "Player1 Move",y = "Percent")+
        geom_hline(linetype="dashed",color="red",yintercept=33)
      grid.arrange(bar1,bar1.1,ncol=1)
    })
    
    output$plot2 <- renderPlot({
      bar2 <- ggplot(data = data(), aes(x = player2_outcome)) +
        geom_bar(aes(y = (..count..)/sum(..count..)*100))+
        labs(x = "Player2 Outcome",y = "Percent")+
        geom_hline(linetype="dashed",color="red",yintercept=33)
      bar2.1 <- ggplot(data = data(), aes(x = player2_move)) +
        geom_bar(aes(y = (..count..)/sum(..count..)*100))+
        labs(x = "Player2 Move",y = "Percent")+
        geom_hline(linetype="dashed",color="red",yintercept=33)
      grid.arrange(bar2,bar2.1, ncol=1)
    }) # end of bar chart plot
    
    #transition matrix output
    output$matrix1 <- renderTable(rownames = TRUE,{
      if(nrow(data())<=1){}
      else{transition(data()$player1_move)}
    })
    
    output$matrix2 <- renderTable(rownames = TRUE,{
      if(nrow(data())<=1){}
      else{transition(data()$player2_move)}
    })
    
    output$matrix3 <- renderTable(rownames = TRUE,{
      if(nrow(data())<=1){}
      else{transition_o(data()$player1_outcome,data()$player1_move)}
    })
    
    output$matrix4 <- renderTable(rownames = TRUE,{
      if(nrow(data())<=1){}
      else{transition_o(data()$player2_outcome,data()$player2_move)}
    })#end of transition matrix output
    
    #title for summary tabpanel
    output$title1 <- renderUI({
      title <- paste(br(),"PLAYER1",br(),br())
      HTML(title)
    })
    output$title2 <- renderText({      
      title <- paste(br(),"PLAYER2",br(),br())
      HTML(title)
    })#end of title of summary part
    
    #helper method for calculating move transition matrix
    transition <- function(data){
      t_matrix <- matrix(0, nrow = 4, ncol = 4)
      names <- c("no_choice","rock","paper","scissors")
      rownames(t_matrix) <- c('N/A','R','P','S')
      colnames(t_matrix) <- c('N/A','R','P','S')
      current <- which(names == data[1])
      for (i in data[-1]){
        if (i == "rock"){
          t_matrix[current,2] <- t_matrix[current,2]+1
          current <- 2
        }
        else if (i == "paper"){
          t_matrix[current,3] <- t_matrix[current,3]+1
          current <- 3
        }
        else if (i == "scissors"){
          t_matrix[current,4] <- t_matrix[current,4]+1
          current <- 4
        }
        else{
          t_matrix[current,1] <- t_matrix[current,1]+1
          current <- 1
        } 
      }
      result <- t_matrix/rowSums(t_matrix)
      result[is.nan(result)] <- 0  
      result
    }#end of helper method 1
    
    #helper method for calculating outcome transition matrix
    transition_o <- function(list_a,list_b){
      t_matrix <- matrix(0, nrow = 3, ncol = 4)
      colnames(t_matrix) <- c('same','forward','reverse','N/A')
      rownames(t_matrix) <- c('win','loss','tie')
      current <- -1
      for (i in seq(1,length(list_a)-1)){
        if (list_b[i]=="no_choice"){
          next
        }
        if(list_a[i]=="win"){
          current<-direction(list_b[i],list_b[i+1])
          t_matrix[1,current] <- t_matrix[1,current]+1
        }
        else if (list_a[i]=="loss"){
          current<-direction(list_b[i],list_b[i+1])
          t_matrix[2,current] <- t_matrix[2,current]+1    
        }
        else{
          current<-direction(list_b[i],list_b[i+1])
          t_matrix[3,current] <- t_matrix[3,current]+1    
        }
      }
      
      result <- t_matrix/rowSums(t_matrix)
      result[is.nan(result)] <- 0
      result
    }#end of helper method2
    
    #helper method for tracking direction
    direction<-function(o1,o2){
      dir <- c("rock","paper","scissors","rock")
      if(o1==o2){
        return <- 1
      }
      else if(o2=="no_choice"){
        return <- 4
      }
      else{
        if(dir[which(dir==o1)[1]+1]==o2){
          return <- 2
        }
        else{
          return <-3
        }
      }
    } #end of helper method for tracking direction
    
  }#end of server

}  
  shinyApp(ui, server)


