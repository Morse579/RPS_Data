#load packages
library(shiny)
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
                      #action buttons
                      column(8,actionButton("prevbutton","<")),
                      actionButton("end","END"),
                      column(2,actionButton("nextbutton",">")),
                      column(8, align="center",uiOutput('result')
                      )
                    )
                  ),
                  #selector for certain round
                  selectInput("round", "Choose a round:",
                              list('#round'=seq(1,100))
                  )
  )
  
  server <- function(input, output,session) {
    #num_round <- seq(1, 100, by = 1)
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
      
      #result display
      whitespace <- paste(HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'),HTML('&nbsp;'))
      data.display <- paste(br(),br(),"<b>","Round: ",numround,br(),
                            "Player1: ",sub.data$player1_move,whitespace,
                            "Player2: ",sub.data$player2_move,br(),
                            "Winner: ",winner,br(),
                            "Player1_points: ",sub.data$player1_points,whitespace,
                            "Player2_points: ",sub.data$player2_points,sep = '\n')

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
    
  }
  
  shinyApp(ui, server)
}

