#load packages
library(shiny)
library(shinydashboard)
library(DT)

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
                      column(8,actionButton("prev","<")),
                      actionButton("endbutton","END"),
                      column(2,actionButton("next",">")),
                      column(8, align="center",uiOutput('result')
                      )
                    )
                  ),
                  #selector for certain round
                  selectInput("round", "Choose a round:",
                              list('#round'=seq(1,100))
                  )
  )
  
  server <- function(input, output) {
    output$result <- renderUI({
      #display csv file
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      numround = input$round
      
      #observeEvent(input$prev, {
      #  numround=character(as.integer(numround)-1)}, once = TRUE)
      
      #output for selector #round
      impordata <- read.csv(inFile$datapath, header = TRUE)
      impordata <- subset(impordata,round_index==numround)
      
      #result display
      data.display <- paste(br(),br(),"Round: ",numround,br(),
                            "Player1: ",impordata$player1_move,
                            "Player2: ",impordata$player2_move,br(),
                            "Winner: ",br(),
                            "Player1_points: ",impordata$player1_points,
                            "Player2_points: ",impordata$player2_points,sep = '\n')
      HTML(data.display)
      
    })#end of output$result
  }
  
  shinyApp(ui, server)
}

