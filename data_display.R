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
                                
                      ),
                      tags$hr(),
                      checkboxInput("header", "Header", TRUE)
                    ),
                    mainPanel(
                      column(8,actionButton("prevbutton","<")),
                      actionButton("endbutton","END"),
                      column(2,actionButton("nextbutton",">")),
                      tableOutput("contents")
                    )
                  ),
                  selectInput("round", "Choose a round:",
                              list('#round'=seq(1,100))
                  ),
                  textOutput("result")
  )
  
  server <- function(input, output) {
    output$contents <- renderTable({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, it will be a data frame with 'name',
      # 'size', 'type', and 'datapath' columns. The 'datapath'
      # column will contain the local filenames where the data can
      # be found.
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      read.csv(inFile$datapath, header = input$header)
    })
  }
  
  shinyApp(ui, server)
}

