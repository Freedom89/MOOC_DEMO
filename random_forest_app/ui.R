library(shiny)

fluidPage(
  titlePanel("Demostration 1 for MOOC Course"),
  p('how to build a randomForest with a trained model'),
  
  sidebarLayout(
    sidebarPanel(
      p('Download sample data set to follow upload requirement'),
      downloadButton('sample_data', label = "Download"),
      br(),
      
      p('Input CSV File for prediction'),
      fileInput('predict',label = 'Data input'),
      
      p('Download Results'),
      downloadButton('prediction_table',label='results')
      
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Random Forest Summary",verbatimTextOutput("raw_output")),
        tabPanel("predict output",htmlOutput("predict_table"))
    )
  )  
)
)