
# Import libraries
library(shiny)
library(data.table)
library(randomForest)

# Read in the RF model
model <- readRDS("model.rds")

# Training set
TrainSet <- read.csv("training.csv", header = TRUE)
TrainSet <- TrainSet[,-1]


####################################
# User interface                   #
####################################

ui <- pageWithSidebar(
  
  # Page header
  headerPanel('Mice Protein Expression'),
  

  # Input values
  sidebarPanel(
    HTML("<h3>Input parameters</h4>"),
    sliderInput("SOD1_N", label = "SOD1_N", value = 1.0,
                min = min(TrainSet$SOD1_N),
                max = max(TrainSet$SOD1_N)
    ),
    sliderInput("APP_N", label = "APP_N", value = 1.0,
                min = min(TrainSet$APP_N),
                max = max(TrainSet$APP_N)),
    sliderInput("pPKCG_N", label = "pPKCG_N", value = 1.0,
                min = min(TrainSet$pPKCG_N),
                max = max(TrainSet$pPKCG_N)),
    sliderInput("pERK_N", label = "pERK_N", value = 1.0,
                min = min(TrainSet$pERK_N),
                max = max(TrainSet$pERK_N)),
    sliderInput("pCAMKII_N", label = "pCAMKII_N", value = 1.0,
                min = min(TrainSet$pCAMKII_N),
                max = max(TrainSet$pCAMKII_N)),
    
    sliderInput("pERK_N", label = "pERK_N", value = 1.0,
                min = min(TrainSet$pERK_N),
                max = max(TrainSet$pERK_N)),
    
    sliderInput("pCAMKII_N", label = "pCAMKII_N", value = 1.0,
                min = min(TrainSet$pCAMKII_N),
                max = max(TrainSet$pCAMKII_N)),
    
    sliderInput("CaNA_N", label = "CaNA_N", value = 1.0,
                min = min(TrainSet$CaNA_N),
                max = max(TrainSet$CaNA_N)),
    
    sliderInput("Tau_N", label = "Tau_N", value = 1.0,
                min = min(TrainSet$Tau_N),
                max = max(TrainSet$Tau_N)),
    
    sliderInput("pP70S6_N", label = "pP70S6_N", value = 1.0,
                min = min(TrainSet$pP70S6_N),
                max = max(TrainSet$pP70S6_N)),
    
    
    sliderInput("pNUMB_N", label = "pNUMB_N", value = 1.0,
                min = min(TrainSet$pNUMB_N),
                max = max(TrainSet$pNUMB_N)),
    
    sliderInput("BRAF_N", label = "BRAF_N", value = 1.0,
                min = min(TrainSet$BRAF_N),
                max = max(TrainSet$BRAF_N)),
    
    
    
    sliderInput("Ubiquitin_N", label = "Ubiquitin_N", value = 1.0,
                min = min(TrainSet$Ubiquitin_N),
                max = max(TrainSet$Ubiquitin_N)),
    
    sliderInput("AKT_N", label = "AKT_N", value = 1.0,
                min = min(TrainSet$AKT_N),
                max = max(TrainSet$AKT_N)),
    
    sliderInput("S6_N", label = "S6_N", value = 1.0,
                min = min(TrainSet$S6_N),
                max = max(TrainSet$S6_N)),
    
    ###########################################################33
    
    sliderInput("AcetylH3K9_N", label = "AcetylH3K9_N", value = 1.0,
                min = min(TrainSet$AcetylH3K9_N),
                max = max(TrainSet$AcetylH3K9_N)),
    
    actionButton("submitbutton", "Submit", class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
    
  )
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c('SOD1_N','APP_N','pPKCG_N','pERK_N','pCAMKII_N','CaNA_N','Tau_N','pP70S6_N','pNUMB_N', 'BRAF_N', 'Ubiquitin_N', 'AKT_N','S6_N',
                'AcetylH3K9_N'),
      Value = as.character(c(input$SOD1_N,
                             input$APP_N,
                             input$pPKCG_N,
                             input$pERK_N,
                             input$pCAMKII_N,
                             input$CaNA_N,
                             input$Tau_N,
                             input$pP70S6_N,
                             input$pNUMB_N,
                             input$BRAF_N,
                             input$Ubiquitin_N,
                             input$AKT_N,
                             input$S6_N,
                             input$AcetylH3K9_N
                        
                             
                             )),
      stringsAsFactors = FALSE)
    
    class <- 0
    df <- rbind(df, class)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)