library(shiny)
library(randomForest)

# Load trained model
model <- readRDS("model/farmer_distress_rf_model.rds")

ui <- fluidPage(
  
  titlePanel("Farmer Distress Early Warning System"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      numericInput("rainfall", "Rainfall Deviation (%)", value = 0),
      
      numericInput("temp", "Temperature Anomaly", value = 0),
      
      numericInput("paddy", "Paddy Yield", value = 4),
      
      numericInput("sugarcane", "Sugarcane Yield", value = 90),
      
      numericInput("groundnut", "Groundnut Yield", value = 2),
      
      numericInput("maize", "Maize Yield", value = 3),
      
      numericInput("paddy_price", "Paddy Price", value = 1900),
      
      numericInput("sugar_price", "Sugarcane Price", value = 3000),
      
      numericInput("ground_price", "Groundnut Price", value = 5000),
      
      numericInput("maize_price", "Maize Price", value = 2100),
      
      actionButton("predict", "Predict Distress")
      
    ),
    
    mainPanel(
      
      h3("Prediction Result"),
      
      verbatimTextOutput("result")
      
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$predict, {
    
    new_data <- data.frame(
      
      CRI = abs(input$rainfall)/100,
      
      CSI = (input$paddy + input$sugarcane +
               input$groundnut + input$maize)/100,
      
      MRI = (input$paddy_price + input$sugar_price +
               input$ground_price + input$maize_price)/10000,
      
      Distress_Risk_Score = runif(1,0,1)
      
    )
    
    prediction <- predict(model, new_data)
    
    output$result <- renderText({
      
      if(prediction == 1){
        "⚠ High Risk of Farmer Distress"
      } else {
        "✅ Low Risk of Farmer Distress"
      }
      
    })
    
  })
  
}

shinyApp(ui = ui, server = server)