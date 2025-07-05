library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(reshape2)

ui <- fluidPage(
  titlePanel("Aplikasi Prediksi Variabel Y berdasarkan Variabel X"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Unggah Dataset (hanya .csv yang diterima)"),
      
      # Dropdown variabel X dan Y
      uiOutput("xvar_ui"),
      uiOutput("yvar_ui"),
      
      actionButton("train", "Latih Model Regresi"),
      downloadButton("saveModel", "💾 Simpan Model"),
      
      fileInput("modelFile", "Muat Model (.rds)", accept = ".rds"),
      
      textInput("newX", "Masukkan Nilai X Baru:"),
      actionButton("predict", "Prediksi")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", dataTableOutput("dataPreview")),
        tabPanel("Correlation Matrix", plotOutput("corPlot")),
        tabPanel("Exploratory Analysis", plotOutput("edaPlot")),
        tabPanel("Model Regresi", verbatimTextOutput("modelSummary")),
        tabPanel("Prediksi Data Baru", verbatimTextOutput("prediction"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Validasi dan baca file csv
  dataset <- reactive({
    req(input$datafile)
    ext <- tools::file_ext(input$datafile$name)
    validate(
      need(ext == "csv", "❌ Format file tidak valid! Harap unggah file .csv")
    )
    read.csv(input$datafile$datapath)
  })
  
  # Dropdown dinamis untuk X dan Y
  output$xvar_ui <- renderUI({
    req(dataset())
    selectInput("xvar", "Pilih Variabel X:", choices = names(dataset()))
  })
  
  output$yvar_ui <- renderUI({
    req(dataset())
    selectInput("yvar", "Pilih Variabel Y:", choices = names(dataset()))
  })
  
  # Preview data
  output$dataPreview <- renderDataTable({
    dataset()
  })
  
  # Plot korelasi
  output$corPlot <- renderPlot({
    data <- dataset()
    num_data <- data %>% select(where(is.numeric))
    corr <- cor(num_data)
    ggplot(melt(corr), aes(Var1, Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                           midpoint = 0, limit = c(-1, 1)) +
      theme_minimal() +
      labs(title = "Matriks Korelasi")
  })
  
  # EDA plot
  output$edaPlot <- renderPlot({
    req(input$xvar, input$yvar)
    data <- dataset()
    ggplot(data, aes_string(x = input$xvar, y = input$yvar)) +
      geom_point() + 
      geom_smooth(method = "lm") +
      labs(x = input$xvar, y = input$yvar)
  })
  
  # Model regresi
  model <- reactiveVal(NULL)
  
  observeEvent(input$train, {
    req(input$xvar, input$yvar)
    data <- dataset()
    form <- as.formula(paste(input$yvar, "~", input$xvar))
    model(lm(form, data = data))
  })
  
  # Ringkasan model
  output$modelSummary <- renderPrint({
    req(model())
    summary(model())
  })
  
  # Simpan model
  output$saveModel <- downloadHandler(
    filename = function() { "model_regresi.rds" },
    content = function(file) {
      saveRDS(model(), file)
    }
  )
  
  # Load model eksternal
  observeEvent(input$modelFile, {
    req(input$modelFile)
    loaded_model <- readRDS(input$modelFile$datapath)
    model(loaded_model)
  })
  
  # Prediksi nilai baru
  prediction <- eventReactive(input$predict, {
    req(model(), input$xvar)
    xval <- as.numeric(input$newX)
    validate(
      need(!is.na(xval), "❌ Masukkan angka valid untuk prediksi.")
    )
    predict(model(), newdata = setNames(data.frame(xval), input$xvar))
  })
  
  output$prediction <- renderPrint({
    prediction()
  })
}

shinyApp(ui, server)
