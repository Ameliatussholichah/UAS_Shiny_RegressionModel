library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(reshape2)

ui <- fluidPage(
  titlePanel("Aplikasi Prediksi Variabel Y Berdasarkan Variabel X"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Unggah Data Training (csv)"),
      uiOutput("xvar_ui"),
      uiOutput("yvar_ui"),
      actionButton("train", "Latih Model"),
      downloadButton("saveModel", "💾 Simpan Model (.rds)"),
      actionButton("loadTrainedModel", "🔁 Muat Model"),
      
      
      fileInput("testFile", "Unggah Data Testing (csv)", accept = ".csv"),
      actionButton("predictTest", "🔍 Prediksi Data Testing")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Preview", dataTableOutput("dataPreview")),
        tabPanel("Correlation Matrix", plotOutput("corPlot")),
        tabPanel("Eksploratory Analysis", plotOutput("edaPlot")),
        tabPanel("Model Regresi", verbatimTextOutput("modelSummary"),
                 plotOutput("actualVsPredicted")),
        tabPanel("Prediksi Data Baru", dataTableOutput("testPrediction"))
      )
    )
  )
)

server <- function(input, output, session) {
  dataset <- reactive({
    req(input$datafile)
    ext <- tools::file_ext(input$datafile$name)
    validate(need(ext == "csv", "❌ Harap unggah file .csv untuk data training."))
    read.csv(input$datafile$datapath)
  })
  
  output$xvar_ui <- renderUI({
    req(dataset())
    selectInput("xvar", "Pilih Variabel X:", choices = names(dataset()))
  })
  
  output$yvar_ui <- renderUI({
    req(dataset())
    selectInput("yvar", "Pilih Variabel Y:", choices = names(dataset()))
  })
  
  output$dataPreview <- renderDataTable({
    dataset()
  })
  
  output$corPlot <- renderPlot({
    data <- dataset()
    num_data <- data %>% select(where(is.numeric))
    corr <- cor(num_data)
    corr_melt <- melt(corr)
    ggplot(corr_melt, aes(Var1, Var2, fill = value)) +
      geom_tile(color = "white") +
      geom_text(aes(label = round(value, 2)), color = "black", size = 4) +
      scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                           midpoint = 0, limit = c(-1, 1), name = "Korelasi") +
      theme_minimal() +
      labs(title = "Matriks Korelasi") +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
    
  })
  
  output$edaPlot <- renderPlot({
    req(input$xvar, input$yvar)
    data <- dataset()
    ggplot(data, aes_string(x = input$xvar, y = input$yvar)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = input$xvar, y = input$yvar, title = "Scatterplot dan Regresi Linier")
  })
  
  model <- reactiveVal(NULL)
  
  observeEvent(input$train, {
    req(input$xvar, input$yvar)
    data <- dataset()
    formula <- as.formula(paste(input$yvar, "~", input$xvar))
    trained_model <- lm(formula, data = data)
    model(trained_model)
    
    # Simpan model ke file sementara
    saveRDS(trained_model, file = "trained_model_temp.rds")
  })
  
  output$modelSummary <- renderPrint({
    req(model())
    summary(model())
  })
    output$actualVsPredicted <- renderPlot({
      req(model(), input$xvar, input$yvar)
      data <- dataset()
      
      # Buat prediksi
      predicted <- predict(model(), newdata = data)
      actual <- data[[input$yvar]]
      
      df_plot <- data.frame(Aktual = actual, Prediksi = predicted)
      
      ggplot(df_plot, aes(x = Aktual, y = Prediksi)) +
        geom_point(color = "purple") +
        geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
        labs(
          title = "Plot Actual vs Predicted",
          x = "Actual",
          y = "Predicted"
        ) +
        theme_minimal()
  })
  
  output$saveModel <- downloadHandler(
    filename = function() { "model_regresi.rds" },
    content = function(file) {
      saveRDS(model(), file)
    }
  )
  
  observeEvent(input$loadTrainedModel, {
    if (file.exists("trained_model_temp.rds")) {
      latest_model <- readRDS("trained_model_temp.rds")
      model(latest_model)
      showNotification("✅ Model terbaru berhasil dimuat ulang!", type = "message")
    } else {
      showNotification("⚠️ Model belum dilatih atau file belum tersedia.", type = "error")
    }
  })
  
  testData <- reactive({
    req(input$testFile)
    ext <- tools::file_ext(input$testFile$name)
    validate(need(ext == "csv", "❌ Harap unggah file .csv untuk data testing."))
    read.csv(input$testFile$datapath)
  })
  
  testPrediction <- eventReactive(input$predictTest, {
    req(model(), testData(), input$xvar)
    test <- testData()
    validate(
      need(input$xvar %in% names(test), paste0("❌ Variabel '", input$xvar, "' tidak ditemukan di data testing."))
    )
    prediction <- predict(model(), newdata = test)
    hasil <- cbind(test, Prediksi_Y = prediction)
    return(hasil)
  })
  
  output$testPrediction <- renderDataTable({
    testPrediction()
  })
}

shinyApp(ui, server)