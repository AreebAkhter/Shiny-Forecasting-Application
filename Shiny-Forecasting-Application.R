#########################################################
#  R Shiny Web Forecasting App Project


# load libraries
library(shiny)
library(fpp2)

# import data
magna_data <- read.csv('C:/Users/Teckid.123/Desktop/magna_data.csv')

df = magna_data 

# define as time series
df = ts(df, start=2000, frequency=4)
tail(df)

MGA = df[,2]
altsales = df[,3]
CS = df[,4]

# Define UI 
ui <- fluidPage(pageWithSidebar(
  
  # Application title
  headerPanel("Shiny Web Forecasting App"),
  
  # Sidebar with controls to select the dataset and forecast period duration
  sidebarPanel(
    selectInput("variable", "Variable:",
                list("MGA sales" = "MGA", 
                     "Auto sales" = "altsales",
                     "Consumer sentiment" = "CS")),
    sliderInput(inputId = "period",
                label ="Number of periods (in months) for forecasting:",
                min = 1, max = 24, step = 1, value = 1),
    
    submitButton("Update View")
  ),
  
  

  # Show the caption and forecast plots
  mainPanel(
    h3(textOutput("caption")),
    
    tabsetPanel(
      tabPanel("Timeseries plot", plotOutput("tsPlot")),
      tabPanel("Stationary Test", textOutput("stationarytext"), textOutput("stationaryPVALUE")),
      tabPanel("Exponential Smoothing (ETS) Forecast", plotOutput("etsForecastPlot"), tableOutput("etsForecastTable")), 
      tabPanel("ARIMA Forecast", plotOutput("arimaForecastPlot"), tableOutput("arimaForecastTable")),
      tabPanel("Nnetar Forecast", plotOutput("NnetarForecastPlot"), tableOutput("NnetarForecastTable")),
      tabPanel("STL Forecast", plotOutput("STLForecastPlot"), tableOutput("STLForecastTable")),
      tabPanel("TBATS Forecast", plotOutput("tbatsForecastPlot"), tableOutput("tbatsForecastTable")),
      tabPanel("Average (ARIMA, ETS, Nnetar, STL, TBATS) Forecasts", tableOutput("averageForecastTable"))
    )
  )
))



server <- (function(input, output){
  
  getDataset <- reactive({
    if (input$variable=="MGA")
    {
      return(MGA)
    }
    else if (input$variable=="altsales")
    {
      return(altsales)
    }
    else
    {
      return(CS)
    }
  })
  
  output$caption <- renderText({
    paste("Dataset: ", input$variable)
  })
  
  output$tsPlot <- renderPlot({
    y <- getDataset()
    p1 = autoplot(y) + ylab("Quantity")+ geom_line(col="blue", size=3)+ggtitle("Magna Time Series")+
      theme(plot.title  = element_text(size=12, face = "bold") ,
          axis.text.x = element_text(size=12, face = "bold"),
          axis.text.y = element_text(size=12, face = "bold"),
          axis.title  = element_text(size=12, face = "italic")) 
    p2 = ggAcf(y)
    gridExtra::grid.arrange(p1, p2, nrow=2)
  })
 
  output$arimaForecastPlot <- renderPlot({
    fit <- auto.arima(getDataset())
    plot(forecast(fit, h=input$period))
  })
  
  output$arimaForecastPlot <- renderPlot({
    fit <- auto.arima(getDataset())
    plot(forecast(fit, h=input$period))
  })
  
  output$arimaForecastTable <- renderTable({
    fit <- auto.arima(getDataset())
    forecast(fit, h=input$period)
  })
  
  output$stationaryPVALUE <- renderPrint({
    y <- getDataset()
    s <- Box.test(y, lag = 1,type="Ljung")
    s
  })

  output$etsForecastPlot <- renderPlot({
    fit <- ets(getDataset())
    plot(forecast(fit, h=input$period))
  })
  
  output$etsForecastTable <- renderTable({
    fit <- ets(getDataset())
    forecast(fit, h=input$period)
  })
  
  output$tbatsForecastPlot <- renderPlot({
    fit <- nnetar(getDataset())
    plot(forecast(fit, h=input$period))
  })
  
  output$tbatsForecastTable <- renderTable({
    fit <- nnetar(getDataset())
    forecast(fit, h=input$period)
  })
  
  output$STLForecastPlot <- renderPlot({
    fit <- stlf(getDataset())
    plot(forecast(fit, h=input$period))
  })
  
  output$STLForecastTable <- renderTable({
    fit <- stlf(getDataset())
    forecast(fit, h=input$period)
  })
  
  output$NnetarForecastPlot <- renderPlot({
    fit <- tbats(getDataset())
    plot(forecast(fit, h=input$period))
  })
  
  output$NnetarForecastTable <- renderTable({
    fit <- tbats(getDataset())
    forecast(fit, h=input$period)
  })
  
  output$averageForecastTable <- renderTable({
    
    fit1 = auto.arima(getDataset())
    fc1 = forecast(fit1, h=input$period)  
    
    fit2 <- ets(getDataset())
    fc2 = forecast(fit2, h=input$period)
    
    fit3 <- nnetar(getDataset())
    fc3 = forecast(fit3, h=input$period)
    
    fit4 = stlf(getDataset())
    fc4 = forecast(fit4, h=input$period)
    
    fit5 <- tbats(getDataset())
    fc5 = forecast(fit5, h=input$period)
    
    
    fc = (fc1$mean + fc2$mean + fc3$mean + fc4$mean + fc5$mean )/5
  })
})


# Run the application 
shinyApp(ui = ui, server = server)
