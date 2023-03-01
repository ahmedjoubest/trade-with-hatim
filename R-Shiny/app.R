
from_close_to_renko_ohlc <- function(df,brick_size) {
  
  time <- as.Date(c())
  trend <- c()
  i <- 1
  
  while (i <= nrow(df)) {
    j <- i+1
    while(j<nrow(df)){
      price_change <- df$Close[j] - df$Close[i]
      if(abs(price_change) > brick_size){
        time <- c(time,df$Date[j])
        trend <- c(trend,ifelse(price_change > 0, "up","down"))
        i <- j
        break
      }
      j <- j + 1
    }
    if(j==nrow(df)){
      break
    }
  }
  
  return(
    list(
      initial_price = df$Close[1],
      time = time,
      trend = trend
    )
  )
}

HTML_css <- function() {
  library(shiny)
  return(HTML("
       .cssload-loader {
       width: 244px;
       height: 49px;
       line-height: 49px;
       text-align: center;
       position: absolute;
       left: 50%;
       transform: translate(-50%, -50%);
       -o-transform: translate(-50%, -50%);
       -ms-transform: translate(-50%, -50%);
       -webkit-transform: translate(-50%, -50%);
       -moz-transform: translate(-50%, -50%);
       font-family: helvetica, arial, sans-serif;
       text-transform: uppercase;
       font-weight: 900;
       font-size:18px;
       color: #0275D8;
       letter-spacing: 0.2em;
       }
       .cssload-loader::before, .cssload-loader::after {
       content: '';
       display: block;
       width: 15px;
       height: 15px;
       background: #0275D8;
       position: absolute;
       animation: cssload-load 0.81s infinite alternate ease-in-out;
       -o-animation: cssload-load 0.81s infinite alternate ease-in-out;
       -ms-animation: cssload-load 0.81s infinite alternate ease-in-out;
       -webkit-animation: cssload-load 0.81s infinite alternate ease-in-out;
       -moz-animation: cssload-load 0.81s infinite alternate ease-in-out;
       }
       .cssload-loader::before {
       top: 0;
       }
       .cssload-loader::after {
       bottom: 0;
       }
       
       
       
       @keyframes cssload-load {
       0% {
       left: 0;
       height: 9px;
       width: 350px;
       }
       50% {
       height: 9px;
       width: 350px;
       }
       100% {
       left: 229px;
       height: 9px;
       width: 350px;
       }
       }
       
       @-o-keyframes cssload-load {
       0% {
       left: 0;
       height: 29px;
       width: 15px;
       }
       50% {
       height: 8px;
       width: 39px;
       }
       100% {
       left: 229px;
       height: 29px;
       width: 15px;
       }
       }
       
       @-ms-keyframes cssload-load {
       0% {
       left: 0;
       height: 29px;
       width: 15px;
       }
       50% {
       height: 8px;
       width: 39px;
       }
       100% {
       left: 229px;
       height: 29px;
       width: 15px;
       }
       }
       
       @-webkit-keyframes cssload-load {
       0% {
       left: 0;
       height: 29px;
       width: 15px;
       }
       50% {
       height: 8px;
       width: 39px;
       }
       100% {
       left: 229px;
       height: 29px;
       width: 15px;
       }
       }
       
       @-moz-keyframes cssload-load {
       0% {
       left: 0;
       height: 29px;
       width: 15px;
       }
       50% {
       height: 8px;
       width: 39px;
       }
       100% {
       left: 229px;
       height: 29px;
       width: 15px;
       }
       }
       "
  ))
}

library(highcharter)
library(xts)
library(shiny)
library(shinycssloaders)

load("Df_yahoo.RData")
df <- Df_yahoo[,c("Ticker","Date","Close")]


ui <- fluidPage(
  
  # Custom loading pop up with css classes
  tags$head(tags$style(HTML_css())),
  
  # Custom tabs area
  tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: #F5F5F5;  color:black}
    .tabbable > .nav                 {background-color: #F5F5F5;  color:black}
    .tabbable > .nav > li[class=active]    > a {background-color: white; color:black}
  ")),
  
  # Application title
  titlePanel("PHASE 1 - MR-SA-M-HF", windowTitle = "PHASE 1 - MR-SA-M-HF"),
  sidebarLayout(
    sidebarPanel(width = 3, selectInput("Asset","Select an asset",
                                        choices = unique(df$Ticker), selected = "AAPL")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("From Price Action to Binary Vector", 
                 h3("Price Action:"),
                 p("This line chart represents the price movement (Close price) of 
                   the selected asset in a daily timeframe."),
                 highchartOutput("plot1") %>% 
                   withSpinner(color="#3C8DBC",type=4, size=0.8),
                 downloadButton("download_data", paste0("Download Historical Data")),
                 br(),
                 h3("Renko Chart:"),
                 p("Based on the selected brick size below, the Renko Chart are representative
                 of the event itself with no regards to time correlation, which gives a clear view
                   of the price action."),
                 fluidRow(
                   column(2,
                          numericInput("brick_size", "Select the brick size", 10)),
                   column(2,
                          br(),
                          actionButton("update_brick_size","Update the brick size"))
                 ),
                 highchartOutput("plot2") %>% 
                   withSpinner(color="#3C8DBC",type=4, size=0.8),
                 br(),
                 h3("Results: binary vector"),
                 p("Based on the binary values of renko chart, we could build the following result:"),
                 highchartOutput("plot3") %>% 
                   withSpinner(color="#3C8DBC",type=4, size=0.8)
        ),
        tabPanel("Analyis 2"),
        tabPanel("Analysis 3")
      )
    )
  )
)


server <- function(input, output, session) {
  plot_data <- reactiveValues(
    DF = NULL,
    binary = NULL
  )
  
  observeEvent(c(input$Asset, input$update_brick_size), ignoreInit = F,{
    print("daz mn observe")
    # browser()
    list <- from_close_to_renko_ohlc(df[df$Ticker==input$Asset,],input$brick_size)
    
    # DF creation ohlc
    
    # initialization
    DF <- data.frame(
      # Date = list$time[1],
      Open = ifelse(list$trend[1]=="up",
                    list$initial_price-input$brick_size,
                    list$initial_price+input$brick_size),
      High = max(list$initial_price,
                 ifelse(list$trend[1]=="up",
                        list$initial_price-input$brick_size,
                        list$initial_price+input$brick_size)),
      Low = min(list$initial_price,
                ifelse(list$trend[1]=="up",
                       list$initial_price-input$brick_size,
                       list$initial_price+input$brick_size)),
      Close = list$initial_price,
      row.names = list$time[1]
    )
    
    for (i in 2:length(list$time)){
      DF <- rbind(DF,
                  data.frame(
                    # Date = list$time[i],
                    Open = DF$Close[i-1],
                    High = max(DF$Close[i-1],
                               ifelse(list$trend[i]=="up",
                                      DF$Close[i-1]+input$brick_size,
                                      DF$Close[i-1]-input$brick_size)),
                    Low = min(ifelse(list$trend[i]=="up",
                                     DF$Close[i-1]+input$brick_size,
                                     DF$Close[i-1]-input$brick_size),
                              DF$Close[i-1]),
                    Close = ifelse(list$trend[i]=="up",
                                   DF$Close[i-1]+input$brick_size,
                                   DF$Close[i-1]-input$brick_size),
                    row.names = list$time[i]
                  ))
    }
    
    
    for (i in 2:length(list$trend)) {
      if (list$trend[i-1] != list$trend[i]) {
        if (list$trend[i] == "up") {
          sapply( i:nrow(DF), function (i) {
            DF$Open[i] <<- DF$Open[i] + input$brick_size
            DF$High[i] <<- DF$High[i] + input$brick_size
            DF$Low[i]  <<- DF$Low[i] + input$brick_size
            DF$Close[i] <<- DF$Close[i] + input$brick_size
          })
        } else {
          sapply( i:nrow(DF), function (i) {
            DF$Open[i] <<- DF$Open[i] - input$brick_size
            DF$High[i] <<- DF$High[i] - input$brick_size
            DF$Low[i]  <<- DF$Low[i] - input$brick_size
            DF$Close[i] <<- DF$Close[i] - input$brick_size
          })
        }
      }
    }
    
    binary <- ifelse(list$trend =="up", 1,-1)
    
    plot_data$binary <- binary
    plot_data$DF <- DF
    
  })
  
  output$plot1 <- renderHighchart({
    hchart(xts(df$Close[df$Ticker==input$Asset],(df[df$Ticker==input$Asset,'Date'])), type ="spline") %>% 
      hc_tooltip(valueDecimals = 2, followPointer = T) %>% 
      hc_plotOptions(series =list(dataGrouping =list(forced = T,units = list(list( 'day',list(1))))),
                     candlestick = list(color="#EF5350", upColor="#26A69A")) %>% 
      hc_rangeSelector(selected = 7)
  })
  
  output$plot2 <- renderHighchart({
    hchart(as.xts(plot_data$DF)) %>% 
      hc_tooltip(valueDecimals = 2, followPointer = T) %>% 
      hc_plotOptions(series =list(dataGrouping =list(forced = T,units = list(list( 'day',list(1))))),
                     candlestick = list(color="#EF5350", upColor="#26A69A")) %>% 
      hc_rangeSelector(selected = 7)
  })
  
  output$plot3 <- renderHighchart({
    hchart(xts(cumsum(plot_data$binary),index(as.xts(plot_data$DF)))) %>% 
      hc_yAxis(gridLineWidth = 1, gridLineColor = "#F2F2F2",
               plotLines = lapply(
                 1:max(cumsum(plot_data$binary)),
                 function(i){
                   list(value = i, color = "grey",
                        width = 1)
                 }
               ))
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("Historical_Data.csv", sep="")
    },
    content = function(file) {
      write.csv(df[df$Ticker==input$Asset,], file)
    }
  )
}

shinyApp(ui, server)