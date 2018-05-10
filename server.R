library(shiny)
library(shinydashboard)
library(DT)
library(quantmod)

source('plot.R')

features = c("Open","High","Low","Close","Volume","Adjusted")
train = read.csv('data/551_train.csv',header=T,sep=',')
sp = train$stock.Volatility
sm = train$stock.Momentum
ip = train$index.Volatility
im = train$index.Momentum

svm_result = read.csv('data/SVM/svm_modelresult.csv',header=T,sep=',')
rf_result = read.csv('data/RandomForest/rf_modelresult_t100.csv',header=T,sep=',')


server <- function(input,output){
  ##### stock data #####
  stockData <- reactive({
    data.ac = getSymbols(input$sym,from = input$dates[1],to = input$dates[2],env = NULL)
    colnames(data.ac) = features
    return(data.frame(Date=index(data.ac),coredata(data.ac)))
  })
  ##### stock plot #####
  plotElements <- reactive({
    bbands = BBands(stockData()[,c("High","Low","Close")])
    dataWB = subset(cbind(stockData(),data.frame(bbands[,1:3])),
                    Date >= input$dates[1] & Date <= input$dates[2])
    for(i in 1:length(dataWB[,1])){
      if(dataWB$Close[i] >= dataWB$Open[i]){
        dataWB$direction[i] = "Increasing"
      }else{
        dataWB$direction[i] = "Decreasing"
      }
    }
    
    inc = list(line=list(color="#17BECF"))
    dec = list(line=list(color="#7F7F7F"))
    p = dataWB %>%
      plot_ly(x=~Date, type="candlestick",
              open = ~Open, close = ~ Close,
              high = ~High, low = ~Low, name = input$sym,
              increasing = inc, decreasing = dec) %>%
      add_lines(x = ~Date, y = ~up , name = "B Bands",
                line = list(color = '#ccc', width = 0.5),
                legendgroup = "Bollinger Bands",
                hoverinfo = "none", inherit = F) %>%
      add_lines(x = ~Date, y = ~dn, name = "B Bands",
                line = list(color = '#ccc', width = 0.5),
                legendgroup = "Bollinger Bands", inherit = F,
                showlegend = FALSE, hoverinfo = "none") %>%
      add_lines(x = ~Date, y = ~mavg, name = "Mv Avg",
                line = list(color = '#E377C2', width = 0.5),
                hoverinfo = "none", inherit = F) %>%
      layout(title = paste("Historical Price Data Of ",input$sym),
             yaxis = list(title = "Price"),
             legend = list(orientation='h',
                           xanchor = 'center',
                           yanchor = 'top',
                           bgcolor = 'transparent',
                           x = 0.5, y = 1))

    return(p)
  })
  
  ##### svm result #####
  svmData <- reactive({
    data.svm.m = svm_result[which(svm_result$m%in%input$num_m),-1]
    data.svm.n1 = data.svm.m[which(data.svm.m$n1%in%input$num_n1),]
    data.svm.n2 = data.svm.m[which(data.svm.n1$n2%in%input$num_n2),]
    return(data.svm.n2)
  })
  
  svmMN <-reactive({
    data.svm.mn = svm_result[which(svm_result$m%in%input$num_m),-1]
    # p = plot_ly(x=~as.factor(data.svm.m$n1),y=~data.svm.m$mean,color=~as.factor(data.svm.m$n2))
    return(ggplotly(svm_plot(data.svm.mn)))
    # return(p)
  })
  
  svmAM <-reactive({
    data.svm.am = aggregate(svm_result$mean,list(svm_result$m),mean)
    data.svm.am$color = c(I('blue'),I('blue'),I('blue'),I('blue'),I('blue'),I('blue'))
    colnames(data.svm.am) = c('m','mean','color')
    data.svm.am[which(data.svm.am$m==input$num_m),3] = I('red')
    p = plot_ly(data.svm.am,x = ~m)%>%
      add_trace(y=~mean,mode = 'lines')%>%
      add_trace(y=~mean, type = 'scatter', mode = 'markers', color = ~color, marker = list(size=15))%>%
      layout(title='Mean Prediction Accuracy vs m', showlegend = F)
    
    return(p)
  })
  
  
  
  ##### rf result #####
  rfData <- reactive({
    data.rf.m = rf_result[which(rf_result$m%in%input$num_m_rf),-1]
    data.rf.n1 = data.rf.m[which(data.rf.m$n1%in%input$num_n1_rf),]
    data.rf.n2 = data.rf.m[which(data.rf.n1$n2%in%input$num_n2_rf),]
    return(data.rf.n2)
  })
  
  rfMN <-reactive({
    data.rf.mn = rf_result[which(rf_result$m%in%input$num_m_rf),-1]
    # p = plot_ly(x=~as.factor(data.svm.m$n1),y=~data.svm.m$mean,color=~as.factor(data.svm.m$n2))
    return(ggplotly(rf_plot(data.rf.mn)))
    # return(p)
  })
  
  
  ##### svm-rf difference #####
  svm.rf <- reactive({
    data.svm = svm_result[which(svm_result$m%in%input$num_m_an),-1]
    data.rf = rf_result[which(rf_result$m%in%input$num_m_an),-1]
    return(ggplotly(diff_plot(data.svm,data.rf)))
  })
  
  rfAM <-reactive({
    data.rf.am = aggregate(rf_result$mean,list(rf_result$m),mean)
    data.rf.am$color = c(I('blue'),I('blue'),I('blue'),I('blue'),I('blue'),I('blue'))
    colnames(data.rf.am) = c('m','mean','color')
    data.rf.am[which(data.rf.am$m==input$num_m_rf),3] = I('red')
    p = plot_ly(data.rf.am,x = ~m)%>%
      add_trace(y=~mean,mode = 'lines')%>%
      add_trace(y=~mean, type = 'scatter', mode = 'markers', color = ~color, marker = list(size=15))%>%
      layout(title='Mean Prediction Accuracy vs m', showlegend = F)
    
    return(p)
  })
  
  
  output$table <- DT::renderDataTable(DT::datatable({
    stockData()[,c("Date",input$feature)]
  }))
  
  output$stockPlot <- renderPlotly({
    plotElements()
  })
  output$debug_out <- renderPrint({

  })

  
  output$ip_vis <- renderPlotly({
    plot_ly(x=~ip,type = 'histogram',alpha = 0.6, color = "#edb863") %>% layout(title='n1=5',
                                                            yaxis = list(title = "Hist"),
                                                            xaxis = list(title = "index price percentage change"))
  })
  
  output$sp_vis <- renderPlotly({
    plot_ly(x=~sp,type = 'histogram',alpha = 0.6) %>% layout(title='n2=5',
                                                            yaxis = list(title = "Hist"),
                                                            xaxis = list(title = "index price percentage change"))
  })
  
  output$im_vis <- renderPlotly({
    plot_ly(x=~im,type = 'histogram',alpha = 0.6, color = "#edb863") %>% layout(title='n1=5',
                                                             yaxis = list(title = "Hist"),
                                                             xaxis = list(title = "momentum"))
  })
  
  output$sm_vis <- renderPlotly({
    plot_ly(x=~sm,type = 'histogram',alpha = 0.6) %>% layout(title='n2=5',
                                                             yaxis = list(title = "Hist"),
                                                             xaxis = list(title = "momentum"))
  })
  
  output$svm_result <- DT::renderDataTable(DT::datatable({
    svmData()
  }))
  
  output$rf_result <- DT::renderDataTable(DT::datatable({
    rfData()
  }))
  
  output$svm_mn <- renderPlotly({
    svmMN()
  })
  output$svm_am <- renderPlotly({
    svmAM()
  })
  output$rf_mn <- renderPlotly({
    rfMN()
  })
  output$rf_am <- renderPlotly({
    rfAM()
  })
  output$svm_rf_diff <- renderPlotly({
    svm.rf()
  })

}