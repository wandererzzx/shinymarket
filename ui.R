library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(plotly)
library(shinycssloaders)


symbs = c("MSFT","CERN","AAPL","AMAT","TXN","CA","KLAC","LRCX","MU",
  "INTC","ADI","WDC","ADBE","SYMC","CSCO","XLNX","QCOM",
  "INTU","NTAP","CTXS","CHKP","ADSK","CTSH","NVDA","AKAM",
  "GRMN","STX","BIDU")

features = c('Open','High','Low','Close','Volume','Adjusted')

num_m = c(1,5,10,20,90,270)
days = c(5,10,20,90,270)


intro1 = "1. The authors used SVM to predict the direction, 
<strong>increasing</strong> or <strong>decreasing</strong>, of different stocks, 
based on the historical data. They trained various models to 
predict <strong>the next m day's direction</strong> of stocks, where <strong>m</strong> can
be (1, 5, 10, 20, 90, 270).Features extracted depend on another 
two parameters, <strong>n<sub>1</sub></strong> and <strong>n<sub>2</sub></strong>, which can be chosen from
(5, 10, 20, 90, 270)."

intro2 = "2. Problem is formulized as classification, with two classes,
<strong>increasing</strong>(label 1) and <strong>decreasing</strong>(label -1). "

intro3 = "3. Based on different combinations of <strong>m</strong>, <strong>n<sub>1</sub></strong> and <strong>n<sub>2</sub></strong>, 
total 150 models are trained. Following are some results "

analysis = "We compare the test accuracy of different models, and the result shows in most
cases, SVM outperform RF. But when <strong>m</strong> keeps increasing, the RF model can do better in some <strong>n<sub>1</sub></strong> and 
<strong>n<sub>2</sub></strong> combination. When <strong>m</strong> increases to 270, though the accuracy of RF exceeds SVM, their absolute accuracies are
both not satisfying." 

ROC1 = 'After comparing all the SVM models,we pick out the combination which reaches highest test accucracy 
( <strong>m = 90</strong>, <strong>n<sub>1</sub> = 20</strong>, and <strong>n<sub>2</sub> = 5</strong> ), and retrain it with 
10-fold cross validation, then we draw the ROC of this model and calculate its AUC.'


ROC2 = 'According to the result, we can observe even the best model can only beat random guessing to some extent. 
This, however, becomes one support for the <a href = "https://en.wikipedia.org/wiki/Efficient-market_hypothesis">Efficient Market Hypothesis (EMH)</a>.'

##### Introduction tab #####
originalData <- fluidPage(
  titlePanel(' Data Display'),
  fluidRow(
    column(4,
           selectizeInput('sym',label = h4('Select symbol'),choices = symbs,selected = 'AAPL',
                          options = list(create = TRUE))
    ),
    column(4,
           dateRangeInput('dates',label = h4('Date range'),
                          start = '2007-01-01',
                          end = '2015-01-01')
    ),
    column(4,
           selectizeInput('feature',label = h4('Select features'),choices = features,multiple = T,
                          selected = features))
  ),
  fluidRow(
    withSpinner(DT::dataTableOutput('table'))
    ),
  fluidRow(
    withSpinner(plotlyOutput('stockPlot'))
  )
)

##### processd data UI #####
processedData <- fluidPage(
  titlePanel("Feature Extraction"),
  withMathJax(),
  fluidRow(
    tabBox(
      title = "Index Price Volatility",
      id = 'ip',
      tabPanel("Definition",helpText('$$\\frac{\\sum_{i=t-n+1}^t\\frac{I_i-I_{i-1}}{I_{i-1}}}{n_1}$$
                                     $$I_i\\ stands\\ for\\ the\\ close\\ price\\ of\\ day\\ i$$',
                                     style = "font-size: 15px;")),
      tabPanel('Sample-Vis',
               withSpinner(plotlyOutput('ip_vis')))
    ),
    tabBox(
      title = "Index Momentum",
      id = 'im',
      tabPanel("Definition",helpText('$$\\frac{\\sum_{i=t-n+1}^td_i}{n_1}$$
                                     $$d_i\\ is\\ the\\ direction\\ indicator\\ for\\ index,\\ 
                                     each\\ day\\ is\\ labeled\\ 1$$ $$\\ if\\ closing\\ price\\ that\\ day\\ is\\ 
                                     higher\\ than\\ the\\ day\\ before,$$ $$\\ otherwise\\ labeled\\ -1$$',
                                     style = "font-size: 15px;")),
      tabPanel('Sample-Vis',
               withSpinner(plotlyOutput('im_vis')))
    )
  ),
  fluidRow(
         tabBox(
           title = "Stock Price Volatility",
           id = 'sp',
           tabPanel("Definition",helpText('$$\\frac{\\sum_{i=t-n+1}^t\\frac{C_i-C_{i-1}}{C_{i-1}}}{n_2}$$
                                          $$C_i\\ stands\\ for\\ the\\ close\\ price\\ of\\ day\\ i$$',
                                          style = "font-size: 15px;")),
           tabPanel('Sample-Vis',
                    withSpinner(plotlyOutput('sp_vis')))
            ),
         tabBox(
           title = "Stock Momentum",
           id = 'sm',
           tabPanel("Definition",helpText('$$\\frac{\\sum_{i=t-n+1}^ty_i}{n_2}$$
                                          $$y_i\\ is\\ the\\ direction\\ indicator\\ for\\ each\\ stock,\\
                                          each\\ day\\ is\\ labeled\\ 1$$ $$\\ if\\ closing\\ price\\ that\\ day\\ is\\ 
                                     higher\\ than\\ the\\ day\\ before,$$ $$\\ otherwise\\ labeled\\ -1$$',
                                          style = "font-size: 15px;")),
           tabPanel('Sample-Vis',
                    withSpinner(plotlyOutput('sm_vis')))
         )
  ),
  fluidRow(
    box(
      title = "Label Creation", status = "primary", solidHeader = TRUE,
      collapsible = TRUE, width = 12,
      helpText('$$We\\ use\\ these\\ features\\ to\\ predict\\ the\\  
               direction\\ of\\ price\\ change\\ between\\ day\\ t\\ and\\ t+m,\\ where\\
               m\\in\\{1,5,10,20,90,270\\}$$
               $$label_i = 1\\ $$ $$if\\ C_{i+m}-C_i > 0$$ $$else\\ -1$$')
    )
  )
  
)

##### svm result tab UI #####
svmresult<-fluidPage(
  titlePanel('SVM Test Accuracy'),
  fluidRow(
    column(4,
           selectizeInput('num_m',label = h4('Select m'),choices = num_m,selected = 1)),
    column(4,
           selectizeInput('num_n1',label = h4('Select n1'),choices = days,selected = days,multiple = T)),
    column(4,
           selectizeInput('num_n2',label = h4('Select n2'),choices = days,selected = days,multiple = T))
  ),
  fluidRow(
    withSpinner(DT::dataTableOutput('svm_result'))
  ),
  div(class='disAdj'),
  div(class='disAdj'),
  
  fluidRow(
    box(title = "Accuracy Line Plot ",status = 'primary',solidHeader = T,
        collapsible = T,
      withSpinner(plotlyOutput('svm_am'))),
    box(title = "Accuracy Bar Plot",status = 'primary',solidHeader = T,
        collapsible = T,
        withSpinner(plotlyOutput('svm_mn')))
    
  )
)



##### randomforest result UI #####
rfresult<-fluidPage(
  titlePanel('RandomForest Test Accuracy'),
  fluidRow(
    column(4,
           selectizeInput('num_m_rf',label = h4('Select m'),choices = num_m,selected = 1)),
    column(4,
           selectizeInput('num_n1_rf',label = h4('Select n1'),choices = days,selected = days,multiple = T)),
    column(4,
           selectizeInput('num_n2_rf',label = h4('Select n2'),choices = days,selected = days,multiple = T))
  ),
  fluidRow(
    withSpinner(DT::dataTableOutput('rf_result'))
  ),
  div(class='disAdj'),
  div(class='disAdj'),
  fluidRow(
    box(title = "Accuracy Line Plot ",status = 'primary',solidHeader = T,
        collapsible = T,
        withSpinner(plotlyOutput('rf_am'))),
    box(title = "Accuracy Bar Plot",status = 'primary',solidHeader = T,
        collapsible = T,
        withSpinner(plotlyOutput('rf_mn')))
  )
)

##### analysis UI #####
analysis <- fluidPage(
  titlePanel('Results Comparison'),
  fluidRow(selectizeInput('num_m_an',label = h4('Select m'),choices = num_m,selected = 90)),
  fluidRow(
    withSpinner(plotlyOutput('svm_rf_diff'))
  ),
  fluidRow(
    tags$blockquote(
      HTML(analysis),style='font-size:20px;'
    )
  )
)

##### ROC UI #####
roc <- fluidPage(
  titlePanel('ROC of SVM'),
  fluidRow(
    column(5,div(class='disAdj-border'),
           tags$blockquote(
             HTML(ROC1),style='font-size:20px;'
           ),
           tags$blockquote(
             HTML(ROC2),style='font-size:20px;'
           )
           ),
    column(5,tags$img(src='ROC.png'))
  )
)



##### introduction tab #####
introduction <- fluidPage(
  titlePanel('Reference Review'),
  tags$a("Predicting Stock Price Direction using Support Vector Machines",href="https://www.cs.princeton.edu/sites/default/files/uploads/saahil_madge.pdf"
         ,style='font-size:25px'),
  fluidRow(
    tags$br(),
    tags$blockquote(
      HTML(intro1),style='font-size:20px'
    ),
    tags$blockquote(
      HTML(intro2),style='font-size:20px'
    ),
    tags$blockquote(
      HTML(intro3),style='font-size:20px'
    )
  ),
  fluidRow(
    column(5,
           tags$div(tags$img(src='am_o.png',width = "580px", height = "400px"))
          ),
    column(1
          ),
    column(5,
           tags$div(tags$img(src='annm_o.png',width = "580px", height = "400px"))
          )
  )
)


##### combine all the UI #####
ui <- dashboardPage(
  dashboardHeader(title = 'SPDForecast'),
  dashboardSidebar(
    tags$head(
      tags$link(rel = 'stylesheet',type = 'text/css', href = 'custom.css')
    ),
    sidebarMenu(
      menuItem('Introduction',tabName = 'intro', icon = icon('comment')),
      menuItem('Data', icon = icon('folder-open'),startExpanded=TRUE,
               div(class='disAdj'),
               menuSubItem(span(class='submenu','Original Data'),tabName = 'odata'),
               div(class='border',style='border-bottom: 1px solid #222d32;'),
               div(class='disAdj'),
               menuSubItem(span(class='submenu','Processed Data'),tabName = 'pdata')),
      menuItem('SVM Model',tabName = 'svm', icon = icon('pause')),
      menuItem('RF Model',tabName = 'model', icon = icon('barcode')),
      menuItem('More Analysis', icon = icon('list-alt'),startExpanded=TRUE,
               div(class='disAdj'),
               menuSubItem(span(class='submenu','Result Comparison'),tabName = 'rcp'),
               div(class='border',style='border-bottom: 1px solid #222d32;'),
               div(class='disAdj'),
               menuSubItem(span(class='submenu','ROC of SVM'),tabName = 'roc'))
    ),
    div(class='disAdj-border'),
    div(class='border',style='border-bottom: 1px solid #2c3b41;'),
    div(class='user-panel',
        div(class='pull-left image',
            img()),
        div(class='pull-left info',
            p(a('Zixiao Zhang')),
            p('zz2500@columbia.edu'))
        ),
    div(class='user-panel',
        div(class='pull-left image',
            img()),
        div(class='pull-left info',
            p(a('Xuelun Li')),
            p('xl2678@columbia.edu'))
    )
        
    
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'intro',
              introduction
              ),
      tabItem(tabName = 'odata',
              originalData
              ),
      tabItem(tabName = 'pdata',
              processedData
              ),
      tabItem(tabName = 'svm',
              svmresult
              # some layout
              ),
      tabItem(tabName = 'model',
              rfresult
              ),
      tabItem(tabName = 'rcp',
              analysis
              ),
      tabItem(tabName = 'roc',
              roc
              )
      
    )
  )
)