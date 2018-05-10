##### this file is used for feature extraction and model training and testing #####
##### we also implement 10-fold cross validation and draw the roc of our best tested model #####
library(xts)
library(quantmod)
library(e1071)
library(TTR)
library(plyr)
library(ggplot2)
library(ddalpha)
library(caret)
library(pROC)

################### COLLECT DATA ####################
stockData <- new.env()
Stocks <- c("MSFT","CERN","AAPL","AMAT","TXN","CA","KLAC","LRCX","MU",
            "INTC","ADI","WDC","ADBE","SYMC","CSCO","XLNX","QCOM",
            "INTU","NTAP","CTXS","CHKP","ADSK","CTSH","NVDA","AKAM",
            "GRMN","STX","BIDU")

getSymbols(Stocks, from='2007-01-01',to='2015-01-01')
getSymbols('^NDXT',from='2007-01-01',to='2015-01-01')

################### CREATE FEATURES ####################
days = c(5, 10, 20, 90, 270)
forecast = c(1, 5, 10, 20, 90, 270)

add_features = function(company, cname){
  ### fill missing data ###
  company = na.approx(company)
  ### add label ###
  cp = paste0(cname,'.Close')
  company = merge(company, lm1=lag(company[,cp], 1))
  company$label = ifelse(company[,paste0(cp,'.1')]>company[,cp], -1, 1)
  company$dr = Delt(company[, paste0(cp,'.1')], company[,cp], k = 0)
  
  for (i in days){
    company = add_volatility(i, company)
  }
  
  for (i in days){
    company = add_momentum(i, company)
  }
  company <- company[,!names(company) %in% c(paste0(cp,'.1'),'label')]
  
  ### add labels ###
  for (j in forecast){
    company = merge(company, ifelse(lag(company[,cp], -j)>company[,cp], 1, -1))
  }
  
  ### change headers ###
  colnames(company) = c("Open", "High", "Low", "Close", "Volume", "Adjusted",
                        "DailyReturn", "Volatility.5", "Volatility.10", 
                        "Volatility.20", "Volatility.90", "Volatility.270", 
                        "Momentum.5", "Momentum.10", "Momentum.20", "Momentum.90",
                        "Momentum.270", "Label.1", "Label.5", "Label.10",
                        "Label.20", "Label.90", "Label.270")
  return (company)
}

### add volatility ###
add_volatility = function(i, company){
  company = merge(company, lm3=SMA(company[,'dr'], n = i))
  return (company)
}

### add momentum ###
add_momentum = function(i, company){
  company = merge(company, lm4=SMA(company[,'label'], n = i))
  return (company)
}

################### ADD TO INDEX/STOCKS ####################
add_features_to_all = function(cname){
  com = get(cname, pos=stockData)  # get data from stockData environment  
  com = add_features(com, cname)
  return (com)
}

### add features to index ###
NDXTset = add_features_to_all("NDXT") 

### add features to data ###
for (cname in Stocks){
  #print(cname)
  com_with_features = add_features_to_all(cname)
  #print(head(com_with_features))
  assign(paste0(cname,"set"), com_with_features)
}

################### DATASET ####################
### extract data for different n ###
extract_features = function(name, i){
  remain.cols = c("Close", paste0("Volatility.", i), paste0("Momentum.", i), 
                  "Label.1", "Label.5", "Label.10", "Label.20", "Label.90", 
                  "Label.270")
  sub = get(paste0(name,'set'), pos=stockData)[, remain.cols]
  return (sub)
}

for (i in days){
  assign(paste0("NDXTset.",i), extract_features('NDXT',i))
}

for (com in Stocks){
  for (i in days){
    assign(paste0(com,"set.",i), extract_features(com, i))
  }
}

################### MODEL ####################
create_train_test_data = function(n1, n2, n, m){
  train = NULL
  test = list()
  ### index template ###
  index.pre.set = get(paste0('NDXTset.',n1), pos=stockData)[(n+1):(2014-m)]
  index.set = index.pre.set[, names(index.pre.set) %in% 
                              c(paste0('Volatility.',n1), paste0('Momentum.',n1))]
  #print(index.set)
  colnames(index.set) = c("index.Volatility", "index.Momentum")
  index.train.set = index.set['/2012']
  index.test.set = index.set['2013/']
  
  ### stock template ###### create train/test dataset ###
  for (i in 1:length(Stocks)){
    com = Stocks[i]
    stock.pre.set = get(paste0(com,'set.',n2), pos=stockData)[(n+1):(2014-m)]
    stock.set = stock.pre.set[, names(stock.pre.set) %in%
                                c(paste0('Volatility.',n2), paste0('Momentum.',n2),
                                  paste0('Label.', m))]
    colnames(stock.set) = c("stock.Volatility", "stock.Momentum", "Label")
    stock.train.set = stock.set['/2012']
    stock.test.set = stock.set['2013/']
    temp.train = merge(index.train.set, stock.train.set)
    train  = rbind(train, temp.train)
    temp.test = merge(index.test.set, stock.test.set)
    test[[i]] = temp.test
  }
  return (list(train, test))
}

create_svm_model = function(n1, n2, m){
  n = max(n1, n2)
  result = create_train_test_data(n1, n2, n, m)
  train = result[[1]]
  test = result[[2]]
  svm.model = svm(Label~., data = train, type="C-classification", 
                  kernel="radial", degree = 3, gamma = 0.25)
  return (list(svm.model, test))
}

accuracy = function(model, data){
  pred = predict(model, data[,-5])
  cm = as.matrix(table(pred = pred, true = data[,5]))
  n = sum(cm)
  accuracy = sum(diag(cm)) / n 
  return (accuracy)
}

accuracy_evaluate = function(model, test){
  acc = c()
  for (idx in seq(length(Stocks))){
    acc = c(acc, accuracy(model, test[[idx]]))
    #print(idx)
    #print(acc[[idx]])
  }
  acc.mean = mean(acc)
  acc.median = median(acc)
  acc.max = max(acc)
  acc.min = min(acc)
  return (list(acc.mean, acc.median, acc.max, acc.min))
}

for (m in forecast){
  for (n1 in days){
    for (n2 in days){
      print(paste0(m, n1, n2))
      model.para = create_svm_model(n1, n2, m)
      svm.model = model.para[[1]]
      test = model.para[[2]]
      assign(paste0('svm.', n1, '.', n2, '.pre', m), svm.model)
      assign(paste0('accuracy.', n1, '.', n2, '.pre', m), accuracy_evaluate(svm.model, test))
    }
  }
}

######### saving data ##############
data = get(paste0('accuracy.', 5, '.', 5, '.pre', 1), pos=stockData)
line = list(1, 5, 5, data[[1]], data[[2]], data[[3]], data[[4]])
df = data.frame(matrix(unlist(line), nrow=1, byrow=T))

col.heading = c('m', 'n1', 'n2', 'mean', 'median', 'max', 'min')
l = list()
i = 2
for (m in forecast){
  for (n1 in days){
    for (n2 in days){
      data = get(paste0('accuracy.', n1, '.', n2, '.pre', m), pos=stockData)
      line = list(m, n1, n2, data[[1]], data[[2]], data[[3]], data[[4]])
      l[[i]] = line
      i = i+1
    }
  }
}
df = data.frame(matrix(unlist(l), nrow=150, byrow=T))
names(df) = col.heading
write.csv(df,file="/Users/arrowlittle/Desktop/stock/modelresult.csv")


for (m in forecast){
  for (n1 in days){
    for (n2 in days){
      model = get(paste0('svm.', n1, '.', n2, '.pre', m), pos=stockData)
      write.svm(model, svm.file = paste0('/Users/arrowlittle/Desktop/stock/svm.', n1, '.', n2, '.pre', m, ".svm"), 
                scale.file = paste0('/Users/arrowlittle/Desktop/stock/svm.', n1, '.', n2, '.pre', m, ".scale"))
    }
  }
}
################### RANDOM FOREST ################
set.seed(111)
create_rf_model = function(n1, n2, m){
  n = max(n1, n2)
  result = create_train_test_data(n1, n2, n, m)
  train = result[[1]]
  test = result[[2]]
  rf.model = randomForest(as.factor(Label)~., data = train, importance=T, ntree=100)
  return (list(rf.model, test))
}

rf.model = get(paste0('rf.', n1, '.', n2, '.pre', m), pos=stockData)


################### READ PREDICTION ACCURACY ############
rf.data = as.data.frame(read.csv(file="/Users/arrowlittle/Desktop/stock/rf_modelresult_t100.csv"))[,-1]
svm.data = as.data.frame(read.csv(file="/Users/arrowlittle/Desktop/stock/modelresult.csv"))[,-1]
rf.data = rf.data[126:150,-1]
svm.data = svm.data[126:150,-1]

################### VISUALIZATION #################
### svm plot ###
svm_plot = function(data){
  svm.plot = ggplot(data=svm.data, aes(fill=as.factor(n2), y=mean, x=as.factor(n1))) + 
    geom_bar(position="dodge", stat="identity", width=0.7) + xlab("n1") + 
    ggtitle("SVM Mean Prediction Accuracy") +
    scale_fill_brewer(palette = "RdYlBu", name = "n2") + theme_bw() +
    geom_errorbar(aes(ymax=max, ymin=min), position="dodge", width=0.7,size=.15)
  return (svm.plot)
}
svm_plot(svm.data)

### rf plot ###
rf_plot = function(data){
  rf.plot = ggplot(data=rf.data, aes(fill=as.factor(n2), y=mean, x=as.factor(n1))) + 
    geom_bar(position="dodge", stat="identity", width=0.7) + xlab("n1") + 
    ggtitle("Random Forest Mean Prediction Accuracy") +
    scale_fill_brewer(palette = "PRGn", name = "n2") + theme_bw() +
    geom_errorbar(aes(ymax=max, ymin=min), position="dodge", width=0.7,size=.15)
  return(rf.plot)
}
rf_plot(rf.data)

### combined plot ###
svm_rf_plot = function(svm.data, rf.data){
  svm.rf.data = cbind(svm.data[1:2], svm.data[3:6]-rf.data[3:6])
  svm.rf.plot = ggplot() + 
    geom_bar(data=svm.rf.data, aes(fill=as.factor(n2), y=mean, x=as.factor(n1)),
             position="dodge", stat="identity", width=0.7) + xlab("n1") +
    scale_fill_brewer(palette = "RdYlBu", name = "n2") + theme_bw() +
    ggtitle("Prediction Accuracy Difference Between SVM and RF m=270")
  return(svm.rf.plot)
}
svm_rf_plot(svm.data, rf.data)

#################### CROSS VALIDATION ###############
n1 = 20
n2 = 5
n = max(n1, n2)
m = 90

create_cv_data = function(n1, n2, n, m){
  cv.set = NULL
  ### index template ###
  index.pre.set = get(paste0('NDXTset.',n1), pos=stockData)[(n+1):(2014-m)]
  index.set = index.pre.set[, names(index.pre.set) %in% 
                              c(paste0('Volatility.',n1), paste0('Momentum.',n1))]
  #print(index.set)
  colnames(index.set) = c("index.Volatility", "index.Momentum")
  
  ### stock template ###### create train/test dataset ###
  for (i in 1:length(Stocks)){
    com = Stocks[i]
    stock.pre.set = get(paste0(com,'set.',n2), pos=stockData)[(n+1):(2014-m)]
    stock.set = stock.pre.set[, names(stock.pre.set) %in%
                                c(paste0('Volatility.',n2), paste0('Momentum.',n2),
                                  paste0('Label.', m))]
    colnames(stock.set) = c("stock.Volatility", "stock.Momentum", "Label")
    temp.set = merge(index.set, stock.set)
    cv.set  = rbind(cv.set, temp.set)
  }
  return (cv.set)
}
cv.data = as.data.frame(create_cv_data(20, 5, 20, 90))
cv.data$Label = as.factor(cv.data$Label)
levels(cv.data$Label) <- c("dec", "inc")

write.csv(cv.data, file="/Users/arrowlittle/Desktop/stock/cvdata.csv")

################### cv svm model ###
train_control <- trainControl(method="cv", number=10, 
                                summaryFunction=twoClassSummary, savePredictions = T,
                                classProbs = T)
grid <- expand.grid(sigma = c(0.25), C = c(1))
cv.svm.model <- train(Label~., data=cv.data, tuneGrid = grid, metric = "ROC",
                        trControl=train_control, method = "svmRadial")
roc.curve =roc(as.numeric(cv.svm.model$pred$obs), as.numeric(cv.svm.model$pred$inc), direction="<")
plot(roc.curve, col=586, lwd=3, main="ROC Curve", print.auc=TRUE)
    




