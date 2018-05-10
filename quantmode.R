library(quantmod)
library(xts)
library(e1071)
library(TTR)
library(data.table)
library(randomForest)



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
  
  for (j in forecast){
    company = merge(company,ifelse(lag(company[,cp], -j)>company[,cp],1,-1))
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
  print(cname)
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
  # print(index.set)
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

# wd = "D:/ColumbiaDoc/MSEE/StatisticalLearning/project/marketSVM"
# 
# create_dataset = function(n1,n2,m){
#   n = max(n1,n2)
#   result = create_train_test_data(n1,n2,n,m)
#   train = result[[1]]
#   test = result[[2]]
#   dir.create(paste0(wd,'/train'))
#   write.csv(train,file=paste0(wd,'/train/',n1,n2,m,'_train.csv'))
# }


for(m in forecast){
  for (n1 in days){
    for (n2 in days){
      create_dataset(n1,n2,m)
    }
  }
}


# create_svm_model = function(n1, n2, m){
#   n = max(n1, n2)
#   result = create_train_test_data(n1, n2, n, m)
#   train = result[[1]]
#   test = result[[2]]
#   svm.model = svm(Label~., data = train, type = "C-classification")
#   return (list(svm.model, test))
# }


####### create randomForest model ######
set.seed(111)
create_rf_model = function(n1, n2, m){
  n = max(n1, n2)
  result = create_train_test_data(n1, n2, n, m)
  train = result[[1]]
  test = result[[2]]
  rf.model = randomForest(as.factor(Label)~., data = train,importance=T,ntree=100)
  return (list(rf.model, test))
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
      model.para = create_rf_model(n1, n2, m)
      rf.model = model.para[[1]]
      test = model.para[[2]]
      assign(paste0('rf.', n1, '.', n2, '.pre', m), rf.model)
      assign(paste0('accuracy.', n1, '.', n2, '.pre', m), accuracy_evaluate(rf.model, test))
    }
  }
}




# model.para = create_rf_model(270, 20, 5)
# rf.model = model.para[[1]]
# test = model.para[[2]]
# assign(paste0('rf.', 270, '.', 20, '.pre', 5), rf.model)







# for (m in 1){
#   for (n1 in 5){
#     for (n2 in 5){
#       model.para = create_svm_model(n1, n2, m)
#       svm.model = model.para[[1]]
#       test = model.para[[2]]
#       assign(paste0('svm.', n1, '.', n2, '.pre', m), svm.model)
#       # accuracy
#     }
#   }
# }
# 

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
write.csv(df,file="D:/ColumbiaDoc/MSEE/StatisticalLearning/project/marketSVM/data/RandomForest/rfmodelresult.csv")


for (m in forecast){
  for (n1 in days){
    for (n2 in days){
      model = get(paste0('svm.', n1, '.', n2, '.pre', m), pos=stockData)
      write.svm(model, svm.file = paste0('/Users/arrowlittle/Desktop/stock/svm.', n1, '.', n2, '.pre', m, ".svm"), 
                scale.file = paste0('/Users/arrowlittle/Desktop/stock/svm.', n1, '.', n2, '.pre', m, ".scale"))
    }
  }
}





