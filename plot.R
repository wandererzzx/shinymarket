################### READ RANDOM FOREST ############
# rf.data = as.data.frame(read.csv(file="/Users/arrowlittle/Desktop/stock/rf_modelresult_t100.csv"))[,-1]
# svm.data = as.data.frame(read.csv(file="/Users/arrowlittle/Desktop/stock/modelresult.csv"))[,-1]
# rf.data = rf.data[101:125,-1]
# svm.data = svm.data[101:125,-1]

################### VISUALIZATION #################
### svm plot ###
svm_plot = function(data){
  svm.plot = ggplot(data=data, aes(fill=as.factor(data$n2), y=mean, x=as.factor(data$n1))) + 
    geom_bar(position="dodge", stat="identity", width=0.7) + xlab("n1") + 
    ggtitle(paste("SVM Mean Prediction Accuracy ( m = ",data$m,")")) +
    scale_fill_brewer(palette = "RdYlBu", name = "n2") + theme_bw() +
    geom_errorbar(aes(ymax=max, ymin=min), position="dodge", width=0.7,size=.15)
  return (svm.plot)
}
# svm_plot(svm.data)

### rf plot ###
rf_plot = function(data){
  rf.plot = ggplot(data=data, aes(fill= as.factor(data$n2), y=mean, x=as.factor(data$n1))) + 
    geom_bar(position="dodge", stat="identity", width=0.7) + xlab("n1") + 
    ggtitle("Random Forest Mean Prediction Accuracy") +
    scale_fill_brewer(palette = "PRGn", name = "n2") + theme_bw() +
    geom_errorbar(aes(ymax=max, ymin=min), position="dodge", width=0.7,size=.15)
  return(rf.plot)
}
# rf_plot(rf.data)

### combined plot ###
diff_plot = function(data.svm,data.rf){
  svm.rf.data = cbind(data.svm[1:2], data.svm[3:6]-data.rf[3:6])
  svm.rf.plot = ggplot() +
    geom_bar(data=svm.rf.data, aes(fill=as.factor(data.svm$n2), y=mean, x=as.factor(data.svm$n1)),
             position="dodge", stat="identity", width=0.7) + xlab("n1") +
    scale_fill_brewer(palette = "RdYlBu", name = "n2") + theme_bw() +
    ggtitle("Difference Mean Prediction Accuracy Between SVM and RF")
  return(svm.rf.plot)
}

