
##### This file contains functions that use ggplot to create our result graph on the website
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


### rf plot ###
rf_plot = function(data){
  rf.plot = ggplot(data=data, aes(fill= as.factor(data$n2), y=mean, x=as.factor(data$n1))) + 
    geom_bar(position="dodge", stat="identity", width=0.7) + xlab("n1") + 
    ggtitle("Random Forest Mean Prediction Accuracy") +
    scale_fill_brewer(palette = "PRGn", name = "n2") + theme_bw() +
    geom_errorbar(aes(ymax=max, ymin=min), position="dodge", width=0.7,size=.15)
  return(rf.plot)
}


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

