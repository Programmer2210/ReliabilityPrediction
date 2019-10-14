
##################################################################
# This R Script is applied logistic regression algorithm to classify
# different categories of bugs using 10-Times 10-Fold cross validation
# methodology. Result obtained is accuracy, precission and recall 
# of predictors: 
# Independent Variables are: Martin Package Level Metrics 
# Dependent  Variables are seperately: Bugs, MajorBugs, Critical Bugs, Non Trival Bugs
#####################################################################

data2=NULL
#Reading the processed and compiled data having Martin Metrics and Bugs information
data2 <- read.csv("pdepkgmtrcs2.csv")
data2
colnames(data2)

data2=data.frame(data2)
colnames(data2)
#Extracting the relavent variables 
data2<-data2[, c("A", "N", "criticalBugs", "Ce", "D", "I", "Ca")]
data2
library("dplyr")

attach(data2)
#Manually producing 10 folds of data 
n.folds<-10
folds <- cut(sample(seq_len(nrow(data2))),  breaks=n.folds, labels=FALSE)
all.confusion.tables
TP
FN
FP
TN
accuracy
precission
recall
MFM
all.confusion.tables=NULL
TP=NULL
FN=NULL
FP=NULL
TN=NULL
MFM=NULL
accuracy=NULL
precission=NULL
recall=NULL


sensitivity
specificity

sensitivity=NULL
specificity=NULL

thresh.pred=NULL
logit.prob=NULL

all.confusion.tables <- list()

colnames(data2)

#Looping through test and train data 10 times using fo loop 

for(i in seq_len(n.folds))
{
  
  
  train <-filter(data2, folds !=i)
  test <- filter(data2, folds==i)
  
  #Logistic Regression Algorithm for different catorgires of bugs
  #Remember all these are exectuted seperetely for different categories of bugs
   
  glm.model <- glm((criticalBugs>0) ~   N+A+Ce+Ca+I+D,  data=train, family= "binomial")
  #glm.model <- glm((bugs>0) ~   N+A+Ce+Ca+I+D,  data=train, family= "binomial")
  #glm.model <- glm((nonTrivialBugs>0) ~   N+A+Ce+Ca+I+D,  data=train, family= "binomial")
 # glm.model <- glm((majorBugs>0) ~   N+A+Ce+Ca+I+D,  data=train, family= "binomial")
  
  model.pred<-predict(glm.model, newdata=test[,-3])
  
  thresh.pred <-model.pred>=0.5
  #mapply(result,thresh.pred, SIMPLIFY = TRUE)
  
  all.confusion.tables[[i]] <- table(factor(test$criticalBugs>0, levels=c(F,T)), factor(thresh.pred, levels=c(F,T)))
  
  TN[i]=all.confusion.tables[[i]][1,1]
  FN[i]=all.confusion.tables[[i]][2,1]
  FP[i]=all.confusion.tables[[i]][1,2]
  TP[i]=all.confusion.tables[[i]][2,2]
  
  
  accuracy[i] <- (TP[i] + TN[i]) / (TN[i] + FN[i] + FP[i] + TP[i])
  precission[i] <-  (TP[i]) / (TP[i] + FP[i])
  recall[i] <- TP[i] / (TP[i] + FN[i])
  
} 

#After 10 iterations, calculating the performance metrics 


all.confusion.tables

accuracy[is.nan(accuracy)]<-0
precission[is.nan(precission)]<-0
recall[is.nan(recall)]<-0

ac<-summary(accuracy)
ac<-mean(accuracy)
pr<-mean(precission)
rec<-mean(recall)

ac
pr
rec





