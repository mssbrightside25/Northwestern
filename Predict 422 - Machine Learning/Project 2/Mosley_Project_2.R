library(readr)
library(ROCR)

#read in classification data
class.data<-read.csv("classification-output-data.csv")

#Use the table() function to get the raw confusion matrix for this scored dataset. Make sure you
#understand the output. In particular, do the rows represent the actual or predicted class? The
#columns?
class.data <- class.data[,c("class","scored.class","scored.probability")]

confusion.matrix <- table(class.data$scored.class, class.data$class)
con.mat <- apply(apply(confusion.matrix, 1, rev), 1, rev)

#set column and row names for con.mat
colnames(con.mat) <- c("Class (Actual) 1", "Class (Actual) 0"); rownames(con.mat) <- c("Scored (Predicted) 1", "Scored (Predicted) 0"); con.mat

#Write a function that take three inputs, the data set as a dataframe with actual and predicted
#classifications identified, and returns the accuracy of the predictions.
#dt = datatable
#df = dataframe
class.accuracy <- function(df, predicted, actual) {
  dt <- table(df[ ,predicted], df[ ,actual])
  confusion.matrix2 <- apply(apply(dt, 1, rev), 1, rev)
  TP <- confusion.matrix2[1,1]; FP <- confusion.matrix2[1,2]
  FN <- confusion.matrix2[2,1]; TN <- confusion.matrix2[2,2]
  accuracy <- (TP + TN) / (TP + FP + TN + FN)
  return (accuracy)
}

#Write a function that take three inputs, the data set as a dataframe with actual and predicted
#classifications identified, and returns the classification error rate of the predictions.
#dt = datatable
#df = dataframe
class.error <- function(df, predicted, actual) {
  dt <- table(df[ ,predicted], df[ ,actual])
  confusion.matrix2 <- apply(apply(dt, 1, rev), 1, rev)
  TP <- confusion.matrix2[1,1]; FP <- confusion.matrix2[1,2]
  FN <- confusion.matrix2[2,1]; TN <- confusion.matrix2[2,2]
  error <- (FP + FN) / (TP + FP + TN + FN)
  return (error)
}

#Verify that you get an accuracy and an error rate that sums to one.
class.accuracy(class.data, "scored.class", "class") + class.error(class.data, "scored.class", "class")
  # [1] 1 -- yes

#Write a function that take three inputs, the data set as a dataframe with actual and predicted
#classifications identified, and returns the precision of the predictions.
#dt = datatable
#df = dataframe
class.precision <- function(df, predicted, actual) {
  dt <- table(df[ ,predicted], df[ ,actual])
  confusion.matrix2 <- apply(apply(dt, 1, rev), 1, rev)
  TP <- confusion.matrix2[1,1]; FP <- confusion.matrix2[1,2]
  FN <- confusion.matrix2[2,1]; TN <- confusion.matrix2[2,2]
  precision <- TP / (TP + FP)
  return (precision)
}

#Write a function that take three inputs, the data set as a dataframe with actual and predicted classifications
#identified, and returns the sensitivity of the predictions. Sensitivity is also known as recall.
#dt = datatable
#df = dataframe
class.sensitivity <- function(df, predicted, actual) {
  dt <- table(df[ ,predicted], df[ ,actual])
  confusion.matrix2 <- apply(apply(dt, 1, rev), 1, rev)
  TP <- confusion.matrix2[1,1]; FP <- confusion.matrix2[1,2]
  FN <- confusion.matrix2[2,1]; TN <- confusion.matrix2[2,2]
  sensitivity <- TP / (TP + FN)
  return (sensitivity)
}

#Write a function that take three inputs, the data set as a dataframe with actual and predicted classifications
#identified, and returns the specificity of the predictions.
#dt = datatable
#df = dataframe
class.specificity <- function(df, predicted, actual) {
  dt <- table(df[ ,predicted], df[ ,actual])
  confusion.matrix2 <- apply(apply(dt, 1, rev), 1, rev)
  TP <- confusion.matrix2[1,1]; FP <- confusion.matrix2[1,2]
  FN <- confusion.matrix2[2,1]; TN <- confusion.matrix2[2,2]
  specificity <- TN / (TN + FP)
  return (specificity)
}

#Write a function that take three inputs, the data set as a dataframe with actual and predicted classifications
#identified, and returns the F1 score of the predictions.
#dt = datatable
#df = dataframe
class.f1.score <- function(df, predicted, actual) {
  dt <- table(df[ ,predicted], df[ ,actual])
  confusion.matrix2 <- apply(apply(dt, 1, rev), 1, rev)
  TP <- confusion.matrix2[1,1]; FP <- confusion.matrix2[1,2]
  FN <- confusion.matrix2[2,1]; TN <- confusion.matrix2[2,2]
  f1.score <- (2 *(TP / (TP + FP)) * (TP / (TP + FN))) / ((TP / (TP + FP)) + (TP / (TP + FN)))
  return (f1.score)
}

#Let's consider the following question: What are the bounds on the F1 score? Show that the F1
#score will always be between 0 and 1. (Hint: If 0<a<1 and 0<b<1 then ab<a.)

#Write a function that generates an ROC curve from a data set using two inputs, a true classification
#column (i.e., class) and a probability column (i.e., scored.probability). Your function should return
#the plot of the ROC curve and the calculated area under the ROC curve (AUC). Note that I
#recommend using a sequence of thresholds ranging from 0 to 1 at 0.01 intervals.
ROC.curve <- function(df, probabilities, actual) {
  thresholds <- seq(0, 1, 1/180) #0-180 data points
  
  true.pos.rate <- false.pos.rate <- numeric(length(thresholds))
  pos <- sum(df[,actual] == 1) #positive sum
  neg <- sum(df[,actual] == 0) #negative sum
  
  #for loop
  for (i in 1:length(thresholds)) {
    
    #Upper bound probability threshold 
    class.subset <- subset(df, df[ ,probabilities] <= thresholds[i])
   
    #Condition numerator 
    
    true.pos <- sum(class.subset[class.subset[,actual] == 1, probabilities] > 0.1) #TP
    true.neg <- sum(class.subset[class.subset[,actual] == 0, probabilities] <= 0.1) #TN
    false.pos <- sum(class.subset[class.subset[,actual] == 0, probabilities] > 0.1) #FP
    false.neg <- sum(class.subset[class.subset[,actual] == 1, probabilities] <= 0.1) #FN
    
    # true.pos <- sum(class.subset[class.subset[,actual] == 1, probabilities] > 0.5) #TP
    # true.neg <- sum(class.subset[class.subset[,actual] == 0, probabilities] <= 0.5) #TN
    # false.pos <- sum(class.subset[class.subset[,actual] == 0, probabilities] > 0.5) #FP
    # false.neg <- sum(class.subset[class.subset[,actual] == 1, probabilities] <= 0.5) #FN
  
    # true.pos <- sum(class.subset[class.subset[,actual] == 1, probabilities] > 0.3) #TP
    # true.neg <- sum(class.subset[class.subset[,actual] == 0, probabilities] <= 0.3) #TN
    # false.pos <- sum(class.subset[class.subset[,actual] == 0, probabilities] > 0.3) #FP
    # false.neg <- sum(class.subset[class.subset[,actual] == 1, probabilities] <= 0.3) #FN
    
    # true.pos <- sum(class.subset[class.subset[,actual] == 1, probabilities] > 0.1) #TP
    # true.neg <- sum(class.subset[class.subset[,actual] == 0, probabilities] <= 0.3) #TN
    # false.pos <- sum(class.subset[class.subset[,actual] == 0, probabilities] > 0.5) #FP
    # false.neg <- sum(class.subset[class.subset[,actual] == 1, probabilities] <= 0.7) #FN
    
    # true.pos <- sum(class.subset[class.subset[,actual] == 1, probabilities] > 0.7) #TP
    # true.neg <- sum(class.subset[class.subset[,actual] == 0, probabilities] <= 0.7) #TN
    # false.pos <- sum(class.subset[class.subset[,actual] == 0, probabilities] > 0.7) #FP
    # false.neg <- sum(class.subset[class.subset[,actual] == 1, probabilities] <= 0.7) #FN
    
    true.pos.rate[i] <- 1 - (true.pos + false.neg) / pos #sensitivity
    false.pos.rate[i] <- 1 - (true.neg + false.pos) / neg #specificity
  }
  
  #plot ROC
  plot(false.pos.rate, true.pos.rate, type = "l", lwd = 2,
       xlab = "False.Pos.Rate (1-specificity)", 
       ylab = "True.Pos.Rate (sensitivity)")
  
  #add straight line through plot
  abline(0, 1)
  
  false.pos.rate.diff <- c(abs(diff(false.pos.rate)), 0); true.pos.rate.diff <- c(abs(diff(true.pos.rate)), 0) #diff() returns n-1 suitably lagged and iterated differences
 
  #calculate the area under the ROC curve using the formula for the area of a trapezoid
  AUC <- sum(true.pos.rate * false.pos.rate.diff) - sum(true.pos.rate.diff * false.pos.rate.diff) / 2
  return (AUC)
}

#Use your created R functions and the provided classification output data set to produce all of the
#classification metrics discussed above.

#error
class.error(class.data, "scored.class", "class")

#accuracy 
class.accuracy(class.data, "scored.class", "class")

#precision
class.precision(class.data, "scored.class", "class")

#sensitivity
class.sensitivity(class.data, "scored.class", "class")

#specificity
class.specificity(class.data, "scored.class", "class")

#f1 score
class.f1.score(class.data, "scored.class", "class")

#ROC curve
ROC.curve(class.data, "scored.probability", "class")

#Investigate the caret package. In particular, consider the functions confusionMatrix(),
#sensitivity(), and specificity(). Apply the functions to the data set. How do the results compare
#with your own functions?
library(caret) #Classification and Regression Training

#drop missing data
data = na.omit(class.data)

#confusionMatrix () calculates a cross tabulation of obs. and pred. classes with assoc. stats
confusionMatrix(class.data$scored.class, class.data$class, class.data$scored.probability)
  #receiving error: `data` and `reference` should be factors with the same levels.

#caret package using sensitivity function
sensitivity(factor(class.data$scored.class), factor(class.data$class), positive = "1")

#caret package using specificity function
specificity(factor(class.data$scored.class), factor(class.data$class), negative = "0")

#Investigate the pROC package. Use it to generate an ROC curve for the data set. How do the
#results compare with your own functions?
library(pROC)

plot(roc(class.data$class, class.data$scored.probability, smooth=F))
auc(class.data$class, class.data$scored.probability)
