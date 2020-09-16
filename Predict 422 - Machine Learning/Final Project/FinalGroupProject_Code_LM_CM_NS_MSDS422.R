library(forecast) 
library(psych)

# load the data
charity <- read.csv(file.choose()) # load the "charity.csv" file
charity.t <- charity

#-------------------------------------------
# 1. EDA, CLEANING, and FEATURE ENGINEERING
#-------------------------------------------
df_train <- charity.t[charity$part=="train",]
df_valid <- charity.t[charity$part=="valid",]
df_test <- charity.t[charity$part=="test",]
df_train_damt = df_train[df_train$donr==1,] 


# EDA

# look at columns
colnames(charity.t)
head(charity.t)
summary(charity.t)
str(charity.t)

#look at training data
summary(df_train)
describe(df_train)

# look at distibutions of donr and damt responses
prop.table(table(df_train$donr)) # intentionally balanced
hist(df_train_damt$damt)


# look at number of NAs per column
apply(is.na(charity.t),2,sum)


# look at correlations with donr/damt variable across all data
library(corrplot)
df_tmp= subset(df_train, select=-c(ID, damt))
tokeep <- which(sapply(df_tmp,is.numeric))
df_numeric = df_tmp[ , tokeep]
corrplot(cor(df_numeric), method="circle")
cor(df_numeric[,"donr"], df_numeric)

# look at correlations with damt when conditioning on donr=1
df_tmp= subset(df_train_damt, select=-c(ID, donr))
tokeep <- which(sapply(df_tmp,is.numeric))
df_numeric = df_tmp[ , tokeep]
corrplot(cor(df_numeric), method="circle", na.label="0")
cor(df_numeric[,"damt"], df_numeric)

# look at predictor means across donr categories
colnames(df_train)
aggregate(df_train[, 2:21], list(df_train$donr), mean)

# look at damt means across select categorical variables
# condition on those that have donated!!
aggregate(df_train_damt$damt, list(df_train_damt$reg2), mean)
aggregate(df_train_damt$damt, list(df_train_damt$home), mean)
aggregate(df_train_damt$damt, list(df_train_damt$chld), mean)
aggregate(df_train_damt$damt, list(df_train_damt$wrat), mean)
aggregate(df_train_damt$damt, list(df_train_damt$genf), mean)
aggregate(df_train_damt$damt, list(df_train_damt$hinc), mean)
head(df_train)

# cross-tab
table(df_train$home, df_train$donr)

# scatterplots
colnames(charity.t)
plot(df_train_damt$rgif, df_train_damt$damt, main="rgif by damt", 
     xlab="rgif", ylab="damt")
plot(df_train_damt$lgif, df_train_damt$damt, main="lgif by damt", 
     xlab="lgif", ylab="damt")
plot(df_train_damt$agif, df_train_damt$damt, main="agif by damt", 
     xlab="agif", ylab="damt")
plot(df_train_damt$chld, df_train_damt$damt, main="chld by damt", 
     xlab="chld", ylab="damt")
plot(df_train_damt$reg4, df_train_damt$damt, main="reg4 by damt", 
     xlab="reg4", ylab="damt")


# overlapping histograms
multihist <- function(df , feature, grouping, positive, negative){
  values = df[[feature]]
  max_y = max(hist(values)$counts)
  max_x = max(values)
  hist(df[df$donr==positive,][[feature]], col=rgb(1,0,0,1/4),
       main=paste('Histogram for ', feature),
       xlim=c(0,max_x),xlab=feature,
       ylim=c(0,max_y))
  hist(df[df$donr==negative,][[feature]], xlim=c(0,max(values)),
       col=rgb(0,0,1,1/4), 
       add=T)
  legend("topright", c(as.character(1), as.character(0)),
         col=c(rgb(1,0,0,1/4), rgb(0,0,1,1/4)), lwd=10)
}
multihist(df_train, 'chld', 'donr', 1, 0)
multihist(df_train, 'hinc', 'donr', 1, 0)
multihist(df_train, 'home', 'donr', 1, 0)
multihist(df_train, 'wrat', 'donr', 1, 0)
multihist(df_train, 'reg2', 'donr', 1, 0)
multihist(df_train, 'avhv', 'donr', 1, 0)



# OUTLIER DETECTION, TRANSFORMATION, FEATURE CREATION
summary(df_train$avhv)
quantile(df_train$avhv, 0.99)
# individual histograms
hist(df_train$home)
hist(df_train$chld)
hist(df_train$hinc)
hist(df_train$genf)
hist(df_train$wrat)
hist(df_train$avhv)
hist(df_train$incm)
hist(df_train$inca)
hist(df_train$plow)
hist(df_train$npro)
hist(df_train$tgif)
hist(df_train$lgif)
hist(df_train$rgif)
hist(df_train$agif)
hist(df_train$tdon)
hist(df_train$tlag)
hist(charity.t$avhv)


# a matrix of histograms
# windows() # only works on Windows
x11() # Linux and mac
multi.hist(subset(charity.t, select-c(ID)))
dev.off()


# transform avhv
# log transform (suggested by professor) works well
# in addressing right-skewness, but
# let's also try BoxCox transformation instead
hist(log(charity.t$avhv))
lambda <- BoxCox.lambda(charity.t$avhv)  
hist(BoxCox(charity.t$avhv, lambda))
# since a vanilla log transform looks
# more normal, let's use that instead of BoxCox
charity.t$avhv = log(charity.t$avhv) 
hist(charity.t$avhv)

# transform plow using log transform
summary(charity.t$plow)
hist(charity.t$plow+1)
hist(log(charity.t$plow+1))
#charity.t$plow = log(charity.t$plow+1)

# xgboost feature importance
library(xgboost)
library(magrittr)

#xgboost for donr
# classification feature importance
p_c <- list(objective = "binary:logistic",
            booster = "gbtree",
            eval_metric = "auc",
            nthread = 8,
            eta = 0.025,
            max_depth = 6,
            min_child_weight = 19,
            gamma = 0,
            subsample = 0.8,
            colsample_bytree = 0.632,
            alpha = 0,
            lambda = 0.05,
            nrounds = 2000)


m_xgb_c = xgboost(params=p_c, data=data.matrix(df_train[,2:21]), 
                  label=df_train$donr, nrounds=p_c$nrounds)
cols = colnames(df_train[,2:21])
xgb.importance(cols, model=m_xgb_c) %>%
  xgb.plot.importance(top_n = 20)
head(predict(m_xgb_c, data.matrix(df_train[,2:21])))
head(df_train$donr)

# regression feature importance (condition on donr=1)
p_r <- list(objective = "reg:linear",
            booster = "gbtree",
            eval_metric = "rmse",
            nthread = 8,
            eta = 0.025,
            max_depth = 6,
            min_child_weight = 19,
            gamma = 0,
            subsample = 0.8,
            colsample_bytree = 0.632,
            alpha = 0,
            lambda = 0.05,
            nrounds = 5000)


m_xgb_r = xgboost(params=p_r, data=data.matrix(df_train_damt[,2:21]), 
                  label=df_train_damt$damt, nrounds=p_r$nrounds)
cols = colnames(df_train_damt[,2:21])
xgb.importance(cols, model=m_xgb_r) %>%
  xgb.plot.importance(top_n = 40)
xgb.importance(cols, model=m_xgb_r)
# Important: XGBoost for regression to predict damt
#           seems to produce different results on my Windows machine
#           versus my MAC machine.  MAC machine results
#           are incorrect despite using same version of XGBoost.  
#           GBM produces correct results on both machines
#           so we will run that next to identify
#           important features.

set.seed(1)
model.gbm1.r = gbm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                     avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                   data = subset(df_train_damt, select=c(-ID,-donr)), distribution = "gaussian",
                   n.trees = 10000, interaction.depth = 4)
summary(model.gbm1.r)
pred.valid.gbm1 <- predict(model.gbm1.r, n.trees=10000, 
                           newdata = subset(df_valid, donr==1, select=c(-ID,-donr,-damt))) # validation predictions
mean((df_valid[df_valid$donr==1,]$damt - pred.valid.gbm1)^2) # mean prediction error
# GBM identifies rgif, lgif, agif, reg4, and chld
#  as most predictive of damt variable on both
#  of my machines.
#  Results are consistent with what we see 
#   in the correlation matrix produced
#   by conditioning on donr=1



#-------------------------------------------
# END OF EDA
#-------------------------------------------


#Set up data for analysis: train/validate/test
data.train <- charity.t[charity$part=="train",]
x.train <- data.train[,2:21]
c.train <- data.train[,22] # donr
n.train.c <- length(c.train) # 3984
y.train <- data.train[c.train==1,23] # damt for observations with donr=1
n.train.y <- length(y.train) # 1995

data.valid <- charity.t[charity$part=="valid",]
x.valid <- data.valid[,2:21]
c.valid <- data.valid[,22] # donr
n.valid.c <- length(c.valid) # 2018
y.valid <- data.valid[c.valid==1,23] # damt for observations with donr=1
n.valid.y <- length(y.valid) # 999

data.test <- charity.t[charity$part=="test",]
n.test <- dim(data.test)[1] # 2007
x.test <- data.test[,2:21]

x.train.mean <- apply(x.train, 2, mean)
x.train.sd <- apply(x.train, 2, sd)
x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) # standardize to have zero mean and unit sd
apply(x.train.std, 2, mean) # check zero mean
apply(x.train.std, 2, sd) # check unit sd

#Datasets for model building
data.train.std.c <- data.frame(x.train.std, donr=c.train) # to classify donr
data.train.std.y <- data.frame(x.train.std[c.train==1,], damt=y.train) # to predict damt when donr=1

x.valid.std <- t((t(x.valid)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.valid.std.c <- data.frame(x.valid.std, donr=c.valid) # to classify donr
data.valid.std.y <- data.frame(x.valid.std[c.valid==1,], damt=y.valid) # to predict damt when donr=1

x.test.std <- t((t(x.test)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.test.std <- data.frame(x.test.std)

##### CLASSIFICATION MODELING ######

# linear discriminant analysis
# PROVIDED BY PROFESSOR

library(MASS)

model.lda1 <- lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c) # include additional terms on the fly using I()

# Note: strictly speaking, LDA should not be used with qualitative predictors,
# but in practice it often is if the goal is simply to find a good predictive model

post.valid.lda1 <- predict(model.lda1, data.valid.std.c)$posterior[,2] # n.valid.c post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.lda1 <- cumsum(14.5*c.valid[order(post.valid.lda1, decreasing=T)]-2)
plot(profit.lda1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.lda1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.lda1)) # report number of mailings and maximum profit
# 1329.0 11624.5

cutoff.lda1 <- sort(post.valid.lda1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.lda1 <- ifelse(post.valid.lda1>cutoff.lda1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.lda1, c.valid) # classification table
#               c.valid
#chat.valid.lda1   0   1
#              0 675  14
#              1 344 985
# check n.mail.valid = 344+985 = 1329
# check profit = 14.5*985-2*1329 = 11624.5

#----------------------------------------------------------------------------------------------
#LDA MODEL 2 - USING ONLY 5 MOST IMPORTANT VARIABLES FROM XGBOOST VARIABLE IMPORTANCE OUTPUT
model.lda2 <- lda(donr ~ home + chld + wrat + hinc + reg2, 
                  data.train.std.c) 
# using home, chld, wrat, hinc, reg2

# Note: strictly speaking, LDA should not be used with qualitative predictors,
# but in practice it often is if the goal is simply to find a good predictive model

post.valid.lda2 <- predict(model.lda2, data.valid.std.c)$posterior[,2] # n.valid.c post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.lda2 <- cumsum(14.5*c.valid[order(post.valid.lda2, decreasing=T)]-2)
plot(profit.lda2) # see how profits change as more mailings are made
n.mail.valid2 <- which.max(profit.lda2) # number of mailings that maximizes profits
lines(c(n.mail.valid2, n.mail.valid2), c(0, max(profit.lda2)), lty = 2, col = "red")
c(n.mail.valid2, max(profit.lda2)) # report number of mailings and maximum profit
# 1420 11094.50

#lines(c(n.mail.valid, n.mail.valid), c(0, max(profit.log)), lty = 2, col = "red")
#c(n.mail.valid, max(profit.log)) # report number of mailings and maximum profit

cutoff.lda2 <- sort(post.valid.lda2, decreasing=T)[n.mail.valid2+1] # set cutoff based on n.mail.valid
chat.valid.lda2 <- ifelse(post.valid.lda2>cutoff.lda2, 1, 0) # mail to everyone above the cutoff
table(chat.valid.lda2, c.valid) # classification table
#               c.valid
#chat.valid.lda1   0   1
#              0 560  38
#              1 459 961
# check n.mail.valid = 459+961 = 1420
# check profit = 14.5*961-2*1420 = 11094.5

#----------------------------------------------------------------------------------------------

#QDA Model

model.qda <- qda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                   avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, data.train.std.c)

post.valid.qda <- predict(model.qda, data.valid.std.c)$posterior[,2] 

profit.qda <- cumsum(14.5*c.valid[order(post.valid.qda, decreasing=T)]-2)
plot(profit.qda) 
n.mail.valid <- which.max(profit.qda) 
c(n.mail.valid, max(profit.qda))
# 1372.0 11219.5

cutoff.qda <- sort(post.valid.qda, decreasing=T)[n.mail.valid+1] 
chat.valid.qda <- ifelse(post.valid.qda>cutoff.qda, 1, 0) 
round(mean(chat.valid.qda == c.valid)*100, 2)
# 77.95% correct classification rate

table(chat.valid.qda, c.valid)
#               c.valid
# chat.valid.qda   0   1
#               0 610  36
#               1 409 963
# check n.mail.valid = 409+963 = 1372
# check profit = 14.5*963-2*1372 = 11219.5

#-------------------------------------------------------------------------------------------------------------------

#GAM Model

library(splines)
library(gam)

model.gam1<- gam(as.factor(donr) ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                   avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, data.train.std.c, family = binomial("logit"))

post.valid.gam1 <- predict(model.gam1, data.valid.std.c, type="response")

profit.gam1 <- cumsum(14.5*c.valid[order(post.valid.gam1, decreasing=T)]-2)
plot(profit.gam1) 
n.mail.valid <- which.max(profit.gam1) 
c(n.mail.valid, max(profit.gam1)) 
# 1291.0 11642.5

cutoff.gam1 <- sort(post.valid.gam1, decreasing=T)[n.mail.valid+1] 
chat.valid.gam1 <- ifelse(post.valid.gam1>cutoff.gam1, 1, 0) 
round(mean(chat.valid.gam1 == c.valid)*100, 2)
# 83.75% correct classification rate

table(chat.valid.gam1, c.valid)

#                 c.valid
# chat.valid.gam   0   1
#              0 709  18
#              1 310 981
# check n.mail.valid = 309+981 = 1291
# check profit = 14.5*981-2*1290 = 11642.5


#---------------------------------------------------------------------------------------------

# logistic regression
# PROVIDED BY PROFESSOR

model.log1 <- glm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data.train.std.c, family=binomial("logit"))

post.valid.log1 <- predict(model.log1, data.valid.std.c, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.log1 <- cumsum(14.5*c.valid[order(post.valid.log1, decreasing=T)]-2)
plot(profit.log1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.log1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.log1)) # report number of mailings and maximum profit
# 1291.0 11642.5 

cutoff.log1 <- sort(post.valid.log1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.log1 <- ifelse(post.valid.log1>cutoff.log1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.log1, c.valid) # classification table
#               c.valid
#chat.valid.log1   0   1
#              0 709  18
#              1 310 981
# check n.mail.valid = 310+981 = 1291
# check profit = 14.5*981-2*1291 = 11642.5

#---------------------------------------------------------------------------------------------

# GBM, as boosting approach  (added by L. Maiya)
library(gbm)
set.seed(1)
model.gbm1 = gbm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                   avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, data = data.train.std.c, distribution = "bernoulli",
                 n.trees = 10000, interaction.depth = 4)
summary(model.gbm1)
set.seed(1)
post.valid.gbm1 = predict.gbm(model.gbm1, newdata = data.valid.std.c,
                              n.trees = 10000, type = "response")
head(predict.gbm(model.gbm1, newdata = data.valid.std.c,
            n.trees = 10000, type = "response"))

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.gbm1 <- cumsum(14.5*c.valid[order(post.valid.gbm1, decreasing=T)]-2)
plot(profit.gbm1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.gbm1) # number of mailings that maximizes profits
lines(c(n.mail.valid, n.mail.valid), c(0, max(profit.gbm1)), lty = 2, col = "red")
c(n.mail.valid, max(profit.gbm1)) # report number of mailings and maximum profit
# Profit for 1219 mailings: $11,946

cutoff.gbm1 <- sort(post.valid.gbm1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.gbm1 <- ifelse(post.valid.gbm1>cutoff.gbm1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.gbm1, c.valid) # classification table
#               c.valid
#chat.valid.log1   0   1
#              0 792 7
#              1 227 992

#---------------------------------------------------------------------------------------------

#XGBoost (added by L. Maiya)
# using raw data (no transformations here)
# we're using the same model we used the fitted model we used 
# for feature importance above
post.valid.xgb1 = predict(m_xgb_c, newdata = data.matrix(x.valid))

profit.xgb1 <- cumsum(14.5*c.valid[order(post.valid.xgb1, decreasing=T)]-2)
plot(profit.xgb1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.xgb1) # number of mailings that maximizes profits
lines(c(n.mail.valid, n.mail.valid), c(0, max(profit.xgb1)), lty = 2, col = "red")
c(n.mail.valid, max(profit.xgb1)) # report number of mailings and maximum profit
# 1236 11870.5

cutoff.xgb1 <- sort(post.valid.xgb1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.xgb1 <- ifelse(post.valid.xgb1>cutoff.xgb1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.xgb1, c.valid) 
# classification table
#               c.valid
#chat.valid.xgb1   0   1
#              0 748  6
#              1 271 993

#--------------------------------------------------------------------------------------------------------

# Logistic Regression
#library(VIF)
colnames(data.train.std.c)
logistic.model <- glm(donr ~ .,
                      data = data.train.std.c, family = binomial("logit"))
#sort(vif(logistic.model), decreasing = TRUE)
# inca, incm nad avhv have highest vif scores
summary(logistic.model)
# inca, avhv not importent in model, inca has smaller coef. estimate -> remove inca
logistic1.model <- glm(donr ~ . - inca,
                      data = data.train.std.c, family = binomial("logit"))
#sort(vif(logistic.model), decreasing = TRUE)
# all good

post.valid.log <- predict(logistic.model, newdata = data.valid.std.c, type = "response")
pred.log <- ifelse(post.valid.log > 0.5, 1, 0)
round(mean(pred.log == c.valid)*100, 2)
# 83.8% correct classification rate
profit.log <- cumsum(14.5*c.valid[order(post.valid.log, decreasing=T)]-2)
plot(profit.log, type = "l", xlab = "# Mailings", ylab = "Profit ($)", main = "Logistic Model") # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.log) # number of mailings that maximizes profits
lines(c(n.mail.valid, n.mail.valid), c(0, max(profit.log)), lty = 2, col = "red")
c(n.mail.valid, max(profit.log)) # report number of mailings and maximum profit
# 1389 mailings; $11,417.5 profit

cutoff.log <- sort(post.valid.log, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.log <- ifelse(post.valid.log>cutoff.log, 1, 0) # mail to everyone above the cutoff
table(chat.valid.log, c.valid) # classification table
#               c.valid
# chat.valid.log   0   1
#              0 609  20
#              1 410 979

#-----------------------------------------------------------------------------------------------------------

# Decision Tree
library(tree)

tree.model <- tree(as.factor(donr) ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                     avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, data = data.train.std.c)
set.seed(1)
tree.cv <- cv.tree(tree.model, FUN = prune.misclass)
tree.cv
tree.cv$size[which.min(tree.cv$dev)]
plot(tree.cv$size, tree.cv$dev, type = "b", xlab = "Size", ylab = "CV Error")
points(tree.cv$size[2], tree.cv$dev[2], col = "red", pch = 19)
plot(tree.cv$k, tree.cv$dev, type = "b")
# Best CV error at size 16 or 15 (go with smaller)

tree.model <- prune.misclass(tree.model, best = 15)
post.valid.tree <- predict(tree.model, newdata = data.valid.std.c, type = "vector")
head(post.valid.tree[,2])
post.valid.tree = post.valid.tree[,2]

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.tree <- cumsum(14.5*c.valid[order(post.valid.tree, decreasing=T)]-2)
plot(profit.tree) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.tree) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.tree)) # report number of mailings and maximum profit
# Profit for 1487 mailings: $11,381

cutoff.tree <- sort(post.valid.tree, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.tree <- ifelse(post.valid.tree>cutoff.gbm1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.tree, c.valid) # classification table
#               c.valid
#chat.valid.tree   0   1
#              0 779 57
#              1 240 942




# randomForest
library(randomForest)

set.seed(1)
rf.model1 <- randomForest(as.factor(donr) ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                            avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, data = data.train.std.c, mtry = 20, importance = TRUE)
set.seed(1)
rf.model2 <- randomForest(as.factor(donr) ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                            avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, data = data.train.std.c, mtry = 12, importance = TRUE)
set.seed(1)
rf.model3 <- randomForest(as.factor(donr) ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                            avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, data = data.train.std.c, importance = TRUE)

yhat.rf1 <- predict(rf.model1, newdata = data.valid.std.c)
yhat.rf2 <- predict(rf.model2, newdata = data.valid.std.c)
yhat.rf3 <- predict(rf.model3, newdata = data.valid.std.c)
round(mean(yhat.rf1 == c.valid)*100, 2) # 90.09% correct classification rate
round(mean(yhat.rf2 == c.valid)*100, 2) # 89.69% correct classification rate
round(mean(yhat.rf3 == c.valid)*100, 2) # 90.09% correct classification rate

post.valid.rf1 <- predict(rf.model2, newdata = data.valid.std.c, type = "prob")
post.valid.rf1 = post.valid.rf1[,2]


# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.rf1 <- cumsum(14.5*c.valid[order(post.valid.rf1, decreasing=T)]-2)
plot(profit.rf1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.rf1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.rf1)) # report number of mailings and maximum profit
# Profit for 1211 mailings: $11,701

cutoff.rf1 <- sort(post.valid.rf1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.rf1 <- ifelse(post.valid.rf1>cutoff.gbm1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.rf1, c.valid) # classification table
#               c.valid
#chat.valid.rf1   0   1
#              0 791 28
#              1 228 971


# Neural Network of size 4
library(nnet)
set.seed(1)
model.nnet1 <- nnet(as.factor(donr) ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                      avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, data.train.std.c, 
                    size = 4)
post.valid.nnet1 <- predict(model.nnet1, data.valid.std.c)

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.nnet1 <- cumsum(14.5*c.valid[order(post.valid.nnet1, decreasing = T)] - 2)
plot(profit.nnet1) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.nnet1) # number of mailings that maximizes profits
lines(c(n.mail.valid, n.mail.valid), c(0, max(profit.nnet1)), lty = 2, col = "red")
c(n.mail.valid, max(profit.nnet1)) # report number of mailings and maximum profit
# 1317 11692

cutoff.nnet1 <- sort(post.valid.nnet1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.nnet1 <- ifelse(post.valid.nnet1 > cutoff.nnet1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.nnet1, c.valid) # classification table
#                   c.valid
# chat.valid.nnet1   0   1
# 0 690  11
# 1 329 988



# GBM with no scaling and log transforms on all right-skewed variables
set.seed(1)
# avhv is already log transformed
# try log transforming remaining right-skewed variables
model.gbm2 = gbm(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                   avhv + I(log(incm+1)) + I(log(inca+1)) + I(log(plow+1)) + npro + tgif + I(log(lgif+1)) + I(log(rgif+1)) + tdon + tlag + agif, data = df_train, distribution = "bernoulli",
                 n.trees = 10000, interaction.depth = 4)
summary(model.gbm2)
set.seed(1)
post.valid.gbm2 = predict.gbm(model.gbm2, newdata = df_valid,
                              n.trees = 10000, type = "response")
head(post.valid.gbm2)
# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.gbm2 <- cumsum(14.5*c.valid[order(post.valid.gbm2, decreasing=T)]-2)
plot(profit.gbm2) # see how profits change as more mailings are made
n.mail.valid <- which.max(profit.gbm2) # number of mailings that maximizes profits
lines(c(n.mail.valid, n.mail.valid), c(0, max(profit.gbm2)), lty = 2, col = "red")
c(n.mail.valid, max(profit.gbm2)) # report number of mailings and maximum profit
# Profit for 1218 mailings: $11,933

cutoff.gbm2 <- sort(post.valid.gbm2, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.gbm2 <- ifelse(post.valid.gbm2>cutoff.gbm2, 1, 0) # mail to everyone above the cutoff
table(chat.valid.gbm2, c.valid) 

# classification table
#               c.valid
#chat.valid.log1   0   1
#              0 792 8
#              1 227 991
  

#-----------------
# Results
#--------------------
# n.mail Profit  Model
# 1329   11624.5 LDA1 (provided by professor)
# 1420   11094.5 LDA2 (five most important variables per XGBoost)
# 1372   11219.5 QDA1
# 1290   11642.5 GAM1
# 1291   11642.5 Log1 (provided by professor)
# 1219   11946   GBM1 (Gradient Boosting Machine, 10,000 trees)
# 1218   11933   GBM2 (Gradient Boosting Machine, 10,000 trees - more log transforms)
  
# 1236   11870.5 XGB1 (XGBoost)
# 1389   11417.5 logistic (GLM logistic all variables with only log(avhv))
# 1487   11381   tree (decision tree)   
# 1211   11701   rf1 (random forest with mtry=20, 500 trees)
# 1317   11692   nnet of size=4






#---------------------------------------------------------------------------------------------------------

# select model.gbm1 since it has maximum profit in the validation sample
post.test = predict.gbm(model.gbm1, newdata = data.test.std, n.trees = 10000, type = "response")
head(post.test)


# Oversampling adjustment for calculating number of mailings for test set
n.mail.valid <- which.max(profit.gbm1)
tr.rate <- .1 # typical response rate is .1
vr.rate <- .5 # whereas validation response rate is .5
adj.test.1 <- (n.mail.valid/n.valid.c)/(vr.rate/tr.rate) # adjustment for mail yes
adj.test.0 <- ((n.valid.c-n.mail.valid)/n.valid.c)/((1-vr.rate)/(1-tr.rate)) # adjustment for mail no
adj.test <- adj.test.1/(adj.test.1+adj.test.0) # scale into a proportion
n.mail.test <- round(n.test*adj.test, 0) # calculate number of mailings for test set

cutoff.test <- sort(post.test, decreasing=T)[n.mail.test+1] # set cutoff based on n.mail.test
chat.test <- ifelse(post.test>cutoff.test, 1, 0) # mail to everyone above the cutoff
table(chat.test)
# old results for logistic regression
#    0    1 
# 1676  331
# new results for GBM1
# 1716 291

# based on this model we'll mail to the 291 highest posterior probabilities


#---------------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------------

##### PREDICTION MODELING ######

# Least squares regression (professor's model)
model.ls1 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + 
                  avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                data.train.std.y)

pred.valid.ls1 <- predict(model.ls1, newdata = data.valid.std.y) # validation predictions
mean((y.valid - pred.valid.ls1)^2) # mean prediction error
# 1.867523
sd((y.valid - pred.valid.ls1)^2)/sqrt(n.valid.y) # std error
# 0.1696615

# LS2:  (professor's model) drop wrat for illustrative purposes
model.ls2 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + 
                  avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                data.train.std.y)

pred.valid.ls2 <- predict(model.ls2, newdata = data.valid.std.y) # validation predictions
mean((y.valid - pred.valid.ls2)^2) # mean prediction error
# 1.867433
sd((y.valid - pred.valid.ls2)^2)/sqrt(n.valid.y) # std error
# 0.1696498


# LS3:  use top 5 important variables discovered by XGBoost above
# rgif, lgif, agif, reg4, chld
model.ls3 <- lm(damt ~ rgif + lgif + agif + reg4 + chld, 
                data.train.std.y)

pred.valid.ls3 <- predict(model.ls3, newdata = data.valid.std.y) # validation predictions
mean((y.valid - pred.valid.ls3)^2) # mean prediction error
# 2.173481
sd((y.valid - pred.valid.ls3)^2)/sqrt(n.valid.y) # std error
# 0.1696498


# LS4:  best subset selection using BIC
library(leaps)
regfit.full=regsubsets(damt~.,data=data.train.std.y,nvmax=20)
reg.summary=summary(regfit.full)
names(reg.summary)
reg.summary$rsq
par(mfrow=c(1,1))
which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(10,reg.summary$bic[10],col="red",cex=2,pch=20)
plot(regfit.full,scale="bic")
coef(regfit.full,10)
# reg3 + reg4 + home + chld + hinc + incm + plow + npro + rgif + agif

# compute test MSE (twice for sanity check)
model.ls4 = lm(damt ~ reg3 + reg4 + home + chld + hinc + 
                 incm + plow + npro + rgif + agif, data=data.train.std.y)
pred.valid.ls4 <- predict(model.ls4, newdata = data.valid.std.y) # validation predictions
mean((y.valid - pred.valid.ls4)^2) # mean prediction error
# 1.857947
sd((y.valid - pred.valid.ls4)^2)/sqrt(n.valid.y) # std error
# 0.1693538


#-------------------------------------------------------------------------------------------------------------

#GBM - Model Added by L. Maiya
summary(data.train.std.y)
set.seed(1)
model.gbm1.r = gbm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                     avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                   data = data.train.std.y, distribution = "gaussian",
                   n.trees = 10000, interaction.depth = 4)
# old.par <- par(no.readonly=T) 
# par(las=2)
summary(model.gbm1.r)
set.seed(1)
pred.valid.gbm1 <- predict(model.gbm1.r, n.trees=10000, newdata = data.valid.std.y) # validation predictions
mean((y.valid - pred.valid.gbm1)^2) # mean prediction error
# 1.379185
sd((y.valid - pred.valid.gbm1)^2)/sqrt(n.valid.y) 
#0.1605556


# RandomForest [bagging approach]
# Random Forest [Bagging approach]
set.seed(1)
model.rf1.r = randomForest(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,                   
                           data = data.train.std.y, distribution = "gaussian",
                           ntree = 10000)

summary(model.rf1.r)
set.seed(1)
pred.valid.rf1 <- predict(model.rf1.r, data.valid.std.y) # validation predictions
mean((y.valid - pred.valid.rf1)^2) # mean prediction error
# 1.663989
sd((y.valid - pred.valid.rf1)^2)/sqrt(n.valid.y) 
#0.1734954



# Ridge Regression
library(glmnet)

# find best lambda using 10-fold CV
set.seed(1306)
lambda <- 10^seq(10, -2, length = 100)
ridge.model <- glmnet(as.matrix(data.train.std.y[,-21]), data.train.std.y[,"damt"],
                      alpha = 0, lambda = lambda)
cv.out=cv.glmnet(as.matrix(data.train.std.y[,-21]), data.train.std.y[,"damt"],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
#bestlam=cv.out$lambda.1se
bestlam
# min = 0.1107589
# 1se = 0.711966

ridge.pred1 <- predict(ridge.model, s = cv.out$lambda.min, newx = as.matrix(data.valid.std.y[,-21]), type='response')
tmp = ridge.pred1 - y.valid
tmp[[1]]
head(ridge.pred1)
head(y.valid)

mean((ridge.pred1 - y.valid)**2)
# 1.873231
sd((ridge.pred1 - y.valid)**2)/sqrt(n.valid.y)
# 0.1711169

ridge.pred2 <- predict(ridge.model, s = cv.out$lambda.1se, newx = as.matrix(data.valid.std.y[,-21]))
mean((ridge.pred2 - y.valid)**2)
# 1.994972
sd((ridge.pred2 - y.valid)**2)/sqrt(n.valid.y)
# 0.1793768


# Lasso Regression

# find best lambda using 10-fold CV
set.seed(1306)
lambda <- 10^seq(10, -2, length = 100)
lasso.model <- glmnet(as.matrix(data.train.std.y[,-21]), data.train.std.y[,"damt"],
                      alpha = 1, lambda = lambda)
cv.out=cv.glmnet(as.matrix(data.train.std.y[,-21]), data.train.std.y[,"damt"],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
#bestlam=cv.out$lambda.1se
bestlam
#min = 0.002618881
#1se = 0.08983992

mat.train = data.valid.std.y[,-21]
colnames(mat.train)
nrow(mat.train)
head(mat.train,1)
lasso.pred1 <- predict(lasso.model, s = cv.out$lambda.min, newx = as.matrix(data.valid.std.y[,-21]), type='response')
tmp = lasso.pred1 - y.valid
tmp[[1]]
head(lasso.pred1)
head(y.valid)

mean((lasso.pred1 - y.valid)**2)
# 1.86133
sd((lasso.pred1 - y.valid)**2)/sqrt(n.valid.y)
# 0.1694185

lasso.pred2 <- predict(lasso.model, s = cv.out$lambda.1se, newx = as.matrix(data.valid.std.y[,-21]))
mean((lasso.pred2 - y.valid)**2)
# 1.971464
sd((lasso.pred2 - y.valid)**2)/sqrt(n.valid.y)
# 0.171317

# Examine Lasso model coefficients
lasso.coef=predict(lasso.model,type="coefficients",s=bestlam)[1:20,]
lasso.coef

lasso.coef[lasso.coef!=0]
#----------------------------------------------------------------------------------------------

# #Prediction Model - Lasso
# 
# x <- model.matrix(damt ~.-damt, data=data.train.std.y)
# y<- data.train.std.y$damt
# xv <- model.matrix(damt ~.-damt, data=data.train.std.y)
# yv <- data.train.std.y$damt
# 
# par(mfrow=c(1,2))
# model.lasso1 <- glmnet(x,y,alpha=1)
# plot(model.lasso1)
# 
# set.seed(1)
# cv.out=cv.glmnet(x,y,alpha=1)
# plot(cv.out)
# 
# bestlam=cv.out$lambda.min
# bestlam
# #0.00877745
# 
# bestlam2=cv.out$lambda.1se
# bestlam2
# # 0.1187631
# 
# set.seed(1)
# pred.valid.lasso1 <- predict(model.lasso1, s = bestlam,newx=xv)
# 
# mean((y.valid - pred.valid.lasso1)^2)
# # 6.201392
# sd((y.valid - pred.valid.lasso1)^2)/sqrt(n.valid.y)
# # 0.335155
# 
# pred.valid.lasso2 <- predict(model.lasso1, s = bestlam2,newx=xv)
# 
# mean((y.valid - pred.valid.lasso2)^2)
# # 5.536593
# 
# sd((y.valid - pred.valid.lasso2)^2)/sqrt(n.valid.y)
# # 0.3117314


##Prediction Model - PCR

library(pls)

set.seed(2)
model.pcr1 <- pcr(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + I(hinc^2) + genf + wrat + 
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif, 
                  data = data.train.std.y, scale=FALSE, validation="CV")

summary(model.pcr1)

pred.valid.pcr1 <- predict(model.pcr1, newdata = data.valid.std.y)
mean((y.valid - pred.valid.pcr1)^2)
# 2.288604
sd((y.valid - pred.valid.pcr1)^2)/sqrt(n.valid.y) 
# 0.1934919




#------------------------------------------------------------------------------------------------

# Results

# MPE  Model
# 1.867523 LS1
# 1.867433 LS2
# 2.173481 LS3
# 1.857947 LS4
# 1.663989 RF1
# 1.379185 GBM1
# 1.873231 RIDGE.PRED1
# 1.994972 RIDGE.PRED2
# 1.86133 LASSO.PRED1
# 1.971464 LASSO.PRED2
# 2.288604 PCR1


# select model.gbm1.r since it has minimum mean prediction error in the validation sample

yhat.test <- predict(model.gbm1.r, n.trees=10000, newdata = data.test.std) # test predictions

#----------------------------------------------------------------------------------------------




# FINAL RESULTS

# Save final results for both classification and regression

length(chat.test) # check length = 2007
length(yhat.test) # check length = 2007
chat.test[1:10] # check this consists of 0s and 1s
yhat.test[1:10] # check this consists of plausible predictions of damt

ip <- data.frame(chat=chat.test, yhat=yhat.test) # data frame with two variables: chat and yhat
write.csv(ip, file="LM_CM_NB.csv", row.names=FALSE) # use your initials for the file name

# submit the csv file in Canvas for evaluation based on actual test donr and damt values

#-------------------------------------------------------------------------------------------------------------
# ADDENDUM WITH BEST SUBSET SELECTION
#-------------------------------------------------------------------------------------------------------------

# Best Subset
library(moments)


charity.t$chld0 <- ifelse(charity.t$chld == 0, 1, 0)
charity.t$wratHIGH <- ifelse(charity.t$wrat > 5, 1, 0)

# additional transformations
hist(charity.t$incm)      # slightly skewed
skewness(charity.t$incm)
skewness(BoxCox(charity.t$incm, BoxCox.lambda(charity.t$incm)))
skewness(log(charity.t$incm))
charity.t$incm <- log(charity.t$incm)
hist(charity.t$incm)

hist(charity.t$inca)          # slightly skewed
skewness(charity.t$inca)
skewness(BoxCox(charity.t$inca, BoxCox.lambda(charity.t$inca)))
skewness(log(charity.t$inca))
charity.t$inca <- log(charity.t$inca)

hist(charity.t$tgif)      # slightly skewed
skewness(charity.t$tgif)
skewness(BoxCox(charity.t$tgif, BoxCox.lambda(charity.t$tgif)))
skewness(log(charity.t$tgif))
charity.t$tgif <- BoxCox(charity.t$tgif, BoxCox.lambda(charity.t$tgif))

hist(charity.t$lgif)      # slightly skewed
skewness(charity.t$lgif)
skewness(BoxCox(charity.t$lgif, BoxCox.lambda(charity.t$lgif)))
skewness(log(charity.t$lgif))
charity.t$lgif <- BoxCox(charity.t$lgif, BoxCox.lambda(charity.t$lgif))

hist(charity.t$rgif)      # slightly skewed
skewness(charity.t$rgif)
skewness(log(charity.t$rgif))
charity.t$rgif <- log(charity.t$rgif)

hist(charity.t$agif)      # slightly skewed
skewness(charity.t$agif)
skewness(log(charity.t$agif))
charity.t$agif <- log(charity.t$agif)

## Training/Validation/Test Split ##
# Training
data.train <- charity.t[charity$part=="train",]
x.train <- data.train[,c(2:21,25,26)]
c.train <- data.train[,22] # donr
n.train.c <- length(c.train) # 3984
y.train <- data.train[c.train==1,23] # damt for observations with donr=1
n.train.y <- length(y.train) # 1995

# Validation
data.valid <- charity.t[charity$part=="valid",]
x.valid <- data.valid[,c(2:21,25,26)]
c.valid <- data.valid[,22] # donr
n.valid.c <- length(c.valid) # 2018
y.valid <- data.valid[c.valid==1,23] # damt for observations with donr=1
n.valid.y <- length(y.valid) # 999

# Test
data.test <- charity.t[charity$part=="test",]
n.test <- dim(data.test)[1] # 2007
x.test <- data.test[,c(2:21,25,26)]

# Standardize
x.train.mean <- apply(x.train, 2, mean)
x.train.sd <- apply(x.train, 2, sd)
x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) # standardize to have zero mean and unit sd
apply(x.train.std, 2, mean) # check zero mean
apply(x.train.std, 2, sd) # check unit sd
data.train.std.c <- data.frame(x.train.std, donr=c.train) # to classify donr
data.train.std.y <- data.frame(x.train.std[c.train==1,], damt=y.train) # to predict damt when donr=1

x.valid.std <- t((t(x.valid)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.valid.std.c <- data.frame(x.valid.std, donr=c.valid) # to classify donr
data.valid.std.y <- data.frame(x.valid.std[c.valid==1,], damt=y.valid) # to predict damt when donr=1

x.test.std <- t((t(x.test)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.test.std <- data.frame(x.test.std)

best.subset.models1 <- regsubsets(damt ~ ., data = data.train.std.y, nvmax = 22, method = "forward")
best.subset.models2 <- regsubsets(damt ~ ., data = data.train.std.y, nvmax = 22, method = "backward")
best.subset.models3 <- regsubsets(damt ~ ., data = data.train.std.y, nvmax = 22, method = "seqrep")

which.max(round(summary(best.subset.models1)$rsq, 3)) # 17
which.max(round(summary(best.subset.models2)$rsq, 3)) # 17
which.max(round(summary(best.subset.models3)$rsq, 3)) # 18

which.min(round(summary(best.subset.models1)$cp, 2))  # 16
which.min(round(summary(best.subset.models2)$cp, 2))  # 16
which.min(round(summary(best.subset.models3)$cp, 2))  # 16

which.min(round(summary(best.subset.models1)$bic))    # 13
which.min(round(summary(best.subset.models2)$bic))    # 13
which.min(round(summary(best.subset.models3)$bic))    # 13

coef(best.subset.models1, 1)
coef(best.subset.models2, 1)
coef(best.subset.models3, 1) # same

sum(round(coef(best.subset.models1, 13), 4) != round(coef(best.subset.models2, 13), 4))
sum(round(coef(best.subset.models2, 13), 4) != round(coef(best.subset.models3, 13), 4)) # same
names(coef(best.subset.models1, 13))

sum(round(coef(best.subset.models1, 16), 4) != round(coef(best.subset.models2, 16), 4))
sum(round(coef(best.subset.models2, 16), 4) != round(coef(best.subset.models3, 16), 4)) # same
names(coef(best.subset.models1, 16))

sum(round(coef(best.subset.models1, 17), 4) != round(coef(best.subset.models2, 17), 4))
sum(round(coef(best.subset.models1, 17), 4) != round(coef(best.subset.models3, 17), 4))
# forward/backward same, stepwise different
names(coef(best.subset.models1, 17))
names(coef(best.subset.models3, 17))

sum(round(coef(best.subset.models1, 18), 4) != round(coef(best.subset.models2, 18), 4))
sum(round(coef(best.subset.models2, 18), 4) != round(coef(best.subset.models3, 18), 4)) # same
names(coef(best.subset.models1, 18))

best1.1 <- lm(damt ~ lgif, data = data.train.std.y)
best1.13 <- lm(damt ~ reg3 + reg4 + home + chld + hinc + wrat + incm + plow + tgif + lgif + rgif +
                 agif + wratHIGH, data = data.train.std.y)
best1.16 <- lm(damt ~ reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + incm + plow + tgif +
                 lgif + rgif + tdon + agif + wratHIGH, data = data.train.std.y)
best1.17 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + incm + plow +
                 tgif + lgif + rgif + tdon + agif + wratHIGH, data = data.train.std.y)
best2.17 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + avhv + incm +
                 inca + plow + npro + tgif + lgif + rgif, data = data.train.std.y)
best1.18 <- lm(damt ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc + genf + wrat + incm + plow +
                 tgif + lgif + rgif + tdon + tlag + agif + wratHIGH, data = data.train.std.y)

pred1.1 <- predict(best1.1, newdata = data.valid.std.y)
pred1.13 <- predict(best1.13, newdata = data.valid.std.y)
pred1.16 <- predict(best1.16, newdata = data.valid.std.y)
pred1.17 <- predict(best1.17, newdata = data.valid.std.y)
pred2.17 <- predict(best2.17, newdata = data.valid.std.y)
pred1.18 <- predict(best1.18, newdata = data.valid.std.y)

data.frame(size = c(1,13,16,17,"17_step",18),
           mean_error = c(mean((y.valid - pred1.1)**2),
                          mean((y.valid - pred1.13)**2),
                          mean((y.valid - pred1.16)**2),
                          mean((y.valid - pred1.17)**2),
                          mean((y.valid - pred2.17)**2),
                          mean((y.valid - pred1.18)**2)),
           sd_error = c(sd((y.valid - pred1.1)**2)/sqrt(n.valid.y),
                        sd((y.valid - pred1.13)**2)/sqrt(n.valid.y),
                        sd((y.valid - pred1.16)**2)/sqrt(n.valid.y),
                        sd((y.valid - pred1.17)**2)/sqrt(n.valid.y),
                        sd((y.valid - pred2.17)**2)/sqrt(n.valid.y),
                        sd((y.valid - pred1.18)**2)/sqrt(n.valid.y)))
# best model is 17 predictor, forward/backward selection

#sort(vif(best1.17), decreasing = TRUE)
# lgif has vif of ~6; will allow

par(mfrow = c(1, 2))
hist(best1.17$residuals, main = "Histogram of Residuals", xlab = "Residuals")
qqnorm(best1.17$residuals)
qqline(best1.17$residuals)
par(mfrow = c(1, 1))

plot(best1.17$fitted.values, best1.17$residuals, xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2, col = "red")

mean((y.valid - pred1.17)**2) # mean prediction error
# 1.515698
sd((y.valid - pred1.17)**2)/sqrt(n.valid.y) # std error
# 0.1589511




