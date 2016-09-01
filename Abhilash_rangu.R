rm(list=ls())
## loading libraries
library(caret)
library(dummies)
library(plyr)


## loading data (edit the paths)

setwd("C:/Users/Abhilash/Desktop/Main project/Restaurant prediction kaggle")

#loading the data
train<-read.csv("train.csv",header = T,sep=',',stringsAsFactors=F)
test<-read.csv("test.csv",header=T,sep=',',stringsAsFactors=F)
which.max(train$revenue)
which.min(train$revenue)

#Treating outliers 
train[17,43] <- 0.5 * (train[17,43])
train[22,43] <- 1.5 * (train[17,43])

## cleaning data
## binding the train and test data 
Data <- rbind(train[,-ncol(train)], test)


# creating feature variables
## extracting years, month, days from open date 
Data$year <- substr(as.character(Data$Open.Date),7,10)
Data$month <- substr(as.character(Data$Open.Date),1,2)
Data$day <- substr(as.character(Data$Open.Date),4,5)

##converting opendate in to date type 
Data$Date <- as.Date(strptime(Data$Open.Date, "%m/%d/%Y"))

## calculating no of days since the opening of restaurant 
Data$days <- as.numeric(as.Date("2014-02-02")-Data$Date)

##factorizing city type attribute
Data$City.Group <- as.factor(Data$City.Group)
##
Data$Type[Data$Type == "DT"] <- "IL"
Data$Type[Data$Type == "MB"] <- "FC"

## factorizing type attribute 
Data$Type <- as.factor(Data$Type)

## removing OPen date , date and city 
Data <- subset(Data, select = -c(Open.Date, Date, City))


## subsetting train data set while dropping id, month ,day and days  and test dataset
train_svm <- Data[1:nrow(train),-c(1,41,42,43)]
test_svm <- Data[(nrow(train)+1):nrow(Data),]


# converting variables to factors 
catg =c("P1","P5","P6","P7","P8","P9","P10","P11",
        "P12","P14", "P15", "P16", "P17", "P18", "P19",
        "P20", "P21", "P22", "P23", "P24", "P25", 
        "P30", "P31", "P32", "P33", "P34", "P35", "P36", "P37")

train_svm[,catg] <- data.frame(apply(train_svm[,catg], 2, factor))
test_svm[,catg] <- data.frame(apply(test_svm[,catg], 2, factor))

# converting some categorical variables into dummies
Data <- dummy.data.frame(Data, names=c("P1","P5","P6","P7","P8","P9","P10","P11",
                                         "P12","P14", "P15", "P16", "P17", "P18", "P19", 
                                         "P20", "P21", "P22", "P23", "P24", "P25", "P30",
                                         "P31", "P32", "P33", "P34", "P35", "P36", "P37"), all=T)


#creating dummies to all categorical variables 
ldf <- lapply(1:ncol(Data), function(k)
{
    return(data.frame("column" = colnames(Data)[k],
                      "unique" = length(unique(Data[1:nrow(train),k]))))
})

ldf <- ldply(ldf, data.frame)

# removing variables with unique values
Data <- Data[,!names(Data) %in% ldf$column[ldf$unique == 1]]

# removing highly correlated variables
for (i in (3:ncol(Data)))
{
    Data[,i] <- as.numeric(Data[,i])
}

cor <- cor(Data[1:nrow(train), 3:ncol(Data)])
high_cor <- findCorrelation(cor, cutoff = 0.99)

high_cor <- high_cor[high_cor != 186]

Data <- Data[,-c(high_cor+1)]

# splitting into train and test
X_train <- Data[1:nrow(train),-1]
X_test <- Data[(nrow(train)+1):nrow(Data),]

# building model on log of revenue
result <- log(train$revenue)

train_svm <- train[,-43]
library(clusterSim)
##normalizing the Train and Test dataset 
train_svm <- data.Normalization (train_svm,type="n5",normalization="column")
test_svm <- data.Normalization (test_svm,type="n5",normalization="column")
summary(train_svm)
str(train_svm)


#-------------- model Building -------------------
####### Random Forest#####################

## loading libraries
suppressWarnings(library(pROC))
library(randomForest)


RandomForestRegression_CV <- function(X_train,y,X_test=data.frame(),cv=5,ntree=50,nodesize=5,seed=123,metric="mae")
{
  score <- function(a,b,metric)
  {
    switch(metric,
           mae = sum(abs(a-b))/length(a),
           rmse = sqrt(sum((a-b)^2)/length(a)))
  }
  
  cat("Preparing Data\n")
  X_train$order <- seq(1, nrow(X_train))
  X_train$result <- as.numeric(y)
  
  set.seed(seed)
  X_train$randomCV <- floor(runif(nrow(X_train), 1, (cv+1)))
  
  cat(cv, "-fold Cross Validation\n", sep = "")
  for (i in 1:cv)
  {
    X_build <- subset(X_train, randomCV != i, select = -c(order, randomCV))
    X_val <- subset(X_train, randomCV == i) 
    
    model_rf <- randomForest(result ~., data = X_build, ntree = ntree, nodesize = nodesize)
    
    pred_rf <- predict(model_rf, X_val)
    X_val <- cbind(X_val, pred_rf)
    
    if (nrow(X_test) > 0)
    {
      pred_rf <- predict(model_rf, X_test)
    }
    
    cat("CV Fold-", i, " ", metric, ": ", score(X_val$result, X_val$pred_rf, metric), "\n", sep = "")
    
    if (i == 1)
    {
      output <- X_val
      if (nrow(X_test) > 0)
      {
        X_test <- cbind(X_test, pred_rf)
      }      
    }
    
    if (i > 1)
    {
      output <- rbind(output, X_val)
      if (nrow(X_test) > 0)
      {
        X_test$pred_rf <- (X_test$pred_rf * (i-1) + pred_rf)/i
      }            
    }
    
    gc()
  } 
  
  output <- output[order(output$order),]
  cat("\nRandomForest ", cv, "-Fold CV ", metric, ": ", score(output$result, output$pred_rf, metric), "\n", sep = "")
  
  output <- subset(output, select = c("order", "pred_rf"))
  return(list(output, X_test))  
}

########### Svm ############
## loading libraries
suppressWarnings(library(pROC))
library(e1071)


svmRegression_CV <- function(X_train,y,X_test=data.frame(),scale = FALSE,cv=5,seed=123,metric="rmse")
{
  score <- function(a,b,metric)
  {
    switch(metric,
           mae = sum(abs(a-b))/length(a),
           rmse = sqrt(sum((a-b)^2)/length(a)))
  }
  
  cat("Preparing Data\n")
  X_train$order <- seq(1, nrow(X_train))
  X_train$result <- as.numeric(y)
  
  set.seed(seed)
  X_train$randomCV <- floor(runif(nrow(X_train), 1, (cv+1)))
  
  cat(cv, "-fold Cross Validation\n", sep = "")
  for (i in 1:cv)
  {
    X_build <- subset(X_train, randomCV != i, select = -c(order, randomCV))
    X_val <- subset(X_train, randomCV == i) 
    
    model_rf <- svm(result ~., data = X_build, ntree = ntree, nodesize = nodesize)
    
    pred_rf <- predict(model_rf, X_val)
    X_val <- cbind(X_val, pred_rf)
    
    if (nrow(X_test) > 0)
    {
      pred_rf <- predict(model_rf, X_test)
    }
    
    cat("CV Fold-", i, " ", metric, ": ", score(X_val$result, X_val$pred_rf, metric), "\n", sep = "")
    
    if (i == 1)
    {
      output <- X_val
      if (nrow(X_test) > 0)
      {
        X_test <- cbind(X_test, pred_rf)
      }      
    }
    
    if (i > 1)
    {
      output <- rbind(output, X_val)
      if (nrow(X_test) > 0)
      {
        X_test$pred_rf <- (X_test$pred_rf * (i-1) + pred_rf)/i
      }            
    }
    
    gc()
  } 
  
  output <- output[order(output$order),]
  cat("\nSVM ", cv, "-Fold CV ", metric, ": ", score(output$result, output$pred_rf, metric), "\n", sep = "")
  
  output <- subset(output, select = c("order", "pred_rf"))
  return(list(output, X_test))  
}


# 5-fold cross validation and scoring for svm model 
model_svm_1 <- svmRegression_CV(X_train,result,X_test,scale = F,cv=5,seed=235,metric="rmse")
model_svm_2 <- svmRegression_CV(X_train,result,X_test,cv=5,scale = F,seed=357,metric="rmse")
model_svm_3 <- svmRegression_CV(X_train,result,X_test,cv=5,scale = F,seed=13,metric="rmse")
model_svm_4 <- svmRegression_CV(X_train,result,X_test,cv=5,scale = F,seed=753,metric="rmse")
model_svm_5 <- svmRegression_CV(X_train,result,X_test,cv=5,scale = F,seed=532,metric="rmse")

# 5-fold cross validation and scoring
model_rf_1 <- RandomForestRegression_CV(X_train,result,X_test,cv=5,ntree=25,nodesize=5,seed=235,metric="rmse")
model_rf_2 <- RandomForestRegression_CV(X_train,result,X_test,cv=5,ntree=25,nodesize=5,seed=357,metric="rmse")
model_rf_3 <- RandomForestRegression_CV(X_train,result,X_test,cv=5,ntree=25,nodesize=5,seed=13,metric="rmse")
model_rf_4 <- RandomForestRegression_CV(X_train,result,X_test,cv=5,ntree=25,nodesize=5,seed=753,metric="rmse")
model_rf_5 <- RandomForestRegression_CV(X_train,result,X_test,cv=5,ntree=25,nodesize=5,seed=532,metric="rmse")

#submission
test_svm_1 <- model_svm_1[[2]]
test_svm_2 <- model_svm_2[[2]]
test_svm_3 <- model_svm_3[[2]]
test_svm_4 <- model_svm_4[[2]]
test_svm_5 <- model_svm_5[[2]]


## submission
test_rf_1 <- model_rf_1[[2]]
test_rf_2 <- model_rf_2[[2]]
test_rf_3 <- model_rf_3[[2]]
test_rf_4 <- model_rf_4[[2]]
test_rf_5 <- model_rf_5[[2]]

submit <- data.frame("Id" = test_rf_1$Id,
                     "Prediction" = 0.2*exp(test_rf_1$pred_rf) + 0.2*exp(test_rf_2$pred_rf) + 0.2*exp(test_rf_3$pred_rf) + 0.2*exp(test_rf_4$pred_rf) + 0.2*exp(test_rf_5$pred_rf))
submit_svm <- data.frame("Id" = test_svm_1$Id,
                     "Prediction" = 0.2*exp(test_svm_2$pred_rf) + 0.2*exp(test_svm_3$pred_rf) + 0.2*exp(test_svm_4$pred_rf) + 0.2*exp(test_svm_5$pred_rf) + 0.2*exp(test_svm_1$pred_rf))
submit_ensemble <- data.frame("Id" = test_rf_1$Id,
                              "Prediction" = 0.2*exp(test_svm_2$pred_rf) + 0.8*exp(test_rf_4$pred_rf) )

str(submit)



write.csv(submit, "submitRF.csv", row.names=F)

write.csv(submit_ensemble, "submit_ensemble.csv", row.names=F)

write.csv(submit_svm,"submit_svm.csv", row.names=F)




#---------------Plots-------------------------------

library(plotly)
plot_ly(x =train$revenue , type = "histogram")



plot_ly(x = train$revenue, opacity = 0.6, type = "histogram") %>%
  add_trace(x = result , opacity = 0.8, type = "histogram") %>%
  layout(barmode="overlay")




# revenue vs open date
plot_ly(Data ,x= revenue, y = days, 
        mode = "markers", color = revenue,  opacity = revenue)

















