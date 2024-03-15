
rm(list=ls())                                   # empty the environment

#setwd("your computer/your folder")             # set work directory; 




#load the packages we need
library(broom)
library(earth)
library(kknn)
library(nnet)


# load the dataset
data_dc <- read.csv("data_card.csv", header = T, stringsAsFactors = T, sep = ',')
set.seed(1)


is_training <- runif(nrow(data_dc)) < 0.9


data_train <- subset(data_dc,is_training)
data_valid <-subset(data_dc,!is_training)



model1 = lm(card~ age + income + share + expenditure + owner + selfemp + dependents + months + majorcards + active, data = data_train)

model2 = lm(card~ reports + age + income + share + expenditure + owner + selfemp + dependents + months + majorcards + active, data = data_train)

model3 = lm(card ~ reports + age + income + I(income^2) + share + expenditure + owner + selfemp + dependents + months + majorcards + active, data = data_train)

model4= lm(card ~ reports + age + I(age^2) + income + I(income^2) + share + expenditure + owner + selfemp + dependents + months + majorcards + active, data = data_train)



summary(model1)
summary(model2)
summary(model3)
summary(model4)

dif_model1 <- data_valid$card - predict(model1, data_valid)
dif_model2 <- data_valid$card - predict(model2, data_valid)
dif_model3 <- data_valid$card - predict(model3, data_valid)
dif_model4 <- data_valid$card - predict(model4, data_valid)



mean(dif_model1^2)^.5
mean(dif_model2^2)^.5
mean(dif_model3^2)^.5
mean(dif_model4^2)^.5


set.seed(1)
nFold <- 5

ValNum <- floor(runif(nrow(data_dc))*nFold) + 1
head(ValNum)

model_performance <- matrix(NA,nFold,4)

# loop through each fold for cross validation
for(i in 1:nFold){
  
  
  # step 2i: Get the training and validation data for this fold
  trainData <- subset(data_dc,ValNum !=i)
  validData <- subset(data_dc,ValNum ==i)
  # step 2ii: Estimate the model for this training data
  model1 = lm(card~ age + income + share + expenditure + owner + selfemp + dependents + months + majorcards + active, data = data_train)
  
  model2 = lm(card~ reports + age + income + share + expenditure + owner + selfemp + dependents + months + majorcards + active, data = data_train)
  
  model3 = lm(card ~ reports + age + income + I(income^2) + share + expenditure + owner + selfemp + dependents + months + majorcards + active, data = data_train)
  
  model4= lm(card ~ reports + age + I(age^2) + income + I(income^2) + share + expenditure + owner + selfemp + dependents + months + majorcards + active, data = data_train)
  
  
  # step 2iii: Calculate out of sample RMSE for this validData
  valid1 <-mean((validData$card -predict(model1, validData))^2)^0.5
  valid2 <-mean((validData$card -predict(model2, validData))^2)^0.5
  valid3 <-mean((validData$card -predict(model3, validData))^2)^0.5
  valid4 <-mean((validData$card -predict(model4, validData))^2)^0.5
  # store model performance
  model_performance[i,] <-c(valid1,valid2,valid3,valid4)
}

colMeans(model_performance)





ValNum2 <- floor(runif(nrow(data_dc))*nFold) + 1
head(ValNum2)

model_performance2 <- matrix(NA,nFold,2)

# loop through each fold for cross validation
for(i in 1:nFold){
  
  
  # step 2i: Get the training and validation data for this fold
  trainData2 <- subset(data_dc,ValNum2 !=i)
  validData2 <- subset(data_dc,ValNum2 ==i)
  # step 2ii: Estimate the model for this training data
  modelM = earth(card~ reports + age + income + share + expenditure + owner + selfemp + dependents + months + majorcards + active, data = data_train)
  
  modelK = kknn(card~ reports + age + income + share + expenditure + owner + selfemp + dependents + months + majorcards + active, data_train,data_valid,k=7)
  
  # step 2iii: Calculate out of sample RMSE for this validData
  validM <-mean((data_valid$card - predict(modelM, data_valid))^2)^0.5
  validK <-mean((data_valid$card- modelK$fitted.values)^2)^0.5
  # store model performance
  model_performance2[i,] <-c(validM,validK)
}

colMeans(model_performance2)
