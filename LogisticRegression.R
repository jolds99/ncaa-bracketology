library(caret)
library(tidyverse)
library(intrval)
library(car)
library(Rfast)
head(alldata)
str(alldata)


## Train = 70% of each season, include conference champs
set.seed(2020)
train_1 = alldata %>% group_by(Season) %>% sample_frac(0.70)
test_1 = anti_join(alldata, train)

table(train_1$`Make Tournament`)
table(test_1$`Make Tournament`)

      ## Basic Model
      model_0 = glm(`Make Tournament` ~ Wins, data = train_1, family = binomial)
      summary(model_0)
      exp(coef(model_0)[-1])
      
      probs_0 = round(predict(model_0,test_1,type = "response"),2)
      predictions_0 = ifelse(probs_0 > 0.5, 1, 0)
      predictions_0 = factor(predictions_0, levels = c(0,1))
      actual_0 = test_1$`Make Tournament`
      mean(predictions_0 == actual_0)
      predict_summary_0 = matrix(NA,2,2)
      predict_summary_0[,1] = cbind(length(which(predictions_0 == 1)),length(which(actual_0 == 1)))
      predict_summary_0[,2] = cbind(length(which(predictions_0 == 0)),length(which(actual_0 == 0)))
      colnames(predict_summary_0) = c("Make Tourn.", "Miss Tourn.")
      rownames(predict_summary_0) = c("Predicted", "Actual")
      predict_summary_0
      
      ## Model with Wins and SOS
      model_1 = glm(`Make Tournament` ~ Wins + SOS, data = train_1, family = binomial)
      
      summary(model_1)
      exp(coef(model_1)[-1])
      vif(model_1)
      
      probs_1 = round(predict(model_1,test_1,type = "response"),2)
      predictions_1 = ifelse(probs_1 > 0.5, 1, 0)
      predictions_1 = factor(predictions_1, levels = c(0,1))
      actual_1 = test_1$`Make Tournament`
      mean(predictions_1 == actual_1)
      predict_summary_1 = matrix(NA,2,2)
      predict_summary_1[,1] = cbind(length(which(predictions_1 == 1)),length(which(actual_1 == 1)))
      predict_summary_1[,2] = cbind(length(which(predictions_1 == 0)),length(which(actual_1 == 0)))
      colnames(predict_summary_1) = c("Make Tourn.", "Miss Tourn.")
      rownames(predict_summary_1) = c("Predicted", "Actual")
      predict_summary_1
      
      ## Model with Wins, SOS, WAB Rank 
      model_2 = glm(`Make Tournament` ~ Wins + SOS + `WAB Rank`, data = train_1, family = binomial)
      
      summary(model_2)
      exp(coef(model_2)[-1])
      vif(model_2)
      
      probs_2 = round(predict(model_2,test_1,type = "response"),2)
      predictions_2 = ifelse(probs_2 > 0.5, 1, 0)
      predictions_2 = factor(predictions_2, levels = c(0,1))
      actual_2 = test_1$`Make Tournament`
      mean(predictions_2 == actual_2)
      predict_summary_2 = matrix(NA,2,2)
      predict_summary_2[,1] = cbind(length(which(predictions_2 == 1)),length(which(actual_2 == 1)))
      predict_summary_2[,2] = cbind(length(which(predictions_2 == 0)),length(which(actual_2 == 0)))
      colnames(predict_summary_2) = c("Make Tourn.", "Miss Tourn.")
      rownames(predict_summary_2) = c("Predicted", "Actual")
      predict_summary_2
      
      ## Model with Wins & Non Conf SOS
      model_3 = glm(`Make Tournament` ~ Wins + `Non-Conf SOS`, data = train_1, family = binomial)
      
      summary(model_3)
      exp(coef(model_3)[-1])
      vif(model_3)
      
      probs_3 = round(predict(model_3,test_1,type = "response"),2)
      predictions_3 = ifelse(probs_3 > 0.5, 1, 0)
      predictions_3 = factor(predictions_3, levels = c(0,1))
      actual_3 = test_1$`Make Tournament`
      mean(predictions_3 == actual_3)
      predict_summary_3 = matrix(NA,2,2)
      predict_summary_3[,1] = cbind(length(which(predictions_3 == 1)),length(which(actual_3 == 1)))
      predict_summary_3[,2] = cbind(length(which(predictions_3 == 0)),length(which(actual_3 == 0)))
      colnames(predict_summary_3) = c("Make Tourn.", "Miss Tourn.")
      rownames(predict_summary_3) = c("Predicted", "Actual")
      predict_summary_3

## Potential Fix to Imbalance
down_train_1 <- downSample(x = train_1[, colnames(train_1) %ni% "`Make Tournament`"],
                         y = train_1$`Make Tournament`)
table(down_train_1$`Make Tournament`)
up_train_1 <- upSample(x = train_1[, colnames(train_1) %ni% "`Make Tournament`"],
                     y = train_1$`Make Tournament`)
table(up_train_1$`Make Tournament`)


## Train = 2015-2018 Seasons, no Conference Champs
nccdata = alldata %>% filter(`Conference Champ` != 1)
train_2 = nccdata %>% filter(Season != 2019) 
test_2 = nccdata %>% filter(Season == 2019)

model_0 = glm(`Make Tournament` ~ Wins, data = train_2, family = binomial)

summary(model_0)
exp(coef(model_0))[-1]

probs_0 = round(predict(model_0,test_2,type = "response"),3)
predictions_0 = as.data.frame(cbind(test_2$School, probs_0))
predictions_0 = predictions_0[order(-probs_0),]
predicted_selections = predictions_0[1:36,1:2]
colnames(predicted_selections) = c("School", "Probability")
actual_selections = test_2 %>% filter(`Make Tournament` == 1) %>% select(School)
predicted_selections = as.character(predicted_selections$School)
actual_selections = as.character(actual_selections$School)

mean(actual_selections %in% predicted_selections == TRUE)
