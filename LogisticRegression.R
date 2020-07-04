library(caret)
library(tidyverse)
library(intrval)
library(car)
library(Rfast)
library(data.table)

## Train = 2015-2018 Seasons, no Conference Champs
## Test = 2019 Season, no Conference Champs
nccdata = alldata %>% filter(`Conference Champ` != 1)
ccdata = alldata %>% filter(`Conference Champ` == 1)
train_2 = nccdata %>% filter(Season != 2019) 
test_2 = nccdata %>% filter(Season == 2019)

## Multiplying variables with values between 0 and 1 by 100 for better model comparison
### DONT FORGET I DID THIS ###
nccdata$SOS = nccdata$SOS * 100
nccdata$`Conference Win %` = nccdata$`Conference Win %` * 100
nccdata$`Conference SOS` = nccdata$`Conference SOS` * 100
nccdata$`Non-Conf SOS` = nccdata$`Non-Conf SOS` * 100
nccdata$Barthag = nccdata$Barthag * 100
##############################

#### INDIVIDUAL MODELS
model_L12 = glm(`Make Tournament` ~ `Last 12 Wins`, data = nccdata, family = binomial)
summary(model_L12)
coef_L12 = rbind(round(summary(model_L12)$coefficients[2,],8))

model_CF = glm(`Make Tournament` ~ `Conference Finish`, data = nccdata, family = binomial)
summary(CF)
coef_CF = rbind(round(summary(model_CF)$coefficients[2,],8))

model_RPI = glm(`Make Tournament` ~ `RPI Rank`, data = nccdata, family = binomial)
summary(model_RPI)
coef_RPI = rbind(round(summary(model_RPI)$coefficients[2,],8))

model_W = glm(`Make Tournament` ~ `Wins`, data = nccdata, family = binomial)
summary(model_W)
coef_W = rbind(round(summary(model_W)$coefficients[2,],8))

model_AOE = glm(`Make Tournament` ~ `Adj. Offensive Efficiency`, data = nccdata, family = binomial)
summary(model_AOE)
coef_AOE = rbind(round(summary(model_AOE)$coefficients[2,],8))

model_DOE = glm(`Make Tournament` ~ `Adj. Defensive Efficiency`, data = nccdata, family = binomial)
summary(model_12)
coef_DOE = rbind(round(summary(model_DOE)$coefficients[2,],8))

model_BT = glm(`Make Tournament` ~ `Barthag`, data = nccdata, family = binomial)
summary(model_BT)
coef_BT = rbind(round(summary(model_BT)$coefficients[2,],8))

model_SOS = glm(`Make Tournament` ~ `SOS`, data = nccdata, family = binomial)
summary(model_SOS)
coef_SOS = rbind(round(summary(model_SOS)$coefficients[2,],8))

model_NCSOS = glm(`Make Tournament` ~ `Non-Conf SOS`, data = nccdata, family = binomial)
summary(model_NCSOS)
coef_NCSOS = rbind(round(summary(model_NCSOS)$coefficients[2,],8))

model_CSOS = glm(`Make Tournament` ~ `Conference SOS`, data = nccdata, family = binomial)
summary(model_CSOS)
coef_CSOS = rbind(round(summary(model_CSOS)$coefficients[2,],8))

model_CWP = glm(`Make Tournament` ~ `Conference Win %`, data = nccdata, family = binomial)
summary(model_CWP)
coef_CWP = rbind(round(summary(model_CWP)$coefficients[2,],8))

model_WAB = glm(`Make Tournament` ~ `Wins Above Bubble`, data = nccdata, family = binomial)
summary(model_WAB)
coef_WAB = rbind(round(summary(model_WAB)$coefficients[2,],8))

model_EA = glm(`Make Tournament` ~ `Efficiency Avg`, data = nccdata, family = binomial)
summary(model_EA)
coef_EA = rbind(round(summary(model_EA)$coefficients[2,],8))

coefs = as.data.frame(rbind(coef_L12, coef_CF, coef_RPI, coef_W, coef_AOE, coef_DOE, coef_BT,
              coef_SOS, coef_NCSOS, coef_CSOS, coef_CWP, coef_WAB, coef_EA)) 
    
coefs$Name = c("L12", "CF", "RPI", "W", "AOE", "DOE", "BT", "SOS", "NCSOS", "CSOS", "CF", "WAB", "EA")
    

## Building Multi-variate Models

model_full = glm(`Make Tournament` ~ `Last 12 Wins` + `Conference Finish` + `RPI Rank` + 
                   Wins + `Adj. Offensive Efficiency` + `Adj. Defensive Efficiency` + Barthag + 
                   SOS + `Non-Conf SOS` + `Conference SOS` + `Conference Finish` + `Wins Above Bubble`,
                 data = train_2, family = binomial)
summary(model_full)
vif(model_full)



    
    ## Confidence Interval 
    exp(summary(model_0)$coefficients[2,1] + qnorm(c(0.025,0.5,0.975)) * summary(model_0)$coefficients[2,2]) 
    
    ## Functions to Gather Prediction Accuracy Rate from Models
    calc_test_probs = function(model,test){
      probs = round(predict(model,test,type = "response"),3)
      predictions = as.data.frame(cbind(test$School, probs))
      predictions = predictions[order(-probs),]
      ps_test = predictions[1:36,1:2]
      colnames(ps_test) = c("School", "Probability")
      ps_test = ps_test %>% arrange(School)
      as_test = test %>% filter(`Make Tournament` == 1) %>% select(School)
      ps_test = as.character(ps_test$School)
      as_test = as.character(as_test$School)
      ms_test = as.data.frame(cbind(ps_test, as_test))
      colnames(ms_test) = c("Predicted Schools", "Actual Schools")
      cp = mean(ms_test$`Actual Schools` %in% ms_test$`Predicted Schools` == TRUE)
      cp
    }
    
    calc_train_probs = function(model, train){
      probs = round(predict(model,train,type = "response"),3)
      predictions = as.data.frame(cbind(train$School, train$Season, probs))
      colnames(predictions) = c("School", "Season", "Probability")
      ps_train = predictions %>% group_by(Season) %>% arrange(Season,-probs)
      ps_train = by(ps_train, ps_train["Season"], head, 36)
      ps_train = Reduce(rbind,ps_train)
      ps_train = ps_train %>% group_by(Season) %>% arrange(School, .by_group = TRUE)
      ps_train[,1:2] = lapply(ps_train[,1:2], as.character)
      as_train = train %>% filter(`Make Tournament` == 1) %>% select(School, Season)
      ms_train = as.data.frame(cbind(ps_train$School, ps_train$Season, as_train$School))
      colnames(ms_train) = c("Predicted Schools", "Season", "Actual Schools")
      correct_15 = sum(ms_train$`Actual Schools`[1:36] %in% ms_train$`Predicted Schools`[1:36] == TRUE)
      correct_16 = sum(ms_train$`Actual Schools`[37:72] %in% ms_train$`Predicted Schools`[37:72] == TRUE)
      correct_17 = sum(ms_train$`Actual Schools`[73:108] %in% ms_train$`Predicted Schools`[73:108] == TRUE)
      correct_18 = sum(ms_train$`Actual Schools`[109:144] %in% ms_train$`Predicted Schools`[109:144] == TRUE)
      cp = sum(correct_15,correct_16,correct_17,correct_18)/144
      cp
    }
    
    
    
    
    
    ## Potential Fix to Imbalance (TBD)
    down_train_1 <- downSample(x = train_1[, colnames(train_1) %ni% "`Make Tournament`"],
                               y = train_1$`Make Tournament`)
    table(down_train_1$`Make Tournament`)
    up_train_1 <- upSample(x = train_1[, colnames(train_1) %ni% "`Make Tournament`"],
                           y = train_1$`Make Tournament`)
    table(up_train_1$`Make Tournament`)
    
    