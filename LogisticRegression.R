## Load Packages
library(caret)
library(tidyverse)
library(car)
library(Rfast)
library(lmtest)
library(glmnet)
library(corrplot)
library(ROCR)
library(Hmisc)
library(ggrepel)
library(glmnet)
library(knitr)

## Creating data frame of just conference champs
ccdata = alldata %>% filter(`Conference Champ` == 1)

## Train = 2015-2018 Seasons, no Conference Champs
## Test = 2019 Season, no Conference Champs
train_2 = nccdata %>% filter(Season != 2019)
test_2 = nccdata %>% filter(Season == 2019)

## Bivariate Correlations
cors = cor(nccdata[,c(4:6,10,12,14,16,19:23,25,28,29)])
round(cors,2)
cors = as.data.frame(cors)

cors2 = rcorr(as.matrix(nccdata[,c(4:6,10,12,14,16,19:23,25,28,29)]))
cors2
corrplot(cors2$r, type="upper", order="hclust", 
         p.mat = cors2$P, sig.level = 0.05, insig = "blank")

#### INDIVIDUAL MODELS
model_L12 = glm(`Make Tournament` ~ `Last 12 Wins`, data = train_2, family = binomial)
summary(model_L12)
coef_L12 = rbind(round(summary(model_L12)$coefficients[2,],8))

model_CF = glm(`Make Tournament` ~ `Conference Finish`, data = train_2, family = binomial)
summary(model_CF)
coef_CF = rbind(round(summary(model_CF)$coefficients[2,],8))

model_RPI = glm(`Make Tournament` ~ `RPI Rank`, data = train_2, family = binomial)
summary(model_RPI)
coef_RPI = rbind(round(summary(model_RPI)$coefficients[2,],8))

model_W = glm(`Make Tournament` ~ `Wins`, data = train_2, family = binomial)
summary(model_W)
coef_W = rbind(round(summary(model_W)$coefficients[2,],8))

model_AOE = glm(`Make Tournament` ~ `Adj. Offensive Efficiency`, data = train_2, family = binomial)
summary(model_AOE)
coef_AOE = rbind(round(summary(model_AOE)$coefficients[2,],8))

model_DOE = glm(`Make Tournament` ~ `Adj. Defensive Efficiency`, data = train_2, family = binomial)
summary(model_DOE)
coef_DOE = rbind(round(summary(model_DOE)$coefficients[2,],8))

model_BT = glm(`Make Tournament` ~ `Barthag`, data = train_2, family = binomial)
summary(model_BT)
coef_BT = rbind(round(summary(model_BT)$coefficients[2,],8))

model_SOS = glm(`Make Tournament` ~ `SOS`, data = train_2, family = binomial)
summary(model_SOS)
coef_SOS = rbind(round(summary(model_SOS)$coefficients[2,],8))

model_NCSOS = glm(`Make Tournament` ~ `Non-Conf SOS`, data = train_2, family = binomial)
summary(model_NCSOS)
coef_NCSOS = rbind(round(summary(model_NCSOS)$coefficients[2,],8))

model_CSOS = glm(`Make Tournament` ~ `Conference SOS`, data = train_2, family = binomial)
summary(model_CSOS)
coef_CSOS = rbind(round(summary(model_CSOS)$coefficients[2,],8))

model_CWP = glm(`Make Tournament` ~ `Conference Win %`, data = train_2, family = binomial)
summary(model_CWP)
coef_CWP = rbind(round(summary(model_CWP)$coefficients[2,],8))

model_WAB = glm(`Make Tournament` ~ `Wins Above Bubble`, data = train_2, family = binomial)
summary(model_WAB)
coef_WAB = rbind(round(summary(model_WAB)$coefficients[2,],8))

model_ERA = glm(`Make Tournament` ~ `Efficiency Rank Avg`, data = train_2, family = binomial)
summary(model_ERA)
coef_ERA = rbind(round(summary(model_ERA)$coefficients[2,],8))

model_P5 = glm(`Make Tournament` ~ Power5 , data = train_2, family = binomial)
summary(model_P5)
coef_P5 = rbind(round(summary(model_P5)$coefficients[2,],8))

model_WP = glm(`Make Tournament` ~ `Win Percentage` , data = train_2, family = binomial)
summary(model_WP)
coef_WP = rbind(round(summary(model_WP)$coefficients[2,],8))

coefs = as.data.frame(rbind(coef_L12, coef_CF, coef_RPI, coef_W, coef_AOE, coef_DOE, coef_BT,
              coef_SOS, coef_NCSOS, coef_CSOS, coef_CWP, coef_WAB, coef_ERA, coef_P5, coef_WP)) 
    
coefs$Name = c("L12", "CF", "RPI", "W", "AOE", "DOE", "BT", "SOS", "NCSOS", "CSOS", "CWP", "WAB", "ERA", "P5", "WP")

## Building Multi-variate GLM Models
 ## FULL Model - includes all variables because all were univariately significant,
 ## except just used Efficiency Rank average, rather than both offensive and defensive
    
  model_full = glm(`Make Tournament` ~ `Last 12 Wins` +`Conference Finish` + `RPI Rank` + 
                       Wins + `Adj. Offensive Efficiency` + `Adj. Defensive Efficiency` + 
                       `Efficiency Rank Avg` + Barthag + SOS + `Non-Conf SOS` + 
                       `Conference SOS` + `Conference Win %` + `Wins Above Bubble` + 
                       Power5 + `Win Percentage`,
                       data = train_2, family = binomial)
    summary(model_full)
    vif(model_full)
    
    ## MUST run functions found near bottom of script
    calc_test_probs(model_full, test_2)
    calc_train_probs(model_full, train_2)
    
    ## First reduced model (only significant variables from full)
    model_reduced1 = glm(`Make Tournament` ~ `RPI Rank` + `Conference SOS`, 
                         data = train_2, family = binomial)
    
    summary(model_reduced1)
    vif(model_reduced1)
    
    delta.coef = abs((coef(model_reduced1)-coef(model_full)[-c(2,3,5:11,13:16)])/coef(model_full)[-c(2,3,5:11,13:16)])
    round(delta.coef,3)
    
    anova(model_full,model_reduced1, test = "Chisq")
    
    calc_test_probs(model_reduced1, test_2)
    calc_train_probs(model_reduced1, train_2)
    
    ## Second Reduced Model - Removed all variables that did not impact the significant variables - RPI Rank and Conf. SOS
    model_reduced2 = glm(`Make Tournament` ~ `Last 12 Wins` + `Conference Finish`
                          + `RPI Rank`+ Barthag + SOS + `Non-Conf SOS` + `Conference SOS`
                         + `Conference Win %` + `Wins Above Bubble`,
                         data = train_2, family = binomial)
    summary(model_reduced2)
    vif(model_reduced2)
    
    delta.coef = abs((coef(model_reduced2)[-c(2:3,5:7,9:10)]-coef(model_full)[-c(2:3,5:11,13:16)])/coef(model_full)[-c(2:3,5:11,13:16)])
    round(delta.coef,3)
    
    anova(model_full,model_reduced2, test = "Chisq")
    
    calc_test_probs(model_reduced2, test_2)
    calc_train_probs(model_reduced2, train_2)
    
    ## Third Reduced Model - found Last 12 Wins could be taken from second reduced model, still no coefficient change
        
    model_reduced3 = glm(`Make Tournament` ~  `Conference Finish`
                         + `RPI Rank` + Barthag + SOS + `Non-Conf SOS` + `Conference SOS`
                         + `Conference Win %` + `Wins Above Bubble`,
                         data = train_2, family = binomial)
    summary(model_reduced3)
    vif(model_reduced3)
    
    delta.coef = abs((coef(model_reduced3)[-c(2,4:6,8:9)]-coef(model_full)[-c(2:3,5:11,13:16)])/coef(model_full)[-c(2:3,5:11,13:16)])
    round(delta.coef,3)
    
    anova(model_full,model_reduced3, test = "Chisq")
    
    calc_test_probs(model_reduced3, test_2)
    calc_train_probs(model_reduced3, train_2)
    
    ## Note all models have multicollinarity concerns (see VIF figures), 
    ## primarily due to relationship between SOS, Non-Conf SOS, Conf SOS
   
    ## GLMNET Models
    
        model_net = glmnet(x = as.matrix(train_2[,c(4:6,10,12,14,16,19:23,25,28,29)]), y = train_2$`Make Tournament`, family = binomial)
        model_cvnet = cv.glmnet(x = as.matrix(train_2[,c(4:6,10,12,14,16,19:23,25,28,29)]), y = train_2$`Make Tournament`, family = binomial)
    
        ## Taking Wins Above Bubble Out
        model_net = glmnet(x = as.matrix(train_2[,c(4:6,10,12,14,16,19:22,25,28,29)]), y = train_2$`Make Tournament`, family = binomial)
        model_cvnet = cv.glmnet(x = as.matrix(train_2[,c(4:6,10,12,14,16,19:22,25,28,29)]), y = train_2$`Make Tournament`, family = binomial, alpha = 0.5)
      
          ## Load Model - if needed
        ## model_cvnet = readRDS("Models/CV-GLMNET Model")
          
  
          model_cvnet$lambda.min
          model_cvnet$lambda.1se
          
          coef(model_cvnet, s = "lambda.min")
          coef(model_cvnet, s = "lambda.1se")
          
          ## Creating Table of Coefficient Values
          cv_coefs = matrix(NA, nrow = 15, ncol = 2)
          
          cv_coefs[1:15,1] = c("Intercept", "Last 12 Wins", "Conference Finish", "RPI Rank", 
                               "Wins", "Adj. Offensive Efficiency", "Adj. Defensive Efficiency", 
                               "Barthag", "SOS", "Non-Conf SOS", "Conference SOS", "Conference Win %", 
                               "Efficiency Rank Avg", "Power 5", "Win Percentage")
          cv_coefs[1:15,2] = c(-20.464, 0.004, -0.200, -0.036, 0.183, 0.154, -0.090,
                               0.000, 13.952, 0.000, 2.715, 0.000, 0.000, 0.000, 0.000)
          
          cv_coefs = as.data.frame(cv_coefs)
          colnames(cv_coefs) = c("Variable", "Coefficient Value")
          kable(cv_coefs, format = "latex")
          
        ## Confusion Matrix, ROC Curve & AUC Calculation
        ptest = predict(model_cvnet, newx = as.matrix(test_2[,c(4:6,10,12,14,16,19:22,25,28,29)]), s = "lambda.1se", type = "response")
        for(i in 1:321){
          if(ptest[i] > 0.5){
            ptest[i] = 1
          } else{
            ptest[i] = 0
          }
        }
        
        confusion.glmnet(ptest, newx = as.matrix(test_2[,c(4:6,10,12,14,16,19:22,25,28,29)]), newy = test_2$`Make Tournament`, family = "binomial")
       
        pred = prediction(ptest, test_2$`Make Tournament`)
        perf = performance(pred,"tpr","fpr")
        performance(pred,"auc") 
        plot(perf,colorize=FALSE, col="black") 
        lines(c(0,1),c(0,1),col = "gray", lty = 4 )
        
        auc_ROCR = performance(pred, measure = "auc")
        auc_ROCR = auc_ROCR@y.values[[1]]
        
        roc.glmnet(ptest,newx = as.matrix(test_2[,c(4:6,10,12,14,16,19:22,25,28,29)]), newy = test_2$`Make Tournament`, family = "binomial")
        plot(roc.glmnet(model_cvnet,newx = as.matrix(test_2[,c(4:6,10,12,14,16,19:22,25,28,29)]), newy = test_2$`Make Tournament`, family = "binomial"))
        
        ptrain = predict(model_cvnet, newx = as.matrix(train_2[,c(4:6,10,12,14,16,19:22,25,28,29)]), s = "lambda.1se", type = "response")
        for(i in 1:1274){
          if(ptrain[i] > 0.5){
            ptrain[i] = 1
          } else{
            ptrain[i] = 0
          }
        }
        
        confusion.glmnet(ptrain, newx = as.matrix(train_2[,c(4:6,10,12,14,16,19:22,25,28,29)]), newy = train_2$`Make Tournament`, family = "binomial")
        
        calc_glmnet_test_probs(model_cvnet, "lambda.min")
        calc_glmnet_train_probs(model_cvnet, "lambda.min")
        calc_glmnet_test_probs(model_cvnet, "lambda.1se")
        calc_glmnet_train_probs(model_cvnet, "lambda.1se")
        
      ## Investigating CV.GLM Predictions
       
        ## ggplot showing fitted probability of making tournament by actual result, by season
        p_testinv = round(predict(model_cvnet, newx = as.matrix(test_2[,c(4:6,10,12,14,16,19:22,25,28,29)]), s = "lambda.1se", type = "response"),3)
        predictions_testinv = as.data.frame(cbind(test_2$School,p_testinv))
        colnames(predictions_testinv) = c("School", "Prob")
        ps_testinv = predictions_testinv %>% arrange(desc(Prob))
        as_testinv = test_2 %>% select(School, `Make Tournament`)
        
        p_traininv = round(predict(model_cvnet, newx = as.matrix(train_2[,c(4:6,10,12,14,16,19:22,25,28,29)]), s = "lambda.1se", type = "response"),3)
        predictions_traininv = as.data.frame(cbind(train_2$School, train_2$Season,p_traininv))
        colnames(predictions_traininv) = c("School", "Season", "Prob")
        ps_traininv = predictions_traininv %>% group_by(Season) %>% arrange(Season,(desc(p_traininv)))
        ps_traininv = ps_traininv %>% group_by(Season) %>% arrange(School, .by_group = TRUE)
        as_traininv = train_2 %>% select(School, `Make Tournament`,Season)
        
        ms_2015 = merge(ps_traininv %>% filter(Season == 2015), as_traininv %>% filter(Season == 2015), by = "School")
        ms_2015$Prob = as.numeric(ms_2015$Prob)
  
              ggplot(ms_2015, aes(x = `Make Tournament`, y = Prob)) + geom_point(size = 0.5) +
                   geom_text(aes(label=School),hjust=0, vjust=0, size = 2.5) + 
                scale_y_continuous(breaks = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) + 
                   labs(y = "Predicted Probability")
                 
              ggplot(ms_2015, aes(x = `Make Tournament`, y = `Prob`, color = factor(ncc_2015$Power5))) + 
                   geom_point(size=3.5) + scale_y_continuous(breaks = seq(0,1,by = 0.1)) +
                   geom_text_repel(direction = "x",aes(label=ifelse((Prob > 0.15 & Prob < 0.45 & `Make Tournament` == 1) | (Prob > 0.4 & `Make Tournament` == 0) ,as.character(School),''))
                             ,hjust=-0.05,vjust=0, size = 7) + 
                   geom_hline(yintercept = (0.431+0.359)/2, linetype = "dashed", color = "red") + 
                   labs(y = "Predicted Probability") + guides(color=guide_legend("Power 5")) + 
                   scale_color_manual(values=c("dark orange", "black"), breaks = c(1,0)) + 
                   ggtitle("2015 Teams Eligible for an At-Large Tournament Bid Ordered by Predicted Probabilities") + 
                theme(plot.title = element_text(size = 21,hjust = 0.5), axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15),axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18, margin = margin(r = 7.5)),legend.text=element_text(size=15),legend.title=element_text(size=18))
        ms_2016 = merge(ps_traininv %>% filter(Season == 2016), as_traininv %>% filter(Season == 2016), by = "School")
        ms_2016$Prob = as.numeric(ms_2016$Prob)
            
              ggplot(ms_2016, aes(x = `Make Tournament`, y = Prob )) + geom_point(size = 0.5) +
                  geom_text(aes(label=School),hjust=0, vjust=0, size = 2.5) + 
                  scale_y_continuous(breaks = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) + 
                  labs(y = "Predicted Probability")
        
              ggplot(ms_2016, aes(x = `Make Tournament`, y = `Prob`, color = factor(ncc_2016$Power5))) + 
                geom_point(size=3.5) + scale_y_continuous(breaks = seq(0,1,by = 0.1)) +
                geom_text_repel(direction = "x",aes(label=ifelse((Prob > 0.15 & Prob < 0.472 & `Make Tournament` == 1) | (Prob > 0.482 & `Make Tournament` == 0) ,as.character(School),''))
                          ,hjust=-0.075,vjust=0.5, size = 7) + 
                geom_hline(yintercept = (0.490 + 0.481)/2,linetype = "dashed", color = "red") + 
                labs(y = "Predicted Probability") + guides(color=guide_legend("Power 5")) + 
                scale_color_manual(values=c("dark orange", "black"), breaks = c(1,0)) + 
                ggtitle("2016 Teams Eligible for an At-Large Tournament Bid Ordered by Predicted Probabilities") + 
                theme(plot.title = element_text(size = 21,hjust = 0.5), axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15),axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18, margin = margin(r = 7.5)),legend.text=element_text(size=15),legend.title=element_text(size=18))
        
        ms_2017 = merge(ps_traininv %>% filter(Season == 2017), as_traininv %>% filter(Season == 2017), by = "School")
        ms_2017$Prob = as.numeric(ms_2017$Prob)
              ggplot(ms_2017, aes(x = `Make Tournament`, y = Prob )) + geom_point(size = 0.5) +
                  geom_text(aes(label=School),hjust=0, vjust=0, size = 2.5) + 
                  scale_y_continuous(breaks = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) + 
                  labs(y = "Predicted Probability")
              
              ggplot(ms_2017, aes(x = `Make Tournament`, y = `Prob`, color = factor(ncc_2017$Power5))) + 
                geom_point(size=3.5) + scale_y_continuous(breaks = seq(0,1,by = 0.1)) +
                geom_text_repel(direction = "x",aes(label=ifelse((Prob > 0.15 & Prob < 0.50 & `Make Tournament` == 1) | (Prob > 0.52 & `Make Tournament` == 0) ,as.character(School),''))
                          ,hjust=-0.05,vjust=-0.25, size = 7) + 
                geom_hline(yintercept = (0.529 + 0.513)/2,linetype = "dashed", color = "red")  + 
                labs(y = "Predicted Probability") + guides(color=guide_legend("Power 5")) + 
                scale_color_manual(values=c("dark orange", "black"), breaks = c(1,0)) + 
                ggtitle("2017 Teams Eligible for an At-Large Tournament Bid Ordered by Predicted Probabilities") + 
                theme(plot.title = element_text(size = 21,hjust = 0.5), axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15),axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18, margin = margin(r = 7.5)),legend.text=element_text(size=15),legend.title=element_text(size=18))
       
        ms_2018 = merge(ps_traininv %>% filter(Season == 2018), as_traininv %>% filter(Season == 2018), by = "School")
        ms_2018$Prob = as.numeric(ms_2018$Prob)
              ggplot(ms_2018, aes(x = `Make Tournament`, y = Prob )) + geom_point(size = 0.5) +
                  geom_text(aes(label=School),hjust=0, vjust=0, size = 2.5) + 
                  scale_y_continuous(breaks = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) + 
                  labs(y = "Predicted Probability")
              
              ggplot(ms_2018, aes(x = `Make Tournament`, y = `Prob`, color = factor(ncc_2018$Power5))) + 
                geom_point(size=3.5) + scale_y_continuous(breaks = seq(0,1,by = 0.1)) +
                geom_text_repel(direction = "x", aes(label=ifelse((Prob > 0.15 & Prob < 0.51 & `Make Tournament` == 1) | (Prob > 0.55 & `Make Tournament` == 0) ,as.character(School),''))
                          ,hjust=-0.05,vjust=-0.25, size = 7) + 
                geom_hline(yintercept = (0.577+0.510)/2, linetype = "dashed", color = "red")  + 
                labs(y = "Predicted Probability") + guides(color=guide_legend("Power 5")) + 
                scale_color_manual(values=c("dark orange", "black"), breaks = c(1,0)) + 
                ggtitle("2018 Teams Eligible for an At-Large Tournament Bid Ordered by Predicted Probabilities") + 
                theme(plot.title = element_text(size = 21,hjust = 0.5), axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15),axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18, margin = margin(r = 7.5)),legend.text=element_text(size=15),legend.title=element_text(size=18))
      
        ms_2019 = merge(ps_testinv, as_testinv, by = "School")
        ms_2019$Prob = as.numeric(ms_2019$Prob)
              ggplot(ms_2019, aes(x = `Make Tournament`, y = Prob )) + geom_point(size = 0.5) +
                    geom_text(aes(label=School),hjust=0, vjust=0, size = 2.5) + 
                    scale_y_continuous(breaks = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)) + 
                    labs(y = "Predicted Probability")
        
              ggplot(ms_2019, aes(x = `Make Tournament`, y = `Prob`, color = factor(ncc_2019$Power5))) + 
                geom_point(size=3.5) + scale_y_continuous(breaks = seq(0,1,by = 0.1)) +
                geom_text_repel(direction = "x",aes(label=ifelse((Prob > 0 & Prob < 0.40 & `Make Tournament` == 1) | (Prob > 0.55 & `Make Tournament` == 0) ,as.character(School),''))
                          ,hjust=-0.05,vjust=-0.2, size = 7) + 
                geom_hline(yintercept = (0.494+0.459)/2, linetype = "dashed", color = "red") +  
                labs(y = "Predicted Probability") + guides(color=guide_legend("Power 5")) + 
                scale_color_manual(values=c("dark orange", "black"), breaks = c(1,0)) + 
                ggtitle("2019 Teams Eligible for an At-Large Tournament Bid Ordered by Predicted Probabilities") + 
                theme(plot.title = element_text(size = 21,hjust = 0.5), axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15),axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18, margin = margin(r = 7.5)),legend.text=element_text(size=15),legend.title=element_text(size=18))
        
        ## Identifying teams incorrectly selected and missed
        p = predict(model_cvnet, newx = as.matrix(test_2[,c(4:6,10,12,14,16,19:22,25,28,29)]), s = "lambda.1se", type = "response")
        predictions = as.data.frame(cbind(test_2$School,p))
        colnames(predictions) = c("School", "Prob")
        ps_test = predictions %>% arrange(desc(Prob))
        ps_test = ps_test[1:36,]
        as_test = test_2 %>% filter(`Make Tournament` == 1) %>% select(School, `Make Tournament`)
        ms_test = as.data.frame(cbind(ps_test,as_test))
        colnames(ms_test) = c("Predicted Schools", "Prob", "Actual Schools", "Make Tournament")
        
        p = predict(model_cvnet, newx = as.matrix(train_2[,c(4:6,10,12,14,16,19:22,25,28,29)]), s = "lambda.1se", type = "response")
        predictions = as.data.frame(cbind(train_2$School, train_2$Season,p))
        colnames(predictions) = c("School", "Season", "Prob")
        ps_train = predictions %>% group_by(Season) %>% arrange(Season,(desc(p)))
        ps_train = by(ps_train, ps_train["Season"], head, 36)
        ps_train = Reduce(rbind,ps_train)
        ps_train = ps_train %>% group_by(Season) %>% arrange(School, .by_group = TRUE)
        ps_train[,1:2] = lapply(ps_train[,1:2], as.character)
        as_train = train_2 %>% filter(`Make Tournament` == 1) %>% select(School, Season)
        ms_train = as.data.frame(cbind(ps_train$School, ps_train$Season, as_train$School))
        colnames(ms_train) = c("Predicted Schools", "Season", "Actual Schools")
        
        ## 2015
        setdiff(ms_train$`Actual Schools`[1:36], ms_train$`Predicted Schools`[1:36])
        setdiff(ms_train$`Predicted Schools`[1:36], ms_train$`Actual Schools`[1:36])
        ## Boise St. #11 seed in First Four, Cincinnati #8 seed
        ## CSU, Miami #1 or #2 seed in NIT
        
        ## 2016
        setdiff(ms_train$`Actual Schools`[37:72], ms_train$`Predicted Schools`[37:72])
        setdiff(ms_train$`Predicted Schools`[37:72], ms_train$`Actual Schools`[37:72])
        ## Tulsa, Michigan #11 seeds in First Four, Syracuse & Temple #10 seeds
        ## St. Bonaventure, Florida, South Carolina, Saint Mary's (CA) #1 or #2 seeds in NIT
        
        ## 2017
        setdiff(ms_train$`Actual Schools`[73:108], ms_train$`Predicted Schools`[73:108])
        setdiff(ms_train$`Predicted Schools`[73:108], ms_train$`Actual Schools`[73:108])
        ## USC #11 seed in First Four
        ## Illinois State #1 seed in NIT
        
        ## 2018
        setdiff(ms_train$`Actual Schools`[109:144], ms_train$`Predicted Schools`[109:144])
        setdiff(ms_train$`Predicted Schools`[109:144], ms_train$`Actual Schools`[109:144])
        ## Arizona St, Syracuse #11 seeds in First Four, Virginia Tech #8 seed, Florida St #9 seeds
        ## USC #1 seed in NIT, Louisville, Marquette #2 seeds in NIT Nebraska #5 seed in NIT
        
        ## 2019
        setdiff(ms_test$`Actual Schools`, ms_test$`Predicted Schools`)
        setdiff(ms_test$`Predicted Schools`, ms_test$`Actual Schools`)
        ## Belmont, St. Johns #11 seeds in First Four Game, Ohio State #11 seed
        ## TCU, Texas, Creighton #1,#2 seeds in NIT,
        
        
        ## Building Table of Prediction Results
        prediction_results = matrix(NA,nrow = 5, ncol = 3)
        prediction_results[1:5,1] = c("2015", "2016", "2017", "2018", "2019")
        prediction_results[1:5,2] = c(34,32,35,32,33)
        prediction_results[1:5,3] = c(94.4,88.8,97.2,88.8,91.7)
        prediction_results = as.data.frame(prediction_results)
        colnames(prediction_results) = c("Season", "# of Correct Selections", "Correct Selections Percentage")
        kable(prediction_results, format = "latex")
        
        ## Creating Dataset of Teams Incorrectly Left Out of Tournament
        which(train_2$School == "Boise State" & train_2$Season == 2015)
        which(train_2$School == "Cincinnati" & train_2$Season == 2015)
        
        which(train_2$School == "Tulsa" & train_2$Season == 2016)
        which(train_2$School == "Temple" & train_2$Season == 2016)
        which(train_2$School == "Michigan" & train_2$Season == 2016)
        which(train_2$School == "Syracuse" & train_2$Season == 2016)
        
        which(train_2$School == "Southern California" & train_2$Season == 2017)

        which(train_2$School == "Arizona State" & train_2$Season == 2018)
        which(train_2$School == "Syracuse" & train_2$Season == 2018)
        which(train_2$School == "Virginia Tech" & train_2$Season == 2018)
        which(train_2$School == "Florida State" & train_2$Season == 2018)
        
        which(test_2$School == "Belmont")
        which(test_2$School == "St. John's (NY)")
        which(test_2$School == "Ohio State")
        
        leftout = rbind(train_2[c(21,46,601,581,472,580,893,967,1220,1254,1040),], test_2[c(20,264,204),])
        
        
        ## Creating Dataset of Teams Incorrectly Put In Tournament
        which(train_2$School == "Colorado State" & train_2$Season == 2015)
        which(train_2$School == "Miami (FL)" & train_2$Season == 2015)
        
        which(train_2$School == "St. Bonaventure" & train_2$Season == 2016)
        which(train_2$School == "San Diego State" & train_2$Season == 2016)
        which(train_2$School == "South Carolina" & train_2$Season == 2016)
        which(train_2$School == "Florida" & train_2$Season == 2016)
        
        which(train_2$School == "Illinois State" & train_2$Season == 2017)
        
        which(train_2$School == "Southern California" & train_2$Season == 2018)
        which(train_2$School == "Marquette" & train_2$Season == 2018)
        which(train_2$School == "Louisville" & train_2$Season == 2018)
        which(train_2$School == "Nebraska" & train_2$Season == 2018)
        
        which(test_2$School == "Texas Christian")
        which(test_2$School == "Texas")
        which(test_2$School == "Creighton")
        
        putin = rbind(train_2[c(53,154,575,555,564,400,752,1209,1099,1093,1125),], test_2[c(282,275,59),])
        
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
  
    calc_glmnet_test_probs = function(model,s){
      p = predict(model, newx = as.matrix(test_2[,c(4:6,10,12,14,16,19:22,25,28,29)]), s , type = "response")
      predictions = as.data.frame(cbind(test_2$School,p))
      colnames(predictions) = c("School", "Prob")
      ps_test = predictions %>% arrange(desc(Prob))
      ps_test = ps_test[1:36,]
      as_test = test_2 %>% filter(`Make Tournament` == 1) %>% select(School, `Make Tournament`)
      ms_test = as.data.frame(cbind(ps_test,as_test))
      mean(as_test$School %in% ps_test$School)
    }
    
    calc_glmnet_train_probs = function(model,s){
      p = predict(model, newx = as.matrix(train_2[,c(4:6,10,12,14,16,19:22,25,28,29)]), s, type = "response")
      predictions = as.data.frame(cbind(train_2$School, train_2$Season,p))
      colnames(predictions) = c("School", "Season", "Prob")
      ps_train = predictions %>% group_by(Season) %>% arrange(Season,(desc(p)))
      ps_train = by(ps_train, ps_train["Season"], head, 36)
      ps_train = Reduce(rbind,ps_train)
      ps_train[,1:2] = lapply(ps_train[,1:2], as.character)
      as_train = train_2 %>% filter(`Make Tournament` == 1) %>% select(School, Season)
      ms_train = as.data.frame(cbind(ps_train$School, ps_train$Season, as_train$School))
      colnames(ms_train) = c("Predicted Schools", "Season", "Actual Schools")
      correct_15 = sum(ms_train$`Actual Schools`[1:36] %in% ms_train$`Predicted Schools`[1:36] == TRUE)
      correct_16 = sum(ms_train$`Actual Schools`[37:72] %in% ms_train$`Predicted Schools`[37:72] == TRUE)
      correct_17 = sum(ms_train$`Actual Schools`[73:108] %in% ms_train$`Predicted Schools`[73:108] == TRUE)
      correct_18 = sum(ms_train$`Actual Schools`[109:144] %in% ms_train$`Predicted Schools`[109:144] == TRUE)
      cp = sum(correct_15,correct_16,correct_17,correct_18)/144
      cp
    }
    
    
      ## Fitting GLMNET Model on each season separately
    
        model_cvnet15 = cv.glmnet(x = as.matrix(ncc_2015[,c(4:6,10,12,14,16,19:22,25,28,29)]), y = ncc_2015$`Make Tournament`, family = binomial)
        model_cvnet16 = cv.glmnet(x = as.matrix(ncc_2016[,c(4:6,10,12,14,16,19:22,25,28,29)]), y = ncc_2016$`Make Tournament`, family = binomial)
        model_cvnet17 = cv.glmnet(x = as.matrix(ncc_2017[,c(4:6,10,12,14,16,19:22,25,28,29)]), y = ncc_2017$`Make Tournament`, family = binomial)
        model_cvnet18 = cv.glmnet(x = as.matrix(ncc_2018[,c(4:6,10,12,14,16,19:22,25,28,29)]), y = ncc_2018$`Make Tournament`, family = binomial)
        model_cvnet19 = cv.glmnet(x = as.matrix(ncc_2019[,c(4:6,10,12,14,16,19:22,25,28,29)]), y = ncc_2019$`Make Tournament`, family = binomial)
        
        ## Comparing Coefficients
            cv_coef_function = function(model,s){
              data = data.frame(Variable = coef(model, s)@Dimnames[[1]])
              for(i in 1:length(data$Variable)){
                if(coef(model,s)[i] == 0){
                  data$`Coefficient Value`[i] = 0
                }else{
                  data$`Coefficient Value`[i] = round(coef(model,s)[i],3)
                }
              }
              data
            }
            
            cv_15se = cv_coef_function(model_cvnet15, "lambda.1se")
            cv_16se = cv_coef_function(model_cvnet16, "lambda.1se")
            cv_17se = cv_coef_function(model_cvnet17, "lambda.1se")
            cv_18se = cv_coef_function(model_cvnet18, "lambda.1se")
            cv_19se = cv_coef_function(model_cvnet19, "lambda.1se")
            
            cv_15se$Season = 2015
            cv_16se$Season = 2016
            cv_17se$Season = 2017
            cv_18se$Season = 2018
            cv_19se$Season = 2019
            

            secoefs = rbind(cv_15se, cv_16se, cv_17se, cv_18se, cv_19se)
            secoefs$Variable = rep(c("Intercept", "Last 12", "Conf Finish", "RPI", "Wins", "Adj. OE",
                                  "Adj. DE", "Barthag", "SOS", "Non-Conf SOS", "Conf SOS", "Conf Win %",
                                   "Eff Rank Avg", "Power 5", "Win %"),5)
            
            ggplot(secoefs[-c(1,16,31,46,61),], aes(x = Variable, y = `Coefficient Value`, group = Season, color = Season)) + geom_line(size = 1.4) +
              geom_point() + labs(color = "Season") + ggtitle("Coefficient Values of Each Variable in CV.GLMNET Model By Season") + theme(plot.title = element_text(hjust = 0.5)) + 
              scale_color_gradient() + geom_text_repel(direction = "y", force = 2, nudge_x = 1, aes(label = ifelse(Variable == "SOS",Season,"")), size = 7) + 
              theme(legend.position = "none") + theme(plot.title = element_text(size = 21,hjust = 0.5), axis.text.x = element_text(size = 20, angle = 45, vjust = 0.65), axis.text.y = element_text(size = 15),axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18, margin = margin(r = 7.5)),legend.text=element_text(size=15),legend.title=element_text(size=18))
            
            
            ### JUSTINCASE
            + geom_text(aes(0,(0.577+0.510)/2,label = "Proposed Tournament Cutoff", hjust = -3.95, vjust = -0.5), size = 4.5)
            ###
    
            