
## More Logistic Regression Code (May be the wrong process)
    ## Second reduced model - took out Win Percentage, coefficients do not change signficantly, similar model fit to full by anova
    model_reduced2 = glm(`Make Tournament` ~ `Last 12 Wins` + `Conference Finish` 
                         + `RPI Rank` +  Wins + `Efficiency Rank Avg` + Barthag  
                         + SOS + `Non-Conf SOS` + `Conference SOS`
                         + `Conference Win %` + `Wins Above Bubble` + Power5,
                         data = train_2, family = binomial)
    summary(model_reduced2)
    vif(model_reduced2)
    
    delta.coef = abs((coef(model_reduced2)-coef(model_full)[-c(14)])/coef(model_full)[-c(14)])
    round(delta.coef,3)
    
    anova(model_full,model_reduced2, test = "Chisq")
    
    calc_test_probs(model_reduced2, test_2)
    calc_train_probs(model_reduced2, train_2)
    
    ## Third Reduced Model - took out Efficiency Rank Avg from second reduced, coefficients do not change significantly, similar model fit to full by anova
    model_reduced3 = glm(`Make Tournament` ~ `Last 12 Wins` + `Conference Finish` 
                         + `RPI Rank` + Wins  + Barthag  + SOS + `Non-Conf SOS` +  
                           `Conference SOS` + `Conference Win %` + `Wins Above Bubble` + Power5,
                         data = train_2, family = binomial)
    summary(model_reduced3)
    vif(model_reduced3)
    
    delta.coef = abs((coef(model_reduced3)-coef(model_full)[-c(14,6)])/coef(model_full)[-c(14,6)])
    round(delta.coef,3)
    
    anova(model_full,model_reduced3, test = "Chisq")
    
    calc_test_probs(model_reduced3, test_2)
    calc_train_probs(model_reduced3, train_2)
    
    ## Fourth Reduced Model - took out Power 5 from third reduced, coefficients largely do not change significantly, similar model fit to full by anova
    model_reduced4 = glm(`Make Tournament` ~  `Last 12 Wins` +`Conference Finish` 
                         + `RPI Rank` + Wins   + Barthag  + SOS  +  `Non-Conf SOS` + 
                           `Conference SOS` + `Conference Win %`  + `Wins Above Bubble`,
                         data = train_2, family = binomial)
    summary(model_reduced4)
    vif(model_reduced4)
    
    delta.coef = abs((coef(model_reduced4)-coef(model_full)[-c(14,6,13)])/coef(model_full)[-c(14,6,13)])
    round(delta.coef,3)
    
    anova(model_full,model_reduced4, test = "Chisq")
    
    calc_test_probs(model_reduced4, test_2)
    calc_train_probs(model_reduced4, train_2)
    
    ## Fifth Reduced Model - Took out Wins from fourth reduced.Coefficients do not change significantly from full, fit is still similar by anova
    model_reduced5 = glm(`Make Tournament` ~ `Last 12 Wins` + `Conference Finish` 
                         + `RPI Rank` + Barthag  + SOS +  `Non-Conf SOS` 
                         + `Conference SOS`  + `Conference Win %`+ `Wins Above Bubble`,
                         data = train_2, family = binomial)
    summary(model_reduced5)
    vif(model_reduced5)
    
    delta.coef = abs((coef(model_reduced5)-coef(model_full)[-c(14,6,13,5)])/coef(model_full)[-c(14,6,13,5)])
    round(delta.coef,3)
    
    anova(model_full,model_reduced5, test = "Chisq")
    
    
    ## Sixth Reduced Model - Took out Last 12 Wins from fifth reduced, coefficients largely do not change significantly, similar fit to full by anova
    model_reduced6 = glm(`Make Tournament` ~  `Conference Finish` +  `RPI Rank` + Barthag 
                         + SOS +  `Non-Conf SOS` + `Conference SOS` + `Conference Win %`
                         + `Wins Above Bubble`,data = train_2, family = binomial)
    summary(model_reduced6)
    vif(model_reduced6)
    
    delta.coef = abs((coef(model_reduced6)-coef(model_full)[-c(14,6,13,5,2)])/coef(model_full)[-c(14,6,13,5,2)])
    round(delta.coef,3)
    
    anova(model_full,model_reduced6, test = "Chisq")
    
    
    ## Seventh Reduced Model - Took out Conference Win % from sixth reduced. At this point, coefficients change significantly, but still similar fit to full by anova
    model_reduced7 = glm(`Make Tournament` ~  `Conference Finish` + `RPI Rank` + Barthag 
                         + SOS  +  `Non-Conf SOS`+ `Conference SOS` + `Wins Above Bubble`,
                         data = train_2, family = binomial)
    summary(model_reduced7)
    vif(model_reduced7)
    
    delta.coef = abs((coef(model_reduced7)-coef(model_full)[-c(14,6,13,5,2,11)])/coef(model_full)[-c(14,6,13,5,2,11)])
    round(delta.coef,3)
    anova(model_full,model_reduced7, test = "Chisq")
    
    
    ## Eighth Reduced Model - took out Conference Finish from seventh reduced, similar fit to full by anova
    model_reduced8 = glm(`Make Tournament` ~ `RPI Rank` + Barthag  + SOS 
                         +`Non-Conf SOS` + `Conference SOS`
                         + `Wins Above Bubble`, data = train_2, family = binomial)
    summary(model_reduced8)
    vif(model_reduced8)
    
    delta.coef = abs((coef(model_reduced8)-coef(model_full)[-c(14,6,13,5,2,11,3)])/coef(model_full)[-c(14,6,13,5,2,11,3)])
    round(delta.coef,3)
    
    anova(model_full,model_reduced8, test = "Chisq")
    
    ## Ninth Reduced Model - took out Wins Above Bubble from eighth reduced, similar fit to full by anova
    model_reduced9 = glm(`Make Tournament` ~ `RPI Rank` + Barthag  + SOS 
                         +`Non-Conf SOS` + `Conference SOS`,data = train_2, family = binomial)
    summary(model_reduced9)
    vif(model_reduced9)
    
    delta.coef = abs((coef(model_reduced9)-coef(model_full)[-c(14,6,13,5,2,11,3,12)])/coef(model_full)[-c(14,6,13,5,2,11,3,12)])
    round(delta.coef,3)
    
    anova(model_full,model_reduced9, test = "Chisq")
    
    ## Tenth Reduced Model - took out Non-Conf SOS from ninth reduced, similar fit to full by anova
    model_reduced10 = glm(`Make Tournament` ~ `RPI Rank` + Barthag + SOS 
                          + `Conference SOS`, data = train_2, family = binomial)
    summary(model_reduced10)
    vif(model_reduced10)
    
    delta.coef = abs((coef(model_reduced10)-coef(model_full)[-c(14,6,13,5,2,11,3,12,9)])/coef(model_full)[-c(14,6,13,5,2,11,3,12,9)])
    round(delta.coef,3)
    
    anova(model_full,model_reduced10, test = "Chisq")
    
    ## Eleventh Reduced Model - took out SOS from tenth reduced, similar fit to full by anova
    model_reduced11 = glm(`Make Tournament` ~ `RPI Rank` 
                          + Barthag + `Conference SOS`, data = train_2, family = binomial)
    summary(model_reduced11)
    vif(model_reduced11)
    
    delta.coef = abs((coef(model_reduced11)-coef(model_full)[-c(14,6,13,5,2,11,3,12,9,8)])/coef(model_full)[-c(14,6,13,5,2,11,3,12,9,8)])
    round(delta.coef,3)
    
    anova(model_full,model_reduced11, test = "Chisq")
