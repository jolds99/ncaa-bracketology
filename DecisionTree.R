## Load Packages
library(tree)
library(rpart)

## Table comparing variable averages grouped by if made tournament
ncctable =  nccdata %>% 
  group_by(`Make Tournament`) %>% 
  summarise_at(vars(`Last 12 Wins`, `Conference Finish`, `RPI Rank`,
                    `NET Rank`, `Wins`, `T-Rank`, SOS, `Non-Conf SOS`,
                    `Conference Win %`, `Wins Above Bubble`, `WAB Rank`,
                    `Adj. Offensive Efficiency`, `Adj. Defensive Efficiency`,
                     `Efficiency Avg`),list(name = mean))
colnames(ncctable)[2:15] = c("Last 12 Wins", "Conference Finish", "RPI Rank",
                          "NET Rank", "Wins", "T-Rank", "SOS", "Non-Conf SOS",
                          "Conference SOS", "Wins Above Bubble", "WAB Rank",
                          "Adj. Offensive Efficiency", "Adj. Defensive Efficiency",
                          "Efficiency Avg")
ncctable

## Creating Pairwise Combination Matrices
create_comb_matrix_function = function(year){
count = 0
x = matrix(NA,nrow = 50721, ncol = 54)
x = as.data.frame(x)
for(i in 1:318){
  for(j in (i+1):length(year$School)){
    count = count + 1
    x[count,1:27] = year[i,]
    x[count,28:54] = year[j,]
   }
}
x
}

combo2015 = create_comb_matrix_function(ncc_2015)
combo2016 = create_comb_matrix_function(ncc_2016)
combo2017 = create_comb_matrix_function(ncc_2017)
combo2018 = create_comb_matrix_function(ncc_2018)

create_comb19_matrix_function = function(year){
  count = 0
  x = matrix(NA,nrow = 51360, ncol = 54)
  x = as.data.frame(x)
  for(i in 1:320){
    for(j in (i+1):length(year$School)){
      count = count + 1
      x[count,1:27] = year[i,]
      x[count,28:54] = year[j,]
    }
  }
  x
}

combo2019 = create_comb19_matrix_function(ncc_2019)

 ## Changing column names

change_col_names_function = function(x){
  colnames(x) = c("School 1", "Make Tournament 1", "Conference Champ 1", "Last 12 Wins 1", 
                  "Conference Finish 1", "RPI Rank 1", "NET Rank 1", "Conference 1", "Record 1",
                  "Wins 1", "Losses 1", "Adj. Offensive Efficiency 1", "OE Rank 1",
                  "Adj. Defensive Efficiency 1", "DE Rank 1", "Barthag 1", "T-Rank 1", 
                  "Conference Record 1", "SOS 1", "Non-Conf SOS 1", "Conference SOS 1", 
                  "Conference Win % 1", "Wins Above Bubble 1", "WAB Rank 1","Efficiency Rank Avg 1",
                  "Efficiency Avg 1", "Season 1","School 2", "Make Tournament 2", "Conference Champ 2", "Last 12 Wins 2", 
                  "Conference Finish 2", "RPI Rank 2", "NET Rank 2", "Conference 2", "Record 2",
                  "Wins 2", "Losses 2", "Adj. Offensive Efficiency 2", "OE Rank 2",
                  "Adj. Defensive Efficiency 2", "DE Rank 2", "Barthag 2", "T-Rank 2", 
                  "Conference Record 2", "SOS 2", "Non-Conf SOS 2", "Conference SOS 2", 
                  "Conference Win % 2", "Wins Above Bubble 2", "WAB Rank 2", "Efficiency Rank Avg 2",
                  "Efficiency Avg 2", "Season 2")
  x
}

combo2015 = change_col_names_function(combo2015)
combo2016 = change_col_names_function(combo2016)
combo2017 = change_col_names_function(combo2017)
combo2018 = change_col_names_function(combo2018)
combo2019 = change_col_names_function(combo2019)

## Saving pairwise combination matrices

write_csv(combo2015, "Pairwise Data Sets/combo2015.csv")
write_csv(combo2016, "Pairwise Data Sets/combo2016.csv")
write_csv(combo2017, "Pairwise Data Sets/combo2017.csv")
write_csv(combo2018, "Pairwise Data Sets/combo2018.csv")
write_csv(combo2019, "Pairwise Data Sets/combo2019.csv")

## Decision Tree Replication of 2018 Study

decision_tree1 = function(x){
  x$Victory = rep(NA,length(x$`School 1`))
  for(i in 1:length(x$`School 1`)){
    if(abs(x$`RPI Rank 1`[i] - x$`RPI Rank 2`[i]) > 45){
      if(x$`RPI Rank 1`[i] - x$`RPI Rank 2`[i] < -45){
        x$Victory[i] = x$`School 1`[i]
      } else{
        x$Victory[i] = x$`School 2`[i]
      }
    }
    
    else if(abs(x$`Wins 1`[i] - x$`Wins 2`[i]) > 12){
      if(x$`Wins 1`[i] - x$`Wins 2`[i] > 12){
        x$Victory[i] = x$`School 1`[i]
      } else{ 
        x$Victory[i] = x$`School 2`[i]
      }
    }
    
    else if(abs(x$`RPI Rank 1`[i] - x$`RPI Rank 1`[i]) > 30){
      if(x$`RPI Rank 1`[i] - x$`RPI Rank 2`[i] < -30){
        x$Victory[i] = x$`School 1`[i]
      } else{
        x$Victory[i] = x$`School 2`[i]
      }
    }
    
    else if(abs(x$`RPI Rank 1`[i] - x$`RPI Rank 2`[i]) > 12
            && abs(x$`Barthag 1`[i] - x$`Barthag 2`[i]) > 0.0625){
      if(x$`RPI Rank 1`[i] - x$`RPI Rank 2`[i] < -12 
         && x$`Barthag 1`[i] - x$`Barthag 2`[i] > 0.0625){
        x$Victory[i] = x$`School 1`[i]
      }else{
        x$Victory[i] = x$`School 2`[i]
      }
    }
    else if(abs(x$`Barthag 1`[i] - x$`Barthag 2`[i] > 0.25)){
      if(x$`Barthag 1`[i] - x$`Barthag 2`[i] > 0.25){
        x$Victory[i] = x$`School 1`[i] 
      } else{
        x$Victory[i] = x$`School 2`[i]
      }
    }
    
    else if(abs(x$`SOS 1`[i] - x$`SOS 2`[i]) > 0.15){
      if(x$`SOS 1`[i] - x$`SOS 2`[i] > 0.15){
        x$Victory[i] = x$`School 1`[i]
      } else{
        x$Victory[i] = x$`School 2`[i]
      }
    }
    
    else if(abs(x$`Last 12 Wins 1`[i] - x$`Last 12 Wins 2`[i]) > 3 && 
            abs(x$`RPI Rank 1`[i] - x$`RPI Rank 2`[i]) < 15){
      if(x$`Last 12 Wins 1`[i] - x$`Last 12 Wins 2`[i] > 3){
        x$Victory[i] = x$`School 1`[i]
      } else{ 
        x$Victory[i] = x$`School 2`[i]
      }
    }
    
    else if(abs(x$`SOS 1`[i] - x$`SOS 2`[i]) > 0.075){
      if(x$`SOS 1`[i] - x$`SOS 2`[i] > 0.075){
        x$Victory[i] = x$`School 1`[i]
      } else{
        x$Victory[i] = x$`School 2`[i]
      }
    }
    
    else if(abs(x$`RPI Rank 1`[i] - x$`RPI Rank 2`[i] > 15)){
      if(x$`RPI Rank 1`[i] - x$`RPI Rank 2`[i] < -15){
        x$Victory[i] = x$`School 1`[i] 
      } else{
        x$Victory[i] = x$`School 2`[i]
      }
    }
    
    else if(abs(x$`Wins 1`[i] - x$`Wins 2`[i] > 2)){
      if(x$`Wins 1`[i] - x$`Wins 2`[i] > 2){
        x$Victory[i] = x$`School 1`[i] 
      } else{
        x$Victory[i] = x$`School 2`[i]
      }
    }
    else if(x$`Barthag 1`[i] > x$`Barthag 2`[i]){
      x$Victory[i] = x$`School 1`[i]
    }
    else if(x$`Barthag 2`[i] > x$`Barthag 1`[i]){
      x$Victory[i] = x$`School 2`[i]
    }
  }
  x
}

## Calculating results for each year

victory_2015 = decision_tree1(combo2015)
selections_2015 = victory_2015 %>% filter(is.na(Victory) == FALSE) %>% select(Victory) %>% arrange(Victory) %>% 
  group_by(Victory) %>% summarise(n=n())  %>% ungroup() %>% arrange(-n)
act2015 = ncc_2015 %>% filter(`Make Tournament` == 1)
pre2015 = selections_2015$Victory[1:36]
comp = as.data.frame(cbind(act2015$School,sort(pre2015)))
mean(act2015$School %in% pre2015 == TRUE)

length(which(is.na(victory_2015$Victory)))
length(which(is.na(victory_2015$Victory) == FALSE))


victory_2016 = decision_tree1(combo2016)
selections_2016 = victory_2016 %>% filter(is.na(Victory) == FALSE) %>% select(Victory) %>% arrange(Victory) %>% 
  group_by(Victory) %>% summarise(n=n())  %>% ungroup() %>% arrange(-n)
act2016 = ncc_2016 %>% filter(`Make Tournament` == 1)
pre2016 = selections_2016$Victory[1:36]
comp = as.data.frame(cbind(act2016$School,sort(pre2016)))
mean(act2016$School %in% pre2016 == TRUE)

length(which(is.na(victory_2016$Victory)))
length(which(is.na(victory_2016$Victory) == FALSE))


victory_2017 = decision_tree1(combo2017)
selections_2017 = victory_2017 %>% filter(is.na(Victory) == FALSE) %>% select(Victory) %>% arrange(Victory) %>% 
  group_by(Victory) %>% summarise(n=n())  %>% ungroup() %>% arrange(-n)
act2017 = ncc_2017 %>% filter(`Make Tournament` == 1)
pre2017 = selections_2017$Victory[1:36]
comp = as.data.frame(cbind(act2017$School,sort(pre2017)))
mean(act2017$School %in% pre2017 == TRUE)

length(which(is.na(victory_2017$Victory)))
length(which(is.na(victory_2017$Victory) == FALSE))


victory_2018 = decision_tree1(combo2018)
selections_2018 = victory_2018 %>% filter(is.na(Victory) == FALSE) %>% select(Victory) %>% arrange(Victory) %>% 
  group_by(Victory) %>% summarise(n=n())  %>% ungroup() %>% arrange(-n)
act2018 = ncc_2018 %>% filter(`Make Tournament` == 1)
pre2018 = selections_2018$Victory[1:36]
comp = as.data.frame(cbind(act2018$School,sort(pre2018)))
mean(act2018$School %in% pre2018 == TRUE)

length(which(is.na(victory_2018$Victory)))
length(which(is.na(victory_2018$Victory) == FALSE))


victory_2019 = decision_tree1(combo2019)
selections_2019 = victory_2019 %>% filter(is.na(Victory) == FALSE) %>% select(Victory) %>% arrange(Victory) %>% 
  group_by(Victory) %>% summarise(n=n())  %>% ungroup() %>% arrange(-n)
act2019 = ncc_2019 %>% filter(`Make Tournament` == 1)
pre2019 = selections_2019$Victory[1:36]
comp = as.data.frame(cbind(act2019$School,sort(pre2019)))
mean(act2019$School %in% pre2019 == TRUE)

length(which(is.na(victory_2019$Victory)))
length(which(is.na(victory_2019$Victory) == FALSE))

## Testing Third Reduced Model (See Logistic Regression Script) with Tree & Rpart Packages
tree.test = tree(`Make Tournament` ~ ConfFinish + RPI + Barthag +
                    SOS + NonConfSOS + ConfSOS + ConfWinP + WAB, tree_train)
plot(tree.test)
text(tree.test, pretty = 0)
tree.pred = predict(tree.test, tree_test, type="class")
with(tree_test, table(tree.pred, `Make Tournament`))

tree_train = train_2
colnames(tree_train)[c(4,5,6,16,20,21,22,23,26)] = c("Last12", "ConfFinish", "RPI", "Barthag", "NonConfSOS", "ConfSOS", "ConfWinP", "WAB", "EffAvg")
tree_test = test_2
colnames(tree_test)[c(4,5,6,16,20,21,22,23,26)] = c("Last12", "ConfFinish", "RPI", "Barthag", "NonConfSOS", "ConfSOS", "ConfWinP", "WAB", "EffAvg")

rparttest = rpart(`Make Tournament` ~ `Conference Finish` +`RPI Rank`+ Barthag
                  + SOS + `Non-Conf SOS` + `Conference SOS` + `Conference Win %` + `Wins Above Bubble`, 
                  data = train_2, method = "class")
plot(rparttest)
text(rparttest, use.n=TRUE, all=TRUE, cex=0.8)

rpart.pred = predict(rparttest, test_2, type="class")
with(test_2, table(rpart.pred, `Make Tournament`))


