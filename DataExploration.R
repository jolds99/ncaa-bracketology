## Load Packages
library(ggplot2)
library(tidyverse)
library(dplyr)

## Load full Datasets
full_2015 = read_csv("Full CSV Datasets/full_2015.csv")
full_2016 = read_csv("Full CSV Datasets/full_2016.csv")
full_2017 = read_csv("Full CSV Datasets/full_2017.csv")
full_2018 = read_csv("Full CSV Datasets/full_2018.csv")
full_2019 = read_csv("Full CSV Datasets/full_2019.csv")
full_2020 = read_csv("Full CSV Datasets/full_2020.csv")

## 2015-2019 Data Merged
alldata = rbind(full_2015,full_2016,full_2017,full_2018,full_2019)

## Changing class of various datasets
alldata$`Make Tournament`= as.factor(alldata$`Make Tournament`)
alldata$`Conference Champ` = as.factor(alldata$`Conference Champ`)
alldata$`Conference Finish` = as.numeric(alldata$`Conference Finish`)
alldata$`RPI Rank`= as.numeric(alldata$`RPI Rank`)
alldata$`NET Rank` = as.numeric(alldata$`NET Rank`)
full_2020$`NET Rank` = as.numeric(full_2020$`NET Rank`)
full_2020$`RPI Rank` = as.numeric(full_2020$`RPI Rank`)

## Adding Indicator for if team is in Power 5 Conference
alldata$Power5 = rep(NA, length(alldata$School))
for(i in 1:length(alldata$School)){
  if(alldata$Conference[i] == "SEC" || alldata$Conference[i] == "P12" ||
     alldata$Conference[i] == "B12" || alldata$Conference[i] == "B10" || 
     alldata$Conference[i] == "ACC"){
    alldata$Power5[i] = 1
  }
  else{
    alldata$Power5[i] = 0
  }
} 

## Adding Win Percentage Variable 
alldata$`Win Percentage` = rep(NA, length(alldata$School))
for(i in 1:length(alldata$School)){
  alldata$`Win Percentage`[i] = round(alldata$Wins[i]/sum(alldata$Wins[i],alldata$Losses[i]),3)
}


table1 =  alldata %>% 
  group_by(`Make Tournament`) %>% 
  summarise_at(vars(`Last 12 Wins`, `Conference Finish`, `RPI Rank`,
                    `NET Rank`, `Wins`, `T-Rank`, SOS, `Non-Conf SOS`,
                    `Conference Win %`, `Wins Above Bubble`, `WAB Rank`),list(name = mean))
table1
colnames(table1)[2:12] = c("Last 12 Wins", "Conference Finish", "RPI Rank",
                          "NET Rank", "Wins", "T-Rank", "SOS", "Non-Conf SOS", 
                          "Conference Win %", "Wins Above Bubble", "WAB Rank")

## Creating data frame without conference champs
nccdata = alldata %>% filter(`Conference Champ` != 1)
nccdata = nccdata[-c(454,574),]


## Non-Conf Data by Year
ncc_2015 = nccdata %>% filter(Season == 2015)
ncc_2016 = nccdata %>% filter(Season == 2016)
ncc_2017 = nccdata %>% filter(Season == 2017)
ncc_2018 = nccdata %>% filter(Season == 2018)
ncc_2019 = nccdata %>% filter(Season == 2019)

## RPI vs T-Rank 
ggplot(ncc_2015, aes(x = `T-Rank`, y = `RPI Rank`)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", lwd = 1.5)
ggplot(ncc_2016, aes(x = `T-Rank`, y = `RPI Rank`)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", lwd = 1.5)
ggplot(ncc_2017, aes(x = `T-Rank`, y = `RPI Rank`)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", lwd = 1.5)
ggplot(ncc_2018, aes(x = `T-Rank`, y = `RPI Rank`)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", lwd = 1.5)
ggplot(ncc_2019, aes(x = `T-Rank`, y = `RPI Rank`)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", lwd = 1.5)
ggplot(full_2020, aes(x = `T-Rank`, y = `RPI Rank`)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", lwd = 1.5)

cor.test(full_2020$`RPI Rank`, full_2020$`T-Rank`)
cor.test(ncc_2019$`RPI Rank`, ncc_2019$`T-Rank`)
cor.test(ncc_2018$`RPI Rank`, ncc_2018$`T-Rank`)
cor.test(ncc_2017$`RPI Rank`, ncc_2017$`T-Rank`)
cor.test(ncc_2016$`RPI Rank`, ncc_2016$`T-Rank`)
cor.test(ncc_2015$`RPI Rank`, ncc_2015$`T-Rank`)

## NET vs. T-Rank
ggplot(ncc_2019, aes(x = `T-Rank`, y = `NET Rank`)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", lwd = 1.5)
ggplot(full_2020, aes(x = `T-Rank`, y = `NET Rank`)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", lwd = 1.5)

cor.test(full_2020$`NET Rank`, full_2020$`T-Rank`)
cor.test(ncc_2019$`NET Rank`, ncc_2019$`T-Rank`)

  ## Correlation between RPI and T-Rank is ~ .94, correlation between NET and T-Rank is ~ .98

## Last 12 Wins vs. Make Tournament
    ## Separated by Year
    ggplot(ncc_2015, aes(x = `Last 12 Wins`, fill = `Make Tournament`)) + geom_bar() + 
      scale_fill_manual(values=c("dark orange", "black"))
    ggplot(ncc_2016, aes(x = `Last 12 Wins`, fill = `Make Tournament`)) + geom_bar() + 
      scale_fill_manual(values=c("dark orange", "black"))
    ggplot(ncc_2017, aes(x = `Last 12 Wins`, fill = `Make Tournament`)) + geom_bar() + 
      scale_fill_manual(values=c("dark orange", "black"))
    ggplot(ncc_2018, aes(x = `Last 12 Wins`, fill = `Make Tournament`)) + geom_bar() + 
      scale_fill_manual(values=c("dark orange", "black"))
    ggplot(ncc_2019, aes(x = `Last 12 Wins`, fill = `Make Tournament`)) + geom_bar() + 
      scale_fill_manual(values=c("dark orange", "black"))

    ## Not separated by year
    detach(package:plyr)
    Last12Data = select(nccdata, School, `Last 12 Wins`, `Make Tournament`)
    Last12Data = Last12Data %>%
                      group_by(`Last 12 Wins`) %>%
                         mutate(`In Tournament` = sum(`Make Tournament` == 1),
                                `Miss Tournament` = sum(`Make Tournament` == 0),
                                 Probability = round(`In Tournament`/(`In Tournament` + `Miss Tournament`),3)*100) 

    ggplot(Last12Data, aes(x = `Last 12 Wins`, fill = `Make Tournament`)) + geom_bar() + 
      scale_fill_manual(values=c("dark orange", "black")) + 
      geom_text(aes(y = `In Tournament` + 5,label = paste(Probability,"%",sep = "")),color = "black", size = 3.5)
   
     ## Conference Finish
     ConfFinishData = select(nccdata, School, `Conference Finish`, `Make Tournament`)
     ConfFinishData = ConfFinishData %>%
      group_by(`Conference Finish`) %>%
      mutate(`In Tournament` = sum(`Make Tournament` == 1),
             `Miss Tournament` = sum(`Make Tournament` == 0),
              Probability = round(`In Tournament`/(`In Tournament` + `Miss Tournament`),3)*100) 
      ggplot(ConfFinishData, aes(x = `Conference Finish`, fill = `Make Tournament`)) + geom_bar() + 
      scale_fill_manual(values=c("dark orange", "black")) + 
      geom_text(aes(y = `In Tournament` + 5,label = paste(Probability,"%",sep = "")),color = "black", size = 3.5)
     
    ## RPI 
    ggplot(ncc_2019, aes(x = `RPI Rank`, y = `Make Tournament`)) + geom_point() 
    ggplot(ncc_2018, aes(x = `RPI Rank`, y = `Make Tournament`)) + geom_point() 
    ggplot(ncc_2017, aes(x = `RPI Rank`, y = `Make Tournament`)) + geom_point() 
    ggplot(ncc_2016, aes(x = `RPI Rank`, y = `Make Tournament`)) + geom_point() 
    ggplot(ncc_2015, aes(x = `RPI Rank`, y = `Make Tournament`)) + geom_point() 

    RPIData = select(nccdata, School, `RPI Rank`, `Make Tournament`)
    RPIData = RPIData %>%
      group_by(`RPI Rank`) %>%
      mutate(`In Tournament` = sum(`Make Tournament` == 1),
             `Miss Tournament` = sum(`Make Tournament` == 0),
             Probability = round(`In Tournament`/(`In Tournament` + `Miss Tournament`),3)*100) 
    ggplot(RPIData, aes(x = `RPI Rank`, fill = `Make Tournament`)) + geom_bar() + 
      scale_fill_manual(values=c("dark orange", "black")) 
    
    ## Conference Champ 
    ggplot(ncc_2019, aes(x = `Conference Champ`, y = `Make Tournament`)) + geom_point() + geom_jitter(width = 0.2)
    ggplot(ncc_2018, aes(x = `Conference Champ`, y = `Make Tournament`)) + geom_point() + geom_jitter(width = 0.2)
    ggplot(ncc_2017, aes(x = `Conference Champ`, y = `Make Tournament`)) + geom_point() + geom_jitter(width = 0.2)
    ggplot(ncc_2016, aes(x = `Conference Champ`, y = `Make Tournament`)) + geom_point() + geom_jitter(width = 0.2)
    ggplot(ncc_2015, aes(x = `Conference Champ`, y = `Make Tournament`)) + geom_point() + geom_jitter(width = 0.2)
    
    ## Conference
    ConfData = select(nccdata, School, Conference, `Make Tournament`)
    ConfData = ConfData %>%
      group_by(Conference) %>%
      mutate(`In Tournament` = sum(`Make Tournament` == 1),
             `Miss Tournament` = sum(`Make Tournament` == 0),
             Probability = round(`In Tournament`/(`In Tournament` + `Miss Tournament`),3)*100) 
    ggplot(ConfData, aes(x = Conference, fill = `Make Tournament`)) + geom_bar() + 
      scale_fill_manual(values=c("dark orange", "black")) + 
      geom_text(aes(y = `In Tournament` + 2,label = paste(Probability,"%",sep = "")),color = "black", size = 2.5)
    ggplot(ConfData, aes(x = Conference, fill = `Make Tournament`)) + 
      geom_bar(position = position_dodge()) + scale_fill_manual(values=c("dark orange", "black"))
    
    ## Wins
    WinsData = select(nccdata, School, Wins, `Make Tournament`)
    WinsData$Wins = as.numeric(WinsData$Wins)
    WinsData = WinsData %>%
      group_by(Wins) %>%
      mutate(`In Tournament` = sum(`Make Tournament` == 1),
             `Miss Tournament` = sum(`Make Tournament` == 0),
             Probability = round(`In Tournament`/(`In Tournament` + `Miss Tournament`),3)*100) 
    ggplot(WinsData, aes(x = Wins, fill = `Make Tournament`)) + geom_bar() + 
      scale_fill_manual(values=c("dark orange", "black")) + 
      geom_text(aes(y = `In Tournament` + 2,label = paste(Probability,"%",sep = "")),color = "black", size = 2.25)
    ggplot(nccdata, aes(x = Wins)) + geom_histogram(binwidth = 5)
    
    ## Offensive Efficiency
    ggplot(ncc_2019, aes(x = `Adj. Offensive Efficiency`, y = `Make Tournament`)) + geom_point() 
    ggplot(ncc_2018, aes(x = `Adj. Offensive Efficiency`, y = `Make Tournament`)) + geom_point() 
    ggplot(ncc_2017, aes(x = `Adj. Offensive Efficiency`, y = `Make Tournament`)) + geom_point() 
    ggplot(ncc_2016, aes(x = `Adj. Offensive Efficiency`, y = `Make Tournament`)) + geom_point() 
    ggplot(ncc_2015, aes(x = `Adj. Offensive Efficiency`, y = `Make Tournament`)) + geom_point() 
    
    OEData = select(nccdata, School, `Adj. Offensive Efficiency`, `Make Tournament`)
    OEData = OEData %>%
      group_by(`Adj. Offensive Efficiency`) %>%
      mutate(`In Tournament` = sum(`Make Tournament` == 1),
             `Miss Tournament` = sum(`Make Tournament` == 0),
             Probability = round(`In Tournament`/(`In Tournament` + `Miss Tournament`),3)*100) 
    ggplot(OEData, aes(x = `Adj. Offensive Efficiency`, fill = `Make Tournament`)) + geom_histogram(bins = 10) + 
      scale_fill_manual(values=c("dark orange", "black")) 
    
    ## Defensive Efficiency
    ggplot(ncc_2019, aes(x = `Adj. Defensive Efficiency`, y = `Make Tournament`)) + geom_point() 
    ggplot(ncc_2018, aes(x = `Adj. Defensive Efficiency`, y = `Make Tournament`)) + geom_point() 
    ggplot(ncc_2017, aes(x = `Adj. Defensive Efficiency`, y = `Make Tournament`)) + geom_point() 
    ggplot(ncc_2016, aes(x = `Adj. Defensive Efficiency`, y = `Make Tournament`)) + geom_point() 
    ggplot(ncc_2015, aes(x = `Adj. Defensive Efficiency`, y = `Make Tournament`)) + geom_point() 
    
    DEData = select(nccdata, School, `Adj. Defensive Efficiency`, `Make Tournament`)
    DEData = DEData %>%
      group_by(`Adj. Defensive Efficiency`) %>%
      mutate(`In Tournament` = sum(`Make Tournament` == 1),
             `Miss Tournament` = sum(`Make Tournament` == 0),
             Probability = round(`In Tournament`/(`In Tournament` + `Miss Tournament`),3)*100) 
    ggplot(DEData, aes(x = `Adj. Defensive Efficiency`, fill = `Make Tournament`)) + geom_histogram(bins = 10) + 
      scale_fill_manual(values=c("dark orange", "black"))
    
    ## Efficiency Avg 
    ggplot(ncc_2019, aes(x = `Efficiency Avg`, y = `Make Tournament`)) + geom_point() 
    ggplot(ncc_2018, aes(x = `Efficiency Avg`, y = `Make Tournament`)) + geom_point() 
    ggplot(ncc_2017, aes(x = `Efficiency Avg`, y = `Make Tournament`)) + geom_point() 
    ggplot(ncc_2016, aes(x = `Efficiency Avg`, y = `Make Tournament`)) + geom_point() 
    ggplot(ncc_2015, aes(x = `Efficiency Avg`, y = `Make Tournament`)) + geom_point() 
    
    EffAvgData = select(nccdata, School, `Efficiency Avg`, `Make Tournament`)
    EffAvgData = EffAvgData %>%
      group_by(`Efficiency Avg`) %>%
      mutate(`In Tournament` = sum(`Make Tournament` == 1),
             `Miss Tournament` = sum(`Make Tournament` == 0),
             Probability = round(`In Tournament`/(`In Tournament` + `Miss Tournament`),3)*100) 
    ggplot(EffAvgData, aes(x = `Efficiency Avg`, fill = `Make Tournament`)) + geom_histogram(bins = 15) + 
      scale_fill_manual(values=c("dark orange", "black"))
    ggplot(EffAvgData, aes(x = `Efficiency Avg`, y = `Make Tournament`)) + geom_point()
    
    ## Efficiency Rank Avg
    ggplot(ncc_2019, aes(x = `Efficiency Rank Avg`, y = `Make Tournament`)) + geom_point() 
    ggplot(ncc_2018, aes(x = `Efficiency Rank Avg`, y = `Make Tournament`)) + geom_point() 
    ggplot(ncc_2017, aes(x = `Efficiency Rank Avg`, y = `Make Tournament`)) + geom_point() 
    ggplot(ncc_2016, aes(x = `Efficiency Rank Avg`, y = `Make Tournament`)) + geom_point() 
    ggplot(ncc_2015, aes(x = `Efficiency Rank Avg`, y = `Make Tournament`)) + geom_point() 
    
    EffRankAvgData = select(nccdata, School, `Efficiency Rank Avg`, `Make Tournament`)
    EffRankAvgData = EffRankAvgData %>%
      group_by(`Efficiency Rank Avg`) %>%
      mutate(`In Tournament` = sum(`Make Tournament` == 1),
             `Miss Tournament` = sum(`Make Tournament` == 0),
             Probability = round(`In Tournament`/(`In Tournament` + `Miss Tournament`),3)*100) 
    ggplot(EffRankAvgData, aes(x = `Efficiency Rank Avg`, y = `Make Tournament`)) + geom_point()
    ggplot(EffRankAvgData, aes(x = `Efficiency Rank Avg`, fill = `Make Tournament`)) + geom_histogram(bins = 15) + 
      scale_fill_manual(values=c("dark orange", "black"))
    
    ## T-Rank 
    ggplot(ncc_2019, aes(x = `T-Rank`, y = `Make Tournament`)) + geom_point() 
    ggplot(ncc_2018, aes(x = `T-Rank`, y = `Make Tournament`)) + geom_point() 
    ggplot(ncc_2017, aes(x = `T-Rank`, y = `Make Tournament`)) + geom_point() 
    ggplot(ncc_2016, aes(x = `T-Rank`, y = `Make Tournament`)) + geom_point() 
    ggplot(ncc_2015, aes(x = `T-Rank`, y = `Make Tournament`)) + geom_point() 
    
    TRankData = select(nccdata, School, `T-Rank`, `Make Tournament`)
    TRankData = TRankData %>%
      group_by(`T-Rank`) %>%
      mutate(`In Tournament` = sum(`Make Tournament` == 1),
             `Miss Tournament` = sum(`Make Tournament` == 0),
             Probability = round(`In Tournament`/(`In Tournament` + `Miss Tournament`),3)*100) 
    ggplot(TRankData, aes(x = `T-Rank`, fill = `Make Tournament`)) + geom_bar() + 
      scale_fill_manual(values=c("dark orange", "black")) 
    
    ## Barthag
    ggplot(ncc_2019, aes(x = `Barthag`, y = `Make Tournament`)) + geom_point() 
    ggplot(ncc_2018, aes(x = `Barthag`, y = `Make Tournament`)) + geom_point() 
    ggplot(ncc_2017, aes(x = `Barthag`, y = `Make Tournament`)) + geom_point() 
    ggplot(ncc_2016, aes(x = `Barthag`, y = `Make Tournament`)) + geom_point() 
    ggplot(ncc_2015, aes(x = `Barthag`, y = `Make Tournament`)) + geom_point() 
    
    BarthagData = select(nccdata, School, Barthag, `Make Tournament`)
    BarthagData = BarthagData %>%
      group_by(Barthag) %>%
      mutate(`In Tournament` = sum(`Make Tournament` == 1),
             `Miss Tournament` = sum(`Make Tournament` == 0),
             Probability = round(`In Tournament`/(`In Tournament` + `Miss Tournament`),3)*100) 
    ggplot(BarthagData, aes(x = Barthag, fill = `Make Tournament`)) + geom_histogram(bins = 15) + 
      scale_fill_manual(values=c("dark orange", "black")) 
    
    ## SOS 
    ggplot(ncc_2019, aes(x = `SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.2,0.9))
    ggplot(ncc_2018, aes(x = `SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.2,0.9)) 
    ggplot(ncc_2017, aes(x = `SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.2,0.9)) 
    ggplot(ncc_2016, aes(x = `SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.2,0.9))
    ggplot(ncc_2015, aes(x = `SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.2,0.9))
    
    ggplot(nccdata, aes(x = SOS, fill = `Make Tournament`)) + geom_histogram(bins = 12) + 
      scale_fill_manual(values=c("dark orange", "black")) 
    
    ## Non-Conf SOS 
    ggplot(ncc_2019, aes(x = `Non-Conf SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.2,0.9))
    ggplot(ncc_2018, aes(x = `Non-Conf SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.2,0.9)) 
    ggplot(ncc_2017, aes(x = `Non-Conf SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.2,0.9)) 
    ggplot(ncc_2016, aes(x = `Non-Conf SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.2,0.9))
    ggplot(ncc_2015, aes(x = `Non-Conf SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.2,0.9))
    
    ggplot(nccdata, aes(x = `Non-Conf SOS`, fill = `Make Tournament`)) + geom_histogram(bins = 12) + 
      scale_fill_manual(values=c("dark orange", "black")) 
    
    ## Conference SOS 
    ggplot(ncc_2019, aes(x = `Conference SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.1,0.9))
    ggplot(ncc_2018, aes(x = `Conference SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.1,0.9)) 
    ggplot(ncc_2017, aes(x = `Conference SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.1,0.9)) 
    ggplot(ncc_2016, aes(x = `Conference SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.1,0.9))
    ggplot(ncc_2015, aes(x = `Conference SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.0,1))
    
    ggplot(nccdata, aes(x = `Conference SOS`, fill = `Make Tournament`)) + geom_histogram(bins = 8) + 
      scale_fill_manual(values=c("dark orange", "black")) 
    
    ## Conference Record
    ggplot(ncc_2019, aes(x = `Conference Win %`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0,1))
    ggplot(ncc_2018, aes(x = `Conference Win %`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0,1)) 
    ggplot(ncc_2017, aes(x = `Conference Win %`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0,1)) 
    ggplot(ncc_2016, aes(x = `Conference Win %`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0,1))
    ggplot(ncc_2015, aes(x = `Conference Win %`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0,1))
    
    ggplot(nccdata, aes(x = `Conference Win %`, fill = `Make Tournament`)) + geom_histogram(bins = 12) + 
      scale_fill_manual(values=c("dark orange", "black")) 
    
    ## Wins Above Bubble 
    ggplot(ncc_2019, aes(x = `Wins Above Bubble`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(-25,15))
    ggplot(ncc_2018, aes(x = `Wins Above Bubble`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(-25,15)) 
    ggplot(ncc_2017, aes(x = `Wins Above Bubble`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(-30,15)) 
    ggplot(ncc_2016, aes(x = `Wins Above Bubble`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(-25,15))
    ggplot(ncc_2015, aes(x = `Wins Above Bubble`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(-25,15))
    
    WABData = select(nccdata, School, `WAB Rank`, `Make Tournament`)
    WABData = WABData %>%
      group_by(`WAB Rank`) %>%
      mutate(`In Tournament` = sum(`Make Tournament` == 1),
             `Miss Tournament` = sum(`Make Tournament` == 0),
             Probability = round(`In Tournament`/(`In Tournament` + `Miss Tournament`),3)*100) 
    ggplot(WABData, aes(x = `WAB Rank`, fill = `Make Tournament`)) + geom_bar() + 
      scale_fill_manual(values=c("dark orange", "black")) 
    
    
    ## Power 5
    ggplot(ncc_2019, aes(x = Power5, y = `Make Tournament`)) + geom_point() + geom_jitter(width = 0.1)
    ggplot(ncc_2018, aes(x = Power5, y = `Make Tournament`)) + geom_point() + geom_jitter(width = 0.1)
    ggplot(ncc_2017, aes(x = Power5, y = `Make Tournament`)) + geom_point() + geom_jitter(width = 0.1)
    ggplot(ncc_2016, aes(x = Power5, y = `Make Tournament`)) + geom_point() + geom_jitter(width = 0.1)
    ggplot(ncc_2015, aes(x = Power5, y = `Make Tournament`)) + geom_point() + geom_jitter(width = 0.1)
    
    P5Data = select(nccdata, School, `Power5`, `Make Tournament`)
    P5Data = P5Data %>%
      group_by(Power5) %>%
      mutate(`In Tournament` = sum(`Make Tournament` == 1),
             `Miss Tournament` = sum(`Make Tournament` == 0),
             Probability = round(`In Tournament`/(`In Tournament` + `Miss Tournament`),3)*100) 
    ggplot(P5Data, aes(x = Power5, fill = `Make Tournament`)) + geom_bar() + 
      scale_fill_manual(values=c("dark orange", "black")) 
    
    ## Win Percentage
    ggplot(ncc_2019, aes(x = `Win Percentage`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0,1))
    ggplot(ncc_2018, aes(x = `Win Percentage`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0,1)) 
    ggplot(ncc_2017, aes(x = `Win Percentage`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0,1)) 
    ggplot(ncc_2016, aes(x = `Win Percentage`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0,1))
    ggplot(ncc_2015, aes(x = `Win Percentage`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0,1))
    
    WPData = select(nccdata, School, `Win Percentage`, `Make Tournament`)
    WPData = WPData %>%
      group_by(`Win Percentage`) %>%
      mutate(`In Tournament` = sum(`Make Tournament` == 1),
             `Miss Tournament` = sum(`Make Tournament` == 0),
             Probability = round(`In Tournament`/(`In Tournament` + `Miss Tournament`),3)*100) 
    ggplot(WPData, aes(x = `Win Percentage`, fill = `Make Tournament`)) + geom_histogram(bins = 10) + 
      scale_fill_manual(values=c("dark orange", "black")) 

  
  