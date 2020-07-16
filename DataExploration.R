## Load Packages
library(ggplot2)
library(tidyverse)
library(dplyr)

## Load Full Datasets
full_2015 = read_csv("Full CSV Data Files/full_2015.csv")
full_2016 = read_csv("Full CSV Data Files/full_2016.csv")
full_2017 = read_csv("Full CSV Data Files/full_2017.csv")
full_2018 = read_csv("Full CSV Data Files/full_2018.csv")
full_2019 = read_csv("Full CSV Data Files/full_2019.csv")
full_2020 = read_csv("Full CSV Data Files/full_2020.csv")

## 2015-2019 Data Merged
alldata = rbind(full_2015,full_2016,full_2017,full_2018,full_2019)
alldata$`Make Tournament`= as.factor(alldata$`Make Tournament`)
alldata$`Conference Champ` = as.factor(alldata$`Conference Champ`)
str(alldata) 

table1 =  alldata %>% 
  group_by(`Make Tournament`) %>% 
  summarise_at(vars(`Last 12 Wins`, `Conference Finish`, `RPI Rank`,
                    `NET Rank`, `Wins`, `T-Rank`, SOS, `Non-Conf SOS`,
                    `Conference Win %`, `Wins Above Bubble`, `WAB Rank`),list(name = mean))
table1
colnames(table1)[2:12] = c("Last 12 Wins", "Conference Finish", "RPI Rank",
                          "NET Rank", "Wins", "T-Rank", "SOS", "Non-Conf SOS", 
                          "Conference Win %", "Wins Above Bubble", "WAB Rank")

## RPI vs T-Rank 
ggplot(full_2015, aes(x = `T-Rank`, y = `RPI Rank`)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", lwd = 1.5)
ggplot(full_2016, aes(x = `T-Rank`, y = `RPI Rank`)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", lwd = 1.5)
ggplot(full_2017, aes(x = `T-Rank`, y = `RPI Rank`)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", lwd = 1.5)
ggplot(full_2018, aes(x = `T-Rank`, y = `RPI Rank`)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", lwd = 1.5)
ggplot(full_2019, aes(x = `T-Rank`, y = `RPI Rank`)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", lwd = 1.5)
ggplot(full_2020, aes(x = `T-Rank`, y = `RPI Rank`)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", lwd = 1.5)

cor.test(full_2020$`RPI Rank`, full_2020$`T-Rank`)
cor.test(full_2019$`RPI Rank`, full_2019$`T-Rank`)
cor.test(full_2018$`RPI Rank`, full_2018$`T-Rank`)
cor.test(full_2017$`RPI Rank`, full_2017$`T-Rank`)
cor.test(full_2016$`RPI Rank`, full_2016$`T-Rank`)
cor.test(full_2015$`RPI Rank`, full_2015$`T-Rank`)

## NET vs. T-Rank
ggplot(full_2019, aes(x = `T-Rank`, y = `NET Rank`)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", lwd = 1.5)
ggplot(full_2020, aes(x = `T-Rank`, y = `NET Rank`)) + geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red", lwd = 1.5)

cor.test(full_2020$`NET Rank`, full_2020$`T-Rank`)
cor.test(full_2019$`NET Rank`, full_2019$`T-Rank`)

  ## Correlation between RPI and T-Rank is ~ .94, correlation between NET and T-Rank is ~ .98

## Last 12 Wins vs. Make Tournament
    ## Separated by Year
    ggplot(full_2015, aes(x = `Last 12 Wins`, fill = `Make Tournament`)) + geom_bar() + 
      scale_fill_manual(values=c("dark orange", "black"))
    ggplot(full_2016, aes(x = `Last 12 Wins`, fill = `Make Tournament`)) + geom_bar() + 
      scale_fill_manual(values=c("dark orange", "black"))
    ggplot(full_2017, aes(x = `Last 12 Wins`, fill = `Make Tournament`)) + geom_bar() + 
      scale_fill_manual(values=c("dark orange", "black"))
    ggplot(full_2018, aes(x = `Last 12 Wins`, fill = `Make Tournament`)) + geom_bar() + 
      scale_fill_manual(values=c("dark orange", "black"))
    ggplot(full_2019, aes(x = `Last 12 Wins`, fill = `Make Tournament`)) + geom_bar() + 
      scale_fill_manual(values=c("dark orange", "black"))

    ## Not separated by year
    Last12Data = select(alldata, School, `Last 12 Wins`, `Make Tournament`)
    Last12Data = Last12Data %>%
                      group_by(`Last 12 Wins`) %>%
                      mutate(`In Tournament` = sum(`Make Tournament` == 1),
                                `Miss Tournament` = sum(`Make Tournament` == 0),
                                 Probability = round(`In Tournament`/(`In Tournament` + `Miss Tournament`),3)*100) 
 
    ggplot(Last12Data, aes(x = `Last 12 Wins`, fill = `Make Tournament`)) + geom_bar() + 
      scale_fill_manual(values=c("dark orange", "black")) + 
      geom_text(aes(y = `In Tournament` + 5,label = paste(Probability,"%",sep = "")),color = "black", size = 3.5)
   
     ## Conference Finish
     ConfFinishData = select(alldata, School, `Conference Finish`, `Make Tournament`)
     ConfFinishData = ConfFinishData %>%
      group_by(`Conference Finish`) %>%
      mutate(`In Tournament` = sum(`Make Tournament` == 1),
             `Miss Tournament` = sum(`Make Tournament` == 0),
              Probability = round(`In Tournament`/(`In Tournament` + `Miss Tournament`),3)*100) 
      ggplot(ConfFinishData, aes(x = `Conference Finish`, fill = `Make Tournament`)) + geom_bar() + 
      scale_fill_manual(values=c("dark orange", "black")) + 
      geom_text(aes(y = `In Tournament` + 5,label = paste(Probability,"%",sep = "")),color = "black", size = 3.5)
     
    ## RPI 
    ggplot(full_2019, aes(x = `RPI Rank`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2018, aes(x = `RPI Rank`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2017, aes(x = `RPI Rank`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2016, aes(x = `RPI Rank`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2015, aes(x = `RPI Rank`, y = `Make Tournament`)) + geom_point() 

    RPIData = select(alldata, School, `RPI Rank`, `Make Tournament`)
    RPIData = RPIData %>%
      group_by(`RPI Rank`) %>%
      mutate(`In Tournament` = sum(`Make Tournament` == 1),
             `Miss Tournament` = sum(`Make Tournament` == 0),
             Probability = round(`In Tournament`/(`In Tournament` + `Miss Tournament`),3)*100) 
    ggplot(RPIData, aes(x = `RPI Rank`, fill = `Make Tournament`)) + geom_bar() + 
      scale_fill_manual(values=c("dark orange", "black")) 
    
    ## Conference Champ 
    ggplot(full_2019, aes(x = `Conference Champ`, y = `Make Tournament`)) + geom_point() + geom_jitter(width = 0.2)
    ggplot(full_2018, aes(x = `Conference Champ`, y = `Make Tournament`)) + geom_point() + geom_jitter(width = 0.2)
    ggplot(full_2017, aes(x = `Conference Champ`, y = `Make Tournament`)) + geom_point() + geom_jitter(width = 0.2)
    ggplot(full_2016, aes(x = `Conference Champ`, y = `Make Tournament`)) + geom_point() + geom_jitter(width = 0.2)
    ggplot(full_2015, aes(x = `Conference Champ`, y = `Make Tournament`)) + geom_point() + geom_jitter(width = 0.2)
    
    ## Conference
    ConfData = select(alldata, School, Conference, `Make Tournament`)
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
    WinsData = select(alldata, School, Wins, `Make Tournament`)
    WinsData$Wins = as.numeric(WinsData$Wins)
    WinsData = WinsData %>%
      group_by(Wins) %>%
      mutate(`In Tournament` = sum(`Make Tournament` == 1),
             `Miss Tournament` = sum(`Make Tournament` == 0),
             Probability = round(`In Tournament`/(`In Tournament` + `Miss Tournament`),3)*100) 
    ggplot(WinsData, aes(x = Wins, fill = `Make Tournament`)) + geom_bar() + 
      scale_fill_manual(values=c("dark orange", "black")) + 
      geom_text(aes(y = `In Tournament` + 2,label = paste(Probability,"%",sep = "")),color = "black", size = 2.25)
    ggplot(alldata, aes(x = Wins)) + geom_histogram(binwidth = 5)
    
    ## Offensive Efficiency
    ggplot(full_2019, aes(x = `Adj. Offensive Efficiency`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2018, aes(x = `Adj. Offensive Efficiency`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2017, aes(x = `Adj. Offensive Efficiency`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2016, aes(x = `Adj. Offensive Efficiency`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2015, aes(x = `Adj. Offensive Efficiency`, y = `Make Tournament`)) + geom_point() 
    
    OEData = select(alldata, School, `Adj. Offensive Efficiency`, `Make Tournament`)
    OEData = OEData %>%
      group_by(`Adj. Offensive Efficiency`) %>%
      mutate(`In Tournament` = sum(`Make Tournament` == 1),
             `Miss Tournament` = sum(`Make Tournament` == 0),
             Probability = round(`In Tournament`/(`In Tournament` + `Miss Tournament`),3)*100) 
    ggplot(OEData, aes(x = `Adj. Offensive Efficiency`, fill = `Make Tournament`)) + geom_histogram(bins = 10) + 
      scale_fill_manual(values=c("dark orange", "black")) 
    
    ## Defensive Efficiency Rank
    ggplot(full_2019, aes(x = `Adj. Defensive Efficiency`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2018, aes(x = `Adj. Defensive Efficiency`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2017, aes(x = `Adj. Defensive Efficiency`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2016, aes(x = `Adj. Defensive Efficiency`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2015, aes(x = `Adj. Defensive Efficiency`, y = `Make Tournament`)) + geom_point() 
    
    DEData = select(alldata, School, `Adj. Defensive Efficiency`, `Make Tournament`)
    DEData = DEData %>%
      group_by(`Adj. Defensive Efficiency`) %>%
      mutate(`In Tournament` = sum(`Make Tournament` == 1),
             `Miss Tournament` = sum(`Make Tournament` == 0),
             Probability = round(`In Tournament`/(`In Tournament` + `Miss Tournament`),3)*100) 
    ggplot(DEData, aes(x = `Adj. Defensive Efficiency`, fill = `Make Tournament`)) + geom_histogram(bins = 10) + 
      scale_fill_manual(values=c("dark orange", "black"))
    
    ## Efficiency Avg 
    ggplot(full_2019, aes(x = `Efficiency Avg`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2018, aes(x = `Efficiency Avg`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2017, aes(x = `Efficiency Avg`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2016, aes(x = `Efficiency Avg`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2015, aes(x = `Efficiency Avg`, y = `Make Tournament`)) + geom_point() 
    
    EffAvgData = select(alldata, School, `Efficiency Avg`, `Make Tournament`)
    EffAvgData = EffAvgData %>%
      group_by(`Efficiency Avg`) %>%
      mutate(`In Tournament` = sum(`Make Tournament` == 1),
             `Miss Tournament` = sum(`Make Tournament` == 0),
             Probability = round(`In Tournament`/(`In Tournament` + `Miss Tournament`),3)*100) 
    ggplot(EffAvgData, aes(x = `Efficiency Avg`, fill = `Make Tournament`)) + geom_histogram(bins = 15) + 
      scale_fill_manual(values=c("dark orange", "black"))
    ggplot(EffAvgData, aes(x = `Efficiency Avg`, y = `Make Tournament`)) + geom_point()
    
    ## Efficiency Rank Avg
    ggplot(full_2019, aes(x = `Efficiency Rank Avg`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2018, aes(x = `Efficiency Rank Avg`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2017, aes(x = `Efficiency Rank Avg`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2016, aes(x = `Efficiency Rank Avg`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2015, aes(x = `Efficiency Rank Avg`, y = `Make Tournament`)) + geom_point() 
    
    EffRankAvgData = select(alldata, School, `Efficiency Rank Avg`, `Make Tournament`)
    EffRankAvgData = EffRankAvgData %>%
      group_by(`Efficiency Rank Avg`) %>%
      mutate(`In Tournament` = sum(`Make Tournament` == 1),
             `Miss Tournament` = sum(`Make Tournament` == 0),
             Probability = round(`In Tournament`/(`In Tournament` + `Miss Tournament`),3)*100) 
    ggplot(EffRankAvgData, aes(x = `Efficiency Rank Avg`, y = `Make Tournament`)) + geom_point()
    
    ## T-Rank 
    ggplot(full_2019, aes(x = `T-Rank`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2018, aes(x = `T-Rank`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2017, aes(x = `T-Rank`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2016, aes(x = `T-Rank`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2015, aes(x = `T-Rank`, y = `Make Tournament`)) + geom_point() 
    
    TRankData = select(alldata, School, `T-Rank`, `Make Tournament`)
    TRankData = TRankData %>%
      group_by(`T-Rank`) %>%
      mutate(`In Tournament` = sum(`Make Tournament` == 1),
             `Miss Tournament` = sum(`Make Tournament` == 0),
             Probability = round(`In Tournament`/(`In Tournament` + `Miss Tournament`),3)*100) 
    ggplot(TRankData, aes(x = `T-Rank`, fill = `Make Tournament`)) + geom_bar() + 
      scale_fill_manual(values=c("dark orange", "black")) 
    
    ## Barthag
    ggplot(full_2019, aes(x = `Barthag`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2018, aes(x = `Barthag`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2017, aes(x = `Barthag`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2016, aes(x = `Barthag`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2015, aes(x = `Barthag`, y = `Make Tournament`)) + geom_point() 
    
    BarthagData = select(alldata, School, Barthag, `Make Tournament`)
    BarthagData = BarthagData %>%
      group_by(Barthag) %>%
      mutate(`In Tournament` = sum(`Make Tournament` == 1),
             `Miss Tournament` = sum(`Make Tournament` == 0),
             Probability = round(`In Tournament`/(`In Tournament` + `Miss Tournament`),3)*100) 
    ggplot(BarthagData, aes(x = Barthag, fill = `Make Tournament`)) + geom_histogram(bins = 15) + 
      scale_fill_manual(values=c("dark orange", "black")) 
    
    ## SOS 
    ggplot(full_2019, aes(x = `SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.2,0.9))
    ggplot(full_2018, aes(x = `SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.2,0.9)) 
    ggplot(full_2017, aes(x = `SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.2,0.9)) 
    ggplot(full_2016, aes(x = `SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.2,0.9))
    ggplot(full_2015, aes(x = `SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.2,0.9))
    
    ggplot(alldata, aes(x = SOS, fill = `Make Tournament`)) + geom_histogram(bins = 12) + 
      scale_fill_manual(values=c("dark orange", "black")) 
    
    ## Non-Conf SOS 
    ggplot(full_2019, aes(x = `Non-Conf SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.2,0.9))
    ggplot(full_2018, aes(x = `Non-Conf SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.2,0.9)) 
    ggplot(full_2017, aes(x = `Non-Conf SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.2,0.9)) 
    ggplot(full_2016, aes(x = `Non-Conf SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.2,0.9))
    ggplot(full_2015, aes(x = `Non-Conf SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.2,0.9))
    
    ggplot(alldata, aes(x = `Non-Conf SOS`, fill = `Make Tournament`)) + geom_histogram(bins = 12) + 
      scale_fill_manual(values=c("dark orange", "black")) 
    
    ## Conference SOS 
    ggplot(full_2019, aes(x = `Conference SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.1,0.9))
    ggplot(full_2018, aes(x = `Conference SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.1,0.9)) 
    ggplot(full_2017, aes(x = `Conference SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.1,0.9)) 
    ggplot(full_2016, aes(x = `Conference SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.1,0.9))
    ggplot(full_2015, aes(x = `Conference SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.0,1))
    
    ggplot(alldata, aes(x = `Conference SOS`, fill = `Make Tournament`)) + geom_histogram(bins = 8) + 
      scale_fill_manual(values=c("dark orange", "black")) 
    
    ## Conference Record
    ggplot(full_2019, aes(x = `Conference Win %`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0,1))
    ggplot(full_2018, aes(x = `Conference Win %`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0,1)) 
    ggplot(full_2017, aes(x = `Conference Win %`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0,1)) 
    ggplot(full_2016, aes(x = `Conference Win %`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0,1))
    ggplot(full_2015, aes(x = `Conference Win %`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0,1))
    
    ggplot(alldata, aes(x = `Conference Win %`, fill = `Make Tournament`)) + geom_histogram(bins = 12) + 
      scale_fill_manual(values=c("dark orange", "black")) 
    
    ## Wins Above Bubble 
    ggplot(full_2019, aes(x = `Wins Above Bubble`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(-25,15))
    ggplot(full_2018, aes(x = `Wins Above Bubble`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(-25,15)) 
    ggplot(full_2017, aes(x = `Wins Above Bubble`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(-30,15)) 
    ggplot(full_2016, aes(x = `Wins Above Bubble`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(-25,15))
    ggplot(full_2015, aes(x = `Wins Above Bubble`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(-25,15))
    
    WABData = select(alldata, School, `WAB Rank`, `Make Tournament`)
    WABData = WABData %>%
      group_by(`WAB Rank`) %>%
      mutate(`In Tournament` = sum(`Make Tournament` == 1),
             `Miss Tournament` = sum(`Make Tournament` == 0),
             Probability = round(`In Tournament`/(`In Tournament` + `Miss Tournament`),3)*100) 
    ggplot(WABData, aes(x = `WAB Rank`, fill = `Make Tournament`)) + geom_bar() + 
      scale_fill_manual(values=c("dark orange", "black")) 
    

    

  
  