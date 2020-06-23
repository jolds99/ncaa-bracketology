## Load Packages
library(ggplot2)

## Load Full Datasets
full_2015 = read_csv("Full CSV Data Files/full_2015.csv")
full_2016 = read_csv("Full CSV Data Files/full_2016.csv")
full_2017 = read_csv("Full CSV Data Files/full_2017.csv")
full_2018 = read_csv("Full CSV Data Files/full_2018.csv")
full_2019 = read_csv("Full CSV Data Files/full_2019.csv")
full_2020 = read_csv("Full CSV Data Files/full_2020.csv")

## Modifications to full data sets
colnames(full_2015)[15] = "T-Rank"
colnames(full_2016)[15] = "T-Rank"
colnames(full_2017)[15] = "T-Rank"
colnames(full_2018)[15] = "T-Rank"
colnames(full_2019)[15] = "T-Rank"
colnames(full_2020)[15] = "T-Rank"

full_2015$`Make Tournament` = as.factor(full_2015$`Make Tournament`)
full_2016$`Make Tournament` = as.factor(full_2016$`Make Tournament`)
full_2017$`Make Tournament` = as.factor(full_2017$`Make Tournament`)
full_2018$`Make Tournament` = as.factor(full_2018$`Make Tournament`)
full_2019$`Make Tournament` = as.factor(full_2019$`Make Tournament`)

full_2020$Wins = stringr::word(full_2020$Record, 1, sep = "-")
full_2020$Losses = stringr::word(full_2020$Record, -1, sep = "-")
full_2020 = full_2020[,c(1:9,23,24,10:22)]

full_2019$Wins = stringr::word(full_2019$Record, 1, sep = "-")
full_2019$Losses = stringr::word(full_2019$Record, -1, sep = "-")
full_2019 = full_2019[,c(1:9,23,24,10:22)]

full_2018$Wins = stringr::word(full_2018$Record, 1, sep = "-")
full_2018$Losses = stringr::word(full_2018$Record, -1, sep = "-")
full_2018 = full_2018[,c(1:9,23,24,10:22)]

full_2017$Wins = stringr::word(full_2017$Record, 1, sep = "-")
full_2017$Losses = stringr::word(full_2017$Record, -1, sep = "-")
full_2017 = full_2017[,c(1:9,23,24,10:22)]

full_2016$Wins = stringr::word(full_2016$Record, 1, sep = "-")
full_2016$Losses = stringr::word(full_2016$Record, -1, sep = "-")
full_2016 = full_2016[,c(1:9,23,24,10:22)]

full_2015$Wins = stringr::word(full_2015$Record, 1, sep = "-")
full_2015$Losses = stringr::word(full_2015$Record, -1, sep = "-")
full_2015 = full_2015[,c(1:9,23,24,10:22)]

full_2020$`Efficiency Avg`= (full_2020$`OE Rank` + full_2020$`DE Rank`)/2
full_2019$`Efficiency Avg`= (full_2019$`OE Rank` + full_2019$`DE Rank`)/2
full_2018$`Efficiency Avg`= (full_2018$`OE Rank` + full_2018$`DE Rank`)/2
full_2017$`Efficiency Avg`= (full_2017$`OE Rank` + full_2017$`DE Rank`)/2
full_2016$`Efficiency Avg`= (full_2016$`OE Rank` + full_2016$`DE Rank`)/2
full_2015$`Efficiency Avg`= (full_2015$`OE Rank` + full_2015$`DE Rank`)/2
## 2015-2019 Data Merged
alldata = rbind(full_2015,full_2016,full_2017,full_2018,full_2019)

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

    ## OE Rank
    ggplot(full_2019, aes(x = `OE Rank`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2018, aes(x = `OE Rank`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2017, aes(x = `OE Rank`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2016, aes(x = `OE Rank`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2015, aes(x = `OE Rank`, y = `Make Tournament`)) + geom_point() 
    
    OEData = select(alldata, School, `OE Rank`, `Make Tournament`)
    OEData = OEData %>%
      group_by(`OE Rank`) %>%
      mutate(`In Tournament` = sum(`Make Tournament` == 1),
             `Miss Tournament` = sum(`Make Tournament` == 0),
             Probability = round(`In Tournament`/(`In Tournament` + `Miss Tournament`),3)*100) 
    ggplot(OEData, aes(x = `OE Rank`, fill = `Make Tournament`)) + geom_bar() + 
      scale_fill_manual(values=c("dark orange", "black")) 
    
    ## DE Rank
    ggplot(full_2019, aes(x = `DE Rank`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2018, aes(x = `DE Rank`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2017, aes(x = `DE Rank`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2016, aes(x = `DE Rank`, y = `Make Tournament`)) + geom_point() 
    ggplot(full_2015, aes(x = `DE Rank`, y = `Make Tournament`)) + geom_point() 
    
    DEData = select(alldata, School, `DE Rank`, `Make Tournament`)
    DEData = DEData %>%
      group_by(`DE Rank`) %>%
      mutate(`In Tournament` = sum(`Make Tournament` == 1),
             `Miss Tournament` = sum(`Make Tournament` == 0),
             Probability = round(`In Tournament`/(`In Tournament` + `Miss Tournament`),3)*100) 
    ggplot(DEData, aes(x = `DE Rank`, fill = `Make Tournament`)) + geom_bar() + 
      scale_fill_manual(values=c("dark orange", "black"))
    
    ## Efficiency Avg Rank
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
    ggplot(EffAvgData, aes(x = `Efficiency Avg`, fill = `Make Tournament`)) + geom_bar() + 
      scale_fill_manual(values=c("dark orange", "black"))
    
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
    
    ggplot(alldata, aes(x = `SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.2,0.9)) + geom_jitter(height = 0.2)
    
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
    
    ggplot(alldata, aes(x = `Non-Conf SOS`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(0.2,0.9)) + geom_jitter(height = 0.2)
    
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
    
    ggplot(alldata, aes(x = `Conference Win %`, y = `Make Tournament`)) + geom_point() + 
      scale_x_continuous(limits=c(-0.1,1.1)) + geom_jitter(height = 0.2)
    
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
    
    
    ## Variables that look significant:
    ## RPI, Conference Champ, Wins, Efficiency Rank Avg, T-Rank, SOS, Conf Win %, WAB Rank
    
    ## Other variables of interest:
    ## Last 12 Wins, Conference Finish, Conference