install.packages("rvest")
library(rvest)
install.packages("RCurl")
library(RCurl)
install.packages("XML")
library(XML)
install.packages("tidyverse")
library(tidyverse)


## RPI (TBD)
rpi_function = function(x){
  url = getURL(x)
  rpi_url = readHTMLTable(url)
  rpi_url = rpi_url[["NULL"]]
  rpi = rpi_url
  
  rpi$Team = as.character(rpi$Team)
  for(i in 1:length(rpi$Team)){
    rpi$Team[i] = gsub("\\d", "", rpi$Team[i])
    rpi$Team[i] = gsub("\\W", "", rpi$Team[i])
  }
  
  rpi = rpi[,c(2,3)]
  rpi = rpi[order(rpi$Team),]
}

rpi_2020 = rpi_function('https://www.teamrankings.com/ncaa-basketball/rpi-ranking/rpi-rating-by-team?date=2020-03-12')
rpi_2019 = rpi_function('https://www.teamrankings.com/ncaa-basketball/rpi-ranking/rpi-rating-by-team?date=2019-03-16')
rpi_2018 = rpi_function('https://www.teamrankings.com/ncaa-basketball/rpi-ranking/rpi-rating-by-team?date=2018-03-10')
rpi_2017 = rpi_function('https://www.teamrankings.com/ncaa-basketball/rpi-ranking/rpi-rating-by-team?date=2017-03-11')
rpi_2016 = rpi_function('https://www.teamrankings.com/ncaa-basketball/rpi-ranking/rpi-rating-by-team?date=2016-03-12')
rpi_2015 = rpi_function('https://www.teamrankings.com/ncaa-basketball/rpi-ranking/rpi-rating-by-team?date=2015-03-14')

## NET Ranking (Only Since 2019), (TBD)
## Unable to find numerical values, only ranking

net_ranking = function(x){
  url = getURL(x)
  net_url = readHTMLTable(url)
  net_url = net_url[["NULL"]]
  net = net_url[, c(2,4)]
  net = net[order(net$Team),]
}

net_2020 = net_ranking('http://warrennolan.com/basketball/2020/net')
net_2019 = net_ranking('http://warrennolan.com/basketball/2019/net')
net_2018 = rep(NA,351)
net_2017 = rep(NA,351)
net_2016 = rep(NA,351)
net_2015 = rep(NA,351)


## SOS/SRS/Conference Record/Road (All found in one chart) 
sos_function = function(x){
url = getURL(x)
sos_url = readHTMLTable(url)
sos_url = sos_url[["adv_school_stats"]]
sos = sos_url[, c(2,7,8,9,10,13,14)]
colnames(sos) = c("School", "SRS", "SOS", "Conf W", "Conf L", "Road W", "Road L")
sos = sos[-c(21,22,43,44,65,66,87,88,109,110,131,132,153,154,175,176,197,198,219,220,241,242,263,264,285,286,307,308,329,330,351,352,373,374),] 
sos$`Conf W`= as.character(sos$`Conf W`)
sos$`Conf W`= as.numeric(sos$`Conf W`)
sos$`Conf L`= as.character(sos$`Conf L`)
sos$`Conf L`= as.numeric(sos$`Conf L`)
sos$`Road W`= as.character(sos$`Road W`)
sos$`Road W`= as.numeric(sos$`Road W`)
sos$`Road L`= as.character(sos$`Road L`)
sos$`Road L`= as.numeric(sos$`Road L`)
sos$School = as.character(sos$School)
sos$`Conf Win %` = round(sos$`Conf W`/ (sos$`Conf W` + sos$`Conf L`),3)
sos$`Road Win %` = round(sos$`Road W`/ (sos$`Road W` + sos$`Road L`),3)
sos = sos[, c(1,2,3,8,9)]
sos
}

sos_2020 = sos_function('https://www.sports-reference.com/cbb/seasons/2020-advanced-school-stats.html')
sos_2019 = sos_function('https://www.sports-reference.com/cbb/seasons/2019-advanced-school-stats.html')
sos_2018 = sos_function('https://www.sports-reference.com/cbb/seasons/2018-advanced-school-stats.html')
sos_2017 = sos_function('https://www.sports-reference.com/cbb/seasons/2017-advanced-school-stats.html')
sos_2016 = sos_function('https://www.sports-reference.com/cbb/seasons/2016-advanced-school-stats.html')
sos_2015 = sos_function('https://www.sports-reference.com/cbb/seasons/2015-advanced-school-stats.html')


## Add Made Tournament Variable

sos_2019$`Make Tournament` = rep(0, length(sos_2019$School))
sos_2018$`Make Tournament` = rep(0, length(sos_2018$School))
sos_2017$`Make Tournament` = rep(0, length(sos_2017$School))
sos_2016$`Make Tournament` = rep(0, length(sos_2016$School))
sos_2015$`Make Tournament` = rep(0, length(sos_2015$School))

make_tournament_function = function(z){
  x = z$School
  y = z$`Make Tournament`
  for(i in 1:length(x)){
    if(isTRUE(grepl("NCAA", x)[i])){
      y[i] = 1
    }
  }
  z$`Make Tournament` = y
  z
}

sos_2019 = make_tournament_function(sos_2019)
sos_2018 = make_tournament_function(sos_2018)
sos_2017 = make_tournament_function(sos_2017)
sos_2016 = make_tournament_function(sos_2016)
sos_2015 = make_tournament_function(sos_2015)

  ## Check to make sure 68 teams for each year
  length(which(sos_2019$`Make Tournament` == 1))
  length(which(sos_2018$`Make Tournament` == 1))
  length(which(sos_2017$`Make Tournament` == 1))
  length(which(sos_2016$`Make Tournament` == 1))
  length(which(sos_2015$`Make Tournament` == 1))


## Removing NCAA & extra space from team names

remove_junk_function = function(z){
  x = z$School
  for(i in 1:length(x)){
    x[i] = gsub("NCAA", "", x[i])
    x[i] = str_trim(x[i], "right")
  }
  z$School = x
  z
}

sos_2019 = remove_junk_function(sos_2019)
sos_2018 = remove_junk_function(sos_2018)
sos_2017 = remove_junk_function(sos_2017)
sos_2016 = remove_junk_function(sos_2016)
sos_2015 = remove_junk_function(sos_2015)

## Adding Tournament Champion/Runner Up Variables

sos_2019$`Conference Champ` = rep(0, length(sos_2019$School))
sos_2018$`Conference Champ` = rep(0, length(sos_2018$School))
sos_2017$`Conference Champ` = rep(0, length(sos_2017$School))
sos_2016$`Conference Champ` = rep(0, length(sos_2016$School))
sos_2015$`Conference Champ` = rep(0, length(sos_2015$School))

sos_2019$`Conference Runner-Up` = rep(0, length(sos_2019$School))
sos_2018$`Conference Runner-Up` = rep(0, length(sos_2018$School))
sos_2017$`Conference Runner-Up` = rep(0, length(sos_2017$School))
sos_2016$`Conference Runner-Up` = rep(0, length(sos_2016$School))
sos_2015$`Conference Runner-Up` = rep(0, length(sos_2015$School))

conference_champ_url_function = function(x){
url = getURL(x)
champ_url = readHTMLTable(url)
champ = champ_url[["conference-summary"]]
champ = champ[,c(12,13)] 
champ$`Tournament Champ` = as.character(champ$`Tournament Champ`)
champ
}

champ_2019 = conference_champ_url_function('https://www.sports-reference.com/cbb/seasons/2019.html')
champ_2018 = conference_champ_url_function('https://www.sports-reference.com/cbb/seasons/2018.html')
champ_2017 = conference_champ_url_function('https://www.sports-reference.com/cbb/seasons/2017.html')
champ_2016 = conference_champ_url_function('https://www.sports-reference.com/cbb/seasons/2016.html')
champ_2015 = conference_champ_url_function('https://www.sports-reference.com/cbb/seasons/2015.html')

  ## Fixing minor issues with data
  champ_2015 = champ_2015[-18,]
  champ_2015[16,2] = "Harvard"
  champ_2016[15,2] = "Yale"

conference_champ_function = function(a,b){
  x = a$`Tournament Champ`
  y = b$School
  z = b$`Conference Champ`
  for(i in 1:length(x)){
    for(j in 1:length(y)){
      if(y[j] == x[i]){
        z[j] = 1
      }
    }
  }
  b$`Conference Champ`= z
  b
}

sos_2019 = conference_champ_function(champ_2019, sos_2019)
sos_2018 = conference_champ_function(champ_2018, sos_2018)
sos_2017 = conference_champ_function(champ_2017, sos_2017)
sos_2016 = conference_champ_function(champ_2016, sos_2016)
sos_2015 = conference_champ_function(champ_2015, sos_2015)

  ## Check to make sure 32 conference champions each year

  length(which(sos_2019$`Conference Champ` == 1))
  length(which(sos_2018$`Conference Champ` == 1))
  length(which(sos_2017$`Conference Champ` == 1))
  length(which(sos_2016$`Conference Champ` == 1))
  length(which(sos_2015$`Conference Champ` == 1))
  
## ADD CODE for RUNNER UP
  

## Last 12 Games Variable
sos_2019$`Last 12 Win %` = rep(NA, length(sos_2019$School))
sos_2018$`Last 12 Win %` = rep(NA, length(sos_2018$School))
sos_2017$`Last 12 Win %` = rep(NA, length(sos_2017$School))
sos_2016$`Last 12 Win %` = rep(NA, length(sos_2016$School)) 
sos_2015$`Last 12 Win %` = rep(NA, length(sos_2015$School))

last_12_function = function(x){
url = getURL('https://www.sports-reference.com/cbb/schools/abilene-christian/2019-schedule.html')
games_url = readHTMLTable(url)
games = odds_url[["schedule"]]
games = games[, c(4,8)]
colnames(games) = c("Type", "Result")
games$Type = as.character(games$Type)
games$Result = as.character(games$Result)
for(i in 1:length(games$Result)){
  if(isFALSE(grepl("REG", games$Type)[i])){
    games = games[-i,]
  }
}
for(i in 1:length(games$Result)){
  if(isTRUE(grepl("Type", games$Type)[i])){
    games = games[-i,] 
  }
}
for(i in 1:length(games$Result)){
  if(isTRUE(grepl("CTOURN", games$Type)[i])){
    games = games[-i,] 
  }
}
for(i in 1:length(games$Result)){
  if(isTRUE(grepl("NCAA", games$Type)[i])){
    games = games[-i,] 
  }
}
games[32,1]
games = games[(length(games$Type)-11):length(games$Type),]
winpercentage = length(which(games$Result == "W")) / (length(which(games$Result == "W"))+length(which(games$Result == "L")))
winpercentage
}

last_12_function('https://www.sports-reference.com/cbb/schools/abilene-christian/2019-schedule.html')
