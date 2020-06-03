install.packages("rvest")
library(rvest)
install.packages("RCurl")
library(RCurl)
install.packages("XML")
library(XML)
install.packages("tidyverse")
library(tidyverse)


#RPI
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

#NET Ranking (Only Since 2019)
#Unable to find numerical values, only ranking

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







