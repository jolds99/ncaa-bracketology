## Load Packages
library(rvest)
library(RCurl)
library(XML)
library(tidyverse)
library(ggplot2)
library(rjson)
library(R.utils)

## Forming MASTER Datasets, organized by year 

  ## Initial Datasets (from www.barttorvik.com)
    
    create_data_function = function(x){
      data = fromJSON(file = x)
      data_hold = NULL
      for(i in 1:length(data)){
        data[[i]] = as.data.frame(data[[i]])
        colnames(data[[i]]) = c("Rank", "Team", "Conference", "Record", "Adj. Offensive Efficiency", "OE Rank",
                                "Adj. Defensive Efficiency", "DE Rank", "Barthag", "Barthag Rank", "Proj. W", "Proj. L", 
                                "Proj. Conference W", "Proj. Conference L", "Conference Record", "SOS", "Non-Conf SOS", 
                                "Conference SOS", "Proj. SOS", "Proj. Non-Conf SOS", "Proj. Conf SOS", "Elite SOS", 
                                "Elite Non-Conf SOS", "Opponent OE", "Opponent DE", "Opponent Proj. OE", "Opponent Proj. DE", 
                                "Conference Adj. OE", "Conference Adj. DE", "Qual O", "Qual D", "Qual Barthag", "Qual Games",
                                "FUN", "Conference Points For", "Conference Points Against", "ConPoss", "ConOE", "ConDE", 
                                "ConSOSRemain", "Conference Win %", "Wins Above Bubble", "WAB Rank", "FUN Rank Adj.", "IDK")
        data_hold = rbind(data_hold,data[[i]])
      }
      data_hold = data_hold[,c(2:10,15:18,41:43)]
      data_hold
    }
    
    data_2020 = create_data_function("JSON Data Files/20200312_team_results.json")
    data_2019 = create_data_function("JSON Data Files/20190316_team_results.json")
    data_2018 = create_data_function("JSON Data Files/20180310_team_results.json")
    data_2017 = create_data_function("JSON Data Files/20170311_team_results.json")
    data_2016 = create_data_function("JSON Data Files/20160312_team_results.json")
    data_2015 = create_data_function("JSON Data Files/20150314_team_results.json")
    
  ## Using SportsReference data to add other variables
  load_sr_names_function = function(x){
  url = getURL(x)
  sr_url = readHTMLTable(url)
  sr_url = sr_url[["adv_school_stats"]]
  sr = sr_url[,2]
  sr = as.data.frame(sr)
  sr = sr[-c(21,22,43,44,65,66,87,88,109,110,131,132,153,154,175,176,197,198,219,220,241,242,263,264,285,286,307,308,329,330,351,352,373,374),]
  sr = as.data.frame(sr)
  colnames(sr)[1] = "School"
  sr$School = as.character(sr$School)
  sr 
  }

  sr_2020 = load_sr_names_function('https://www.sports-reference.com/cbb/seasons/2020-advanced-school-stats.html')
  sr_2019 = load_sr_names_function('https://www.sports-reference.com/cbb/seasons/2019-advanced-school-stats.html')
  sr_2018 = load_sr_names_function('https://www.sports-reference.com/cbb/seasons/2018-advanced-school-stats.html')
  sr_2017 = load_sr_names_function('https://www.sports-reference.com/cbb/seasons/2017-advanced-school-stats.html')
  sr_2016 = load_sr_names_function('https://www.sports-reference.com/cbb/seasons/2016-advanced-school-stats.html')
  sr_2015 = load_sr_names_function('https://www.sports-reference.com/cbb/seasons/2015-advanced-school-stats.html')


  ## Add Made Tournament Variable
  sr_2020$`Make Tournament` = rep(NA, length(sr_2020$School))
  sr_2019$`Make Tournament` = rep(0, length(sr_2019$School))
  sr_2018$`Make Tournament` = rep(0, length(sr_2018$School))
  sr_2017$`Make Tournament` = rep(0, length(sr_2017$School))
  sr_2016$`Make Tournament` = rep(0, length(sr_2016$School))
  sr_2015$`Make Tournament` = rep(0, length(sr_2015$School))

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
  
  sr_2019 = make_tournament_function(sr_2019)
  sr_2018 = make_tournament_function(sr_2018)
  sr_2017 = make_tournament_function(sr_2017)
  sr_2016 = make_tournament_function(sr_2016)
  sr_2015 = make_tournament_function(sr_2015)

    ## Check to make sure 68 teams for each year
    length(which(sr_2019$`Make Tournament` == 1))
    length(which(sr_2018$`Make Tournament` == 1))
    length(which(sr_2017$`Make Tournament` == 1))
    length(which(sr_2016$`Make Tournament` == 1))
    length(which(sr_2015$`Make Tournament` == 1))


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
  
  sr_2019 = remove_junk_function(sr_2019)
  sr_2018 = remove_junk_function(sr_2018)
  sr_2017 = remove_junk_function(sr_2017)
  sr_2016 = remove_junk_function(sr_2016)
  sr_2015 = remove_junk_function(sr_2015)

  ## Adding Tournament Champion Variable
  
  sr_2020$`Conference Champ` = rep(NA, length(sr_2020$School))
  sr_2019$`Conference Champ` = rep(0, length(sr_2019$School))
  sr_2018$`Conference Champ` = rep(0, length(sr_2018$School))
  sr_2017$`Conference Champ` = rep(0, length(sr_2017$School))
  sr_2016$`Conference Champ` = rep(0, length(sr_2016$School))
  sr_2015$`Conference Champ` = rep(0, length(sr_2015$School))
  
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

  sr_2019 = conference_champ_function(champ_2019, sr_2019)
  sr_2018 = conference_champ_function(champ_2018, sr_2018)
  sr_2017 = conference_champ_function(champ_2017, sr_2017)
  sr_2016 = conference_champ_function(champ_2016, sr_2016)
  sr_2015 = conference_champ_function(champ_2015, sr_2015)

      ## Check to make sure 32 conference champions each year
    
      length(which(sr_2019$`Conference Champ` == 1))
      length(which(sr_2018$`Conference Champ` == 1))
      length(which(sr_2017$`Conference Champ` == 1))
      length(which(sr_2016$`Conference Champ` == 1))
      length(which(sr_2015$`Conference Champ` == 1))

  ## Last 12 Games Variable
  sr_2020$`Last 12 Win %` = rep(NA, length(sr_2020$School))
  sr_2019$`Last 12 Win %` = rep(NA, length(sr_2019$School))
  sr_2018$`Last 12 Win %` = rep(NA, length(sr_2018$School))
  sr_2017$`Last 12 Win %` = rep(NA, length(sr_2017$School))
  sr_2016$`Last 12 Win %` = rep(NA, length(sr_2016$School)) 
  sr_2015$`Last 12 Win %` = rep(NA, length(sr_2015$School))

  last_12_function = function(x){
  url = getURL(x)
  games_url = readHTMLTable(url)
  games = games_url[["schedule"]]
  games = games[, c(4,8)]
  colnames(games) = c("Type", "Result")
  games$Type = as.character(games$Type)
  games$Result = as.character(games$Result)
  non_reg = grep("REG", games$Type, invert = TRUE)
  length = length(non_reg)
  if(length != 0){
    games = games[c(-non_reg),]
  }
  games = games[(length(games$Type)-11):length(games$Type),]
  winpercentage = round(length(which(games$Result == "W")) / (length(which(games$Result == "W"))+length(which(games$Result == "L"))),3)
  winpercentage
  }

       ## Checking that same 351 teams from 2015-18
      x = rep(NA,351)
      for(i in 1:length(sr_2015$School)){
        x[i] = identical(sr_2015$School[i],sr_2016$School[i])
      }
      which(x == FALSE)
      
      x = rep(NA,351)
      for(i in 1:length(sr_2015$School)){
        x[i] = identical(sr_2015$School[i],sr_2017$School[i])
      }
      which(x == FALSE)
      
      x = rep(NA,351)
      for(i in 1:length(sr_2015$School)){
        x[i] = identical(sr_2015$School[i],sr_2018$School[i])
      }
      which(x == FALSE)
        ## Same, can use same team list for 4 years. 
      
        ## Checking that same 353 teams from 2019-2020
      x = rep(NA,353)
      for(i in 1:length(sr_2019$School)){
        x[i] = identical(sr_2019$School[i],sr_2020$School[i])
      }
      
      which(x == FALSE)
        ## Not the same, will adjust

  schools_19 = c("abilene-christian", "air-force", "akron", "alabama-am", "alabama-birmingham",
                 "alabama-state", "alabama", "albany-ny", "alcorn-state", "american", "appalachian-state",
                 "arizona-state", "arizona", "arkansas-little-rock", "arkansas-pine-bluff", "arkansas-state",
                 "arkansas", "army", "auburn", "austin-peay", "ball-state", "baylor", "belmont", "bethune-cookman",
                 "binghamton", "boise-state", "boston-college", "boston-university", "bowling-green-state",
                 "bradley", "brigham-young", "brown", "bryant", "bucknell", "buffalo", "butler", "cal-poly",
                 "cal-state-bakersfield", "cal-state-fullerton", "cal-state-northridge", "california-baptist",
                 "california-davis", "california-irvine", "california-riverside", "california-santa-barbara", 
                 "california", "campbell", "canisius", "central-arkansas", "central-connecticut-state", "central-florida",
                 "central-michigan", "charleston-southern", "charlotte", "chattanooga", "chicago-state",
                 "cincinnati", "citadel", "clemson", "cleveland-state", "coastal-carolina", "colgate", 
                 "college-of-charleston", "colorado-state", "colorado", "columbia", "connecticut", "coppin-state", 
                 "cornell", "creighton", "dartmouth", "davidson", "dayton", "delaware-state", "delaware",
                 "denver", "depaul", "detroit-mercy", "drake", "drexel", "duke", "duquesne", "east-carolina", 
                 "east-tennessee-state", "eastern-illinois", "eastern-kentucky", "eastern-michigan", "eastern-washington",
                 "elon", "evansville", "fairfield", "fairleigh-dickinson", "florida-am", "florida-atlantic",
                 "florida-gulf-coast", "florida-international", "florida-state", "florida", "fordham", "fresno-state",
                 "furman", "gardner-webb", "george-mason", "george-washington", "georgetown", "georgia-southern", 
                 "georgia-state", "georgia-tech", "georgia", "gonzaga", "grambling", "grand-canyon", "green-bay",
                 "hampton", "hartford", "harvard", "hawaii", "high-point", "hofstra", "holy-cross", "houston-baptist", 
                 "houston", "howard", "idaho-state", "idaho", "illinois-chicago", "illinois-state", "illinois",
                 "incarnate-word", "indiana-state", "indiana", "iona", "iowa-state", "iowa", "ipfw", "iupui", "jackson-state",
                 "jacksonville-state", "jacksonville", "james-madison", "kansas-state", "kansas", "kennesaw-state", 
                 "kent-state", "kentucky", "la-salle", "lafayette", "lamar", "lehigh", "liberty", "lipscomb",
                 "long-beach-state", "long-island-university", "longwood", "louisiana-lafayette", "louisiana-monroe",
                 "louisiana-state", "louisiana-tech", "louisville", "loyola-il", "loyola-marymount", "loyola-md", 
                 "maine", "manhattan", "marist", "marquette", "marshall", "maryland-baltimore-county", "maryland-eastern-shore",
                 "maryland", "massachusetts-lowell", "massachusetts", "mcneese-state", "memphis", "mercer", "miami-fl",
                 "miami-oh", "michigan-state", "michigan", "middle-tennessee", "milwaukee", "minnesota", "mississippi-state",
                 "mississippi-valley-state", "mississippi", "missouri-kansas-city", "missouri-state", "missouri", "monmouth",
                 "montana-state", "montana", "morehead-state", "morgan-state", "mount-st-marys", "murray-state",
                 "navy", "nebraska-omaha", "nebraska", "nevada-las-vegas", "nevada", "new-hampshire", "new-mexico-state", 
                 "new-mexico", "new-orleans", "niagara", "nicholls-state", "njit", "norfolk-state", "north-alabama",
                 "north-carolina-asheville", "north-carolina-at", "north-carolina-central", "north-carolina-greensboro",
                 "north-carolina-state", "north-carolina-wilmington", "north-carolina", "north-dakota-state", "north-dakota", 
                 "north-florida", "north-texas", "northeastern", "northern-arizona", "northern-colorado", "northern-illinois", 
                 "northern-iowa", "northern-kentucky", "northwestern-state", "northwestern", "notre-dame", "oakland", "ohio-state","ohio", 
                 "oklahoma-state", "oklahoma", "old-dominion", "oral-roberts", "oregon-state", "oregon", "pacific", "penn-state", 
                 "pennsylvania", "pepperdine", "pittsburgh", "portland-state", "portland", "prairie-view", "presbyterian", "princeton",
                 "providence", "purdue", "quinnipiac", "radford", "rhode-island", "rice", "richmond", "rider", "robert-morris",
                 "rutgers", "sacramento-state", "sacred-heart", "saint-francis-pa", "saint-josephs", "saint-louis", "saint-marys-ca",
                 "saint-peters", "sam-houston-state", "samford", "san-diego-state", "san-diego", "san-francisco", "san-jose-state",
                 "santa-clara", "savannah-state", "seattle", "seton-hall", "siena", "south-alabama", "south-carolina-state", 
                 "south-carolina-upstate", "south-carolina", "south-dakota-state", "south-dakota", "south-florida", "southeast-missouri-state",
                 "southeastern-louisiana", "southern-california", "southern-illinois-edwardsville", "southern-illinois", "southern-methodist",
                 "southern-mississippi", "southern-utah", "southern", "st-bonaventure", "st-francis-ny", "st-johns-ny", "stanford", "stephen-f-austin",
                 "stetson", "stony-brook", "syracuse", "temple", "tennessee-martin", "tennessee-state", "tennessee-tech",
                 "tennessee", "texas-am-corpus-christi", "texas-am", "texas-arlington", "texas-christian", "texas-el-paso", 
                 "texas-pan-american", "texas-san-antonio", "texas-southern", "texas-state", "texas-tech", "texas", "toledo",
                 "towson", "troy", "tulane", "tulsa", "ucla", "utah-state", "utah-valley", "utah", "valparaiso", 
                 "vanderbilt", "vermont", "villanova", "virginia-commonwealth", "virginia-military-institute", "virginia-tech", 
                 "virginia", "wagner", "wake-forest", "washington-state", "washington", "weber-state", "west-virginia", 
                 "western-carolina", "western-illinois", "western-kentucky", "western-michigan", "wichita-state", "william-mary",
                 "winthrop", "wisconsin", "wofford", "wright-state", "wyoming", "xavier", "yale", "youngstown-state")
  
  link = rep(NA, 353)
  for(i in 1:353){
    link[i] = paste("https://www.sports-reference.com/cbb/schools/",schools_19[i],"/2019-schedule.html", sep = "")
    sr_2019$`Last 12 Win %`[i] = last_12_function(link[i])
  } 
  
  schools_20 = c("abilene-christian", "air-force", "akron", "alabama-am", "alabama-birmingham",
                 "alabama-state", "alabama", "albany-ny", "alcorn-state", "american", "appalachian-state",
                 "arizona-state", "arizona", "arkansas-little-rock", "arkansas-pine-bluff", "arkansas-state",
                 "arkansas", "army", "auburn", "austin-peay", "ball-state", "baylor", "belmont", "bethune-cookman",
                 "binghamton", "boise-state", "boston-college", "boston-university", "bowling-green-state",
                 "bradley", "brigham-young", "brown", "bryant", "bucknell", "buffalo", "butler", "cal-poly",
                 "cal-state-bakersfield", "cal-state-fullerton", "cal-state-northridge", "california-baptist",
                 "california-davis", "california-irvine", "california-riverside", "california-santa-barbara", 
                 "california", "campbell", "canisius", "central-arkansas", "central-connecticut-state", "central-florida",
                 "central-michigan", "charleston-southern", "charlotte", "chattanooga", "chicago-state",
                 "cincinnati", "citadel", "clemson", "cleveland-state", "coastal-carolina", "colgate", 
                 "college-of-charleston", "colorado-state", "colorado", "columbia", "connecticut", "coppin-state", 
                 "cornell", "creighton", "dartmouth", "davidson", "dayton", "delaware-state", "delaware",
                 "denver", "depaul", "detroit-mercy", "drake", "drexel", "duke", "duquesne", "east-carolina", 
                 "east-tennessee-state", "eastern-illinois", "eastern-kentucky", "eastern-michigan", "eastern-washington",
                 "elon", "evansville", "fairfield", "fairleigh-dickinson", "florida-am", "florida-atlantic",
                 "florida-gulf-coast", "florida-international", "florida-state", "florida", "fordham", "fresno-state",
                 "furman", "gardner-webb", "george-mason", "george-washington", "georgetown", "georgia-southern", 
                 "georgia-state", "georgia-tech", "georgia", "gonzaga", "grambling", "grand-canyon", "green-bay",
                 "hampton", "hartford", "harvard", "hawaii", "high-point", "hofstra", "holy-cross", "houston-baptist", 
                 "houston", "howard", "idaho-state", "idaho", "illinois-chicago", "illinois-state", "illinois",
                 "incarnate-word", "indiana-state", "indiana", "iona", "iowa-state", "iowa", "ipfw", "iupui", "jackson-state",
                 "jacksonville-state", "jacksonville", "james-madison", "kansas-state", "kansas", "kennesaw-state", 
                 "kent-state", "kentucky", "la-salle", "lafayette", "lamar", "lehigh", "liberty", "lipscomb",
                 "long-beach-state", "long-island-university", "longwood", "louisiana-lafayette", "louisiana-monroe",
                 "louisiana-state", "louisiana-tech", "louisville", "loyola-il", "loyola-marymount", "loyola-md", 
                 "maine", "manhattan", "marist", "marquette", "marshall", "maryland-baltimore-county", "maryland-eastern-shore",
                 "maryland", "massachusetts-lowell", "massachusetts", "mcneese-state", "memphis", "mercer", "merrimack", "miami-fl",
                 "miami-oh", "michigan-state", "michigan", "middle-tennessee", "milwaukee", "minnesota", "mississippi-state",
                 "mississippi-valley-state", "mississippi", "missouri-kansas-city", "missouri-state", "missouri", "monmouth",
                 "montana-state", "montana", "morehead-state", "morgan-state", "mount-st-marys", "murray-state",
                 "navy", "nebraska-omaha", "nebraska", "nevada-las-vegas", "nevada", "new-hampshire", "new-mexico-state", 
                 "new-mexico", "new-orleans", "niagara", "nicholls-state", "njit", "norfolk-state", "north-alabama",
                 "north-carolina-asheville", "north-carolina-at", "north-carolina-central", "north-carolina-greensboro",
                 "north-carolina-state", "north-carolina-wilmington", "north-carolina", "north-dakota-state", "north-dakota", 
                 "north-florida", "north-texas", "northeastern", "northern-arizona", "northern-colorado", "northern-illinois", 
                 "northern-iowa", "northern-kentucky", "northwestern-state", "northwestern", "notre-dame", "oakland", "ohio-state","ohio", 
                 "oklahoma-state", "oklahoma", "old-dominion", "oral-roberts", "oregon-state", "oregon", "pacific", "penn-state", 
                 "pennsylvania", "pepperdine", "pittsburgh", "portland-state", "portland", "prairie-view", "presbyterian", "princeton",
                 "providence", "purdue", "quinnipiac", "radford", "rhode-island", "rice", "richmond", "rider", "robert-morris",
                 "rutgers", "sacramento-state", "sacred-heart", "saint-francis-pa", "saint-josephs", "saint-louis", "saint-marys-ca",
                 "saint-peters", "sam-houston-state", "samford", "san-diego-state", "san-diego", "san-francisco", "san-jose-state",
                 "santa-clara", "seattle", "seton-hall", "siena", "south-alabama", "south-carolina-state", 
                 "south-carolina-upstate", "south-carolina", "south-dakota-state", "south-dakota", "south-florida", "southeast-missouri-state",
                 "southeastern-louisiana", "southern-california", "southern-illinois-edwardsville", "southern-illinois", "southern-methodist",
                 "southern-mississippi", "southern-utah", "southern", "st-bonaventure", "st-francis-ny", "st-johns-ny", "stanford", "stephen-f-austin",
                 "stetson", "stony-brook", "syracuse", "temple", "tennessee-martin", "tennessee-state", "tennessee-tech",
                 "tennessee", "texas-am-corpus-christi", "texas-am", "texas-arlington", "texas-christian", "texas-el-paso", 
                 "texas-pan-american", "texas-san-antonio", "texas-southern", "texas-state", "texas-tech", "texas", "toledo",
                 "towson", "troy", "tulane", "tulsa", "ucla", "utah-state", "utah-valley", "utah", "valparaiso", 
                 "vanderbilt", "vermont", "villanova", "virginia-commonwealth", "virginia-military-institute", "virginia-tech", 
                 "virginia", "wagner", "wake-forest", "washington-state", "washington", "weber-state", "west-virginia", 
                 "western-carolina", "western-illinois", "western-kentucky", "western-michigan", "wichita-state", "william-mary",
                 "winthrop", "wisconsin", "wofford", "wright-state", "wyoming", "xavier", "yale", "youngstown-state")
  link = rep(NA,353)
  for(i in 1:353){
    link[i] = paste("https://www.sports-reference.com/cbb/schools/",schools_20[i],"/2020-schedule.html", sep = "")
    sr_2020$`Last 12 Win %`[i] = last_12_function(link[i])
  } 
  
  schools_15_18 = c("abilene-christian", "air-force", "akron", "alabama-am", "alabama-birmingham",
                 "alabama-state", "alabama", "albany-ny", "alcorn-state", "american", "appalachian-state",
                 "arizona-state", "arizona", "arkansas-little-rock", "arkansas-pine-bluff", "arkansas-state",
                 "arkansas", "army", "auburn", "austin-peay", "ball-state", "baylor", "belmont", "bethune-cookman",
                 "binghamton", "boise-state", "boston-college", "boston-university", "bowling-green-state",
                 "bradley", "brigham-young", "brown", "bryant", "bucknell", "buffalo", "butler", "cal-poly",
                 "cal-state-bakersfield", "cal-state-fullerton", "cal-state-northridge",
                 "california-davis", "california-irvine", "california-riverside", "california-santa-barbara", 
                 "california", "campbell", "canisius", "central-arkansas", "central-connecticut-state", "central-florida",
                 "central-michigan", "charleston-southern", "charlotte", "chattanooga", "chicago-state",
                 "cincinnati", "citadel", "clemson", "cleveland-state", "coastal-carolina", "colgate", 
                 "college-of-charleston", "colorado-state", "colorado", "columbia", "connecticut", "coppin-state", 
                 "cornell", "creighton", "dartmouth", "davidson", "dayton", "delaware-state", "delaware",
                 "denver", "depaul", "detroit-mercy", "drake", "drexel", "duke", "duquesne", "east-carolina", 
                 "east-tennessee-state", "eastern-illinois", "eastern-kentucky", "eastern-michigan", "eastern-washington",
                 "elon", "evansville", "fairfield", "fairleigh-dickinson", "florida-am", "florida-atlantic",
                 "florida-gulf-coast", "florida-international", "florida-state", "florida", "fordham", "fresno-state",
                 "furman", "gardner-webb", "george-mason", "george-washington", "georgetown", "georgia-southern", 
                 "georgia-state", "georgia-tech", "georgia", "gonzaga", "grambling", "grand-canyon", "green-bay",
                 "hampton", "hartford", "harvard", "hawaii", "high-point", "hofstra", "holy-cross", "houston-baptist", 
                 "houston", "howard", "idaho-state", "idaho", "illinois-chicago", "illinois-state", "illinois",
                 "incarnate-word", "indiana-state", "indiana", "iona", "iowa-state", "iowa", "ipfw", "iupui", "jackson-state",
                 "jacksonville-state", "jacksonville", "james-madison", "kansas-state", "kansas", "kennesaw-state", 
                 "kent-state", "kentucky", "la-salle", "lafayette", "lamar", "lehigh", "liberty", "lipscomb",
                 "long-beach-state", "long-island-university", "longwood", "louisiana-lafayette", "louisiana-monroe",
                 "louisiana-state", "louisiana-tech", "louisville", "loyola-il", "loyola-marymount", "loyola-md", 
                 "maine", "manhattan", "marist", "marquette", "marshall", "maryland-baltimore-county", "maryland-eastern-shore",
                 "maryland", "massachusetts-lowell", "massachusetts", "mcneese-state", "memphis", "mercer", "miami-fl",
                 "miami-oh", "michigan-state", "michigan", "middle-tennessee", "milwaukee", "minnesota", "mississippi-state",
                 "mississippi-valley-state", "mississippi", "missouri-kansas-city", "missouri-state", "missouri", "monmouth",
                 "montana-state", "montana", "morehead-state", "morgan-state", "mount-st-marys", "murray-state",
                 "navy", "nebraska-omaha", "nebraska", "nevada-las-vegas", "nevada", "new-hampshire", "new-mexico-state", 
                 "new-mexico", "new-orleans", "niagara", "nicholls-state", "njit", "norfolk-state",
                 "north-carolina-asheville", "north-carolina-at", "north-carolina-central", "north-carolina-greensboro",
                 "north-carolina-state", "north-carolina-wilmington", "north-carolina", "north-dakota-state", "north-dakota", 
                 "north-florida", "north-texas", "northeastern", "northern-arizona", "northern-colorado", "northern-illinois", 
                 "northern-iowa", "northern-kentucky", "northwestern-state", "northwestern", "notre-dame", "oakland", "ohio-state","ohio", 
                 "oklahoma-state", "oklahoma", "old-dominion", "oral-roberts", "oregon-state", "oregon", "pacific", "penn-state", 
                 "pennsylvania", "pepperdine", "pittsburgh", "portland-state", "portland", "prairie-view", "presbyterian", "princeton",
                 "providence", "purdue", "quinnipiac", "radford", "rhode-island", "rice", "richmond", "rider", "robert-morris",
                 "rutgers", "sacramento-state", "sacred-heart", "saint-francis-pa", "saint-josephs", "saint-louis", "saint-marys-ca",
                 "saint-peters", "sam-houston-state", "samford", "san-diego-state", "san-diego", "san-francisco", "san-jose-state",
                 "santa-clara", "savannah-state", "seattle", "seton-hall", "siena", "south-alabama", "south-carolina-state", 
                 "south-carolina-upstate", "south-carolina", "south-dakota-state", "south-dakota", "south-florida", "southeast-missouri-state",
                 "southeastern-louisiana", "southern-california", "southern-illinois-edwardsville", "southern-illinois", "southern-methodist",
                 "southern-mississippi", "southern-utah", "southern", "st-bonaventure", "st-francis-ny", "st-johns-ny", "stanford", "stephen-f-austin",
                 "stetson", "stony-brook", "syracuse", "temple", "tennessee-martin", "tennessee-state", "tennessee-tech",
                 "tennessee", "texas-am-corpus-christi", "texas-am", "texas-arlington", "texas-christian", "texas-el-paso", 
                 "texas-pan-american", "texas-san-antonio", "texas-southern", "texas-state", "texas-tech", "texas", "toledo",
                 "towson", "troy", "tulane", "tulsa", "ucla", "utah-state", "utah-valley", "utah", "valparaiso", 
                 "vanderbilt", "vermont", "villanova", "virginia-commonwealth", "virginia-military-institute", "virginia-tech", 
                 "virginia", "wagner", "wake-forest", "washington-state", "washington", "weber-state", "west-virginia", 
                 "western-carolina", "western-illinois", "western-kentucky", "western-michigan", "wichita-state", "william-mary",
                 "winthrop", "wisconsin", "wofford", "wright-state", "wyoming", "xavier", "yale", "youngstown-state")
  
  link = rep(NA,351)
  for(i in 1:351){
    link[i] = paste("https://www.sports-reference.com/cbb/schools/",schools_15_18[i],"/2018-schedule.html", sep = "")
    sr_2018$`Last 12 Win %`[i] = last_12_function(link[i])
    link[i] = paste("https://www.sports-reference.com/cbb/schools/",schools_15_18[i],"/2017-schedule.html", sep = "")
    sr_2017$`Last 12 Win %`[i] = last_12_function(link[i])
    link[i] = paste("https://www.sports-reference.com/cbb/schools/",schools_15_18[i],"/2016-schedule.html", sep = "")
    sr_2016$`Last 12 Win %`[i] = last_12_function(link[i])
    link[i] = paste("https://www.sports-reference.com/cbb/schools/",schools_15_18[i],"/2015-schedule.html", sep = "")
    sr_2015$`Last 12 Win %`[i] = last_12_function(link[i])
  } 
  
  ## Adding # of wins in last 12 games variable (perhaps better for analysis)
  sr_2020$`Last 12 Wins`= rep(NA,353)
  sr_2019$`Last 12 Wins`= rep(NA,353)
  sr_2018$`Last 12 Wins`= rep(NA,351)
  sr_2017$`Last 12 Wins`= rep(NA,351)
  sr_2016$`Last 12 Wins`= rep(NA,351)
  sr_2015$`Last 12 Wins`= rep(NA,351)
  
  for(i in 1:length(sr_2020$`Last 12 Wins`)){
    sr_2020$`Last 12 Wins`[i] = round(sr_2020$`Last 12 Win %`[i]*12,0)
    sr_2019$`Last 12 Wins`[i] = round(sr_2019$`Last 12 Win %`[i]*12,0)
  }
  for(i in 1:length(sr_2018$`Last 12 Wins`)){
    sr_2018$`Last 12 Wins`[i] = round(sr_2018$`Last 12 Win %`[i]*12,0)
    sr_2017$`Last 12 Wins`[i] = round(sr_2017$`Last 12 Win %`[i]*12,0)
    sr_2016$`Last 12 Wins`[i] = round(sr_2016$`Last 12 Win %`[i]*12,0)
    sr_2015$`Last 12 Wins`[i] = round(sr_2015$`Last 12 Win %`[i]*12,0)
  }
  
  ## Adding variable for conference standings
  conferences = c("aac", "acc", "america-east", "atlantic-10", "atlantic-sun",
                  "big-12", "big-east", "big-sky", "big-south", "big-ten", "big-west",
                  "colonial", "cusa", "horizon", "ivy", "maac", "mac", "meac", "mvc",
                  "mwc", "northeast", "ovc", "pac-12", "patriot", "sec", "southern",
                  "southland", "summit", "sun-belt", "swac", "wac", "wcc")
  link_2015 = rep(NA,32)
  link_2016 = rep(NA,32)
  link_2017 = rep(NA,32)
  link_2018 = rep(NA,32)
  link_2019 = rep(NA,32)
  link_2020 = rep(NA,32)
  
  for(i in 1:32){
    link_2015[i] = paste("https://www.sports-reference.com/cbb/conferences/",conferences[i],"/2015.html#all_conference-tournament", sep = "")
    link_2016[i] = paste("https://www.sports-reference.com/cbb/conferences/",conferences[i],"/2016.html#all_conference-tournament", sep = "")
    link_2017[i] = paste("https://www.sports-reference.com/cbb/conferences/",conferences[i],"/2017.html#all_conference-tournament", sep = "")
    link_2018[i] = paste("https://www.sports-reference.com/cbb/conferences/",conferences[i],"/2018.html#all_conference-tournament", sep = "")
    link_2019[i] = paste("https://www.sports-reference.com/cbb/conferences/",conferences[i],"/2019.html#all_conference-tournament", sep = "")
    link_2020[i] = paste("https://www.sports-reference.com/cbb/conferences/",conferences[i],"/2020.html#all_conference-tournament", sep = "")
  }
  
  conference_standings_url_function = function(x){
    url = getURL(x)
    champ_url = readHTMLTable(url)
    champ = champ_url[["standings"]]
    champ = champ[,c(1,2)]
    colnames(champ) = c("Rk", "School")
    x = grep("Rk", champ$Rk, invert = TRUE)
    champ = champ[x,]
    champ$School = as.character(champ$School)
    champ
  }
  
      ## 2015 standings
      standings_2015 = NULL
      for(i in 1:32){
        champ = conference_standings_url_function(link_2015[i])
        standings_2015 = rbind(standings_2015,champ)
      }
      standings_2015[351,c(1:2)] = c(1, "NJIT")
      standings_2015 = standings_2015[order(standings_2015$School),]
      setdiff(sr_2015$School, standings_2015$School)
      standings_2015$School[282] = "SIU Edwardsville"
      standings_2015$School[330] = "VMI"
      standings_2015$Rk = as.character(standings_2015$Rk)

          ## Creating vector with correct order of team names
          combination<-as.matrix(expand.grid(standings_2015$School, sr_2015$School))
          combination1 = as.matrix(combination[combination[,1] == combination[,2]])
          combination1 = combination1[1:351,1]
          combination1 = as.data.frame(combination1)
          colnames(combination1)[1] = "School"
          combination1$School = as.character(combination1$School)
          finish = rep(NA,length(combination1$School))
        
          ## Adding correct rank to correct team name
          for(i in 1:length(combination1$School)){
              if(combination1$School[i] == standings_2015$School[i]){
                finish[i] = standings_2015$Rk[i]
              }
              else if(combination1$School[i] != standings_2015$School[i]){
                finish[i] = standings_2015$Rk[which(adist(combination1$School[i], standings_2015$School) == 0)]
              }
            }
          standings_2015 = cbind(combination1, finish)
          
          ## Check that order of team names matches sr_2015
          ed_dist = rep(NA,351)
          for(i in 1:length(sr_2015$School)){
            ed_dist[i] = adist(sr_2015$School[i], standings_2015$School[i])
          }
          which(ed_dist != 0)
      
      ## 2016 standings
      standings_2016 = NULL
      for(i in 1:32){
        champ = conference_standings_url_function(link_2016[i])
        standings_2016 = rbind(standings_2016,champ)
      }
      standings_2016 = standings_2016[order(standings_2016$School),]
      setdiff(sr_2016$School, standings_2016$School)
      standings_2016$School[282] = "SIU Edwardsville"
      standings_2016$School[330] = "VMI"
      standings_2016$Rk = as.character(standings_2016$Rk)
      
          ## Creating vector with correct order of team names
          combination<-as.matrix(expand.grid(standings_2016$School, sr_2016$School))
          combination1 = as.matrix(combination[combination[,1] == combination[,2]])
          combination1 = combination1[1:351,1]
          combination1 = as.data.frame(combination1)
          colnames(combination1)[1] = "School"
          combination1$School = as.character(combination1$School)
          finish = rep(NA,length(combination1$School))
          
          ## Adding correct rank to correct team name
          for(i in 1:length(combination1$School)){
            if(combination1$School[i] == standings_2016$School[i]){
              finish[i] = standings_2016$Rk[i]
            }
            else if(combination1$School[i] != standings_2016$School[i]){
              finish[i] = standings_2016$Rk[which(adist(combination1$School[i], standings_2016$School) == 0)]
            }
          }
          standings_2016 = cbind(combination1, finish)
          
          ## Check that order of team names matches sr_2016
          ed_dist = rep(NA,351)
          for(i in 1:length(sr_2016$School)){
            ed_dist[i] = adist(sr_2016$School[i], sr_2016$School[i])
          }
          which(ed_dist != 0)
      
      ## 2017 standings
      standings_2017 = NULL
      for(i in 1:32){
        champ = conference_standings_url_function(link_2017[i])
        standings_2017 = rbind(standings_2017,champ)
      }
      standings_2017 = standings_2017[order(standings_2017$School),]
      setdiff(sr_2017$School, standings_2017$School)
      standings_2017$School[282] = "SIU Edwardsville"
      standings_2017$School[330] = "VMI"
      standings_2017$Rk = as.character(standings_2017$Rk)
      
          ## Creating vector with correct order of team names
          combination<-as.matrix(expand.grid(standings_2017$School, sr_2017$School))
          combination1 = as.matrix(combination[combination[,1] == combination[,2]])
          combination1 = combination1[1:351,1]
          combination1 = as.data.frame(combination1)
          colnames(combination1)[1] = "School"
          combination1$School = as.character(combination1$School)
          finish = rep(NA,length(combination1$School))
          
          ## Adding correct rank to correct team name
          for(i in 1:length(combination1$School)){
            if(combination1$School[i] == standings_2017$School[i]){
              finish[i] = standings_2017$Rk[i]
            }
            else if(combination1$School[i] != standings_2017$School[i]){
              finish[i] = standings_2017$Rk[which(adist(combination1$School[i], standings_2017$School) == 0)]
            }
          }
          standings_2017 = cbind(combination1, finish)
          
          ## Check that order of team names matches sr_2017
          ed_dist = rep(NA,351)
          for(i in 1:length(sr_2017$School)){
            ed_dist[i] = adist(sr_2017$School[i], standings_2017$School[i])
          }
          which(ed_dist != 0)
      
      ## 2018 standings
      standings_2018 = NULL
      for(i in 1:32){
        champ = conference_standings_url_function(link_2018[i])
        standings_2018 = rbind(standings_2018,champ)
      }
      standings_2018 = standings_2018[order(standings_2018$School),]
      setdiff(sr_2018$School, standings_2018$School)
      standings_2018$School[282] = "SIU Edwardsville"
      standings_2018$School[330] = "VMI"
      standings_2018$Rk = as.character(standings_2018$Rk)
      
          ## Creating vector with correct order of team names
          combination<-as.matrix(expand.grid(standings_2018$School, sr_2018$School))
          combination1 = as.matrix(combination[combination[,1] == combination[,2]])
          combination1 = combination1[1:351,1]
          combination1 = as.data.frame(combination1)
          colnames(combination1)[1] = "School"
          combination1$School = as.character(combination1$School)
          finish = rep(NA,length(combination1$School))
          
          ## Adding correct rank to correct team name
          for(i in 1:length(combination1$School)){
            if(combination1$School[i] == standings_2018$School[i]){
              finish[i] = standings_2018$Rk[i]
            }
            else if(combination1$School[i] != standings_2018$School[i]){
              finish[i] = standings_2018$Rk[which(adist(combination1$School[i], standings_2018$School) == 0)]
            }
          }
          standings_2018 = cbind(combination1, finish)
          
          ## Checking that order of team names matches sr_2018
          ed_dist = rep(NA,351)
          for(i in 1:length(sr_2018$School)){
            ed_dist[i] = adist(sr_2018$School[i], standings_2018$School[i])
          }
          which(ed_dist != 0)
      
      ## 2019 standings
      standings_2019 = NULL
      for(i in 1:32){
        champ = conference_standings_url_function(link_2019[i])
        standings_2019 = rbind(standings_2019,champ)
      }
      standings_2019 = standings_2019[order(standings_2019$School),]
      setdiff(sr_2019$School, standings_2019$School)
      standings_2019$School[284] = "SIU Edwardsville"
      standings_2019$School[332] = "VMI"
      standings_2019$Rk = as.character(standings_2019$Rk)
      
          ## Creating vector with correct order of team names
          combination<-as.matrix(expand.grid(standings_2019$School, sr_2019$School))
          combination1 = as.matrix(combination[combination[,1] == combination[,2]])
          combination1 = combination1[1:353,1]
          combination1 = as.data.frame(combination1)
          colnames(combination1)[1] = "School"
          combination1$School = as.character(combination1$School)
          finish = rep(NA,length(combination1$School))
          
          ## Adding correct rank to correct team name
          for(i in 1:length(combination1$School)){
            if(combination1$School[i] == standings_2019$School[i]){
              finish[i] = standings_2019$Rk[i]
            }
            else if(combination1$School[i] != standings_2019$School[i]){
              finish[i] = standings_2019$Rk[which(adist(combination1$School[i], standings_2019$School) == 0)]
            }
          }
          standings_2019 = cbind(combination1, finish)
          
          ## Check that order of team names matches sr_2019
          ed_dist = rep(NA,353)
          for(i in 1:length(sr_2019$School)){
            ed_dist[i] = adist(sr_2019$School[i], standings_2019$School[i])
          }
          which(ed_dist != 0)
      
      standings_2020 = NULL
      for(i in 1:32){
        champ = conference_standings_url_function(link_2020[i])
        standings_2020 = rbind(standings_2020,champ)
      }
      standings_2020 = standings_2020[order(standings_2020$School),]
      setdiff(sr_2020$School, standings_2020$School)
      standings_2020$School[284] = "SIU Edwardsville"
      standings_2020$School[332] = "VMI"
      standings_2020$Rk = as.character(standings_2020$Rk)
      
      ## Creating vector with correct order of team names
      combination<-as.matrix(expand.grid(standings_2020$School, sr_2020$School))
      combination1 = as.matrix(combination[combination[,1] == combination[,2]])
      combination1 = combination1[1:353,1]
      combination1 = as.data.frame(combination1)
      colnames(combination1)[1] = "School"
      combination1$School = as.character(combination1$School)
      finish = rep(NA,length(combination1$School))
      
      ## Adding correct rank to correct team name
      for(i in 1:length(combination1$School)){
        if(combination1$School[i] == standings_2020$School[i]){
          finish[i] = standings_2020$Rk[i]
        }
        else if(combination1$School[i] != standings_2020$School[i]){
          finish[i] = standings_2020$Rk[which(adist(combination1$School[i], standings_2020$School) == 0)]
        }
      }
      standings_2020 = cbind(combination1, finish)
      
      ## Check that order of team names matches sos_2020
      ed_dist = rep(NA,353)
      for(i in 1:length(sr_2020$School)){
        ed_dist[i] = adist(sr_2020$School[i], standings_2020$School[i])
      }
      which(ed_dist != 0)
  
  sr_2015 = cbind(sr_2015,standings_2015$finish)
  colnames(sr_2015)[6] = "Conference Finish"
  sr_2016 = cbind(sr_2016,standings_2016$finish)
  colnames(sr_2016)[6] = "Conference Finish"
  sr_2017 = cbind(sr_2017,standings_2017$finish)
  colnames(sr_2017)[6] = "Conference Finish"
  sr_2018 = cbind(sr_2018,standings_2018$finish)
  colnames(sr_2018)[6] = "Conference Finish"
  sr_2019 = cbind(sr_2019,standings_2019$finish)
  colnames(sr_2019)[6] = "Conference Finish"
  sr_2020 = cbind(sr_2020,standings_2020$finish)
  colnames(sr_2020)[6] = "Conference Finish"
  
## Variables NOT yet added to either dataset
  
  ## Creating RPI 
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
  
  ## Creating NET Ranking (Only Since 2019)
  ## Unable to find numerical values, only ranking
  
  net_ranking = function(x){
    url = getURL(x)
    net_url = readHTMLTable(url)
    net_url = net_url[["NULL"]]
    net = net_url[, c(2,4)]
    net$Team = as.character(net$Team)
    net = net[order(net$Team),]
  }
  
  net_2020 = net_ranking('http://warrennolan.com/basketball/2020/net')
  net_2019 = net_ranking('http://warrennolan.com/basketball/2019/net')
  net_2018 = rep(NA,351)
  net_2017 = rep(NA,351)
  net_2016 = rep(NA,351)
  net_2015 = rep(NA,351)
  
  
  library(stringdist)
  d <- expand.grid(sos_2019$School,rpi_2019$Team) # Distance matrix in long form
  names(d) <- c("SOS School","RPI School")
  d$dist <- stringdist(d$`SOS School`,d$`RPI School`, method="jw") # String edit distance (use your favorite function here)
  
  # Greedy assignment heuristic (Your favorite heuristic here)
  greedyAssign <- function(a,b,d){
    x <- numeric(length(a)) # assgn variable: 0 for unassigned but assignable, 
    # 1 for already assigned, -1 for unassigned and unassignable
    while(any(x==0)){
      min_d <- min(d[x==0]) # identify closest pair, arbitrarily selecting 1st if multiple pairs
      a_sel <- a[d==min_d & x==0][1] 
      b_sel <- b[d==min_d & a == a_sel & x==0][1] 
      x[a==a_sel & b == b_sel] <- 1
      x[x==0 & (a==a_sel|b==b_sel)] <- -1
    }
    cbind(a=a[x==1],b=b[x==1],d=d[x==1])
  }
  
  dictionary = data.frame(greedyAssign(as.character(d$`SOS School`),as.character(d$`RPI School`),d$dist))
  dictionary$a = as.character(dictionary$a)
  dictionary$b = as.character(dictionary$b)
  dictionary[35,2] = "NKentucky"
  dictionary[36,2] = "NCAT"
  dictionary[79,2] = "ILChicago"
  dictionary[83,2] = "TNState"
  dictionary[85,2] = "WashState"
  dictionary[87,2] = "LAMonroe"
  dictionary[98,2] = "SUtah"
  dictionary[99,2] = "TXSouthern"
  dictionary[120,2] = "EWashingtn"
  dictionary[141,2] = "MissState"
  dictionary[143,2] = "LATech"
  dictionary[145,2] = "GASouthern"
  dictionary[147,2] = "GATech"
  dictionary[150,2] = "ETennSt"
  dictionary[156,2] = "LIUBrooklyn"
  dictionary[178,2] = "LSU"
  dictionary[187,2] = "NIowa"
  dictionary[190,2] = "NIllinois"
  dictionary[191,2] = "NCWilmgton"
  dictionary[192,2] = "NColorado"
  dictionary[193,2] = "NCAsheville"
  dictionary[194,2] = "NArizona"
  dictionary[195,2] = "SEMissouri"
  dictionary[196,2] = "BYU"
  dictionary[207,2] = "EIllinois"
  dictionary[208,2] = "NWState"
  dictionary[210,2] = "TNTech"
  dictionary[219,2] = "NCCentral"
  dictionary[262,2] = "VATech"
  dictionary[265,2] = "LALafayette"
  dictionary[266,2] = "UMKC"
  dictionary[270,2] = "StFranPA"
  dictionary[281,2] = "SELouisiana"
  dictionary[287,2] = "SCUpstate"
  dictionary[295,2] = "NCState"
  dictionary[296,2] = "NCGrnsboro"
  dictionary[297,2] = "TNMartin"
  dictionary[307,2] = "TXPanAm"
  dictionary[317,2] = "California"
  dictionary[325,2] = "WmMary"
  dictionary[327,2] = "VAMilitary"
  dictionary[335,2] = "LgBeachSt"
  dictionary[346,2] = "FDickinson"
  
  setdiff(rpi_2019$Team[i], dictionary$b[i])
  
  for(i in 1:length(rpi_2019$Team)){
    for(j in 1:length(dictionary$b)){
      if(rpi_2019$Team[i] == dictionary$b[j]){
        rpi_2019$Team[i] = dictionary$a[j]
      }
    }
  }
  
  sos_2019 = cbind(sos_2019, rpi_2019$Rating)
  colnames(sos_2019)[11] = "RPI"

