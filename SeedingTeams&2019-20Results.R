library(dplyr)
library(plyr)

## Seeding Function
determine_seeds_function = function(season, ms_year, ncc_year){
selections = rbind(ccdata %>% filter(Season == season), join(ms_year %>% arrange(desc(Prob)) %>% slice(1:36) %>% select(School),ncc_year, by = "School"))

p = predict(model_cvnet, newx = as.matrix(selections[,c(4:6,10,12,14,16,19:22,25,28,29)]), s = "lambda.1se", type = "response")
seeds = as.data.frame(cbind(selections$School,p))
colnames(seeds) = c("School", "Probability")
seeds$`Tournament Seed` = rep(NA,length(seeds$School))
seeds$`Actual Seed` = rep(NA, length(seeds$School))

seeds = seeds %>% arrange(desc(Probability))
seeds$`Actual Seed` = 1:68
seeds$`Tournament Seed`[1:4] = 1
seeds$`Tournament Seed`[5:8] = 2
seeds$`Tournament Seed`[9:12] = 3
seeds$`Tournament Seed`[13:16] = 4
seeds$`Tournament Seed`[17:20] = 5
seeds$`Tournament Seed`[21:24] = 6
seeds$`Tournament Seed`[25:28] = 7
seeds$`Tournament Seed`[29:32] = 8
seeds$`Tournament Seed`[33:36] = 9
seeds$`Tournament Seed`[37:40] = 10
seeds$`Tournament Seed`[41:46] = 11
seeds$`Tournament Seed`[47:50] = 12
seeds$`Tournament Seed`[51:54] = 13
seeds$`Tournament Seed`[55:58] = 14
seeds$`Tournament Seed`[59:62] = 15
seeds$`Tournament Seed`[63:68] = 16

seeds
}

## Examining Seeding Each Year

selections_2015 = determine_seeds_function(2015, ms_2015, ncc_2015)
selections_2016 = determine_seeds_function(2016, ms_2016, ncc_2016)
selections_2017 = determine_seeds_function(2017, ms_2017, ncc_2017)
selections_2018 = determine_seeds_function(2018, ms_2018, ncc_2018)
selections_2019 = determine_seeds_function(2019, ms_2019, ncc_2019)

## Scraping Tournament Seeds from Wikipedia
   ## Function to scrape Wikipedia data frames
    get_seed_results_function = function(site){
    url = GET(site)
    html = read_html(url) 
    tables = html_nodes(html, 'table')
    tables
    
    tables[grep("Region",tables,ignore.case = T)]
    df1 = html_table(tables[grep("Region",tables,ignore.case = T)],fill = T)[[1]]
    df2 = html_table(tables[grep("Region",tables,ignore.case = T)],fill = T)[[2]]
    df3 = html_table(tables[grep("Region",tables,ignore.case = T)],fill = T)[[3]]
    df4 = html_table(tables[grep("Region",tables,ignore.case = T)],fill = T)[[4]]
    df = rbind(df1, df2, df3, df4)
    df$Seed = gsub("\\D", "", df$Seed)
    df$Seed = as.numeric(df$Seed)
    df
    }

    ## Function doesn't work on 2019, modified slightly
    get_seed_results_function_19 = function(site){
      url = GET(site)
      html = read_html(url) 
      tables = html_nodes(html, 'table')
      tables
      
      tables[grep("Region",tables,ignore.case = T)]
      df1 = html_table(tables[grep("Region",tables,ignore.case = T)],fill = T)[[2]]
      df2 = html_table(tables[grep("Region",tables,ignore.case = T)],fill = T)[[3]]
      df3 = html_table(tables[grep("Region",tables,ignore.case = T)],fill = T)[[4]]
      df4 = html_table(tables[grep("Region",tables,ignore.case = T)],fill = T)[[5]]
      df = rbind(df1, df2, df3, df4)
      df$Seed = gsub("\\D","",df$Seed)
      df$Seed = as.numeric(df$Seed)
      df
    }
    
    results_2015 = get_seed_results_function("https://en.wikipedia.org/wiki/2015_NCAA_Division_I_Men%27s_Basketball_Tournament")
    results_2016 = get_seed_results_function("https://en.wikipedia.org/wiki/2016_NCAA_Division_I_Men%27s_Basketball_Tournament")
    results_2017 = get_seed_results_function("https://en.wikipedia.org/wiki/2017_NCAA_Division_I_Men%27s_Basketball_Tournament")
    results_2018 = get_seed_results_function("https://en.wikipedia.org/wiki/2018_NCAA_Division_I_Men%27s_Basketball_Tournament")
    results_2019 = get_seed_results_function_19("https://en.wikipedia.org/wiki/2019_NCAA_Division_I_Men%27s_Basketball_Tournament")

## Modifying team names in results data frames
    ## 2015
    setdiff(selections_2015$School, results_2015$School)
    results_2015$School[29] = "Brigham Young"
    results_2015$School[57] = "Southern Methodist"
    results_2015$School[24] = "Virginia Commonwealth"
    results_2015$School[60] = "St. John's (NY)"
    results_2015$School[43] = "Louisiana State"
    results_2015$School[48] = "UC-Irvine"
    results_2015$School[49] = "Albany (NY)"
    results_2015$School[65] = "Alabama-Birmingham"
    
    selections_2015 = selections_2015[order(selections_2015$`Tournament Seed`),]
    results_2015 = results_2015[order(results_2015$Seed),]

    ## 2016
    setdiff(selections_2016$School, results_2016$School)
    results_2016$School[3] = "Miami (FL)"
    results_2016$School[4] = "University of California"
    results_2016$School[27] = "Virginia Commonwealth"
    results_2016$School[42] = "Southern California"
    results_2016$School[9] = "Connecticut"
    results_2016$School[30] = "North Carolina-Wilmington"
    results_2016$School[16] = "North Carolina-Asheville"
    
    selections_2016 = selections_2016[order(selections_2016$`Tournament Seed`),]
    results_2016 = results_2016[order(results_2016$Seed),]

    ## 2017
    setdiff(selections_2017$School, results_2017$School)
    
    results_2017$School[6] = "Southern Methodist"
    results_2017$School[25] = "Saint Mary's (CA)"
    results_2017$School[28] = "Virginia Commonwealth"
    results_2017$School[13] = "North Carolina-Wilmington"
    results_2017$School[51] = "UC-Davis"
    
    selections_2017 = selections_2017[order(selections_2017$`Tournament Seed`),]
    results_2017 = results_2017[order(results_2017$Seed),]

    ## 2018
    setdiff(selections_2018$School, results_2018$School)
    
    results_2018$School[57] = "Texas Christian"
    results_2018$School[60] = "North Carolina State"
    results_2018$School[11] = "Loyola (IL)"
    results_2018$School[29] = "North Carolina-Greensboro"
    results_2018$School[68] = "Pennsylvania"
    results_2018$School[16] = "Maryland-Baltimore County"
    results_2018$School[50] = "Long Island University"
    
    selections_2018 = selections_2018[order(selections_2018$`Tournament Seed`),]
    results_2018 = results_2018[order(results_2018$Seed),]

    ## 2019
    setdiff(selections_2019$School, results_2019$School)
    
    results_2019$School[3] = "Louisiana State"
    results_2019$School[9] = "Central Florida"
    results_2019$School[8] = "Virginia Commonwealth"
    results_2019$School[47] = "Saint Mary's (CA)"
    results_2019$School[44] = "Mississippi"
    results_2019$School[49] = "UC-Irvine"
    results_2019$School[52] = "Gardner-Webb"
    results_2019$School[36] = "Prairie View"
    
    selections_2019 = selections_2019[order(selections_2019$`Tournament Seed`),]
    results_2019 = results_2019[order(results_2019$Seed),]

## Function to produce number of correct seeds predicted by model
true_seed_count_function = function(selections_year,results_year){
  for(i in 1:16){
    if(selections_year$`Tournament Seed`[4*i] == 1){ 
      x = selections_year %>% filter(`Tournament Seed` == 1)
      y = results_year %>% filter(`Seed` == 1)
      count = sum(x$School %in% y$School == TRUE)
    } else if (selections_year$`Tournament Seed`[4*i] == 2){
      x = selections_year %>% filter(`Tournament Seed` == 2)
      y = results_year %>% filter(`Seed` == 2)
      count = count + sum(x$School %in% y$School == TRUE)
    } else if (selections_year$`Tournament Seed`[4*i] == 3){
      x = selections_year %>% filter(`Tournament Seed` == 3)
      y = results_year %>% filter(`Seed` == 3)
      count = count + sum(x$School %in% y$School == TRUE)
    } else if (selections_year$`Tournament Seed`[4*i] == 4){
      x = selections_year %>% filter(`Tournament Seed` == 4)
      y = results_year %>% filter(`Seed` == 4)
      count = count + sum(x$School %in% y$School == TRUE)
    } else if (selections_year$`Tournament Seed`[4*i] == 5){
      x = selections_year %>% filter(`Tournament Seed` == 5)
      y = results_year %>% filter(`Seed` == 5)
      count = count + sum(x$School %in% y$School == TRUE)
    } else if (selections_year$`Tournament Seed`[4*i] == 6){
      x = selections_year %>% filter(`Tournament Seed` == 6)
      y = results_year %>% filter(`Seed` == 6)
      count = count + sum(x$School %in% y$School == TRUE)
    }else if (selections_year$`Tournament Seed`[4*i] == 7){
      x = selections_year %>% filter(`Tournament Seed` == 7)
      y = results_year %>% filter(`Seed` == 7)
      count = count + sum(x$School %in% y$School == TRUE)
    } else if (selections_year$`Tournament Seed`[4*i] == 8){
      x = selections_year %>% filter(`Tournament Seed` == 8)
      y = results_year %>% filter(`Seed` == 8)
      count = count + sum(x$School %in% y$School == TRUE)
    } else if (selections_year$`Tournament Seed`[4*i] == 9){
      x = selections_year %>% filter(`Tournament Seed` == 9)
      y = results_year %>% filter(`Seed` == 9)
      count = count + sum(x$School %in% y$School == TRUE)
    } else if (selections_year$`Tournament Seed`[4*i] == 10){
      x = selections_year %>% filter(`Tournament Seed` == 10)
      y = results_year %>% filter(`Seed` == 10)
      count = count + sum(x$School %in% y$School == TRUE)
    } else if (selections_year$`Tournament Seed`[4*i] == 11){
      x = selections_year %>% filter(`Tournament Seed` == 11)
      y = results_year %>% filter(`Seed` == 11)
      count = count + sum(x$School %in% y$School == TRUE)
    } else if (selections_year$`Tournament Seed`[4*i] == 12){
      x = selections_year %>% filter(`Tournament Seed` == 12)
      y = results_year %>% filter(`Seed` == 12)
      count = count + sum(x$School %in% y$School == TRUE)
    } else if (selections_year$`Tournament Seed`[4*i] == 13){
      x = selections_year %>% filter(`Tournament Seed` == 13)
      y = results_year %>% filter(`Seed` == 13)
      count = count + sum(x$School %in% y$School == TRUE)
    } else if (selections_year$`Tournament Seed`[4*i] == 14){
      x = selections_year %>% filter(`Tournament Seed` == 14)
      y = results_year %>% filter(`Seed` == 14)
      count = count + sum(x$School %in% y$School == TRUE)
    } else if (selections_year$`Tournament Seed`[4*i] == 15){
      x = selections_year %>% filter(`Tournament Seed` == 15)
      y = results_year %>% filter(`Seed` == 15)
      count = count + sum(x$School %in% y$School == TRUE)
    } else if (selections_year$`Tournament Seed`[4*i] == 16){
      x = selections_year %>% filter(`Tournament Seed` == 16)
      y = results_year %>% filter(`Seed` == 16)
      count = count + sum(x$School %in% y$School == TRUE)
    }
  } 
count
}

    ## Application of function to each year
    true_seed_2015 = true_seed_count_function(selections_2015, results_2015) 
    true_seed_2016 = true_seed_count_function(selections_2016, results_2016)
    true_seed_2017 = true_seed_count_function(selections_2017, results_2017)
    true_seed_2018 = true_seed_count_function(selections_2018, results_2018)
    true_seed_2019 = true_seed_count_function(selections_2019, results_2019)
    

## Function to produce number of seeds within one of correct seed 
within_one_seed_count_function = function(selections_year,results_year){
  for(i in 1:16){
    if(selections_year$`Tournament Seed`[4*i] == 1){ 
      x = selections_year %>% filter(`Tournament Seed` == 1)
      y = results_year %>% filter(`Seed` == 1 | `Seed` == 2)
      count = sum(x$School %in% y$School == TRUE)
    } else if (selections_year$`Tournament Seed`[4*i] == 2){
      x = selections_year %>% filter(`Tournament Seed` == 2)
      y = results_year %>% filter(`Seed` == 2 | `Seed` == 1 | `Seed` == 3)
      count = count + sum(x$School %in% y$School == TRUE)
    } else if (selections_year$`Tournament Seed`[4*i] == 3){
      x = selections_year %>% filter(`Tournament Seed` == 3)
      y = results_year %>% filter(`Seed` == 3 | `Seed` == 2 | `Seed` == 4)
      count = count + sum(x$School %in% y$School == TRUE)
    } else if (selections_year$`Tournament Seed`[4*i] == 4){
      x = selections_year %>% filter(`Tournament Seed` == 4)
      y = results_year %>% filter(`Seed` == 4 | `Seed` == 3 | `Seed` == 5)
      count = count + sum(x$School %in% y$School == TRUE)
    } else if (selections_year$`Tournament Seed`[4*i] == 5){
      x = selections_year %>% filter(`Tournament Seed` == 5)
      y = results_year %>% filter(`Seed` == 5 | `Seed` == 4 | `Seed` == 6)
      count = count + sum(x$School %in% y$School == TRUE)
    } else if (selections_year$`Tournament Seed`[4*i] == 6){
      x = selections_year %>% filter(`Tournament Seed` == 6)
      y = results_year %>% filter(`Seed` == 6 | `Seed` == 5 | `Seed` == 7)
      count = count + sum(x$School %in% y$School == TRUE)
    }else if (selections_year$`Tournament Seed`[4*i] == 7){
      x = selections_year %>% filter(`Tournament Seed` == 7)
      y = results_year %>% filter(`Seed` == 7 | `Seed` == 6 | `Seed` == 8)
      count = count + sum(x$School %in% y$School == TRUE)
    } else if (selections_year$`Tournament Seed`[4*i] == 8){
      x = selections_year %>% filter(`Tournament Seed` == 8)
      y = results_year %>% filter(`Seed` == 8 | `Seed` == 7 | `Seed` == 9)
      count = count + sum(x$School %in% y$School == TRUE)
    } else if (selections_year$`Tournament Seed`[4*i] == 9){
      x = selections_year %>% filter(`Tournament Seed` == 9)
      y = results_year %>% filter(`Seed` == 9 | `Seed` == 8 | `Seed` == 10)
      count = count + sum(x$School %in% y$School == TRUE)
    } else if (selections_year$`Tournament Seed`[4*i] == 10){
      x = selections_year %>% filter(`Tournament Seed` == 10)
      y = results_year %>% filter(`Seed` == 10 | `Seed` == 9 | `Seed` == 11)
      count = count + sum(x$School %in% y$School == TRUE)
    } else if (selections_year$`Tournament Seed`[4*i] == 11){
      x = selections_year %>% filter(`Tournament Seed` == 11)
      y = results_year %>% filter(`Seed` == 11 | `Seed` == 10 | `Seed` == 12)
      count = count + sum(x$School %in% y$School == TRUE)
    } else if (selections_year$`Tournament Seed`[4*i] == 12){
      x = selections_year %>% filter(`Tournament Seed` == 12)
      y = results_year %>% filter(`Seed` == 12 | `Seed` == 11 | `Seed` == 13)
      count = count + sum(x$School %in% y$School == TRUE)
    } else if (selections_year$`Tournament Seed`[4*i] == 13){
      x = selections_year %>% filter(`Tournament Seed` == 13)
      y = results_year %>% filter(`Seed` == 13 | `Seed` == 12 | `Seed` == 14)
      count = count + sum(x$School %in% y$School == TRUE)
    } else if (selections_year$`Tournament Seed`[4*i] == 14){
      x = selections_year %>% filter(`Tournament Seed` == 14)
      y = results_year %>% filter(`Seed` == 14 | `Seed` == 13 | `Seed` == 15)
      count = count + sum(x$School %in% y$School == TRUE)
    } else if (selections_year$`Tournament Seed`[4*i] == 15){
      x = selections_year %>% filter(`Tournament Seed` == 15)
      y = results_year %>% filter(`Seed` == 15 | `Seed` == 14 | `Seed` == 16)
      count = count + sum(x$School %in% y$School == TRUE)
    } else if (selections_year$`Tournament Seed`[4*i] == 16){
      x = selections_year %>% filter(`Tournament Seed` == 16)
      y = results_year %>% filter(`Seed` == 16 | `Seed` == 15)
      count = count + sum(x$School %in% y$School == TRUE)
    }
  } 
  count
}

    ## Application of function to each season
    within_one_2015 = within_one_seed_count_function(selections_2015, results_2015)  
    within_one_2016 = within_one_seed_count_function(selections_2016, results_2016)
    within_one_2017 = within_one_seed_count_function(selections_2017, results_2017)
    within_one_2018 = within_one_seed_count_function(selections_2018, results_2018)
    within_one_2019 = within_one_seed_count_function(selections_2019, results_2019)
    
    
## Preparing 2019-20 data to run through model
    ## Adding Indicator for if team is in Power 5 Conference
    full_2020$Power5 = rep(NA, length(full_2020$School))
    for(i in 1:length(full_2020$School)){
      if(full_2020$Conference[i] == "SEC" || full_2020$Conference[i] == "P12" ||
         full_2020$Conference[i] == "B12" || full_2020$Conference[i] == "B10" || 
         full_2020$Conference[i] == "ACC"){
        full_2020$Power5[i] = 1
      }
      else{
        full_2020$Power5[i] = 0
      }
    }
    
    ## Adding Win Percentage Variable 
    full_2020$`Win Percentage` = rep(NA, length(full_2020$School))
    for(i in 1:length(full_2020$School)){
      full_2020$`Win Percentage`[i] = round(full_2020$Wins[i]/sum(full_2020$Wins[i],full_2020$Losses[i]),3)
    }

    ## Adding 1's to teams who won their conference tournament or were otherwise deemed conference champs
    which(full_2020$School == "Florida State")
    full_2020$`Conference Champ`[93] = 1
    which(full_2020$School == "Liberty")
    full_2020$`Conference Champ`[144] = 1
    which(full_2020$School == "Winthrop")
    full_2020$`Conference Champ`[346] = 1
    which(full_2020$School == "Hofstra")
    full_2020$`Conference Champ`[114] = 1
    which(full_2020$School == "Northern Kentucky")
    full_2020$`Conference Champ`[220] = 1
    which(full_2020$School == "Yale")
    full_2020$`Conference Champ`[352] = 1
    which(full_2020$School == "Bradley")
    full_2020$`Conference Champ`[29] = 1
    which(full_2020$School == "Utah State")
    full_2020$`Conference Champ`[324] = 1
    which(full_2020$School == "Robert Morris")
    full_2020$`Conference Champ`[253] = 1
    which(full_2020$School == "Belmont")
    full_2020$`Conference Champ`[22] = 1
    which(full_2020$School == "Boston University")
    full_2020$`Conference Champ`[27] = 1
    which(full_2020$School == "East Tennessee State")
    full_2020$`Conference Champ`[79] = 1    
    which(full_2020$School == "North Dakota State")
    full_2020$`Conference Champ`[212] = 1
    which(full_2020$School == "Gonzaga")
    full_2020$`Conference Champ`[105] = 1
 
    ## Eliminating Conference Champs to run through predict function
    test_2020 = full_2020 %>% filter(is.na(`Conference Champ`))
    p_2020 = predict(model_cvnet, newx = as.matrix(test_2020[,c(4:6,10,12,14,16,19:22,25,28,29)]), s = "lambda.1se", type = "response")
    predictions_2020 = as.data.frame(cbind(test_2020$School, test_2020$Conference, p_2020))
    colnames(predictions_2020) = c("School", "Conference", "Prob")
    
    
    conferences_used = c("ACC", "ASun", "BSth", "CAA", "Horz", "Ivy", "MVC", 
                         "MWC", "NEC", "OVC", "Pat", "SC", "Sum", "WCC")
    
    
    conferences_not_used_2020 = predictions_2020 %>% filter(!(Conference %in% conferences_used))
    
    conferences_not_used_2020 = conferences_not_used_2020 %>% group_by(Conference) %>% arrange(Conference, desc(Prob))
    auto_bids_2020 = by(conferences_not_used_2020, conferences_not_used_2020["Conference"], head, 1)
    auto_bids_2020 = Reduce(rbind,auto_bids_2020)
    
    no_conf_champs_2020 = predictions_2020 %>% filter(!(School %in% auto_bids_2020$School))
    no_conf_champs_2020 = no_conf_champs_2020 %>% arrange(desc(Prob))
    at_large_bids_2020 = no_conf_champs_2020[1:36,1:3]
    
    selections_2020 = full_2020 %>% filter(`Conference Champ` == 1) 
    selections_2020 = rbind(selections_2020, full_2020 %>% filter(School %in% auto_bids_2020$School), full_2020 %>% filter(School %in% at_large_bids_2020$School))
    
    probs_2020 = predict(model_cvnet, newx = as.matrix(selections_2020[,c(4:6,10,12,14,16,19:22,25,28,29)]), s = "lambda.1se", type = "response")
    seeds = as.data.frame(cbind(selections_2020$School,probs_2020))
    colnames(seeds) = c("School", "Probability")
    seeds$`Tournament Seed` = rep(NA,length(seeds$School))
    seeds$`Actual Seed` = rep(NA, length(seeds$School))
    
    seeds = seeds %>% arrange(desc(Probability))
    seeds$`Actual Seed` = 1:68
    seeds$`Tournament Seed`[1:4] = 1
    seeds$`Tournament Seed`[5:8] = 2
    seeds$`Tournament Seed`[9:12] = 3
    seeds$`Tournament Seed`[13:16] = 4
    seeds$`Tournament Seed`[17:20] = 5
    seeds$`Tournament Seed`[21:24] = 6
    seeds$`Tournament Seed`[25:28] = 7
    seeds$`Tournament Seed`[29:32] = 8
    seeds$`Tournament Seed`[33:36] = 9
    seeds$`Tournament Seed`[37:40] = 10
    seeds$`Tournament Seed`[41:46] = 11
    seeds$`Tournament Seed`[47:50] = 12
    seeds$`Tournament Seed`[51:54] = 13
    seeds$`Tournament Seed`[55:58] = 14
    seeds$`Tournament Seed`[59:62] = 15
    seeds$`Tournament Seed`[63:68] = 16
    
    selections_2020 = seeds

        ## Plotting Count of Correct Seeds Assigned by Season
    seed_results = matrix(NA,nrow = 10, ncol = 3)
    seed_results[1:10,1] = c(2015,2015,2016,2016,2017,2017,2018,2018,2019,2019)
    seed_results[1:10,2] = c(true_seed_2015, within_one_2015, true_seed_2016,
                             within_one_2016, true_seed_2017, within_one_2017,
                             true_seed_2018, within_one_2018, true_seed_2019, 
                             within_one_2019)
    seed_results[1:10,3] = rep(c("True Seed","Within One Seed"),5)
    seed_results = as.data.frame(seed_results)
    colnames(seed_results) = c("Season", "Total", "Seed Result")
    seed_results$Total = as.numeric(seed_results$Total)
    ggplot(seed_results, aes(x = Season, y = Total, fill = `Seed Result`)) + geom_bar(stat = "identity",position = position_dodge()) + 
      scale_fill_manual(values = c("dark orange", "black")) + geom_text(aes(label=Total), vjust=1.6, color="white", position = position_dodge(0.9), size=7) + 
      scale_y_continuous(breaks = seq(0,68,by = 4), limits = c(0,68)) + ggtitle("Count of Correct Seeds Assigned by Season") + 
      theme(plot.title = element_text(size = 21,hjust = 0.5), axis.text.x = element_text(size = 15), axis.text.y = element_text(size = 15),axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18, margin = margin(r = 7.5)),legend.text=element_text(size=15),legend.title=element_text(size=18))
    
    ## 2016 Seeds Example
    
    compare_2016 = cbind(selections_2016, results_2016)
    compare_2016 = compare_2016[,c(1:6)]
    for(i in 1:length(compare_2016$School)){
      for(j in 1:68){
      if(compare_2016$School[i] == compare_2016$School.1[j]){
        compare_2016$`Actual Seed`[i] = compare_2016$Seed[j]
      }
      }
    }
    compare_2016 = compare_2016[,c(1:4)]
    colnames(compare_2016)[3] = "Predicted Seed"
    compare_2016$Probability = as.numeric(compare_2016$Probability)
    compare_2016$Probability = round(compare_2016$Probability,3)
    