# Modeling Selection Sunday - An Investigation into the March Madness Bracket
**Jonathan Olds - Summmer 2020**

*This project serves as an attempt to accurately replicate the selection and seeding procedures of the NCAA Selection Committee for the Men's College Basketball Championship Tournament using a LASSO-penalized logistic regression model. This work was conducted from June to August of 2020 under the supervision of Associate Professor Bailey Fosdick at Colorado State University.*



# Correct Script Order * 
1. DataScraping.R - Functions that scrape data from the various websites used in data collection - barttorvik.com, sportsreference.com, teamrankings.com and warrennolan.com and compile the data into datasets for each season 2015-2019
2. DataExploration.R - Various graphs exploring the relationships between potential covariates and the indicator designating which teams made the tournament
3. LogisticRegression.R - Complete modeling procedures, beginning with ordinary logistic regression model fitting using glm() package, progressing to LASSO-penalized model, using glmnet() package. Includes functions to check for selection accuracy, identify correctly and incorrectly selected teams and a graphical depiction of coefficient values for each covariate by season. 
4. SeedingTeams&2019-20Results.R - Function to seed selected teams, process of applying model selection and seeding to 2019-2020 season data. 

NOTE: DecisionTree.R and MiscellaneousCodeFragments.R include other approaches of analysis that are not relevant for replicating final results
