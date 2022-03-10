########## DA401 project #########
########## 3/8/2021 ##########
library(dplyr)
######################### Data exploration ############################
############ Data cleaning ############
# Load data

setwd("G:/My Drive/Spring 2022/DA 401/COVID Data")
covid_data <- read.csv("G:/My Drive/Spring 2022/DA 401/COVID Data/COVIDiSTRESS_May_30_cleaned_final.csv") # nolint

############ Data inclusion and exclusion #############

# summary dataset
summary(covid_data)
Hmisc::describe(covid_data)
# variabes end with _txt and Final_open contains text have many NAs
covid_data = subset(covid_data, select = -c(Final_open, Expl_coping_txt, time_spent_in_war_TXT, experience_war_TXT))
# only 79 replies to born_92
# 52 replies to experience_war,
# 51 replies to war_injury (4 said yes)
# 50 replies to loss_during_war
# None replies to time_spent_during_war

covid_data = subset(covid_data, select = -c(born_92, experience_war, war_injury, loss_during_war, time_spent_in_war))
# describe variables
Hmisc::describe(covid_data$born_92)



### DEA ###
# Number of participants by country
# Genders by country

#######################################################################

######################### Script for RQ1 ##############################
###


#######################################################################

######################### Script for RQ2 ##############################

### Join Covid table with GDP table ###

# have income group in Covid table

### Divide datasets ###

### Build training model ###

######################## Decision tree ###############################
### Test training model on validated set ###

### Test revised model on test set ###

### Validation test

######################## Random forest ###############################
### Test training model on validated set ###

### Test revised model on test set ###

### Validation test ###

######################## Generalized linear model ####################
### Test training model on validated set ###

### Test revised model on test set ###