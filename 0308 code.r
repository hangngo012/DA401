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
covid_data <-  subset(covid_data, select = -c(Final_open, Expl_coping_txt, time_spent_in_war_TXT, experience_war_TXT, 
                                            born_92, experience_war, war_injury, loss_during_war, time_spent_in_war))

# only 79 replies to born_92
# 52 replies to experience_war,
# 51 replies to war_injury (4 said yes)
# 50 replies to loss_during_war
# None replies to time_spent_during_war

# describe variables
Hmisc::describe(covid_data$born_92)
dplyr::glimpse(covid_data)

# select variable for RQ1 
perceived_stress <- c("Scale_PSS10_UCLA_1", "Scale_PSS10_UCLA_2", "Scale_PSS10_UCLA_3",
            "Scale_PSS10_UCLA_4", "Scale_PSS10_UCLA_5", "Scale_PSS10_UCLA_6",
            "Scale_PSS10_UCLA_7", "Scale_PSS10_UCLA_8", "Scale_PSS10_UCLA_9",
            "Scale_PSS10_UCLA_10")

perceived_loneliness <- c("Scale_SLON_1", "Scale_SLON_2", "Scale_SLON_3")

trust_in_people <- c("OECD_people_1", "OECD_people_2")

trust_in_gov <- c("OECD_insititutions_1", "OECD_insititutions_2", "OECD_insititutions_3", "OECD_insititutions_4", "OECD_insititutions_5", "OECD_insititutions_6")

corona_concern <- c("Corona_concerns_1", "Corona_concerns_2", "Corona_concerns_3", "Corona_concerns_4", "Corona_concerns_5")

trust_preventative_measure <- c("Trust_countrymeasure")

compliance <- c("Compliance_1", "Compliance_2", "Compliance_3", "Compliance_4", "Compliance_5")

big5 <- c("BFF_15_1", "BFF_15_2", "BFF_15_3", "BFF_15_4",
            "BFF_15_5", "BFF_15_6", "BFF_15_7", "BFF_15_8",
            "BFF_15_9", "BFF_15_10", "BFF_15_11", "BFF_15_12",
            "BFF_15_13", "BFF_15_14", "BFF_15_15")

stressors <- c("Expl_Distress_1", "Expl_Distress_2", "Expl_Distress_3",
                "Expl_Distress_4", "Expl_Distress_5", "Expl_Distress_6",
                "Expl_Distress_7", "Expl_Distress_8", "Expl_Distress_9",
                "Expl_Distress_10", "Expl_Distress_11", "Expl_Distress_12",
                "Expl_Distress_13", "Expl_Distress_14", "Expl_Distress_15",
                "Expl_Distress_16", "Expl_Distress_17", "Expl_Distress_18",
                "Expl_Distress_19", "Expl_Distress_20", "Expl_Distress_21",
                "Expl_Distress_22", "Expl_Distress_23", "Expl_Distress_24")

social_provisions <- c("SPS_1", "SPS_2", "SPS_3",
                        "SPS_4", "SPS_5", "SPS_6",
                        "SPS_7", "SPS_8", "SPS_9")

coping_discomfort <- c("Expl_Coping_1", "Expl_Coping_2", "Expl_Coping_3",
                        "Expl_Coping_4", "Expl_Coping_5", "Expl_Coping_6",
                        "Expl_Coping_7", "Expl_Coping_8", "Expl_Coping_9",
                        "Expl_Coping_10", "Expl_Coping_11", "Expl_Coping_12",
                        "Expl_Coping_13","Expl_Coping_14", "Expl_Coping_15")

# Dataset of all variables for RQ1
allCharacteristics <- covid_data %>% select(all_of(perceived_stress), all_of(perceived_loneliness), 
                                            all_of(trust_in_people), all_of(trust_in_gov), all_of(trust_preventative_measure),
                                            all_of(compliance), all_of(big5), all_of(stressors), all_of(social_provisions), all_of(coping_discomfort),
                                            "PSS10_avg", "SLON3_avg", "SPS_avg", "Duration..in.seconds.")

allCharacteristics <- rename(allCharacteristics, "answer_duration" = "Duration..in.seconds.")
allCharacteristics$answer_duration_mins <- (allCharacteristics$answer_duration)/60

allCharacteristics$count_NA <- rowSums(is.na(allCharacteristics)) # add column: count NA/row

# explore allCharacteristics
summary(allCharacteristics)
ncol(allCharacteristics) # have 94 columns
str(allCharacteristics) # view columns' data types
colSums(is.na(allCharacteristics)) # count NAs each column

# delete rows with NAs values

### EDA ###
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