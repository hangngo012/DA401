########## DA401 project #########
########## 3/8/2021 ##########
library(dplyr)
library(tidyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(psych)
library(corrplot)
######################### Data exploration ############################
############ Data cleaning ############
# Load data

setwd("G:/My Drive/Spring 2022/DA 401/COVID Data")
covid_data <- read.csv("G:/My Drive/Spring 2022/DA 401/COVID Data/COVIDiSTRESS_May_30_cleaned_final.csv") # nolint

############ Data inclusion and exclusion #############

# summary dataset
summary(covid_data)
dplyr::glimpse(covid_data)

# variabes end with _txt and Final_open contains text have many NAs
covid_data <- rename(covid_data, "answer_duration" = "Duration..in.seconds.")
covid_data <-  subset(covid_data, select = -c(Final_open, Expl_coping_txt, time_spent_in_war_TXT, experience_war_TXT, 
                                            born_92, experience_war, war_injury, loss_during_war, time_spent_in_war))

#covid_data_no_na <- covid_data %>% filter(answered_all == "Yes")
#write.csv(covid_data_no_na, "G:/My Drive/Spring 2022/DA 401/COVID Data/COVIDiSTRESS_noNA.csv", row.names = FALSE) # export csv file 

# only 79 replies to born_92
# 52 replies to experience_war,
# 51 replies to war_injury (4 said yes)
# 50 replies to loss_during_war
# None replies to time_spent_during_war


### select variable for RQ1 ###


# Create dataset of all variables for RQ1
allCharacteristics <- covid_data %>% select("OECD_people_1", "OECD_people_2", 
                                            "OECD_insititutions_1", "OECD_insititutions_2", "OECD_insititutions_3", "OECD_insititutions_4", "OECD_insititutions_5", "OECD_insititutions_6", 
                                            "Trust_countrymeasure",
                                            "Compliance_1", "Compliance_2", "Compliance_3", "Compliance_4", "Compliance_5",
                                            "BFF_15_1", "BFF_15_2", "BFF_15_3", "BFF_15_4",
                                            "BFF_15_5", "BFF_15_6", "BFF_15_7", "BFF_15_8",
                                            "BFF_15_9", "BFF_15_10", "BFF_15_11", "BFF_15_12",
                                            "Corona_concerns_1", "Corona_concerns_2", "Corona_concerns_3", "Corona_concerns_4", "Corona_concerns_5",
                                            "Expl_Distress_1", "Expl_Distress_2", "Expl_Distress_3",
                                            "Expl_Distress_4", "Expl_Distress_5", "Expl_Distress_6",
                                            "Expl_Distress_7", "Expl_Distress_8", "Expl_Distress_9",
                                            "Expl_Distress_10", "Expl_Distress_11", "Expl_Distress_12",
                                            "Expl_Distress_13", "Expl_Distress_14", "Expl_Distress_15",
                                            "Expl_Distress_16", "Expl_Distress_17", "Expl_Distress_18",
                                            "Expl_Distress_19", "Expl_Distress_20", "Expl_Distress_21",
                                            "Expl_Distress_22", "Expl_Distress_23", "Expl_Distress_24",
                                            "PSS10_avg", "SLON3_avg", "SPS_avg", "answer_duration")

# delete rows with NAs values
allCharacteristics <- allCharacteristics[complete.cases(allCharacteristics),]

# transfer answer duration from seconds to minutes
allCharacteristics$answer_duration_mins <- (allCharacteristics$answer_duration)/60

# histogram of answer duration to see distribution 
ggplot(allCharacteristics, aes(answer_duration_mins)) + 
  geom_histogram(color = "black", fill = "white", binwidth = 1) +
  xlim(0,60) + 
  labs(title = "Histogram of answer duration in minutes", 
       subtitle = "For 79741 participants", 
       x = "Answer duration in mintues", 
       y = "Number of answers") +
  theme_bw()

# explore allCharacteristics
summary(allCharacteristics)
ncol(allCharacteristics) # have 73 columns
str(allCharacteristics) # view columns' data type

### Data Cleaning ###

# filter out responses who completed the survey under 1/10th of estimation answer duration 
allCharacteristics <- allCharacteristics %>% filter(answer_duration_mins >= 3)


# create dataframe having mean scores of all sub-survey
all_char_avg <- allCharacteristics %>% select("PSS10_avg", "SLON3_avg", "Trust_countrymeasure")
all_char_avg$trust_in_ppl <- rowMeans(subset(allCharacteristics, select = c("OECD_people_1", "OECD_people_2")), na.rm = TRUE)
all_char_avg$trust_in_gov <- rowMeans(subset(allCharacteristics, select = c("OECD_insititutions_1", "OECD_insititutions_2", "OECD_insititutions_3", "OECD_insititutions_4", "OECD_insititutions_5", "OECD_insititutions_6")), na.rm = TRUE)
all_char_avg$trust_in_gov <- rowMeans(subset(allCharacteristics, select = c("Compliance_1", "Compliance_2", "Compliance_3", "Compliance_4", "Compliance_5")), na.rm = TRUE)
all_char_avg$trust_in_gov <- rowMeans(subset(allCharacteristics, select = c("BFF_15_1", "BFF_15_2", "BFF_15_3", "BFF_15_4",
                                                                            "BFF_15_5", "BFF_15_6", "BFF_15_7", "BFF_15_8",
                                                                            "BFF_15_9", "BFF_15_10", "BFF_15_11", "BFF_15_12")), na.rm = TRUE)
all_char_avg$corona_concern <- rowMeans(subset(allCharacteristics, select = c("Corona_concerns_1", "Corona_concerns_2", "Corona_concerns_3", "Corona_concerns_4", "Corona_concerns_5")), na.rm = TRUE)


# create dataframe with no measure of stress and loneliness 
no_pss_slon <- allCharacteristics %>% select("Trust_countrymeasure")
no_pss_slon$trust_in_ppl <- rowMeans(subset(allCharacteristics, select = c("OECD_people_1", "OECD_people_2")), na.rm = TRUE)
no_pss_slon$trust_in_gov <- rowMeans(subset(allCharacteristics, select = c("OECD_insititutions_1", "OECD_insititutions_2", "OECD_insititutions_3", "OECD_insititutions_4", "OECD_insititutions_5", "OECD_insititutions_6")), na.rm = TRUE)
no_pss_slon$trust_in_gov <- rowMeans(subset(allCharacteristics, select = c("Compliance_1", "Compliance_2", "Compliance_3", "Compliance_4", "Compliance_5")), na.rm = TRUE)
no_pss_slon$trust_in_gov <- rowMeans(subset(allCharacteristics, select = c("BFF_15_1", "BFF_15_2", "BFF_15_3", "BFF_15_4",
                                                                            "BFF_15_5", "BFF_15_6", "BFF_15_7", "BFF_15_8",
                                                                            "BFF_15_9", "BFF_15_10", "BFF_15_11", "BFF_15_12")), na.rm = TRUE)
no_pss_slon$corona_concern <- rowMeans(subset(allCharacteristics, select = c("Corona_concerns_1", "Corona_concerns_2", "Corona_concerns_3", "Corona_concerns_4", "Corona_concerns_5")), na.rm = TRUE)




## detect multivariate outliers due to random or careless invalid response ##
## using Mahalanobis and chi-square test ##

# for dataframe of all mean scores
all_char_avg$mahalnobis <- mahalanobis(all_char_avg, colMeans(all_char_avg), cov(all_char_avg))
all_char_avg$p_chi_square <- pchisq(all_char_avg$mahalnobis, df = 3, lower.tail = FALSE)
all_char_avg <- all_char_avg %>% filter(p_chi_square < 0.001)
all_char_avg <- subset(all_char_avg, select = -c(p_chi_square, mahalnobis))


# for dataframe without perceived stress and loneliness
no_pss_slon$mahal <- mahalanobis(no_pss_slon, colMeans(no_pss_slon), cov(no_pss_slon))
no_pss_slon$p_chi_square <- pchisq(no_pss_slon$mahal, df = 3, lower.tail = FALSE) 
df1 <- no_pss_slon %>% filter(p_chi_square < 0.001)
df1 <- subset(df1, select = -c(p_chi_square, mahalnobis))

# Export into csv file for RQ1
#write.csv(df1, "G:/My Drive/Spring 2022/DA 401/COVID Data/all_characteristics_cleaned.csv", row.names = F)
#write.csv(all_char_avg, "G:/My Drive/Spring 2022/DA 401/COVID Data/cluster_all_characteristics_avg_score.csv", row.names = F)
#write.csv(df1, "G:/My Drive/Spring 2022/DA 401/COVID Data/cluster_noPSS_noSLON_avg_score.csv", row.names = F)

### EDA ###
# Check correlation between independent variables 
all_char_avg.cor = cor(all_char_avg, method = c("spearman"))
corrplot(all_char_avg.cor, method = "number")

#######################################################################

######################### Script for RQ1 ##############################
### PCA to reduce number of dimensions ###

### For dataframe have perceived stress and loneliness ###
pca_avg = prcomp(all_char_avg, center = TRUE, scale = TRUE)
pca_avg
summary(pca_avg)

# PC1, PC2, and PC3 explains for 60% of the variance of the data, which is sufficient to choose 

# Saving only 3 columns with the highest proportion of variance in all_char_avg 
all_char_transform = as.data.frame(-pca_avg$x[,1:3])

# extract results for variables
var <- get_pca_var(pca_avg)
names(var) #4 matricesthat contain all results of the active variables are coordinates, correlation between variables and axes, square cosine and contributions

# coordinates of variables
head(var$coord)

### For dataframe doesn't have perceived stress and loneliness ###
pca_none = prcomp(df1, center = TRUE, scale = TRUE)
summary(pca_none) # PC1, PC2, and PC3 cummulative explains for 65% of the variance of the data

# Saving 3 columns with the highest proportion of variance
df1_transform = as.data.frame(-pca_none$x[,1:3])


### K-Means Clustering ###
### Dataframe with stress and loneliness ###
# Determine number of clusters k using elbow method 
fviz_nbclust(all_char_transform, kmeans, method = 'wss') + 
  geom_vline(xintercept = 3, linetype = 2) +
  labs(subtitle = "Elbow method")# the bend of the elbow is at k=3

# Determine number of clusters k using silhouette method 
fviz_nbclust(all_char_transform, kmeans, method = 'silhouette') +
  labs(subtitle = "Silhoutte method") #k = 5 has best quality of clustering (how well each object lies within its cluster)

# Determine number of clusters k using gap statistic method 
set.seed(123)
fviz_nbclust(all_char_transform, kmeans, method = 'gap_stat', nboot = 50) + 
  labs(subtitle = 'Gap statistic method') #  recommend using 1 cluster? or 7 clusters
# this method compares the total within intra-cluster variation 
# for different values of k with their expected values under null reference distribution of the data
# optimal cluster will be the value maximize the gap statistic

# K-means clustering
k = 3 # choosing k = 2

kmeans_all_char = kmeans(all_char_transform, centers = k, nstart = 50)

fviz_cluster(kmeans_all_char, data = all_char_transform, geom = "point") + 
  labs(title = "Clusters of participants' behavioural characteristics at the beginning of COVID", 
       subtitle = "K-means clustering, k = 3")

### Dataframe without stress and loneliness ###
# Determine nnumber of clusters k using elbow method 
fviz_nbclust(df1_transform, kmeans, method = 'wss')

# Determine number of clusters k using silhouette method 
fviz_nbclust(df1_transform, kmeans, method = 'silhouette')

# Determine number of clusters k using gap statistic method 
set.seed(123)
fviz_nbclust(df1_transform, kmeans, method = 'gap_stat', nboot = 50)

# K-means clustering
k1 = 3 # choosing k = 4

kmeans_df1 = kmeans(df1, centers = k1, nstart = 50)

fviz_cluster(kmeans_df1, data = df1, geom = "point")

# Identify cluster characteristics
# Identify datapoints that are in overlap of 2 or more clusters
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