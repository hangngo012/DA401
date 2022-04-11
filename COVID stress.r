########## DA401 project #########
########## 3/8/2021 ##########
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(psych)
library(corrplot)
library(fpc)
library(GGally)
library(regclass)
library(ggpubr)
library(mctest)
library(car)
library(olsrr)
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
covid_data <-  subset(covid_data, select = -c(Final_open, Expl_coping_txt, time_spent_in_war_TXT, experience_war_TXT, 
                                              born_92, experience_war, war_injury, loss_during_war, time_spent_in_war))

covid_data <- rename(covid_data, "answer_duration" = "Duration..in.seconds.")


#write.csv(covid_data_no_na, "G:/My Drive/Spring 2022/DA 401/COVID Data/COVIDiSTRESS_noNA.csv", row.names = FALSE) # export csv file 

# only 79 replies to born_92
# 52 replies to experience_war,
# 51 replies to war_injury (4 said yes)
# 50 replies to loss_during_war
# None replies to time_spent_during_war


### select variable for RQ1 ###


# Create dataset of all variables for RQ1
behaviours <- covid_data %>% select("Trust_countrymeasure",
                                    "Compliance_1", "Compliance_2", "Compliance_3", "Compliance_4", "Compliance_5",
                                    "Corona_concerns_1", "Corona_concerns_2", "Corona_concerns_3", "Corona_concerns_4", "Corona_concerns_5",
                                    "PSS10_avg", "SLON3_avg", "answer_duration")

# delete rows with NAs values
behaviours <- behaviours[complete.cases(behaviours),]

behaviours$answer_duration_mins <- (behaviours$answer_duration)/60

behaviours <- behaviours %>% filter(answer_duration_mins >= 3)

## detect multivariate outliers due to random or careless invalid response ##
## using Mahalanobis and chi-square test ##

behaviours$maha <- mahalanobis(behaviours, colMeans(behaviours), cov(behaviours), tol = 1e-40)
behaviours$p_chi_square <- pchisq(behaviours$maha, df = 3, lower.tail = FALSE)
behavior_removed <- behaviours %>% filter(p_chi_square < 0.001)
behavior_removed <- subset(behavior_removed, select = -c(p_chi_square, maha))

# histogram of answer duration to see distribution 
ggplot(behaviours, aes(answer_duration_mins)) + 
  geom_histogram(color = "black", fill = "white", binwidth = 1) +
  xlim(0,60) + 
  labs(title = "Histogram of answer duration in minutes", 
       subtitle = "For 79741 participants", 
       x = "Answer duration in mintues", 
       y = "Number of answers") +
  theme_bw()

# explore allCharacteristics
summary(behaviours)
str(behaviours) # view columns' data type

### Data Cleaning ###

# create dataframe having mean scores of all sub-survey
behaviours_score <-  behavior_removed %>% select("PSS10_avg", "SLON3_avg", "Trust_countrymeasure")
behaviours_score$compliance <- rowMeans(subset(behavior_removed, select = c("Compliance_1", "Compliance_2", "Compliance_3", "Compliance_4", "Compliance_5")), na.rm = TRUE)
behaviours_score$corona_concern <- rowMeans(subset(behavior_removed, select = c("Corona_concerns_1", "Corona_concerns_2", "Corona_concerns_3", "Corona_concerns_4", "Corona_concerns_5")), na.rm = TRUE)

boxplot(behaviours_score)

# function remove outliers
detect_outlier <- function(x) {
  
  # calculate first quantile
  Quantile1 <- quantile(x, probs=.25)
  
  # calculate third quantile
  Quantile3 <- quantile(x, probs=.75)
  
  # calculate inter quartile range
  IQR = Quantile3-Quantile1
  
  # return true or false
  x > Quantile3 + (IQR*1.5) | x < Quantile1 - (IQR*1.5)
}

# create remove outlier function
remove_outlier <- function(dataframe,
                            columns=names(dataframe)) {
  
  # for loop to traverse in columns vector
  for (col in columns) {
    
    # remove observation if it satisfies outlier function
    dataframe <- dataframe[!detect_outlier(dataframe[[col]]), ]
  }
  
  # return dataframe
  print("Remove outliers")
  print(dataframe)
}

no_outlier_beh <- remove_outlier(behaviours_score, c('PSS10_avg', 'compliance', 'corona_concern')) #remove over 2000 outlier observations

boxplot(no_outlier_beh)


# Export into csv file for RQ1
#write.csv(maha_outlier_removed, "G:/My Drive/Spring 2022/DA 401/COVID Data/mahaDist_outlier_removed.csv", row.names = F)
#write.csv(behaviour_outlier_removed, "G:/My Drive/Spring 2022/DA 401/COVID Data/behaviour_outlier_removed.csv", row.names = F)
#write.csv(df1, "G:/My Drive/Spring 2022/DA 401/COVID Data/all_characteristics_cleaned.csv", row.names = F)
#write.csv(all_char_avg, "G:/My Drive/Spring 2022/DA 401/COVID Data/cluster_all_characteristics_avg_score.csv", row.names = F)
#write.csv(df1, "G:/My Drive/Spring 2022/DA 401/COVID Data/cluster_noPSS_noSLON_avg_score.csv", row.names = F)

### EDA ###
#maha_outlier_removed <- read.csv("G:/My Drive/Spring 2022/DA 401/COVID Data/mahaDist_outlier_removed.csv")

# Check correlation between independent variables 
behaviour_outlier_removed.cor = cor(no_outlier_beh, method = c("spearman"))
corrplot(behaviour_outlier_removed.cor, method = "number")

# Demographics characteristics

# Behaviours associated with characteristics

#######################################################################

######################### Script for RQ1 ##############################
### PCA to reduce number of dimensions ###

### For dataframe have perceived stress and loneliness ###
pca_beh = prcomp(no_outlier_beh, center = TRUE, scale = TRUE)
summary(pca_beh) # 3 variables explain for 77% of the data variance

# extract results for variables
var <- get_pca_var(pca_beh)

# coordinates of variables
head(var$coord) # 3 important variables are perceived stress, compliance, and corona concern

# Saving only 3 columns with the highest proportion of variance in all_char_avg 
all_char_transform = as.data.frame(-pca_beh$x[,1:3])

### K-Means Clustering ###
### Dataframe with stress and loneliness ###

# Determine number of clusters k using elbow method 
# Use map_dbl to run many models with varying value of k (centers)

tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = all_char_transform, centers = k)
  model$tot.withinss
})

# Generate a data frame containing both k and tot_withinss
elbow_df <- data.frame(
  k = 1:10,
  tot_withinss = tot_withinss
)


# Plot the elbow plot
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_line() + geom_point()+
  scale_x_continuous(breaks = 1:10)+ 
  labs(title = "Optimal number of clusters for behavior response to COVID-19", 
       subtitle = "Optimal number of cluster is 3") + 
  ylab("Within cluster sum of squares") + 
  xlab("k (Number of clusters)") + 
  theme_minimal()

# K-means clustering
k = 3 # choosing k = 3

kmeans_all_char = kmeans(all_char_transform, centers = k, nstart = 50)

fviz_cluster(kmeans_all_char, data = all_char_transform, geom = "point", ggtheme = theme_minimal()) + 
  labs(title = "Clusters of participants' behavioural characteristics at the beginning of COVID", 
       subtitle = "K-means clustering, k = 3")

#### Descriptive characteristics of cluster profiles ####
no_outlier_beh$cluster <- kmeans_all_char$cluster

cluster_char <- no_outlier_beh %>% group_by(cluster) %>% 
  summarise(mean_stress = mean(PSS10_avg), 
            mean_alone = mean(SLON3_avg),
            mean_trust_country = mean(Trust_countrymeasure),
            mean_compliance = mean(compliance),
            mean_corona_concern = mean(corona_concern),
            count = n())



## ANOVA to compare characteristics of 3 clusters 
stress.aov <- aov(mean_stress ~ cluster, data = cluster_char)
summary(stress.aov)

alone.aov <- aov(mean_alone ~ cluster, data = cluster_char)
summary(alone.aov)

trustCountry.aov <- aov(mean_trust_country ~ cluster, data = cluster_char)
summary(trustCountry.aov)

compliance.aov <- aov(mean_compliance ~ cluster, data = cluster_char)
summary(compliance.aov)

coronaConcern.aov <- aov(mean_corona_concern ~ cluster, data = cluster_char)
summary(coronaConcern.aov)
# Identify datapoints that are in overlap of 2 or more clusters


#######################################################################

######################### Script for RQ2 ##############################

### Join Covid table with GDP table ###
gni_data <- read.csv("G:/My Drive/Spring 2022/DA 401/COVID Data/API_NY.GDP.PCAP.CD_DS2_en_csv_v2_3731360.csv",fileEncoding="UTF-8-BOM")
income_tag <- read.csv("G:/My Drive/Spring 2022/DA 401/COVID Data/Metadata_Country_API_NY.GDP.PCAP.CD_DS2_en_csv_v2_3731360.csv", fileEncoding="UTF-8-BOM")

# rename column "country" in gdp_data
gni_data <- rename(gni_data, c(Country = Country.Name, Code = Country.Code))

# have income group in Covid table
covid_data$answer_duration_mins <- (covid_data$answer_duration)/60
covid_data <- covid_data %>% filter(answer_duration_mins >= 3)

covid_data$Code <- gni_data$Code[ match(covid_data$Country, gni_data$Country)]
covid_data$income_group <- income_tag$IncomeGroup[ match(covid_data$Code, income_tag$Country.Code)]



# Extract socioeconomics variables
socio_econ <- covid_data %>% select("Dem_age", "Dem_gender", 
                                    "Dem_edu", "Dem_employment", 
                                    "Country", "Dem_maritalstatus", 
                                    "Dem_dependents", "Dem_riskgroup", "PSS10_avg", "income_group")

socio_econ <- socio_econ[complete.cases(socio_econ),]

socio_econ$income_tag <- ifelse(socio_econ$income_group == "High income" | socio_econ$income_group == 'Upper middle income', "Developed", "Developing")

socio_econ <- socio_econ %>% filter(Dem_isolation != "1")

# write.csv(socio_econ, "G:/My Drive/Spring 2022/DA 401/COVID Data/SocioEconomics factors.csv", row.names = F)


socio_econ <- read.csv("G:/My Drive/Spring 2022/DA 401/COVID Data/SocioEconomics factors.csv")
### Summary statistics
summary(socio_econ)
glimpse(socio_econ)
unique(socio_econ$Dem_edu)
socio_econ <- socio_econ %>% mutate(Dem_edu = recode(Dem_edu, "College degree, bachelor, master" = "Bachelor degree above",
                                                     "Some College, short continuing education or equivalent" = "Some College", 
                                                     "Up to 12 years of school" =  "Up to 12 years", 
                                                     "Up to 9 years of school" = "Up to 9 years",
                                                     "Up to 6 years of school" = "Up to 6 years",
                                                     "PhD/Doctorate" = 'PhD level'
                                                     ))

unique(socio_econ$Dem_edu_mom)
socio_econ <- socio_econ %>% mutate(Dem_edu_mom = recode(Dem_edu_mom, "Some College or equivalent" = "Some College",
                                                         "PhD/Doctorate" = "PhD level",
                                                         "Up to 12 years of school" =  "Up to 12 years", 
                                                         "Up to 9 years of school" = "Up to 9 years",
                                                         "Up to 6 years of school" = "Up to 6 years"
                                                         ))

unique(socio_econ$Dem_maritalstatus)

socio_econ %>% count(income_group)
socio_econ %>% count(income_tag)

# EDA
ggplot(socio_econ, aes(x = Dem_age)) + 
  geom_histogram(aes(fill = Dem_gender), col = "black") +
  xlim(15,90)

ggplot(socio_econ, aes(x = Dem_edu, y = Dem_age)) + 
  geom_col(aes(fill = Dem_gender), coll = "black") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

ggplot(socio_econ, aes(x = Dem_edu, y = PSS10_avg)) +
  geom_boxplot()

ggplot(socio_econ, aes(x = Dem_age, y = PSS10_avg)) +
  geom_point() + 
  geom_smooth(method='lm', formula= y~x)

socio_econ_1 <- subset(socio_econ, select = -c(Country, income_group))
ggpairs(socio_econ_1)


### Build training model ###

## Linear regression model of all countries ##
mod1 <- lm(PSS10_avg ~ Dem_age + Dem_gender + Dem_employment + Dem_edu + Dem_dependents + Dem_riskgroup + income_group, data = socio_econ)
summary(mod1)

par(mfrow=c(2,2))
plot(mod1)

ols_vif_tol(mod1)

## Linear regression model of developed countries ##
developed <- socio_econ %>% filter(income_tag == "Developed")
mod2 <- lm(PSS10_avg ~ Dem_age + Dem_gender + Dem_employment + Dem_edu + Dem_dependents + Dem_riskgroup + income_group, data = developed)
summary(mod2)

par(mfrow=c(2,2))
plot(mod2)

ols_vif_tol(mod2)

# List out number of response in each country 
developed_response <- developed %>% group_by(income_tag) %>%
  count(Country) %>% 
  arrange(desc(n))

## Linear regression model of developing countries
developing <- socio_econ %>% filter(income_tag == "Developing")
mod3 <- lm(PSS10_avg ~ Dem_age + Dem_gender + Dem_employment + Dem_edu + Dem_dependents + Dem_riskgroup + income_group, data = developing)
summary(mod3)

par(mfrow=c(2,2))
plot(mod3)

ols_vif_tol(mod3)

# List out number of response in each country 
developing_response <- developing %>% group_by(income_tag) %>%
  count(Country) %>% 
  arrange(desc(n))
