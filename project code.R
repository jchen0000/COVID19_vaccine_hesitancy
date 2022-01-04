rm(list = ls())

setwd("~/OneDrive - cumc.columbia.edu/Columbia/non-academic/2021 humana-mays case comp/files")

library(tidyverse)
library(readr)

## open data file

# data = read_csv("filtered_data.csv")
data_holdout <- read_csv("2021_Competition_Holdout.csv")
colnames(data_holdout)


data_filter <- data_holdout %>% select(ID,
                                       cons_chmi,
                                       `rx_gpi2_17_pmpm_cost_t_12-9-6m_b4`,
                                       est_age,
                                       atlas_percapitainc,
                                       rwjf_uninsured_adults_pct,
                                       atlas_type_2015_mining_no,
                                       atlas_povertyallagespct,
                                       hum_region,
                                       sex_cd,
                                       lang_spoken_cd,
                                       atlas_pct_sbp15,
                                       rwjf_resident_seg_black_inx,
                                       cons_rxadhm,
                                       cons_lwcm07,
                                       atlas_low_education_2015_update,
                                       race_cd)


colnames(data_filter)
data_final <- data_filter#[,-1]
colnames(data_final)


head(data_final)
str(data_final)

#################################################### Describing Sample #####################################

# create categorical variables
data_final$cons_chmi <- cut (data_final$cons_chmi,
                             breaks = c(-Inf, 40, 120, Inf),
                             labels = c("Low", "Middle", "High"))



# levels(factor(data_final$`rx_gpi2_17_pmpm_cost_t_12-9-6m_b4`))
# data_final$`rx_gpi2_17_pmpm_cost_t_12-9-6m_b4` <- factor(data_final$`rx_gpi2_17_pmpm_cost_t_12-9-6m_b4`)

data_final$`rx_gpi2_17_pmpm_cost_t_12-9-6m_b4` <- factor(data_final$`rx_gpi2_17_pmpm_cost_t_12-9-6m_b4`, levels = c("Dec_1x-2x", "Inc_2x-4x", "Dec_2x-4x", "Inc_4x-8x",
                                                                                              "No_Change", "Inc_1x-2x", "New", "Resolved", "No Activity" 
),
labels = c(9,8,7,6,5,4,3,2,1))
data_final$`rx_gpi2_17_pmpm_cost_t_12-9-6m_b4` <- as.numeric(data_final$`rx_gpi2_17_pmpm_cost_t_12-9-6m_b4`)

data_final$sex_cd

data_final$atlas_type_2015_mining_no <- factor(data_final$atlas_type_2015_mining_no, levels = c(1,0), labels = c(2,1))
data_final$atlas_type_2015_mining_no <- as.numeric(data_final$atlas_type_2015_mining_no)

data_final$sex_cd <- factor(data_final$sex_cd, levels = c("M", "F"), labels = c(1,2))
data_final$sex_cd <- as.numeric(data_final$sex_cd)

levels(factor(data_final$hum_region))

data_final$hum_region <- factor(data_final$hum_region, levels = c("GREAT LAKES/CENTRAL NORTH",
                                                                 "PACIFIC", "NORTHEAST", "GULF STATES",
                                                                 "CALIFORNIA/NEVADA", "INTERMOUNTAIN", "EAST",
                                                                 "CENTRAL WEST", "MID-ATLANTIC/NORTH CAROLINA",
                                                                 "SOUTHEAST", "EAST CENTRAL", "CENTRAL",
                                                                 "PR", "TEXAS", "MID-SOUTH", "FLORIDA", "NA"), 
                              labels = c(16,15,14,13,12,11,10,9,8,7,6,5,4,3,2,1,0) )

data_final$hum_region <- as.numeric(data_final$hum_region)



levels(factor(data_final$lang_spoken_cd))

data_final$lang_spoken_cd <- factor(data_final$lang_spoken_cd, levels = c("CRE", "VIE", "KOR", "CHI", "OTH", "SPA", "ENG", "NA"),
                               labels = c(7,6,5,4,3,2,1,0))

data_final$lang_spoken_cd <- as.numeric(data_final$lang_spoken_cd)


cons_rxadhm

data_final$cons_rxadhm <- factor(data_final$cons_rxadhm, levels = c("9","4","8","6","7","5","0","3","1","2","NA"),
                                labels = c(1,2,3,4,5,6,7,8,9,10, 0))

data_final$cons_rxadhm <- as.numeric(data_final$cons_rxadhm)


data_final$atlas_low_education_2015_update <- factor(data_final$atlas_low_education_2015_update, levels = c(1,0),
                          labels = c(2,1))

data_final$atlas_low_education_2015_update <- as.numeric(data_final$atlas_low_education_2015_update)

data_final$race_cd <- factor(data_final$race_cd, levels = c(6,4,3,5,0,2,1), labels = c(7,6,5,4,3,2,1))

data_final$race_cd <- as.numeric(data_final$race_cd)



# split by vaccination group for continuous variables to find median
income <- data_final %>% group_by(covid_vaccination) %>% summarize(median(cons_chmi, na.rm = T))
income

ggplot(data_final, aes(x=cons_chmi, color=covid_vaccination)) + 
  geom_histogram(fill="white", position="dodge")+
  theme(legend.position = "top")

age <- data_final %>% group_by(covid_vaccination) %>% summarize(median(est_age, na.rm = T))
age

income_percap <- data_final %>% group_by(covid_vaccination) %>% summarize(median(atlas_percapitainc, na.rm = T))
income_percap

pct_uninsured <- data_final %>% group_by(covid_vaccination) %>% summarize(median(rwjf_uninsured_adults_pct, na.rm = T))
pct_uninsured

poverty_rate <- data_final %>% group_by(covid_vaccination) %>% summarize(median(atlas_povertyallagespct, na.rm = T))
poverty_rate

#pct_65_living_alone <- data_final %>% group_by(covid_vaccination) %>% summarize(median(atlas_hh65plusalonepct, na.rm = T))
# pct_65_living_alone

pct_pop_school_breakfast <- data_final %>% group_by(covid_vaccination) %>% summarize(median(atlas_pct_sbp15, na.rm = T))
pct_pop_school_breakfast 

residential_segregation <- data_final %>% group_by(covid_vaccination) %>% summarize(median(rwjf_resident_seg_black_inx, na.rm = T))
residential_segregation

# rx_adherence <- data_final %>% group_by(covid_vaccination) %>% summarize(median(cons_rxadhm, na.rm = T))
# rx_adherence

prob_no_doc <- data_final %>% group_by(covid_vaccination) %>% summarize(median(cons_lwcm07, na.rm = T))
prob_no_doc

# sum variable for each ID to get score
# higher = more likely to get vaccine
# lower = less likely to get vaccine

###### SPLIT BY MEDIAN TO CREATE SCORING SYSTEM  ########

# Census Median Household Income
data_final$cons_chmi <- cut(data_final$cons_chmi, breaks = c(-Inf, 60, 65, Inf), labels = c(5, 10, 15))
data_final$cons_chmi <- as.numeric(data_final$cons_chmi)

# Member Age
data_final$est_age <- cut(data_final$est_age, breaks = c(-Inf, 71, 74, Inf), labels = c(3, 6, 9))
data_final$est_age <- as.numeric(data_final$est_age)

# per capital income
data_final$atlas_percapitainc <- cut(data_final$atlas_percapitainc, breaks = c(-Inf, 25714, 26467, Inf), labels = c(5, 10, 15))
data_final$atlas_percapitainc <- as.numeric(data_final$atlas_percapitainc)

#percent of uninsured
data_final$rwjf_uninsured_adults_pct <- cut(data_final$rwjf_uninsured_adults_pct, breaks = c(-Inf, 0.120, 0.134, Inf), labels = c(9, 6, 3))
data_final$rwjf_uninsured_adults_pct <- as.numeric(data_final$rwjf_uninsured_adults_pct)

#poverty rate
data_final$atlas_povertyallagespct <- cut(data_final$atlas_povertyallagespct, breaks = c(-Inf, 14.2, 14.6, Inf), labels = c(9, 6, 3))
data_final$atlas_povertyallagespct <- as.numeric(data_final$atlas_povertyallagespct)

#percent of School Breakfast Program participants
data_final$atlas_pct_sbp15 <- cut(data_final$atlas_pct_sbp15, breaks = c(-Inf, 4.02, 4.11, Inf), labels = c(9, 6, 3))
data_final$atlas_pct_sbp15 <- as.numeric(data_final$atlas_pct_sbp15)

#residential segregation
data_final$rwjf_resident_seg_black_inx <- cut(data_final$rwjf_resident_seg_black_inx, breaks = c(-Inf, 40.78, 48.1, Inf), labels = c(5, 10, 15))
data_final$rwjf_resident_seg_black_inx <- as.numeric(data_final$rwjf_resident_seg_black_inx)

# The probability of less likely seeing a doctor
data_final$cons_lwcm07 <- cut(data_final$cons_lwcm07, breaks = c(-Inf, 0.21852, 0.23427, Inf), labels = c(9, 6, 3))
data_final$cons_lwcm07 <- as.numeric(data_final$cons_lwcm07)


str(data_final)
## Sum By Row to Find Score for Each ID


data_final$Score <- rowSums(data_final[,2:17], na.rm=TRUE)  

data_final

data_final$score <- rowSums(data_final[,2:12])
head(data_final)


max(data_final$score, na.rm=TRUE) # 31
min(data_final$score, na.rm=TRUE) # 8
mean(data_final$score, na.rm=TRUE) # 17.51
median(data_final$score, na.rm=TRUE)# 17
IQR(data_final$score, na.rm=TRUE) # 3

data_ranked <- data_final %>% arrange(desc(data_final$Score))
data_ranked$rank <- rank(-data_ranked$Score)

colnames(data_ranked)

submission <- data_ranked %>% select(ID, Score, rank)
2021CaseCompetition_FirstName_LastName_2021mmdd.csv
write.csv(submission, "2021CaseCompetition_Jiaqi_Chen_20211009.csv")

str(data_ranked)