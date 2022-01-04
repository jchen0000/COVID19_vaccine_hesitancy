rm(list = ls())

setwd("~/OneDrive - cumc.columbia.edu/Columbia/non-academic/2021 humana-mays case comp/files")


library(readr)
library(tidyverse)

data <- read_csv("2021_Competition_Training.csv")
colnames(data)


holdout <- read_csv("2021_Competition_Holdout.csv")
colnames(holdout)

data$covid_vaccination

##### VARIABLES OF INTEREST #####


# result <- financials %>% select(Symbol, Name)

data$`rx_gpi2_17_pmpm_cost_t_12-9-6m_b4`

data_filter <- data %>% select(cons_chmi,
                               `rx_gpi2_17_pmpm_cost_t_12-9-6m_b4`,
                               est_age,
                               atlas_pct_laccess_seniors15,
                               atlas_percapitainc,
                               rwjf_uninsured_adults_pct,
                               atlas_type_2015_mining_no,
                               atlas_povertyallagespct,
                               covid_vaccination,
                               atlas_pct_laccess_lowi15,
                               hum_region,
                               atlas_hh65plusalonepct,
                               sex_cd,
                               lang_spoken_cd,
                               atlas_pct_sbp15,
                               rwjf_resident_seg_black_inx,
                               cons_rxadhm,
                               atlas_medhhinc,
                               cons_lwcm07,
                               atlas_low_education_2015_update,
                               race_cd)

write.csv(data_filter, "filtered_data.csv")
