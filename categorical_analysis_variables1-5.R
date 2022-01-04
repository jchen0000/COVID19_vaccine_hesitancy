rm(list = ls())

setwd("~/Desktop")


library(readr)
library(tidyverse)


data <- read_csv("filtered_data.csv")  

data_final <- data[,-1]
colnames(data_final)


head(data_final)

################################### Categorical variables ###################################

# 1. trend of cost per month of prescriptions related to VACCINES drugs 
#    in the past sixth to ninth month versus ninth to twelfth month prior to the score date 
#    {Based on GPI2 grouping}

data_final$`rx_gpi2_17_pmpm_cost_t_12-9-6m_b4`

# 2. Mining-dependent counties

data_final$atlas_type_2015_mining_no

#3. COVID Vaccination

data_final$covid_vaccination

# 4. Member geographic information - Humana Region

data_final$hum_region

# 5. Member gender

data_final$sex_cd

# 6. Preferred language for member

data_final$lang_spoken_cd

# 7. RX Adherence - Maintenance

data_final$cons_rxadhm # Where to get interpretation?

# 8. Low education counties

data_final$atlas_low_education_2015_update

# 9. Code indicating a member's race 
#    {0 = Unknown, 
#     1 = White, 
#     2 = Black, 
#     3 = Other, 
#     4 = Asian, 
#     5 = Hispanic, 
#     6 = N. American Native}


data_final$race_cd


########################### HISTOGRAMS ######################################

data_final$race_cd <- factor(data_final$race_cd, 
                             levels = c(0,1,2,3,4,5,6),
                             labels = c("Unknown", "White",
                                        "Black", "Other",
                                        "Asian", "Hispanic",
                                        "N. American Native"))

vax <- data_final[data_final$covid_vaccination == "vacc",]
unvax <- data_final[data_final$covid_vaccination == "no_vacc",]





  
p_1 <-  ggplot(data_final) +
    geom_bar(aes(x = `rx_gpi2_17_pmpm_cost_t_12-9-6m_b4`, fill = covid_vaccination), position=position_dodge()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) #+

p_2 <- ggplot(data_final) +
    geom_bar(aes(x = atlas_type_2015_mining_no, fill = covid_vaccination), position=position_dodge()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) #+

p_3 <- ggplot(data_final) +
    geom_bar(aes(x = hum_region, fill = covid_vaccination), position=position_dodge()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
  
p_4 <-  ggplot(data_final) +
    geom_bar(aes(x = sex_cd, fill = covid_vaccination), position=position_dodge()) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

p_5 <-  ggplot(data_final) +
  geom_bar(aes(x = lang_spoken_cd, fill = covid_vaccination), position=position_dodge()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_6 <- ggplot(data_final) +
  geom_bar(aes(x = cons_rxadhm, fill = covid_vaccination), position=position_dodge()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_7 <- ggplot(data_final) +
  geom_bar(aes(x = atlas_low_education_2015_update, fill = covid_vaccination), position=position_dodge()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #+

p_8 <- ggplot(data_final) +
  geom_bar(aes(x = race_cd, fill = covid_vaccination), position=position_dodge()) 

library(ggpubr)

ggarrange(ggarrange(p_1, p_2, p_3, p_4, nrow = 1, align = "hv", 
                    labels = c("Vaccine drugs", "Mining-dependent", 
                               "Geography", "Gender")), 
          ggarrange(p_5, p_6, p_7, p_8, nrow = 1, align = "hv",
                    labels = c("Language", "RX Adherence", "Low edu", "Race")),
          nrow = 2, align = "hv")

# 1. trend of cost per month of prescriptions related to VACCINES drugs 
#    in the past sixth to ninth month versus ninth to twelfth month prior to the score date 
#    {Based on GPI2 grouping}

data_final$`rx_gpi2_17_pmpm_cost_t_12-9-6m_b4`

vac_drug_cost_Dec_1x2x<- data_final[data_final$`rx_gpi2_17_pmpm_cost_t_12-9-6m_b4` == "Dec_1x-2x",]
nrow(vac_drug_cost_Dec_1x2x[vac_drug_cost_Dec_1x2x$covid_vaccination == "vacc",]) / nrow(vac_drug_cost_Dec_1x2x)
#0.2746017

vac_drug_cost_Dec_2x4x<- data_final[data_final$`rx_gpi2_17_pmpm_cost_t_12-9-6m_b4` == "Dec_2x-4x",]
nrow(vac_drug_cost_Dec_2x4x[vac_drug_cost_Dec_2x4x$covid_vaccination == "vacc",]) / nrow(vac_drug_cost_Dec_2x4x)
#0.273794

vac_drug_cost_Inc_1x2x<- data_final[data_final$`rx_gpi2_17_pmpm_cost_t_12-9-6m_b4` == "Inc_1x-2x",]
nrow(vac_drug_cost_Inc_1x2x[vac_drug_cost_Inc_1x2x$covid_vaccination == "vacc",]) / nrow(vac_drug_cost_Inc_1x2x)
#0.2484915

vac_drug_cost_Inc_2x4x<- data_final[data_final$`rx_gpi2_17_pmpm_cost_t_12-9-6m_b4` == "Inc_2x-4x",]
nrow(vac_drug_cost_Inc_2x4x[vac_drug_cost_Inc_2x4x$covid_vaccination == "vacc",]) / nrow(vac_drug_cost_Inc_2x4x)
#0.2742967

vac_drug_cost_Inc_4x8x<- data_final[data_final$`rx_gpi2_17_pmpm_cost_t_12-9-6m_b4` == "Inc_4x8x",]
nrow(vac_drug_cost_Inc_4x8x[vac_drug_cost_Inc_4x8x$covid_vaccination == "vacc",]) / nrow(vac_drug_cost_Inc_4x8x)
#NaN

vac_drug_cost_New<- data_final[data_final$`rx_gpi2_17_pmpm_cost_t_12-9-6m_b4` == "New",]
nrow(vac_drug_cost_New[vac_drug_cost_New$covid_vaccination == "vacc",]) / nrow(vac_drug_cost_New)
#0.2203426

vac_drug_cost_NoAct<- data_final[data_final$`rx_gpi2_17_pmpm_cost_t_12-9-6m_b4` == "No Activity",]
nrow(vac_drug_cost_NoAct[vac_drug_cost_NoAct$covid_vaccination == "vacc",]) / nrow(vac_drug_cost_NoAct)
#0.1659557

vac_drug_cost_Nocha<- data_final[data_final$`rx_gpi2_17_pmpm_cost_t_12-9-6m_b4` == "No Change",]
nrow(vac_drug_cost_Nocha[vac_drug_cost_Nocha$covid_vaccination == "vacc",]) / nrow(vac_drug_cost_Nocha)
#NaN

vac_drug_cost_Res<- data_final[data_final$`rx_gpi2_17_pmpm_cost_t_12-9-6m_b4` == "Resolved",]
nrow(vac_drug_cost_Res[vac_drug_cost_Res$covid_vaccination == "vacc",]) / nrow(vac_drug_cost_Res)
#0.2193892


# 2. Mining-dependent counties

mining_independent<- data_final[data_final$atlas_type_2015_mining_no == "0",]
nrow(mining_independent[mining_independent$covid_vaccination == "vacc",]) / nrow(mining_independent)
#0.178471
mining_dependent<- data_final[data_final$atlas_type_2015_mining_no == "1",]
nrow(mining_dependent[mining_dependent$covid_vaccination == "vacc",]) / nrow(mining_dependent)
#0.3322241

# 4. Member geographic information - Humana Region

data_final$hum_region

geo_Cal_Nev<- data_final[data_final$hum_region == "CALIFORNIA/NEVADA",]
nrow(geo_Cal_Nev[geo_Cal_Nev$covid_vaccination == "vacc",]) / nrow(geo_Cal_Nev)
#0.1932633

geo_Cent<- data_final[data_final$hum_region == "CENTRAL",]
nrow(geo_Cent[geo_Cent$covid_vaccination == "vacc",]) / nrow(geo_Cent)
#0.1634675

geo_Cent_West<- data_final[data_final$hum_region == "CENTRAL WEST",]
nrow(geo_Cent_West[geo_Cent_West$covid_vaccination == "vacc",]) / nrow(geo_Cent_West)
#0.1797275

geo_East<- data_final[data_final$hum_region == "EAST",]
nrow(geo_East[geo_East$covid_vaccination == "vacc",]) / nrow(geo_East)
#0.1804698

geo_East_Cent<- data_final[data_final$hum_region == "EAST CENTRAL",]
nrow(geo_East_Cent[geo_East_Cent$covid_vaccination == "vacc",]) / nrow(geo_East_Cent)
#0.1682661

geo_Florida<- data_final[data_final$hum_region == "FLORIDA",]
nrow(geo_Florida[geo_Florida$covid_vaccination == "vacc",]) / nrow(geo_Florida)
#0.119772

geo_GLCN<- data_final[data_final$hum_region == "GREAT LAKES/CENTRAL NORTH",]
nrow(geo_GLCN[geo_GLCN$covid_vaccination == "vacc",]) / nrow(geo_GLCN)
#0.2270428
geo_GULF<- data_final[data_final$hum_region == "GULF STATES",]
nrow(geo_GULF[geo_GULF$covid_vaccination == "vacc",]) / nrow(geo_GULF)
#0.1966608

geo_INTERMOUNTAIN<- data_final[data_final$hum_region == "INTERMOUNTAIN",]
nrow(geo_INTERMOUNTAIN[geo_INTERMOUNTAIN$covid_vaccination == "vacc",]) / nrow(geo_INTERMOUNTAIN)
#0.1872067

geo_MANC<- data_final[data_final$hum_region == "MID-ATLANTIC/NORTH CAROLINA",]
nrow(geo_MANC[geo_MANC$covid_vaccination == "vacc",]) / nrow(geo_MANC)
#0.1725592

geo_MIDSOUTH<- data_final[data_final$hum_region == "MID-SOUTH",]
nrow(geo_MIDSOUTH[geo_MIDSOUTH$covid_vaccination == "vacc",]) / nrow(geo_MIDSOUTH)
#0.1218095

geo_NORTHEAST<- data_final[data_final$hum_region == "NORTHEAST",]
nrow(geo_NORTHEAST[geo_NORTHEAST$covid_vaccination == "vacc",]) / nrow(geo_NORTHEAST)
#0.2051715

geo_PACIFIC<- data_final[data_final$hum_region == "PACIFIC",]
nrow(geo_PACIFIC[geo_PACIFIC$covid_vaccination == "vacc",]) / nrow(geo_PACIFIC)
#0.2083704

geo_PR<- data_final[data_final$hum_region == "PR",]
nrow(geo_PR[geo_PR$covid_vaccination == "vacc",]) / nrow(geo_PR)
#0.1623839

geo_SOUTHEAST<- data_final[data_final$hum_region == "SOUTHEAST",]
nrow(geo_SOUTHEAST[geo_SOUTHEAST$covid_vaccination == "vacc",]) / nrow(geo_SOUTHEAST)
#0.1713426

geo_TEXAS<- data_final[data_final$hum_region == "TEXAS",]
nrow(geo_TEXAS[geo_TEXAS$covid_vaccination == "vacc",]) / nrow(geo_TEXAS)
#0.1318208


# 5. Member gender

Female<- data_final[data_final$sex_cd == "F",]
nrow(Female[Female$covid_vaccination == "vacc",]) / nrow(Female)
#0.1805473
Male<- data_final[data_final$sex_cd == "M",]
nrow(Male[Male$covid_vaccination == "vacc",]) / nrow(Male)
#0.1659847







