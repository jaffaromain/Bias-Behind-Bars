#### Preamble ####
# Purpose: Prepare and clean data downloaded from the Globe and Mail. This data features information obtained from the Correctional Service of Canada. This must be run first before running any other script in order to utilize cleaned data. 
# Prerequisite: Before running this script, you must download the CSC and place it into the input folder.  The name of the file should be named as 'Globe_and_Mail_CSC_data.csv' the data can be found here at: https://www.theglobeandmail.com/files/editorial/News/nw-na-risk-1023/The_Globe_and_Mail_CSC_OMS_2012-2018_20201022235635.zip
# Author: Jaffa Romain
# Data: 14 December 2020
# Contact: jaffa.romain@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(devtools)
devtools::install_github('cttobin/ggthemr')
library(ggthemr) # theme for plots
ggthemr("fresh") 
# read in raw data 
CSCdata <- read_csv("input/Globe_and_Mail_CSC_data.csv")
# Removing `JUDGE` variable - It is filled with NA values
CSCdata <- CSCdata %>% select(-JUDGE)
# filtering those under provincial jurisdiction - after filtering - 741 829 rows
CSCdata <- CSCdata %>% filter(JURISDICTION != 'PROVINCIAL')
# drop NAs in variables of interest - now left with 686 540
CSCdata <- CSCdata %>% drop_na(RACE, `RACE GROUPING`, `OFFENDER SECURITY LEVEL`, `REINTEGRATION POTENTIAL`)
saveRDS(CSCdata, file = "input/cleaned_CSC.rds")
write_csv(CSCdata, "CSC_data.csv")
### EDA to look at any trends in data ###

# Data frame has separate entries for each offense of offenders - removing duplicate to get unique entry for each offender 
# data for cases when first admitted
unique_data <- CSCdata %>% group_by(`OFFENDER NUMBER`) %>% filter(`FISCAL YEAR` == min(`FISCAL YEAR`))
EDA_data <-unique_data[!duplicated(unique_data$`OFFENDER NUMBER`),] # remove duplicates for each offender 
saveRDS(unique_data, file = "input/CSC_first_fiscal.rds")


# Data has over 30 possible categories of race, and race changes over time with the year
# Need to group them into subcategories - black, indigenous other with race when first admitted
data_by_race <- EDA_data %>% filter(RACE != "Unknown", RACE != "Unable Specify") %>% mutate(race = case_when(RACE == "Chinese"~"Asi-E/Southeast", RACE == "Arab"~"Arab/West Asian", 
                                                     RACE == "East Indian" ~ "Asian-South", RACE == "Korean" ~"Asi-E/Southeast", RACE == "Japanese" ~ "Asi-E/Southeast", RACE == "S. E. Asian" ~ "Asi-E/Southeast",
                                                     RACE == "South Asian"~"Asi-E/Southeast",  RACE == "Euro.-Eastern"~"White", RACE == "Euro.-Northern"~"White", RACE == "Euro.-Southern"~"White",
                                                     RACE == "Euro.-Western"~"White", RACE == "European French" ~ "White", RACE == "Sub-Sahara Afri"~"Black", RACE == "Inuit"~"Indigenous", RACE == "Metis"~"Indigenous",
                                                     RACE == "Filipino" ~"Asi-E/Southeast", RACE == "Hispanic" ~ "Latin American", RACE == "British Isles"~"Other", RACE == "Oceania" ~ "Other", RACE == "Multirac/Ethnic"~"Other",
                                                     RACE == "Caribbean"~"Black", TRUE~RACE)) 
# North Americans are all Indigenous and can be grouped with Metis and Inuit -  code below accounts for this
data_by_race <- data_by_race %>% mutate(race = ifelse(race == "North American" & `RACE GROUPING` == "Indigenous", "Indigenous", race))

# proportion of prisoners by race
by_race <- data_by_race %>% select(race) %>% group_by(race) %>% summarise(n = n()) %>% mutate(freq = n/sum(n))
offenders_by_race <- by_race %>% ggplot(aes(x = race)) + geom_bar(aes(y = freq), stat = "identity", position = "dodge") + 
  labs(x = "Race", y = "Percentage", title = "Proportion of Offenders by Race") + theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 5))
offenders_by_race
saveRDS(offenders_by_race, file = "input/offenders_by_race.rds")

# Focusing on 3 highest proportion of offenders
data_final <- data_by_race %>%  mutate(race_group = ifelse(`RACE GROUPING` == "Indigenous", "Indigenous", ifelse(race == "Black", "Black", ifelse(race== "White", "White", "Other"))))
# proportion by race - now grouped as Indigenous, White, Black and Other
by_race_3 <- data_final %>% select(race_group) %>% group_by(race_group) %>% summarise(n = n()) %>% mutate(freq = n/sum(n))
prop_race_3_cat <- by_race_3 %>% ggplot(aes(x = race_group, y = freq, fill = race_group)) + geom_col(position = "dodge") + geom_text(aes(label = scales::percent(freq), y = freq, group = race_group),
                   position = position_dodge(width = 0.9), vjust = 1.5) + labs(x = "Race", y = "Proportion", title = "Proportion of Offenders by Race") + theme(legend.position = "none")
prop_race_3_cat
saveRDS(prop_race_3_cat, file = "input/prop_race_plot.rds")

saveRDS(data_final, file = "input/data_final.rds")

# by race - Looking at only female offenders
race_gender_plot <- data_final  %>% filter(GENDER == "FEMALE") %>% group_by(race_group) %>%  summarise(n =n()) %>% mutate(freq = n/sum(n)) %>%
ggplot(aes(race_group, y = freq, fill = race_group)) + geom_col(position = "dodge") + geom_text(aes(label = scales::percent(freq), y = freq, group=race_group), 
position = position_dodge(width = 0.9), vjust = 1.5) + labs(x = "Race", y = "Proportion", title = "Female Offenders by race") + theme(legend.position = "none") + coord_flip()
race_gender_plot

saveRDS(race_gender_plot, file = "input/race_gender_plot.rds")

# Looking at Offender Security Level based on Race - when first admitted
security_plot_race <- data_final %>% ggplot(aes(x = `OFFENDER SECURITY LEVEL`)) + geom_bar() + 
  labs(x = "Race", title = " Distribution of Security Level Score by Race") + theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 5)) + facet_wrap(~race)
saveRDS(security_plot, file = "input/security_plot_race.rds")

# Looking at Reintegration Potential based on Race - When first admitted
reintegration_plot <- data_final %>% ggplot(aes(x = `REINTEGRATION POTENTIAL`)) +
geom_bar(alpha = 0.7, color ="black") + labs(title = "Inmate Reintegration Scores", subtitle = "A look at Reintegration Scores by race group and gender",
x = "Reintegration Score", y = "Count") + facet_grid(GENDER~race_group) + theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 5)) 
reintegration_plot
saveRDS(reintegration_plot, file = "input/reintegration_plot.rds")


# Security Classification
security_plot <- data_final %>% ggplot(aes(x = `OFFENDER SECURITY LEVEL`)) +
  geom_bar(alpha = 0.7, color ="black") + labs(title = "Inmate Security Level Score", subtitle = "A look at inmate security classification by race group and gender",
x = "Offender Security Level", y = "Count") + facet_grid(GENDER~race_group) + theme(axis.text.x = element_text(angle = 35, hjust = 1, size = 5)) 
security_plot

saveRDS(security_plot, file = "input/security_plot.rds")


# GENDER DISTRIBUTION

gender_plot <- data_final %>% ggplot(aes(x = GENDER)) + geom_bar() + labs(x = "Gender", title = "Gender Distribution of Inmates", subtitle = "We can see there is a much larger male inmate population.")
gender_plot
saveRDS(gender_plot, file = "input/gender_plot.rds")






