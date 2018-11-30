library(tidyverse)

# setwd('~/Columbia/Capstone/dsi-capstone/')

## specific polling preprocessing
load('Data/LCV_round_3.RData')
specific <- survey_total %>%
  select(party, gender, age, race, education, urbanicity, married,
         vote2018 = `If the election for the U.S. House of Representatives in your district was today, who would you vote for?`,
         district, wave) %>%
  filter(!is.na(vote2018)) %>%
  mutate(vote2018 = fct_recode(vote2018,
                               `Other candidate/not sure` = 'Other candidate',
                               `Other candidate/not sure` = 'Undecided'),
         race = fct_recode(race, Other = 'Other Race'))
write_csv(specific, 'Data/all_specific.csv')
  
## population preprocessing
load('Data/projection_space_national_18-10_02.RData')
population <- pops %>%
  mutate(education = ifelse(education=='No college', 'No Bachelors', 'Bachelors')) %>%
  group_by(age, urbanicity, gender, state, race, education, married, party, congressional_district) %>%
  summarise(N = sum(N)) %>%
  rename(district = congressional_district)%>%
  #Change district to factor
    mutate(district = as.numeric(district))%>%
    mutate(district = sprintf("%02d",district))%>%
    unite(district, state, district, sep = '-',remove =FALSE)
write_csv(population, 'Data/population_data.csv')
