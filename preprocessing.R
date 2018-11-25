library(tidyverse)

# setwd('~/Columbia/Capstone/dsi-capstone/')

## specific polling preprocessing
load('Data/LCV_round_3.RData')
demographics <- c()
specific <- survey_total %>%
  select(party, gender, age, race, education, urbanicity, married,
         vote2018 = `If the election for the U.S. House of Representatives in your district was today, who would you vote for?`,
         district, wave) %>%
  filter(!is.na(vote2018)) %>%
  mutate(vote2018 = fct_recode(vote2018,
                               `Other candidate/not sure` = 'Other candidate',
                               `Other candidate/not sure` = 'Undecided'))
write_csv(specific, 'Data/specific_data.csv')
  
## population preprocessing
load('Data/projection_space_national_18-10_02.RData')
population <- pops %>%
  mutate(education = ifelse(education=='No college', 'No Bachelors', 'Bachelors')) %>%
  group_by(age, urbanicity, gender, state, race, education, married, party, congressional_district) %>%
  summarise(N = sum(N))
write_csv(population, 'Data/population_data.csv')
