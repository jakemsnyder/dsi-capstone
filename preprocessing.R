library(tidyverse)

# setwd('~/Columbia/Capstone/dsi-capstone/')

## generic preprocessing
# The columns we would like to include:
generic_colnames <- c("party", "gender", "age",
                      "race", "education", "urbanicity",
                      "vote2018", "married", "CD_116", "state", "wave")


load("Data/generic_ballot.RData")
generic <- voting2018
names(generic) <- tolower(names(generic))

# To allow different Pollfish names
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

survey1 <- loadRData("Data/Pollfish_Survey_FI10002d.378826.moreifo_completed2.RData")
survey2 <- loadRData("Data/Pollfish_Survey_FI10006b.378910.moreifo_completed2.RData")
survey3 <- loadRData("Data/Pollfish_Survey_IN10001a.406052.moreifo_completed2.RData")
survey4 <- loadRData("Data/Pollfish_Survey_IN10004.324889.moreifo_completed2.RData")
survey5 <- loadRData("Data/Pollfish_Survey_IN10005.324886.moreifo_completed2.RData")
survey6 <- loadRData("Data/Pollfish_Survey_IN10006.324887.moreifo_completed2.RData")
survey7 <- loadRData("Data/Pollfish_Survey_IN10007b.378647.moreifo_completed2.RData")
survey8 <- loadRData("Data/Pollfish_Survey_SC10001a.338700.moreifo_completed2.RData")
survey9 <- loadRData("Data/Pollfish_Survey_SC10002a.338699.moreifo_completed2.RData")
survey10 <- loadRData("Data/Pollfish_Survey_SC10003b.375937.moreifo_completed2.RData")
survey11 <- loadRData("Data/Pollfish_Survey_SC10004c.375939.moreifo_completed2.RData")
survey12 <- loadRData("Data/Pollfish_Survey_SC10005a.338697.moreifo_completed2.RData")

# select columns for all data
col_select <- function(x) { 
  x %>% select(generic_colnames)
}

datasets <- list(survey1, survey2, survey3, survey4,
              survey5, survey6, survey7, survey8,
              survey9, survey10, survey11, survey12)

all_generic_with_districts <- NULL
all_generic <- generic
for (i in datasets){
  i <- i %>% select(generic_colnames)
  all_generic_with_districts <- rbind(all_generic_with_districts, i)
  i <- i %>% select(-CD_116, -state)
  all_generic <- rbind(all_generic, i)
}

all_generic <- all_generic %>%
  mutate(race = fct_recode(race, Other = 'Other Race'))
all_generic_with_districts <- all_generic_with_districts %>%
  mutate(race = fct_recode(race, Other = 'Other Race')) %>%
  rename(district = CD_116)

save(all_generic, file = "Data/all_generic.RData")
write_csv(all_generic, 'Data/all_generic.csv')
save(all_generic_with_districts, file = "Data/all_generic_with_districts.RData")
write_csv(all_generic_with_districts, 'Data/all_generic_with_districts.csv')


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
  rename(district = congressional_district) %>%
  ungroup() %>%
  mutate(district = str_sub(district, 2)) %>%
  unite(district, state, district, sep = '-', remove=FALSE)
write_csv(population, 'Data/population_data.csv')
