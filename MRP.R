library(tidyverse)
library(nnet)

# this.packages <- c("data.table", "fitdistrplus", 
#                    "chron", "R.utils", "descr", "xtable", "car", 
#                    "foreach", "coda", "parallel",
#                    "doParallel", "arm", 'dplyr',
#                    "caret", "glmnet", 'nnet',
#                    "e1071", 'stringr');
# demographic_combinations <- c(list('Education', 'Gender', 'Age'),
#                              combn(c('Education', 'Gender', 'Age'), 2, simplify = F),'Census_Division')
# party, gender, age, race, education, urbanicity, married

demographic_combinations <- list(c('gender', 'age', 'race'),
                                 c('gender', 'race'),
                                 c('race', 'age'),
                                 c('gender', 'age'),
                                 'gender', 'age', 'race')
# state_map <- setNames(state.abb, state.name)

data <- read_csv('Data/all_generic.csv') %>%
  select(gender, age, race, vote2018) %>%
  mutate(vote2018 = case_when(vote2018 == 'Democratic candidate' ~ 'Democrat',
                              vote2018 == 'Republican candidate' ~ 'Republican'),
         race = ifelse(race=='Other Race', 'Other', race)) %>%
  filter(!is.na(vote2018)) %>%
  mutate(gender = factor(gender, levels=c('Male', 'Female')),
         age = factor(age, levels=c('18 - 24', '25 - 34', '35 - 44', '45 - 54', '> 54')),
         race = factor(race, levels=c('White', 'Hispanic', 'Black', 'Other')),
         vote2018 = factor(vote2018, levels=c('Democrat', 'Republican')))

pops <- read_csv('Data/population_data.csv') %>%
  mutate(race = factor(race, levels=levels(data$race)),
         gender = factor(gender, levels=levels(data$gender)),
         age = factor(age, levels=levels(data$age)))


# data <- read.csv('Data/generic_data.csv', stringsAsFactors = FALSE) %>%
#   mutate(State_Abb = recode(State, !!!state_map),
#          Vote_House = factor(Vote_House,
#                              levels=c('Republican',
#                                       'Democrat',
#                                       'Other/Not sure',
#                                       'Wont vote')),
#          Trump_Sentiment = factor(Trump_Sentiment,
#                                   levels=c('Approve Strongly',
#                                            'Approve Weakly',
#                                            'Neither Approve nor Disapprove',
#                                            'Disapprove Weakly',
#                                            'Disapprove Strongly')),
#          Age = factor(Age,
#                       levels=c('18 - 24',
#                                '25 - 34',
#                                '35 - 44',
#                                '45 - 54',
#                                '> 54')),
#          Gender = factor(Gender,
#                          levels=c('female',
#                                   'male')),
#          Education = factor(Education,
#                             levels=c('middle_school',
#                                      'high_school',
#                                      'vocational_technical_college',
#                                      'university',
#                                      'postgraduate')),
#          Census_Division = factor(Census_Division))

pred_pops <- pops %>% group_by(gender, age, race) %>% summarise(N=sum(N))
m <- multinom(vote2018 ~ (gender + age + race)^2, data, maxit=1000)
pred <- predict(m, pred_pops, 'probs')

pred_pops$pred_val = pred_pops$N * pred

df <- list(Demographic_Type = 'National', Demographic = 'National',
           Vote_R = sum(pred_pops$pred_val)/sum(pred_pops$N)) %>% as.tibble()

for(i in 1:length(demographic_combinations)){
  vars = demographic_combinations[[i]]
  temp = pred_pops %>% group_by_(.dots = lapply(vars, as.symbol)) %>%
    summarise(Vote_R = sum(pred_val) / sum(N)) %>%
    unite_(col='Demographic', vars, sep=' ') %>%
    mutate(Demographic_Type = str_to_title(str_flatten(vars, collapse=' ')))
  
  df <- bind_rows(df, temp)
}

write_csv(df, 'Output/mrp_results.csv')
