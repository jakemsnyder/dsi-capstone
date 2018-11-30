
install.packages("tidyquant")
install.packages("rstanarm")
install.packages("lme4")
# install.packages("stringr")
install.packages("genderdata_package")
install.packages("gender")
install.packages("merTools")
install.packages('optimx')

library(ggplot2)
library(scales)
library(ggthemes)
library(dplyr)
library(plyr)
library(arm)
library(rstanarm)
library(lme4)
library(HLMdiag)
library(car)
library(merTools)  
library(tidyverse)
library(nnet)
library(optimx)

demographics <- c('party', 'gender', 'age', 'race', 'education', 'urbanicity', 'married')
demographic_combinations <- c(combn(demographics, simplify=FALSE, 1),
                              combn(demographics, simplify=FALSE, 2),
                              combn(demographics, simplify=FALSE, 3),
                              combn(demographics, simplify=FALSE, 4),
                              combn(demographics, simplify=FALSE, 5),
                              combn(demographics, simplify=FALSE, 6),
                              combn(demographics, simplify=FALSE, 7),
                              'district')

generic <- read_csv('Data/all_generic_with_districts.csv',
                    col_types=cols(party = col_factor(levels=c('Dem', 'Rep', 'Ind')),
                                   gender = col_factor(levels=c('Male', 'Female')),
                                   age = col_factor(levels=c('18 - 24', '25 - 34', '35 - 44', '45 - 54', '> 54')),
                                   race = col_factor(levels=c('White', 'Hispanic', 'Black', 'Other')),
                                   education = col_factor(levels=c('No Bachelors', 'Bachelors')),
                                   urbanicity = col_factor(levels=c('R1', 'R2', 'S3', 'S4', 'U5', 'U6')),
                                   vote2018 = col_character(),
                                   married = col_factor(levels=c('Unmarried', 'Married')),
                                   wave = col_skip())) %>%
    mutate(district = factor(district))%>%
    mutate(state = factor(state))%>%
  mutate(vote2018 = case_when(vote2018 == 'Democratic candidate' ~ 'Democrat',
                              vote2018 == 'Republican candidate' ~ 'Republican')) %>%
  filter(!is.na(vote2018)) %>%
  mutate(vote2018 = factor(vote2018, levels=c('Democrat', 'Republican')))


### Specific
specific <- read_csv('Data/all_specific.csv',
                     col_types=cols(party = col_factor(levels=levels(generic$party)),
                                    gender = col_factor(levels=levels(generic$gender)),
                                    age = col_factor(levels=levels(generic$age)),
                                    race = col_factor(levels=levels(generic$race)),
                                    education = col_factor(levels=levels(generic$education)),
                                    urbanicity = col_factor(levels=levels(generic$urbanicity)),
                                    married = col_factor(levels=levels(generic$married)),
                                    vote2018 = col_character(),
                                    wave = col_skip())) %>%
  mutate(vote2018 = case_when(vote2018 == 'Democratic candidate' ~ 'Democrat',
                              vote2018 == 'Republican candidate' ~ 'Republican')) %>%
  filter(!is.na(vote2018)) %>%
  mutate(vote2018 = factor(vote2018, levels=c('Democrat', 'Republican')))

specific <- separate(data = specific, col = district, into = c("state", "district"), sep = "\\-")
specific <- within(specific,  district <- paste(state, district, sep="-"))
specific<- specific %>%
    mutate(state=factor(state)) %>%
    mutate(district = factor(district))

pops <- read_csv('Data/population_data.csv',
                 col_types=cols(age = col_factor(levels=levels(generic$age)),
                                urbanicity = col_factor(levels=levels(generic$urbanicity)),
                                gender = col_factor(levels=levels(generic$gender)),
                                district = col_factor(levels = levels(generic$district)),
                                state = col_factor(levels=levels(generic$state)),
                                race = col_factor(levels=levels(generic$race)),
                                education = col_factor(levels=levels(generic$education)),
                                married = col_factor(levels=levels(generic$married)),
                                party = col_factor(levels=levels(generic$party)),
                                district = col_factor(levels=levels(generic$district)),
                                N = col_double()))

pred_pops <- pops %>% group_by_if(is.factor) %>% summarise(N=sum(N))
# generic mrp
generic_m <- intercept_fit_generic <- glmer(vote2018 ~ age + urbanicity + gender + race + education + party + married + (1 | state/district), data = generic, family = binomial(link = "logit"),
                                            control = glmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                                                  optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))

pred <- predict(generic_m, pred_pops)

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

write_csv(df, 'Output/generic_glmer_results.csv')

# specific mrp
specific_m <- intercept_fit_specific <- glmer(vote2018 ~ age + urbanicity + gender + race + education + party + married + (1 | state/district), data = specific, family = binomial(link = "logit"),
                                              control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                                                    optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)))
pred <- predict(specific_m, pred_pops, 'probs')

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

write_csv(df, 'Output/specific_glmer_results.csv')
