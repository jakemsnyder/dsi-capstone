library(tidyverse)
library(readxl)

# setwd('~/Columbia/Capstone/dsi-capstone/')
generic_colnames <- c('ID', 'Survey_Start', 'Survey_End', 'OS', 'Country', 'State',
                      'City', 'Provider', 'Gender', 'Age', 'Birthyear', 'Vote_House',
                      'Trump_Sentiment', 'Party', 'Marital_Status', 'N_Children',
                      'Education', 'Employment', 'Career', 'Race', 'Income', 'Zip',
                      'Census_Region', 'Census_Division', 'Congressional_District',
                      'DMA_Code', 'DMA_Name', 'ADID_IDFA')
generic <- read_xlsx('Data/EarlyGenericVoting2018.xlsx')
colnames(generic) <- generic_colnames

generic <- generic %>%
  mutate(Vote_House = case_when(Vote_House == 'Will vote Republican' ~ 'Republican',
                                Vote_House == 'Will vote Democratic' ~ 'Democrat',
                                Vote_House == 'Will vote other/not sure' ~ 'Other/Not sure',
                                Vote_House == "Won't Vote" ~ 'Wont vote',
                                Vote_House == "Won't vote" ~ 'Wont vote'))


specific_filenames <- list.files(pattern = 'Pollfish*', recursive = TRUE)


read_specific_file <- function(filename){
  specific_colnames <- c('ID', 'Survey_Start', 'Survey_End', 'Manufacturer', 'OS', 'Country', 'State',
                         'City', 'Provider', 'Gender', 'Age', 'Birthyear', 'Birth_dayofmonth',
                         'Will_Vote', 'Vote_Ethusiasm', 'Vote_House', 'TEMP', 'Democrat_Opinion',
                         'Climate_Change_Government_Opinion', 'Topic_Importance', 'Clean_Regs_Health',
                         'Concern_Clean_Reg_Rollback', 'Drilling_Public_Lands', 'Drilling_Arctic_Refuge',
                         'Drilling_Coasts', 'Public_Health_Performance', 'Oil_Money', 'Trump_Sentiment',
                         'Party', 'Marital_Status', 'N_Children', 'Education', 'Employment', 'Career',
                         'Race', 'Income', 'Zip', 'Census_Region', 'Census_Division', 'Congressional_District',
                         'DMA_Code', 'DMA_Name', 'Music', 'Productivity', 'Bookworm', 'Socialite', 'Traveler',
                         'Sportsfan', 'Gamer', 'Value_Shopper', 'Foodie', 'Entertainment', 'Fashionista',
                         'Job_Seeker', 'Insurance', 'Real_Estate', 'Car_Purchase', 'ADID_IDFA')
  specific_data <- read_xls(filename, sheet='Individuals')
  
  varcolname <- ifelse(str_detect(colnames(specific_data)[17], 'job'),
                       'Republican_Job', 'Republican_Opinion')
  specific_colnames[17] <- varcolname
  colnames(specific_data) <- specific_colnames
  
  specific_data <- specific_data %>%
    # IMPUTATION HERE - REVIEW FOR MODELS
    mutate(Vote_House = case_when(Will_Vote == 'Definitely not' ~ 'Wont vote',
                                  str_detect(Vote_House, '^Republican') ~ 'Republican',
                                  str_detect(Vote_House, '^Democrat') ~ 'Democrat',
                                  Will_Vote == 'Other Candidate' ~ 'Other/Not Sure',
                                  Will_Vote == 'Undecided' ~ 'Other/Not Sure'),
           Vote_House = factor(Vote_House,
                               levels=c('Republican',
                                        'Democratic',
                                        'Other/Not sure',
                                        'Wont vote')),
           Trump_Sentiment = factor(Trump_Sentiment,
                                    levels=c('Approve Strongly',
                                             'Approve Weakly',
                                             'Neither Approve nor Disapprove',
                                             'Disapprove Weakly',
                                             'Disapprove Strongly'))) %>%
    select_(.dots = generic_colnames)
    
  specific_data
}


specific_df_list <- lapply(specific_filenames, read_specific_file)
specific <- bind_rows(specific_df_list)
rm(specific_df_list)

write_csv(generic, 'Data/generic_data.csv')
write_csv(specific, 'Data/specific_data.csv')

## population pre-processing
load('Data/projection_space_national_18-10_02.RData')
population <- pops %>%
  mutate(education = ifelse(education=='No college', 'No Bachelors', 'Bachelors')) %>%
  group_by(age, urbanicity, gender, state, race, education, married, party, congressional_district) %>%
  summarise(N = sum(N))
write_csv(population, 'Data/population_data.csv')
