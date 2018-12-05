library(tidyverse)

results <- read_csv('Data/2018_results.csv', na=c("", "NA", "\x97")) %>%
  select(-DISTRICT, -CD, -State) %>%
  rename_all(.funs=tolower) %>%
  rename(democrat = dem.,
         republican = rep.,
         other = oth.,
         x_rpt = x..rpt.,
         district = dist) %>%
  mutate_at(.vars = vars(democrat, republican, other, x_rpt),
            .funs = funs(as.numeric(str_sub(., end=-2))/100)) %>%
  # mutate_at(.vars = vars(democrat, republican, other),
  #           .funs = funs(ifelse(is.na(.), 0, .))) %>%
  mutate(state = str_sub(district, 1, 2),
         republican_share = republican/(republican+democrat),
         democrat_share = democrat/(republican+democrat)) %>%
  filter(x_rpt > 0 & !is.na(republican) & !is.na(democrat))

plot_model_eval <- function(pred_df, results_df){
  results <- results_df %>%
    select(state, district, republican_share) %>%
    inner_join(., pred_df, by='district')
  rmse <- results %>%
    summarise(rmse=sqrt(mean((Vote_R - republican_share)^2))) %>%
    mutate(rmse = round(rmse, 3))
  print(sqrt(mean((results$Vote_R - results$republican_share)^2)))
  ggplot(results, aes(x=Vote_R, y=republican_share)) +
  geom_point() +
  annotate("text", x = 0.1, y = 1, label = paste('RMSE:', rmse))
}


generic_glmer <- read_csv('Output/generic_mrp_glmer_results.csv') %>%
  filter(Demographic_Type == 'District') %>%
  select(district=Demographic,
         Vote_R)
plot_model_eval(generic_glmer, results)

specific_glmer <- read_csv('Output/specific_mrp_glmer_results.csv') %>%
  filter(Demographic_Type == 'District') %>%
  select(district=Demographic,
         Vote_R)
plot_model_eval(specific_glmer, results)


generic_glmer_slope <- read_csv('Output/generic_mrp_glmer_results_slopes.csv') %>%
  filter(Demographic_Type == 'District') %>%
  select(district=Demographic,
         Vote_R)
plot_model_eval(generic_glmer_slope, results)

specific_glmer_slope <- read_csv('Output/specific_mrp_glmer_results_slopes.csv') %>%
  filter(Demographic_Type == 'District') %>%
  select(district=Demographic,
         Vote_R)
plot_model_eval(specific_glmer_slope, results)


generic_multinom <- read_csv('Output/generic_mrp_multinom_results.csv',
                              col_types=cols(Vote_R = col_double())) %>%
  filter(Demographic_Type == 'District') %>%
  select(district=Demographic,
         Vote_R) %>%
  filter(!is.na(district))
plot_model_eval(generic_multinom, results)

specific_multinom <- read_csv('Output/specific_mrp_multinom_results.csv',
                              col_types=cols(Vote_R = col_double())) %>%
  filter(Demographic_Type == 'District') %>%
  select(district=Demographic,
         Vote_R) %>%
  filter(!is.na(district))
plot_model_eval(specific_multinom, results)


generic_all_intercepts <- read_csv('Output/generic_mrp_all_intercepts_results.csv',
                              col_types=cols(Vote_R = col_double())) %>%
  filter(Demographic_Type == 'District') %>%
  select(district=Demographic,
         Vote_R) %>%
  filter(!is.na(district))
plot_model_eval(generic_all_intercepts, results)

specific_all_intercepts <- read_csv('Output/specific_mrp_all_intercepts_results.csv',
                              col_types=cols(Vote_R = col_double())) %>%
  filter(Demographic_Type == 'District') %>%
  select(district=Demographic,
         Vote_R) %>%
  filter(!is.na(district))
plot_model_eval(specific_all_intercepts, results)

