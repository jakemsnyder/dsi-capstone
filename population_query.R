library(data.world)

# data.world::set_config(data.world::save_config(auth_token = "YOUR API TOKEN"))

education_data <- 'https://data.world/uscensusbureau/acs-2016-5-e-education/workspace/file?filename=USA_All_States.csv'
# age, sex, racial info also exists here
# US census does not capture vocational_technical_college. they consider them as the highest
# level of education attained outside of that, i.e. high school, university


agesex <- 'https://data.world/uscensusbureau/acs-2016-5-e-agesex'
# racial info also exists here

# keep in mind we want _voting_ population, so we need age in every query from census to filter children
dw_query <- data.world::qry_sql(
  "SELECT state as state_abb, areaname as state,
    b01001_007, b01001_008, b01001_009, b01001_010, -- male 18-24
    b01001_011, b01001_012, -- male 25-34
    b01001_013, b01001_014, -- male 35-44
    b01001_015, b01001_016, -- male 45-54
    b01001_017, b01001_018, b01001_019, b01001_020, b01001_021, b01001_022, b01001_023, b01001_024, b01001_025, -- male 55+
    b01001_031, b01001_032, b01001_033, b01001_034, -- female 18-24
    b01001_035, b01001_036, -- female 25-34
    b01001_037, b01001_038, -- female 35-44
    b01001_039, b01001_040, -- female 45-54
    b01001_041, b01001_042, b01001_043, b01001_044, b01001_045, b01001_046, b01001_047, b01001_048, b01001_049 -- female 55+
  FROM USA_All_States")

demo_data <- data.world::query(dw_query, dataset=agesex) %>%
  mutate(male_18_24 = b01001_007 + b01001_008 + b01001_009 + b01001_010,
         male_25_34 = b01001_011 + b01001_012,
         male_35_44 = b01001_013 + b01001_014,
         male_45_54 = b01001_015 + b01001_016,
         male_55_plus = b01001_017 + b01001_018 + b01001_019 + b01001_020 + b01001_021 +
           b01001_022 + b01001_023 + b01001_024 + b01001_025,
         female_18_24 = b01001_031 + b01001_032 + b01001_033 + b01001_034,
         female_25_34 = b01001_035 + b01001_036,
         female_35_44 = b01001_037 + b01001_038,
         female_45_54 = b01001_039 + b01001_040,
         female_55_plus = b01001_041 + b01001_042 + b01001_043 + b01001_044 + b01001_045 +
           b01001_046 + b01001_047 + b01001_048 + b01001_049) %>%
  select(-starts_with('b0')) %>%
  gather('demographic', 'population', -state_abb, -state) %>%
  separate(demographic, c('gender', 'age_lower', 'age_upper')) %>%
  unite('age', c('age_lower', 'age_upper'), sep=' - ') %>%
  mutate(age = ifelse(age == '55 - plus', '> 54', age))
