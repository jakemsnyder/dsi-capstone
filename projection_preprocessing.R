# setwd('D:/Capstone_GitHub/data/')

library(tidyverse)
library(dplyr)
library(readxl)
load("projection_space_national_18-10_02.RData")

generic <- voting2018
summary(generic)

# lowercase batch 1 columns to match 2nd batch
names(generic) <- tolower(names(generic))
colnames(generic)

# The columns we would like to include:
generic_colnames <- c("party", "gender", "age",
                      "race", "education", "urbanicity",
                      "vote2018", "married","wave")

# To allow different Pollfish names
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

# load batch2
survey1 <- loadRData("Pollfish_Survey_FI10002d.378826.moreifo_completed2.RData")
survey2 <- loadRData("Pollfish_Survey_FI10006b.378910.moreifo_completed2.RData")
survey3 <- loadRData("Pollfish_Survey_IN10001a.406052.moreifo_completed2.RData")
survey4 <- loadRData("Pollfish_Survey_IN10004.324889.moreifo_completed2.RData")
survey5 <- loadRData("Pollfish_Survey_IN10005.324886.moreifo_completed2.RData")
survey6 <- loadRData("Pollfish_Survey_IN10006.324887.moreifo_completed2.RData")
survey7 <- loadRData("Pollfish_Survey_IN10007b.378647.moreifo_completed2.RData")
survey8 <- loadRData("Pollfish_Survey_SC10001a.338700.moreifo_completed2.RData")
survey9 <- loadRData("Pollfish_Survey_SC10002a.338699.moreifo_completed2.RData")
survey10 <- loadRData("Pollfish_Survey_SC10003b.375937.moreifo_completed2.RData")
survey11 <- loadRData("Pollfish_Survey_SC10004c.375939.moreifo_completed2.RData")
survey12 <- loadRData("Pollfish_Survey_SC10005a.338697.moreifo_completed2.RData")

# select columns for all data
col_select <- function(x) { 
  x %>% select(generic_colnames)
}

datasets <- list(generic, survey1, survey2, survey3, survey4,
              survey5, survey6, survey7, survey8,
              survey9, survey10, survey11, survey12)

all_generic <- NULL
for (i in datasets){
  i <- i %>% select(generic_colnames)
  all_generic <- rbind(all_generic, i)
}

save(all_generic, file = "all_generic.RData")
write.csv(all_generic, 'all_generic.csv')
#load("all_generic.RData")







