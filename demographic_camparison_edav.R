library(tidyverse)
library(gridExtra)

#############################################################
## Polling v.s. population -- Demographics                 ##
#############################################################
# setwd("D:/Capstone_GitHub/dsi-capstone/")

# create generic demo
generic <- read.csv("Data/all_generic_with_districts.csv")

age.poll <- generic %>%
  group_by(age) %>%
  summarize(Num=n()) %>%
  mutate(Prop=Num/sum(Num), Type='generic', Var='Age', Cat=as.numeric(age), Label=age) %>%
  ungroup()
gender.poll <- generic %>%
  group_by(gender) %>%
  summarize(Num=n()) %>%
  mutate(Prop=Num/sum(Num), Type='generic', Var='Gender', Cat=as.numeric(gender), Label=gender) %>%
  ungroup()
race.poll <- generic %>%
  group_by(race) %>%
  summarize(Num=n()) %>%
  mutate(Prop=Num/sum(Num), Type='generic', Var='Race', Cat=as.numeric(race), Label=race) %>%
  ungroup()
mar.poll <- generic %>%
  group_by(married) %>%
  summarize(Num=n()) %>%
  mutate(Prop=Num/sum(Num), Type='generic', Var='Marital Status', Cat=as.numeric(married), Label=married) %>%
  ungroup()
edu.poll <- generic %>%
  group_by(education) %>%
  summarize(Num=n()) %>%
  mutate(Prop=Num/sum(Num), Type='generic', Var='Education', Cat=as.numeric(education), Label=education) %>%
  ungroup()

generic_demographics <- rbind(age.poll[, 2:7], gender.poll[, 2:7], race.poll[, 2:7],
                           mar.poll[, 2:7], edu.poll[, 2:7])

# create specific demo 
specific <- read.csv("Data/all_specific.csv")

age.spec <- specific %>%
  group_by(age) %>%
  summarize(Num=n()) %>%
  mutate(Prop=Num/sum(Num), Type='specific', Var='Age', Cat=as.numeric(age), Label=age) %>%
  ungroup()
gender.spec <- specific %>%
  group_by(gender) %>%
  summarize(Num=n()) %>%
  mutate(Prop=Num/sum(Num), Type='specific', Var='Gender', Cat=as.numeric(gender), Label=gender) %>%
  ungroup()
race.spec <- specific %>%
  group_by(race) %>%
  summarize(Num=n()) %>%
  mutate(Prop=Num/sum(Num), Type='specific', Var='Race', Cat=as.numeric(race), Label=race) %>%
  ungroup()
mar.spec <- specific %>%
  group_by(married) %>%
  summarize(Num=n()) %>%
  mutate(Prop=Num/sum(Num), Type='specific', Var='Marital Status', Cat=as.numeric(married), Label=married) %>%
  ungroup()
edu.spec <- specific %>%
  group_by(education) %>%
  summarize(Num=n()) %>%
  mutate(Prop=Num/sum(Num), Type='specific', Var='Education', Cat=as.numeric(education), Label=education) %>%
  ungroup()

specific_demographics <- rbind(age.spec[, 2:7], gender.spec[, 2:7], race.spec[, 2:7],
                              mar.spec[, 2:7], edu.spec[, 2:7])


# create population demo
load("Data/projection_space_national_18-10_02.RData")

age.pop <- pops %>%
  group_by(age) %>%
  summarize(Num=sum(N)) %>%
  mutate(Prop=Num/sum(Num), Type='pop', Var='Age', Cat=as.numeric(age), Label=age) %>%
  ungroup()
gender.pop <- pops %>%
  group_by(gender) %>%
  summarize(Num=sum(N)) %>%
  mutate(Prop=Num/sum(Num), Type='pop', Var='Gender', Cat=as.numeric(gender), Label=gender) %>%
  ungroup()
race.pop <- pops %>%
  group_by(race) %>%
  summarize(Num=sum(N)) %>%
  mutate(Prop=Num/sum(Num), Type='pop', Var='Race', Cat=as.numeric(race), Label=race) %>%
  ungroup()
mar.pop <- pops %>%
  group_by(married) %>%
  summarize(Num=sum(N)) %>%
  mutate(Prop=Num/sum(Num), Type='pop', Var='Marital Status', Cat=as.numeric(married), Label=married) %>%
  ungroup()
edu.pop <- pops %>%
  group_by(education) %>%
  summarize(Num=sum(N)) %>%
  mutate(Prop=Num/sum(Num), Type='pop', Var='Education', Cat=as.numeric(education), Label=education) %>%
  ungroup()
pop_demographics <- rbind(age.pop[, 2:7], gender.pop[, 2:7], race.pop[, 2:7],
                               mar.pop[, 2:7], edu.pop[, 2:7])

# combine all for one plot
plot_demographics <- rbind(pop_demographics, generic_demographics, specific_demographics)

# oops, did something wrong with the category, remap them
plot_demographics <- within(plot_demographics, Cat[Label == '18 - 24'] <- '1')
plot_demographics <- within(plot_demographics, Cat[Label == '25 - 34'] <- '2')
plot_demographics <- within(plot_demographics, Cat[Label == '35 - 44'] <- '3')
plot_demographics <- within(plot_demographics, Cat[Label == '45 - 54'] <- '4')
plot_demographics <- within(plot_demographics, Cat[Label == '> 54'] <- '5')
plot_demographics <- within(plot_demographics, Cat[Label == 'Male'] <- '2')
plot_demographics <- within(plot_demographics, Cat[Label == 'Female'] <- '1')
plot_demographics <- within(plot_demographics, Cat[Label == 'Black'] <- '1')
plot_demographics <- within(plot_demographics, Cat[Label == 'Hispanic'] <- '2')
plot_demographics <- within(plot_demographics, Cat[Label == 'White'] <- '3')
plot_demographics <- within(plot_demographics, Cat[Label == 'Other'] <- '4')
plot_demographics <- within(plot_demographics, Cat[Label == 'Male'] <- '2')
plot_demographics <- within(plot_demographics, Cat[Label == 'Married'] <- '1')
plot_demographics <- within(plot_demographics, Cat[Label == 'Unmarried'] <- '2')
plot_demographics <- within(plot_demographics, Cat[Label == 'Bachelor'] <- '1')
plot_demographics <- within(plot_demographics, Cat[Label == 'No Bachelor'] <- '2')
plot_demographics[plot_demographics == 'College'] <- "Bachelors"
plot_demographics[plot_demographics == 'No college'] <- "No Bachelors"

ggplot(data=plot_demographics, aes(x=as.factor(paste(Cat, Label, sep='\n')),
                                   y=Prop, group=as.factor(Type), linetype=as.factor(Type))) +
  geom_point(stat="identity") +
  geom_line() +
  facet_wrap(~Var, scales="free", nrow=1, ncol=6) +
  theme_bw() +
  scale_fill_manual(values=c('#1f78b4','#33a02c', '#e31a1c', '#ff7f00', '#8856a7'),guide=FALSE) +
  scale_y_continuous(breaks=c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1),
                     labels=c('0%','10%', '20%','30%', '40%', '50%', '60%', '70%', '80%', '90%', '100%')) +
  scale_alpha_manual(values=c(1, .3)) +
  ylab('Proportion') +
  labs(alpha='') +
  theme(legend.position="bottom",
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size=10, angle=20, hjust=1),
        strip.text=element_text(size=10),
        strip.background = element_rect(fill='grey92'),
        panel.grid = element_line(colour="grey", size=0.2))

ggsave("plots/demographic_comparison.jpg", width=16, height=8)
