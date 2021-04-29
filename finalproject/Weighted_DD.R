library(difR)
library(plyr)
library(dplyr)
library(tidyverse)
library(haven)
library(estimatr)

desert10<- read.csv('https://raw.githubusercontent.com/hannahjonesut/Causal-Inference/main/finalproject/usda_deserts_2013.csv')
desert15<- read.csv('https://raw.githubusercontent.com/hannahjonesut/Causal-Inference/main/finalproject/usda_deserts_2017.csv')

desert15 = select(desert15, 1:65)

desert15$year = 2015
desert10$year = 2010

desert_1015<- rbind.fill(desert10, desert15)

US_deserts<- desert_1015 %>%
  mutate(desert = ifelse(LATracts_half == 0 & LATracts1 == 0 & LATracts10 == 0 & LATracts20 == 0, 1, 0), 
         initial_desert = ifelse(year==2010 & desert == 1, 1, 0 ), final_desert = ifelse(year==2015 & desert == 1, 1, 0), treat = ifelse(initial_desert == 0 & final_desert == 1, 1, 0))

health_2013 <- read.csv("~/Downloads/500_Cities__Census_Tract-level_Data__GIS_Friendly_Format___2016_release.csv")
health_2017 <- read.csv("~/Downloads/500_Cities__Census_Tract-level_Data__GIS_Friendly_Format___2019_release.csv")

health_2013 = health_2013[,!grepl("_Crude95CI$",names(health_2013))]
health_2017 = health_2017[,!grepl("_Crude95CI$",names(health_2017))]

health_2013$year = 2013
health_2017$year = 2017

health_1317<- rbind.fill(health_2013, health_2017)

# Food desert is treatment -- does treatment treatment in 2017 cause worse health outcomes

US_dataset <- merge(US_deserts, health_1317, by.x = "CensusTract", by.y = "TractFIPS")
US_dataset <- select(US_dataset, -Rural)

# estimating LOGIT 
logit_desert <- glm(treat ~ Urban + POP2010 + OHU2010 + ACCESS2_CrudePrev + BINGE_CrudePrev
                    + BPHIGH_CrudePrev + CANCER_CrudePrev + CSMOKING_CrudePrev + DIABETES_CrudePrev + OBESITY_CrudePrev, 
                    family = binomial(link = "logit"), 
                    data = US_dataset)

logit_usdata_pscore  <- US_dataset  %>% 
  mutate(pscore = ifelse(treat == 0 | treat == 1, logit_desert$fitted.values, "NA"))

# mean pscore 
logit_sumstat<- logit_usdata_pscore %>% 
  group_by(treat)%>% 
  summarize(mean = mean(pscore), 
            max = max(pscore),
            min = min(pscore))
logit_sumstat

# histogram
logit_usdata_pscore %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

logit_usdata_pscore %>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

logit_usdata_pscore %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

logit_usdata_pscore %>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

#drop pscores <0.1 and >0.9
cut_logit_data<- logit_usdata_pscore %>%
  filter(pscore > 0.1 & pscore < 0.9)

#repeat 1c
cut_logit_data %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

cut_logit_data %>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

cut_logit_data %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

cut_logit_data %>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))

#logit averages

sumstat_cut_logit_data<- cut_logit_data %>% 
  group_by(treat)%>% 
  summarize(logit_mean = mean(pscore), 
            logit_max = max(pscore), 
            logit_min = min(pscore))
sumstat_cut_logit_data

#number 2: Using trimmed Logit with covariates

#LOGIT

cut_logit_data %>% 
  filter(treat == 1) %>% 
  summary(OBESITY_CrudePrev)

mean1 <- cut_logit_data%>% 
  filter(treat == 1) %>% 
  pull(OBESITY_CrudePrev) %>% 
  mean()

cut_logit_data$y1 <- mean1

cut_logit_data%>% 
  filter(treat == 0) %>% 
  summary(OBESITY_CrudePrev)

mean0 <- cut_logit_data%>% 
  filter(treat == 0) %>% 
  pull(OBESITY_CrudePrev) %>% 
  mean()

cut_logit_data$y0 <- mean0

ate <- unique(cut_logit_data$y1 - cut_logit_data$y0)

cut_logit_data <- cut_logit_data%>% 
  select(-y1, -y0)

#LOGIT
#continuation
N <- nrow(cut_logit_data )
#- Manual with non-normalized weights using all data
cut_logit_data  <- cut_logit_data  %>% 
  mutate(d1 = treat/pscore,
         d0 = (1-treat)/(1-pscore))

s1 <- sum(cut_logit_data$d1)
s0 <- sum(cut_logit_data$d0)


cut_logit_data<- cut_logit_data %>% 
  mutate(y1 = treat * OBESITY_CrudePrev/pscore,
         y0 = (1-treat) * OBESITY_CrudePrev/(1-pscore),
         ht = y1 - y0)

#- Manual with normalized weights
cut_logit_data <- cut_logit_data  %>% 
  mutate(y1 = (treat*OBESITY_CrudePrev/pscore)/(s1/N),
         y0 = ((1-treat)*OBESITY_CrudePrev/(1-pscore))/(s0/N),
         norm = y1 - y0)

cut_logit_data  %>% 
  pull(ht) %>% 
  mean()

cut_logit_data %>% 
  pull(norm) %>% 
  mean()
