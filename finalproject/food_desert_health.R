library(tidyverse)
library(haven)
library(estimatr)
library(difR)
library(plyr)
library(dplyr)

#make combined food desert dataset 


desert10<- read.csv('https://raw.githubusercontent.com/hannahjonesut/Causal-Inference/main/finalproject/usda_deserts_2013.csv')
desert15<- read.csv('https://raw.githubusercontent.com/hannahjonesut/Causal-Inference/main/finalproject/usda_deserts_2017.csv')

desert15 = select(desert15, 1:65)

desert15$year = 2015
desert10$year = 2010

desert_1015<- rbind.fill(desert10, desert15)
desert_1015 = select(desert_1015, 1:66)

US_deserts<- desert_1015 %>%
  mutate(desert = ifelse(LATracts_half == 0 & LATracts1 == 0 & LATracts10 == 0 & LATracts20 == 0, 1, 0), 
         initial_desert = ifelse(year==2010 & desert == 1, 1, 0 ), final_desert = ifelse(year==2015 & desert == 1, 1, 0))

deserts_over_time <- US_deserts%>%
  group_by(year) %>%
  summarize(half_urban = sum(LATracts_half), one_urban = sum(LATracts1),ten_rural = sum(LATracts10), 
            twenty_rural = sum(LATracts20), desert_any = sum(desert) )


#make combined health dataset
health_2013 <- read.csv("~/Downloads/500_Cities__Census_Tract-level_Data__GIS_Friendly_Format___2016_release.csv")
health_2017 <- read.csv("~/Downloads/500_Cities__Census_Tract-level_Data__GIS_Friendly_Format___2019_release.csv")

health_2013 = health_2013[,!grepl("_Crude95CI$",names(health_2013))]
health_2017 = health_2017[,!grepl("_Crude95CI$",names(health_2017))]

health_2013$year = 2013
health_2017$year = 2017

health_1317<- rbind.fill(health_2013, health_2017)

# Food desert is treatment -- does treatment treatment in 2017 cause worse health outcomes

US_dataset <- merge(US_deserts, health_1317, by.x = "CensusTract", by.y = "TractFIPS")

avg_obesity<- US_dataset%>%
  group_by(year.y, initial_desert, final_desert)%>%
  summarize(avg_obesity = mean(OBESITY_CrudePrev))


US_dataset_treated <- US_dataset %>%
  group_by(CensusTract)%>%
  filter(initial_desert == 0 & final_desert == 1)

avg_obesity_treated <- US_dataset_treated %>%
  group_by(year.y)%>%
  summarize(avg_obesity = mean(OBESITY_CrudePrev))


US_nondesert_untreated <- US_dataset %>%
  group_by(CensusTract)%>%
  filter(initial_desert == 0 & final_desert == 0) 

avg_obesity_untreated <- US_nondesert_untreated %>%
  group_by(year.y)%>%
  summarize(avg_obesity = mean(OBESITY_CrudePrev))

ggplot() +
  geom_line(data = avg_obesity_untreated, aes(x = year.y, y = avg_obesity))+
  geom_line(data = avg_obesity_treated, aes(x = year.y, y = avg_obesity), color = 'blue')

