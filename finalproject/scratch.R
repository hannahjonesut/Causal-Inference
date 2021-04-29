colnames(desert10) <- paste(colnames(desert10), "2010", sep = "_")
colnames(desert15) <- paste(colnames(desert15), "2015", sep = "_")

merge(desert10, desert15,  by.x = "CensusTract_2010", by.y = "CensusTract_2015")

colnames(health_2013) <- paste(colnames(health_2013), "2013", sep = "_")
colnames(health_2017) <- paste(colnames(health_2017), "2017", sep = "_")

merge(health_2013, health_2017, by.x = "TractFIPS_2013", by.y = "TractFIPS_2017")

health_1317<- health_1317%>%
  mutate(BPHIGH_dif = BPHIGH_CrudePrev_2017-BPHIGH_CrudePrev_2013, DIABETES_dif = DIABETES_CrudePrev_2017-DIABETES_CrudePrev_2013,
         OBESITY_dif = OBESITY_CrudePrev_2017-OBESITY_CrudePrev_2013, HIGHCHOL_dif = HIGHCHOL_CrudePrev_2017-HIGHCHOL_CrudePrev_2013)

TX_health_1317<- health_1317%>%
  filter(StateAbbr_2013 == "TX")

TX_desert_1015<- desert_1015%>%
  filter(State_2010 == "TX")


TX_dataset <- merge(TX_desert_1015, TX_health_1317, by.x = "CensusTract_2010", by.y = "TractFIPS_2013")

LATracts_half_2010 = ifelse(LATracts_half_2010 == 1, 0.5, 0), 
LATracts1_2010 = ifelse(LATracts1_2010 == 1, 1, 0), 
LATracts10_2010 = ifelse(LATracts10_2010 == 1, 10, 0), 
LATracts20_2010 = ifelse(LATracts20_2010 == 1, 20, 0), 
urbandistance2010 = LATracts_half_2010+LATracts1_2010, 
ruraldistance2010 = LATracts10_2010+LATracts20_2010, 
urbanfooddesert2010 = ifelse(Urban_2010 == 1 & urbandistance2010 == 0, 1, 0),
ruralfooddesert2010 = ifelse(Rural_2010 == 1 & ruraldistance2010 == 0, 1, 0),
LATracts_half_2015 = ifelse(LATracts_half_2015 == 1, 0.5, 0), 
LATracts1_2015 = ifelse(LATracts1_2015 == 1, 1, 0), 
LATracts10_2015 = ifelse(LATracts10_2015 == 1, 10, 0), 
LATracts20_2015 = ifelse(LATracts20_2015 == 1, 20, 0), 
urbandistance2015 = LATracts_half_2015+LATracts1_2015, 
ruraldistance2015 = LATracts10_2015+LATracts20_2015, 
urbanfooddesert2015 = ifelse(Urban_2015 == 1 & urbandistance2015 == 0, 1, 0),
ruralfooddesert2015 = ifelse(Urban_2015 == 0 & ruraldistance2015 == 0, 1, 0),



desert_1015 = select(desert_1015, 1:66)

LowIncomeTracts + PovertyRate + MedianFamilyIncome + 

initial_desert = ifelse(year==2010 & desert == 1, 1, 0 ), 
final_desert = ifelse(year==2015 & desert == 1, 1, 0)

US_dataset_treated <- US_dataset %>%
  group_by(CensusTract)%>%
  filter(initial_desert == 0 & final_desert == 1)

US_desert_untreated <- US_dataset %>%
  group_by(CensusTract) %>%
  filter(initial_desert == 1 & final_desert == 1) 

US_nondesert_untreated <- US_dataset %>%
  group_by(CensusTract)%>%
  filter(initial_desert == 0 & final_desert == 0) 