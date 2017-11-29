#Reads in the original dataset downloaded from 
#Crime = read.csv('Crimes_-_2001_to_present.csv')


library(dplyr)
library(lubridate)

#Filter for year 2010 to present (2017) and clear some columns. Saved resulting data

Crime = Crime %>%
  filter(Year >= 2010 & !is.na(Latitude)) %>%
  mutate(Arrest = as.numeric(Arrest == 'true'), Domestic = as.numeric(Domestic == 'true'), Date = as.POSIXct(strptime(Date, "%m/%d/%Y %H:%M:%S %p")))
head(Crime)
write.csv(Crime, file="Crime_2010_to_2017.csv")


#Take a random sample to work with
Crime = read.csv('Crime_2010_to_2017.csv')
Crime_sample = Crime[sample(1:nrow(Crime), 10000,
                             replace=FALSE),]
write.csv(Crime_sample, file='Crime_sample_2010_to_2017.csv')


#Format data after loading it in RStudio everytime
Crime_sample = Crime_sample %>%
  mutate(Date = as.POSIXct(strptime(Date, "%Y-%m-%d %H:%M:%S"))) %>%
  filter(!is.na(Latitude))


#Clean police stops data
Police <- read.csv('Police_stops_1-2016_2-2017_full.csv')

#keep only a few variables
kept_columns <- c('CONTACT_CARD_ID', 'CONTACT_DATE', 'CONTACT_HOUR', 'SUBMITTING_BEAT_CD','SEX_CODE_CD','RACE_CODE_CD','ZIP_CD','BEAT','ENFORCEMENT_ACTION_TAKEN_I','COCAINE_I','HEROIN_I','CANNABIS_AMOUNT', 'OTHER_CON_SUB', 'SEARCH_COCAINE_I', 'SEARCH_HEROIN_I', 'SEARCH_CANNABIS_I', 'SEARCH_OTHER_CON_SUB_I', 'PARA_I', 'S_PARA_I')
drug_i <- c('COCAINE_I','HEROIN_I','CANNABIS_AMOUNT', 'OTHER_CON_SUB', 'SEARCH_COCAINE_I', 'SEARCH_HEROIN_I', 'SEARCH_CANNABIS_I', 'SEARCH_OTHER_CON_SUB_I', 'PARA_I', 'S_PARA_I', 'ENFORCEMENT_ACTION_TAKEN_I')
Police <- Police %>% select(one_of(kept_columns)) %>%
  filter(!grepl("REDACTED", RACE_CODE_CD)) %>%
  na.omit() %>%
  mutate_at(drug_i, function(x){as.numeric(x == "Y")}) %>%
  mutate(DRUG_I = as.numeric(rowSums(.[10:19]) > 0))
write.csv(Police, file="Police_stops_2016_2017.csv")