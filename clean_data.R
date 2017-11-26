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

