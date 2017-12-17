#Author: Phuc Nguyen, Jina Park
#Date: 12/16/2017

#----------------------------------------------------------------------------------

#Format Data of CRIME COUNT PER ZIPCODE for building Bayesian Models

# Load Library
library(tidyr)
library(rjags)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(MacBayes)
library(choroplethrZip)
library(rgdal)
library(devtools)
library(choroplethr)
library(choroplethrMaps)
library(mapproj)
library(grid)
library(gridExtra)


##Uncomment these lines if interested in doing the same analysis for just violent or nonviolent crimes
#violent = c("ROBBERY", "BATTERY", "BURGLARY", "ASSAULT", "HOMICIDE", "ARSON", "CRIM SEXUAL ASSAULT", "SEX OFFENSE")

##To do analysis on Violent crime
#Crime <- Crime %>%
#  filter(Primary.Type %in% violent)

##To do analysis on Non-violent crime
#Crime <- Crime %>%
#  filter(!(Primary.Type %in% violent))

#### Project observations to ZIP codes boundaries

#load sample of crime incident data that have been cleaned using codes in clean_data.R

Crime = read.csv('Crime_sample_2010_to_2017.csv')

#load shapefile
zip_shp <- readOGR(dsn="Boundaries - ZIP Codes", layer="geo_export_e1262361-5c82-45ee-8427-ef228e06dc4a")
temp <- zip_shp
zip_df <- fortify(temp, region = "zip") #turn shapefile into dataframe

#needs to reassign CRS for shapefile
new_CRS <- CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
zip_shp <- sp::spTransform(zip_shp, new_CRS)

#summarize number of crime count per year per zipcode
years <- unique(Crime$Year)
Hotspot_Crime <- data.frame()

for(i in 1:length(years)){
  year <- years[i]
  Current_Year <- Crime %>% filter(Year == year)
  
  locations <- with(Current_Year, as.data.frame(cbind(Longitude, Latitude)))
  coordinates(locations) <- ~Longitude+Latitude
  proj4string(locations) <- CRS("+init=epsg:4326")
  
  #make sure shapefile and crime data points have compatible CRS
  proj4string(zip_shp) <- new_CRS
  proj4string(locations) <- new_CRS
  #project data points to polygons
  by_zip <- over(locations, zip_shp)
  
  #count crime per zipcode
  by_zip <- by_zip %>%
    group_by(zip) %>%
    dplyr::summarise(total=n()) %>%
    filter(!is.na(zip)) %>%
    mutate(id = as.character(zip))
  
  #merge crime count per zipcode with shapefile for mapping
  total_map <- left_join(zip_df, by_zip)
  total_map <- total_map %>% mutate(Year = year)
  
  Hotspot_Crime <- rbind(Hotspot_Crime, total_map)
}


# Add demographic information to Model Data

data("df_zip_demographics")
Demographics <- df_zip_demographics %>% mutate(zip = region)
Hotspot_Crime <- inner_join(Hotspot_Crime, Demographics, by="zip") 
Model_Data <- Hotspot_Crime %>%
  select(-c(lat, long, order, piece, group, hole)) %>% distinct



# Nesting data, standardize year variable and create groups of zipcodes based on percentage white 

Model_Data <- transform(Model_Data, zipid=match(zip, unique(zip)))
Model_Data$time = Model_Data$Year-2009
Model_Data$racegr <- ifelse(Model_Data$percent_white >80, 1, 
                            ifelse(Model_Data$percent_white <=80 & Model_Data$percent_white > 60,2,
                                   ifelse(Model_Data$percent_white <=60 & Model_Data$percent_white > 40,3,
                                          ifelse(Model_Data$percent_white <=40 & Model_Data$percent_white > 20,4, 5)))) 
Model_Data$income = scale(Model_Data$per_capita_income)[,1]

# Save model data
#write.csv(Model_Data, "Crime_models_data.csv")
#write.csv(Model_Data, "Crime_models_violent_data.csv")
#write.csv(Model_Data, "Crime_models_nonviolent_data.csv")