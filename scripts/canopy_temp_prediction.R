
library(tidyverse)
library(ggplot2)
library(emmeans)
library(reshape2)
library(lmerTest)
library(nlme)
library(lubridate)
library(readr)

agro <- read.csv("C:/Users/jam19e/dataschool/cotton_canopy/data/cotton_canopy_sensor_agronomic_data.csv") 
str(agro)
View(agro)

# convert block to a factor (from integer)
# overwrites column keeping column name same
agro$lint_yield_kg_per_ha <- factor(agro$lint_yield_kg_per_ha)
class(agro$lint_yield_kg_per_ha)

# load weather data
weather_raw <- read.csv("data/ACRI_2001_2017.csv") %>% 
  filter(Year>=2014)

str(weather_raw)
head(weather_raw)

colnames(weather_raw) <- c("Datetime", "Year", "Day","Hour",
                           "maxAT","minAT", "maxST","minST","maxRH","minRH", "WS", "WD","MaxWS", "solar_rad", "rain","tot_rain","wtf")

weather <- weather_raw  %>% 
  mutate(datetime = as.POSIXct(Datetime, format="%d/%m/%Y %I:%M:%S %p" , tz = "Australia/Brisbane")) %>% 
  group_by(Datetime) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  mutate(Year = year(datetime),
         Day = yday(datetime),
         Hour = hour(datetime),
         Minute = minute(datetime),
         meanAT = (maxAT + minAT)/2, 
         meanRH = (maxRH + minRH)/2) %>% 
  select(Year, Day, Hour, meanAT, meanRH, rain, solar_rad, WS, rain)

tz(weather$Datetime)

ggplot(weather , aes(Hour, meanAT)) + geom_smooth()


# Specify and load the sensor files
# Set the working directory where the data is stored

#setwd(file.path("C:/Users/jam19e/dataschool/cotton_canopy/data/canopy/sensors/"))
file_names <- list.files("C:/Users/jam19e/dataschool/cotton_canopy/data/canopy/sensors/", full.names = T,pattern = "*.csv")
df <- map_df(file_names, read.csv) %>% 
  bind_rows()
df

#https://stackoverflow.com/questions/46299777/add-filename-column-to-table-as-multiple-files-are-read-and-bound








SensorExtract <- df %>% 
  mutate(canopytemp = as.numeric(Canopytemp),
         Datetime = ymd_hms(Datetime, tz = "Australia/Sydney")) %>% 
  select(Datetime, Canopytemp) %>% 
  #filter(Datetime >= "2018/03/05"& Datetime <= "2018/06/01") %>% 
  filter(Canopytemp >= 5 & Canopytemp <= 50) %>% 
  #mutate(ID = as.factor(ID),
         #Datetime = floor_date(Datetime,unit = "minutes"),
         #Datetime = force_tz(Datetime, tz = timezone)) %>% #averaging repeat values in same minute
  group_by(Datetime) %>%
  summarise(Canopytemp = mean(Canopytemp))

SensorExtract





canopy_temp <- SensorExtract_trt %>% 
  group_by(ID, Datetime = ceiling_date(Datetime, "15 mins")) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  mutate(Datetime = force_tz(Datetime, tz = timezone),
         Year = year(Datetime),
         Day = yday(Datetime),
         Hour = (hour(Datetime)) ) %>% 
  select(ID, Datetime, Year, Day, Hour, CanopyTemp) %>% 
  ungroup() 


colnames(canopy_temp) <- c("ID",
                           "Datetime", "Year", "Day","Hour",
                           "TempCT")
