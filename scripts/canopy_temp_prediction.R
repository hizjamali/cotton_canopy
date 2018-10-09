
library(tidyverse)
library(ggplot2)
library(emmeans)
library(reshape2)
library(lmerTest)
library(nlme)
library(lubridate)
library(readr)

agro <- read.csv(("data/cotton_canopy_sensor_agronomic_data.csv")) 
str(agro)
View(agro)

# convert block to a factor (from integer)
# overwrites column keeping column name same
agro$lint_yield_kg_per_ha <- factor(agro$lint_yield_kg_per_ha)
class(agro$lint_yield_kg_per_ha)

# load weather data
weather_raw <- read.csv("data/ACRI_2001_2017.csv")
str(weather_raw)
View(weather_raw)

weather <- weather_raw  %>% 
  mutate(DateTime = as.POSIXct(DateTime, format="%d/%m/%Y %I:%M:%S %p")) %>% 
  group_by(DateTime = as.POSIXct(ceiling(as.numeric(DateTime) / (15*60)) * (15*60), origin='1970-01-01', tz = "Australia/Sydney")) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  mutate(Year = year(Datetime),
         Day = yday(Datetime),
         Hour = hour(Datetime),
         Minute = minute(Datetime),
         meanAT = (maxAT + minAT)/2, 
         meanRH = (maxRH + minRH)/2) %>% 
  select(Year, Day, Hour, meanAT, meanRH, rain, solar_rad)
