
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

# upload weather data
weather_raw <- read.csv("C:/Users/jam19e/dataschool/cotton_canopy/data/ACRI_2001_2017.csv") %>% 
  filter(between(Year,2015, 2016))

str(weather_raw)
head(weather_raw)

colnames(weather_raw) <- c("datetime", "Year", "Day","Hour",
                           "maxAT","minAT", "maxST","minST","maxRH","minRH", "WS", "WD","MaxWS", "solar_rad", "rain","tot_rain")

#Plot weather data

ggplot(weather_raw , aes(datetime, maxAT)) + geom_smooth()

#weather <- NULL
#system.time(

weather <- weather_raw  %>% 
  mutate(datetime = as.POSIXct(datetime, format="%d/%m/%Y %H:%M" , tz = "Australia/Brisbane")) %>% 
  #select(datetime, Year, Day, Hour, maxAT , minAT, maxRH , minRH, WS, solar_rad, rain)%>%
  #group_by(datetime = ceiling_date(datetime, "15 mins")) %>% 
  #group_by(datetime) %>% 
  #summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  mutate(Year = year(datetime),
         Day = yday(datetime),
         Hour = hour(datetime),
         Minute = minute(datetime),
         meanAT = (maxAT + minAT)/2, 
         meanRH = (maxRH + minRH)/2) %>% 
  select(Year, Day, Hour, meanAT, meanRH, WS, solar_rad, rain)

tz(weather$datetime)
head(weather)

#Plot weather data

ggplot(weather , aes(datetime, meanAT)) + geom_smooth()
  


# Specify and load the sensor files
# Set the working directory where the data is stored
#setwd(file.path("C:/Users/jam19e/dataschool/cotton_canopy/data/canopy/sensors/")) or use actual file path
#Create a file that lists filenames

file_names <- list.files(path = "C:/Users/jam19e/dataschool/cotton_canopy/data/canopy", 
                         full.names = T, pattern = "*.csv") 

# Create a new dataframe that combines all sensor files and adds a column with sensor ID
sensor_all <- data_frame(filename = file_names) %>%
  mutate(file_contents = map(filename, ~ read_csv(., skip= 1, col_names = F)))

# add column names using REGEX
# remove .csv from name
# select required three columns 
sensor_extract <- unnest(sensor_all) %>% 
  rename(datetime = X1, 
         canopytemp = X2) %>% 
  mutate(ID = str_remove(filename,"[:graph:]+/")) %>% 
  mutate(ID = str_remove(ID,".csv")) %>%
  mutate(datetime = dmy_hm(datetime, tz = "Australia/Sydney")) %>% 
  filter(between(canopytemp, 5, 50)) %>% #remove bad data points
  select(ID, datetime, canopytemp)

# Graphs for individual sensors
ggplot(data = sensor_extract,
       aes(x=datetime, y= canopytemp))+
  geom_line(size = 1)+
  facet_wrap(~ID)

# Create a df with mean CT at 15 min time intervals

canopy_temp <- sensor_extract %>% 
  group_by(ID, datetime = ceiling_date(datetime, "15 mins")) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  mutate(Datetime = force_tz(datetime, tz = "Australia/Brisbane"),
         Year = year(datetime),
         Day = yday(datetime),
         Hour = hour(datetime)) %>% 
  mutate(Treatment=ifelse(ID =="2015_ACRI_LW_3"|ID =="2015_ACRI_LW_5", "Irrigated","Stressed"))%>% 
  select(ID, datetime, Treatment, Year, Day, Hour, canopytemp) %>% 
  ungroup()



#Combine CT and weather data
canopy_weather <- full_join(canopy_temp,weather) %>% 
  filter(!is.na(ID))
  
# Visulaize weather and canopy temperature data

ggplot(data=subset(canopy_weather, Day%in% c(1)),
       aes(x=datetime, y= meanAT))+
  geom_line()+
  facet_wrap(~ID)



# Model data
lm_CT <- lm(canopytemp~meanAT + ID, data=canopy_weather) #
anova(lm_CT)
summary(lm_CT)

lmer_CT <- lmer(sqrt(canopytemp)~meanAT+ meanRH + solar_rad + (1|ID) , canopy_weather)
anova(lmer_CT)
summary(lmer_CT)
plot(lmer_CT)

emmeans(lmer_CT, pairwise~canopytemp|ID)



