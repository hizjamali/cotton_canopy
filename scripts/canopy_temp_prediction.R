
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
  filter(between(Year,2015, 2016))

str(weather_raw)
head(weather_raw)

colnames(weather_raw) <- c("datetime", "Year", "Day","Hour",
                           "maxAT","minAT", "maxST","minST","maxRH","minRH", "WS", "WD","MaxWS", "solar_rad", "rain","tot_rain")


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

tz(weather_raw$datetime)
head(weather)

#Plot weather data

ggplot(weather , aes(datetime, meanAT)) + geom_smooth()
  


# Specify and load the sensor files
# Set the working directory where the data is stored
#setwd(file.path("C:/Users/jam19e/dataschool/cotton_canopy/data/canopy/sensors/")) or use actual file path
#Create a file that lists filenames

file_names <- list.files(path = "C:/Users/jam19e/dataschool/cotton_canopy/data/canopy/sensors/", 
                         full.names = T, pattern = "*.csv") %>% 

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
  select(ID, datetime, Year, Day, Hour, canopytemp) %>% 
  ungroup()



#Combine CT and weather data
canopy_weather <- full_join(canopy_temp,weather)

# Visulaize weather and canopy temperature data

ggplot(data = canopy_weather,
       aes(x=meanAT, y= canopytemp))+
  geom_line(size = 1)+
  facet_wrap(~ID) %>% 
filter(between(Day,1, 60))


# Model data
lm_bubbles <- lm(biomass_g_per_pot~treatment + block + position, data=bubbles) #
anova(lm_bubbles)
summary(lm_bubbles)

lmer_bubbles <- lmer(biomass_g_per_pot~treatment+(1|block) + position, data=bubbles)
anova(lmer_bubbles)
summary(lmer_bubbles)
plot(lmer_bubbles)

emmeans(lmer_bubbles, pairwise~treatment|species)



