
# StressTime load data 

# Chris Nunn

# Created:
# 25/7/17

# Last edited:
# 25/7/17

# clear R of all objects
rm(list=ls())

# Packages:
library(tidyverse)
library(readxl)
library(lubridate)
# library(ggpmisc)
# library(stringr)
library(rebus)


#setwd("H:/R scripts/StressTime")

setwd("\\\\nexus.csiro.au\\csiro\\Agriculture\\Operations Agriculture\\Myall Vale\\Groups\\COTPhys\\AusBIOTIC database\\Definative_cT_Raw\\For Chris\\1617_ACRI")

timezone <-  "Australia/Sydney"



##===========================================

#~~~~~~~~~~~~~~~~~~~~~~
# Load the Sensor Data (sensorDB files)

file_names <- list.files(full.names = T,pattern = "*.csv", recursive = T)

df <- data_frame(filename = file_names) %>%
  mutate(file_contents = map(filename, ~ read_csv(file.path(getwd(), .),
                                                  skip= 0,
                                                  col_names = TRUE)) # a new data column
  )
 

SensorExtract_sensorDB <- unnest(df) %>% 
  mutate(ID = str_remove(filename,"./")) %>% 
  mutate(ID = str_remove(ID,".csv")) %>%
  mutate(ID = str_remove(ID,or("s","S"))) %>% 
  mutate(CanopyTemp = ifelse(is.na(CanopyTemp), Canopytemp, CanopyTemp)) %>% 
  mutate(Datetime = dmy_hm(Datetime)) %>% 
  select(ID, Datetime, CanopyTemp)

SensorExtract_SDcard <- SensorExtract %>% 
  rename(CanopyTempSD = CanopyTemp) %>% 
  mutate(ID = as.character(ID))

SensorExtract_combined <- full_join(SensorExtract_sensorDB, SensorExtract_SDcard) %>% 
  filter(Datetime < Sys.time()) %>% 
  #filter(Datetime >= "2017/12/01"& Datetime <= "2018/12/01") %>% 
  filter(between(CanopyTemp, 5, 50)) %>% 
  mutate(Datetime = floor_date(Datetime, unit = "minutes"),
         Datetime = force_tz(Datetime, tz = timezone)) %>% #averaging repeat values in same minute
  group_by(ID, Datetime = ceiling_date(Datetime, "15 mins")) %>% 
  summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  mutate(CanopyTemp)


#~~~~~~~~~~~~~~~~~~~~~~~~~
# Graphs Below
#~~~~~~~~~~~~~~~~~~~~~~~~~

ggplot(data = SensorExtract_combined,
       aes(x=Datetime, y= CanopyTemp))+
  geom_point(size = 1)+
  facet_wrap(~ID)


ggplot(data = filter(SensorExtract_combined, ID == "1204"),
       aes(x=Datetime, y= CanopyTemp))+
  geom_point(size = 1)+
  geom_point(aes(y = CanopyTempSD), colour = "red", size =1)
                

#~~~~~~~~~~~~~~~~~~~~~~~~~

metadata <- read_excel("..\\..\\definitive canopy temperature.xlsx", sheet = 1)  %>% 
  mutate(ID = str_remove(`Sensor ID`,or("s","S")))

sensors_and_meta <- metadata %>% 
  select(ID = `Sensor ID`,
         Location,
         Plantingyear = `Planting Year`,
         rowConfig =  `Row Configuration`,
         yield = `Lint Yield kg/ha`,
         experimentName = `Experiment Name`,
         Treatment, 
         plantingDOY = `Planting Date (DOY)`) %>% 
  filter(Plantingyear == 2016) %>% 
  right_join(SensorExtract_combined) %>% 
  mutate(CanopyTemp_patch1 = ifelse(is.na(CanopyTemp), CanopyTempSD, CanopyTemp))  

sensors_gap_patching <- sensors_and_meta %>% 
  group_by(Location, Datetime, Plantingyear, rowConfig, experimentName, Treatment, plantingDOY) %>% 
  summarise(canopyTempTreatMean = mean(CanopyTemp_patch1, na.rm = T)) %>% 
  mutate(Date = date(Datetime))

min(SensorExtract_combined$Datetime) - max(SensorExtract_combined$Datetime)


date_range <- data.frame(Datetime = seq(min(SensorExtract_combined$Datetime), max(SensorExtract_combined$Datetime), by = '15 min'))

sensors_patched <- sensors_gap_patching %>% 
  full_join(date_range) %>% 
  distinct() %>% 
  full_join(sensors_and_meta) %>% 
  mutate(canopyTempFilled = ifelse(is.na(CanopyTemp_patch1), canopyTempTreatMean, CanopyTemp_patch1)) %>% 
  distinct()

ggplot(sensors_patched, aes(x=Datetime, y = ))

