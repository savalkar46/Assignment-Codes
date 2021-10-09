library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(pastecs)
library(corrplot)
library(plyr)
library(PerformanceAnalytics)

ATMOS<- read.csv("E:/CourseWork/Fall2021_BSYSE_540_Instrumentation/Lab_Reports/Lab3_ATMOS/ATMOS_Data.csv")
str(ATMOS)
ATMOS <- ATMOS |> separate(TIMESTAMP, c("Month", "Day", "Year"), sep = "/")
ATMOS <- ATMOS |> separate(Year, c("Year", "Time"), sep = " ")
ATMOS <- ATMOS |> separate(Time, c("Hour", "Minute"), sep = ":")

## Solar modification
Solar <- ATMOS |> group_by(Hour, Minute) |> select(Solar) 
Solar <- Solar |> unite(Time, Hour, Minute, sep = ":")
Solar$Time <- as.POSIXct(Solar$Time, format="%H:%M")
cut(Solar$Time, breaks="5 min")
Solar <- aggregate(Solar["Solar"], 
                   list(fiveMin=cut(Solar$Time, "5 mins")),
                   mean)|> separate(fiveMin, c("Day", "Time"), sep = " ")
Solar |> 
  mutate(Time = as.POSIXct(hms::parse_hm(Time))) |> 
  ggplot(aes(Time, Solar)) +
  geom_line(color="orange") +
  scale_x_datetime(date_labels = "%H:%M")

##Vapor Pressure
Vapor_Pressure <- ATMOS |> group_by(Hour, Minute) |> select(Vapor_Pressure) 
Vapor_Pressure <- Vapor_Pressure |> unite(Time, Hour, Minute, sep = ":")
Vapor_Pressure$Time <- as.POSIXct(Vapor_Pressure$Time, format="%H:%M")
cut(Vapor_Pressure$Time, breaks="5 min")
Vapor_Pressure<- aggregate(Vapor_Pressure["Vapor_Pressure"], 
                   list(fiveMin=cut(Vapor_Pressure$Time, "5 mins")),
                   mean)|> separate(fiveMin, c("Day", "Time"), sep = " ")
Vapor_Pressure |> 
  mutate(Time = as.POSIXct(hms::parse_hm(Time))) |> 
  ggplot(aes(Time, Vapor_Pressure)) +
  geom_line(color="orange") +
  scale_x_datetime(date_labels = "%H:%M")

## AirTemp modification
AirTemp <- ATMOS |> group_by(Hour, Minute) |> select(AirTemp)  
AirTemp <- AirTemp |> unite(Time, Hour, Minute, sep = ":")
AirTemp$Time <- as.POSIXct(AirTemp$Time, format="%H:%M")
cut(AirTemp$Time, breaks="5 min")
AirTemp<- aggregate(AirTemp["AirTemp"], 
                   list(fiveMin=cut(AirTemp$Time, "5 mins")),
                   mean) |> separate(fiveMin, c("Day", "Time"), sep = " ")
AirTemp |> 
  mutate(Time = as.POSIXct(hms::parse_hm(Time))) |> 
  ggplot(aes(Time, AirTemp)) +
  geom_line(color="blue") +
  scale_x_datetime(date_labels = "%H:%M")

## Relative humidity modification
RelHumidity <- ATMOS |> group_by(Hour, Minute) |> select(RelHumidity )  
RelHumidity <- RelHumidity |> unite(Time, Hour, Minute, sep = ":")
RelHumidity$Time <- as.POSIXct(RelHumidity$Time, format="%H:%M")
cut(RelHumidity$Time, breaks="5 min")
RelHumidity<- aggregate(RelHumidity["RelHumidity"], 
                    list(fiveMin=cut(RelHumidity$Time, "5 mins")),
                    mean) |> separate(fiveMin, c("Day", "Time"), sep = " ")
RelHumidity |> 
  mutate(Time = as.POSIXct(hms::parse_hm(Time))) |> 
  ggplot(aes(Time, RelHumidity)) +
  geom_line(color="blue") +
  scale_x_datetime(date_labels = "%H:%M")

##WindSpeed modification
WindSpeed<- ATMOS |> group_by(Hour, Minute) |> select(WindSpeed)  
WindSpeed<- WindSpeed|> unite(Time, Hour, Minute, sep = ":")
WindSpeed$Time <- as.POSIXct(WindSpeed$Time, format="%H:%M")
cut(WindSpeed$Time, breaks="5 min")
WindSpeed<- aggregate(WindSpeed["WindSpeed"], 
                        list(fiveMin=cut(WindSpeed$Time, "5 mins")),
                        mean) |> separate(fiveMin, c("Day", "Time"), sep = " ")
WindSpeed|> 
  mutate(Time = as.POSIXct(hms::parse_hm(Time))) |> 
  ggplot(aes(Time, WindSpeed)) +
  geom_line(color="blue") +
  scale_x_datetime(date_labels = "%H:%M")

## Join cleaned data

WindSpeed$Time <- colnames(WindSpeed)
RelHumidity$Time <- colnames(RelHumidity)
AirTemp$Time <- colnames(AirTemp)
Solar$Time<- colnames(Solar)
Vapor_Pressure$Time<- colnames(Vapor_Pressure)

ATMOS_clean<- join_all(list(Solar,AirTemp,Vapor_Pressure,RelHumidity,WindSpeed), by = 'Time', type = 'full')
##ATMOS_clean$Time_N <- seq.POSIXt(ATMOS_clean$Time, by = "5 min")

#write.csv(ATMOS, "E:/CourseWork/Fall2021_BSYSE_540_Instrumentation/Lab_Reports/Lab3_ATMOS/ATMOS_BC.csv")

write.csv(ATMOS_clean, "E:/CourseWork/Fall2021_BSYSE_540_Instrumentation/Lab_Reports/Lab3_ATMOS/1.csv")

ATMOS_C <- read.csv("E:/CourseWork/Fall2021_BSYSE_540_Instrumentation/Lab_Reports/Lab3_ATMOS/ATMOS_Clean2.csv")
str(ATMOS_C)
ggplot(ATMOS_C , aes(Minutes , Value )) + 
  geom_line(color="blue") + 
  facet_wrap(~Variable, scales = "free")+geom_point(color="blue")+theme_bw()+ggtitle(('Variation of parameters measured wrt time'))


##statistics
ATMOS_Stat <- read.csv("E:/CourseWork/Fall2021_BSYSE_540_Instrumentation/Lab_Reports/Lab3_ATMOS/ATMOS_Stat2.csv")
Statistics_ATMOS<- stat.desc(ATMOS_Stat)
pairs(ATMOS_Stat[,4:8])
ATMOS_Stat_Analytics <- ATMOS_Stat[c(4:8)]
chart.Correlation(ATMOS_Stat_Analytics, histogram=TRUE, pch=19)