library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(pastecs)
library(corrplot)
library(plyr)
library(PerformanceAnalytics)

##statistics
ATMOS_Stat <- read.csv("E:/CourseWork/Fall2021_BSYSE_540_Instrumentation/Lab_Reports/Lab3_ATMOS/ATMOS_Stat2.csv")
Statistics_ATMOS<- stat.desc(ATMOS_Stat)
write.csv(Statistics_ATMOS, "E:/CourseWork/Fall2021_BSYSE_540_Instrumentation/Lab_Reports/Lab3_ATMOS/ATMOS_Stat_Output.csv")
pairs(ATMOS_Stat[,4:7])
ATMOS_Stat_Analytics <- ATMOS_Stat[c(4:7)]
chart.Correlation(ATMOS_Stat_Analytics, histogram=TRUE, pch=19)
str(ATMOS_Stat)
ATMOS_Stat|> 
  mutate(Time = as.POSIXct(hms::parse_hm(Time))) |> 
  ggplot(aes(x=Time, y=Vapor_Pressure)) + 
  geom_line(color="darkgreen") +geom_point(color="darkgreen")+theme_bw(base_size=18)+
  ylab("Vapor Pressure (kPa)")+xlab(" Time (minutes) ")+
  ggtitle("Variation of Vapor Pressure with time")
  scale_x_datetime(date_labels = "%H:%M")
  
ATMOS_Stat|> 
    mutate(Time = as.POSIXct(hms::parse_hm(Time))) |> 
    ggplot(aes(x=Time, y=Solar )) + 
    geom_line(color="firebrick4") +geom_point(color="firebrick4")+theme_bw(base_size=18)+
  ylab("Solar Radiation (W/m^2)")+xlab(" Time (minutes) ")+
    ggtitle("Variation of Solar Radiation with time")
  scale_x_datetime(date_labels = "%H:%M")
  
ATMOS_Stat|> 
  mutate(Time = as.POSIXct(hms::parse_hm(Time))) |> 
  ggplot(aes(x=Time, y=AirTemp)) +
  geom_line(color="blue4") +geom_point(color="blue4")+ theme_bw(base_size=18)+
  ylab("Air Temperature (deg C)")+xlab(" Time (minutes) ")+
  ggtitle("Variation of Air Temperature with time")
  scale_x_datetime(date_labels = "%H:%M")

ATMOS_Stat|> 
  mutate(Time = as.POSIXct(hms::parse_hm(Time))) |> 
  ggplot(aes(x=Time, y=RelHumidity )) +
  geom_line(color="darkcyan") + geom_point(color="darkcyan")+  theme_bw(base_size=18)+
  ylab("Relative Humidity (%)")+xlab(" Time (minutes) ")+
  ggtitle("Variation of Relative Humidity with time")
  scale_x_datetime(date_labels = "%H:%M")

ATMOS_Stat|> 
  mutate(Time = as.POSIXct(hms::parse_hm(Time))) |> 
  ggplot(aes(x=Time, y=WindSpeed)) +
  geom_line(color="magenta") +geom_point(color="magenta")+
  theme_bw(base_size=18)+
  ylab("Wind Speed (m/s)")+xlab(" Time (minutes) ")+
    ggtitle("Variation of Wind Speed with time")
  scale_x_datetime(date_labels = "%H:%M")

ATMOS_C <- read.csv("E:/CourseWork/Fall2021_BSYSE_540_Instrumentation/Lab_Reports/Lab3_ATMOS/ATMOS_Clean 2.csv")
  str(ATMOS_C)
  ggplot(ATMOS_C , aes(Minutes , Value )) + 
    geom_line(color="blue") + 
    facet_wrap(~Variable, scales = "free")+geom_point(color="blue")+theme_bw()+ggtitle(('Variation of parameters measured wrt time'))
  