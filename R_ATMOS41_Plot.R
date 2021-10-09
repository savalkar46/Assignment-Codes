##title: "ATMOS41_Plot"
##author: "Supriya Savalkar"
##date: "10/06/2021"
## convert to markdown later

library(tidyverse)
library(ggplot2)
library(lubridate)
library(plyr)
ATMOS_Stat <- read.csv("E:/CourseWork/Fall2021_BSYSE_540_Instrumentation/Lab_Reports/Lab3_ATMOS/ATMOS_Stat2.csv")
ATMOS_Stat|> 
  mutate(Time = as.POSIXct(hms::parse_hm(Time))) |> 
  ggplot(aes(x=Time, y=Solar )) + 
  geom_line(color="firebrick4") +geom_point(color="firebrick4")+
  ggtitle("Variation of Solar Radiation with time")
scale_x_datetime(date_labels = "%H:%M")

ATMOS_Stat|> 
  mutate(Time = as.POSIXct(hms::parse_hm(Time))) |> 
  ggplot(aes(x=Time, y=AirTemp)) +
  geom_line(color="blue4") +geom_point(color="blue4")+ ggtitle("Variation of Air Temperature with time")
scale_x_datetime(date_labels = "%H:%M")