library(zoo)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(plotly)
library(tidyverse)


###### OPENING FILE
df = read.table("Weed_Price.csv",header=TRUE,sep=",",dec=".",stringsAsFactors = TRUE)
# any(is.na(data))
attach(df)
summary(df)
head(df)
date <- as.Date(date)


###### COMPLETING MISSING VALUES WITH LAST NONE NA VALUE
df = df[with(df, order(State,date)),]
na.locf(df)

###### PLOTING ALABAMA VALUES
ggplot(data2, aes(date, value)) +
  geom_line() +
  geom_area(color="black", fill="red") +
  annotate("text", x=as.Date("2017-01-01"), y=19000, label=("Bitcoin value over time"))

###### OTHER FUNCTIONS
sum(is.na(LowQN))
df[is.na(LowQ)]
