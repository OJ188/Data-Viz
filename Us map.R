# Alabama,32.7794°N,86.8287°W
# Alaska,64.0685°N,152.2782°W
# Arizona,34.2744°N,111.6602°W
# Arkansas,34.8938°N,92.4426°W
# California,37.1841°N,119.4696°W
# Colorado,38.9972°N,105.5478°W
# Connecticut,41.6219°N,72.7273°W
# Delaware,38.9896°N,75.5050°W
# District of Columbia,38.9101°N,77.0147°W
# Florida,28.6305°N,82.4497°W
# Georgia,32.6415°N,83.4426°W,[6]
# Hawaii,20.2927°N,156.3737°W
# Idaho,44.3509°N,114.6130°W
# Illinois,40.0417°N,89.1965°W
# Indiana,39.8942°N,86.2816°W
# Iowa,42.0751°N,93.4960°W
# Kansas,38.4937°N,98.3804°W
# Kentucky,37.5347°N,85.3021°W
# Louisiana,31.0689°N,91.9968°W
# Maine,45.3695°N,69.2428°W
# Maryland,39.0550°N,76.7909°W
# Massachusetts,42.2596°N,71.8083°W
# Michigan,44.3467°N,85.4102°W
# Minnesota,46.2807°N,94.3053°W
# Mississippi,32.7364°N,89.6678°W
# Missouri,38.3566°N,92.4580°W
# Montana,47.0527°N,109.6333°W
# Nebraska,41.5378°N,99.7951°W
# Nevada,39.3289°N,116.6312°W
# New Hampshire,43.6805°N,71.5811°W
# New Jersey,40.1907°N,74.6728°W
# New Mexico,34.4071°N,106.1126°W
# New York,42.9538°N,75.5268°W
# North Carolina,35.5557°N,79.3877°W
# North Dakota,47.4501°N,100.4659°W
# Ohio,40.2862°N,82.7937°W
# Oklahoma,35.5889°N,97.4943°W
# Oregon,43.9336°N,120.5583°W
# Pennsylvania,40.8781°N,77.7996°W
# Rhode Island,41.6762°N,71.5562°W
# South Carolina,33.9169°N,80.8964°W
# South Dakota,44.4443°N,100.2263°W
# Tennessee,35.8580°N,86.3505°W
# Texas,31.4757°N,99.3312°W
# Utah,39.3055°N,111.6703°W
# Vermont,44.0687°N,72.6658°W
# Virginia,37.5215°N,78.8537°W
# Washington,47.3826°N,120.4472°W
# West Virginia,38.6409°N,80.6227°W
# Wisconsin,44.6243°N,89.9941°W
# Wyoming,42.9957°N,107.5512°W




setwd("C:/Users/Théo/Desktop/GitHub/Weed-Veez")
# Library
library(zoo)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(plotly)
library(tidyverse)
library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(lubridate)
library(rlist)
library(treemap)
###### OPENING FILE
df = read.table("Weed_Price.csv",header=TRUE,sep=",",dec=".",stringsAsFactors = TRUE)
attach(df)
df = df[!(State =="District of Columbia"),]
State = unique(df$State)
date <- as.Date(date)

###### COMPLETING MISSING VALUES WITH LAST NONE NA VALUE
df = df[with(df, order(State,df$date)),]
na.locf(df)


Mean_state = data.frame(aggregate(df$HighQ~df$State, data=df, 
                                    FUN=function(df) c(mean=mean(df), count=length(df))))
HighQ = Mean_state$df.HighQ[1:50]
Mean_state = data.frame(aggregate(df$MedQ~df$State, data=df, 
                                    FUN=function(df) c(mean=mean(df), count=length(df))))
MedQ = Mean_state$df.MedQ[1:50]
Mean_state = data.frame(aggregate(df$LowQ~df$State, data=df, 
                                    FUN=function(df) c(mean=mean(df), count=length(df))))
LowQ = Mean_state$df.LowQ[1:50]
Mean_state = data.frame(aggregate(
  (df$HighQ+df$MedQ+df$LowQ)/3~df$State, data=df, 
  FUN=function(df) c(mean=mean(df), count=length(df))))
MeanQ = Mean_state$X.df.HighQ...df.MedQ...df.LowQ..3[1:50]


df = df[!(State =="District of Columbia"),]
State = unique(df$State)




# US map

# library(plotly)
# setwd("C:/Users/Théo/Desktop/GitHub/Weed-Veez")
# dfm = read.table("USmap.csv",header=TRUE,sep=",",dec=".",stringsAsFactors = FALSE)
dfr <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")
# dfr$hover <- with(df, paste(df$State, "<br>", "High Quality", df$HighQ, "Medium Quality", df$MedQ,
                            # "Low Quality", df$LowQ))
dfr$hover = paste(State, "<br>", "High Quality", HighQ, "Medium Quality", MedQ,
                            "Low Quality", LowQ)
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

p <- plot_geo(dfr, locationmode = 'USA-states') %>%
  # add_trace(
  #   z = ~total.exports, text = ~hover, locations = ~code,
  #   color = ~total.exports, colors = 'Purples'
  # ) %>%
  add_trace(
    z = ~MeanQ, text = ~hover, locations = ~code,
    color = ~MeanQ, colors = 'Purples'
  ) %>%
  colorbar(title = "$/oz.") %>%
  layout(
    title = 'Mean of weed prices per state ($/oz.)',
    geo = g
  )
