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

###### OPENING FILE
df = read.table("Weed_Price.csv",header=TRUE,sep=",",dec=".",stringsAsFactors = TRUE)
# any(is.na(data))
attach(df)
summary(df)
head(df)
df$date <- as.Date(df$date)


###### COMPLETING MISSING VALUES WITH LAST NONE NA VALUE
df = df[with(df, order(State,date)),]
na.locf(df)


###### PLOTING North Dakota VALUES x=date[value==max(value)], y=max(value)
North_Dakota = cbind.data.frame(as.Date(date[State=="North Dakota"]), HighQ[State=="North Dakota"])
North_Dakota_names = c("North_Dakota_date","North_Dakota_HighQ")
# summary(North Dakota)
names(North_Dakota) = North_Dakota_names
attach(North_Dakota)

#Interactive ploting North Dakota
ggplot(North_Dakota, aes(North_Dakota_date, North_Dakota_HighQ)) +
  geom_line() +
  geom_area(color="black", fill="red") +
  ylim(0,max(North_Dakota_HighQ)+0.2*max(North_Dakota_HighQ)) +
  annotate("text", x=date[HighQ==max(HighQ)]+150, y=max(HighQ)+20,
    label=("Highest price : North Dakota, 1 oz. (28g), 415$")) +
  annotate(geom="point", x=date[HighQ==max(HighQ)], y=max(HighQ), shape=21, size=10, fill="transparent")

# Interactive graph
don <- xts(x = North_Dakota_HighQ, order.by = North_Dakota_date)
# graph
dygraph(don) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)




###### Lollipo chart : Mean by state, HighQ, MedQ and LowQ
Mean_HighQ_state = aggregate(HighQ~State, data=df, FUN=function(df) c(mean=mean(df), count=length(df)))
Mean_MedQ_state = aggregate(MedQ~State, data=df, FUN=function(df) c(mean=mean(df), count=length(df)))
Mean_LowQ_state = aggregate(LowQ~State, data=df, FUN=function(df) c(mean=mean(df), count=length(df)))

geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) 

# Reorder
Mean_HighQ_state %>%
  arrange(Mean_HighQ_state$HighQ.mean) %>%
  mutate(State=factor(Mean_HighQ_state$State,Mean_HighQ_state$State)) %>%
  ggplot( aes(x=Mean_HighQ_state$State, y=Mean_HighQ_state$HighQ.mean)) +
    geom_segment( aes(x=Mean_HighQ_state$State, xend=Mean_HighQ_state$State, y=0, yend=Mean_HighQ_state$HighQ), color="skyblue") +
    geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) +
    theme_light() +
    coord_flip() +
    # theme(
    #   panel.grid.major.y = element_blank(),
    #   panel.border = element_blank(),
    #   axis.ticks.y = element_blank()
    # ) +
    xlab("")
    # ylab("Value of Y")

data %>%
  filter(!is.na(Value)) %>%
  arrange(Value) %>%
  mutate(Country=factor(Country, Country)) %>%
  ggplot( aes(x=Country, y=Value) ) +
  geom_segment( aes(x=Country ,xend=Country, y=0, yend=Value), color="grey") +
  geom_point(size=3, color="#69b3a2") +
  coord_flip() +
  xlab("")

# IN CASE OF : Error in .Call.graphics(C_palette2, .Call(C_palette2, NULL)) : invalid graphics state
# USE : dev.off()


###### OTHER FUNCTIONS
sum(is.na(LowQN))
df[is.na(LowQ)]


#  Kangda graphs1(Tendance de la croissance Nevada HighQN 2014-01-01)
df %>%
  filter(State=="Nevada" ) %>%
  ggplot( aes(x=date, y=HighQN)) +
  geom_line()

# Kangda graphs2(treemap 2014-01-01)
df1=read.table("Weed_Price.csv",header=TRUE,sep=",",dec=".")
df2=df1[c(1:51),]
df2$date <- as.Date(df2$date)
library(treemap)

treemap(df2,
        index="State",
        vSize="HighQN",
        type="index"
)