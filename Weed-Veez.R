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
date <- as.Date(date)


###### COMPLETING MISSING VALUES WITH LAST NONE NA VALUE
df = df[with(df, order(State,date)),]
na.locf(df)


###### PLOTING Washington VALUES
Washington = cbind.data.frame(as.Date(date[State=="Washington"]), 
                              HighQ[State=="Washington"])
Washington_names = c("Washington_date","Washington_HighQ")
names(Washington) = Washington_names
attach(Washington)

label_Washington = paste(c("Highest price in", "Washington : 1oz.",max(HighQ),"$"), 
                         collapse = " ")
label_dygraph = paste(c("Price fluctuation in Washington", 
                      collapse = " "))
# Ploting Washington
ggplot(Washington, aes(Washington_date, Washington_HighQ)) +
  geom_line() +
  ylim(min(Washington_HighQ)-0.2*min(Washington_HighQ),
       max(Washington_HighQ)+0.2*max(Washington_HighQ)) +
  geom_area(color="black", fill="red") +
  annotate("text", x=date[HighQ==max(HighQ)], y=max(HighQ),
    label=label_Washington) +
  annotate(geom="point", x=date[HighQ==max(HighQ)], 
           y=max(HighQ), shape=21, size=10, fill="transparent")


# Interactive graph
don <- xts(x = Washington_HighQ, order.by = Washington_date)
# graph
dygraph(don, main=label_dygraph) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, 
            drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, 
              hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)




###### Lollipop chart : Mean by state, HighQ, MedQ, LowQ and average

# 1) HighQ 2) MedQ 3) LowQ 4) Average of the 3
# Choix = 1
Choix = 2
# Choix = 3
# Choix = 4


if(Choix==1)
{
  Mean_state = data.frame(aggregate(HighQ~State, data=df, 
                                    FUN=function(df) c(mean=mean(df), count=length(df))))
  Mean_state$HighQ = Mean_state$HighQ[1:51]
  Mean_state=Mean_state[order(Mean_state[,2]),]
  Q = Mean_state$HighQ
  title = "High Quality"
}
if(Choix==2)
{
  Mean_state = data.frame(aggregate(MedQ~State, data=df, 
                                    FUN=function(df) c(mean=mean(df), count=length(df))))
  Mean_state$MedQ = Mean_state$MedQ[1:51]
  Mean_state=Mean_state[order(Mean_state[,2]),]
  Q = Mean_state$MedQ
  title = "Medium Quality"
} 
if(Choix==3)
{
  Mean_state = data.frame(aggregate(LowQ~State, data=df, 
                                    FUN=function(df) c(mean=mean(df), count=length(df))))
  Mean_state$LowQ = Mean_state$LowQ[1:51]
  Mean_state=Mean_state[order(Mean_state[,2]),]
  Q = Mean_state$LowQ
  title = "Low Quality"
}
if(Choix==4)
{
  Mean_state = data.frame(aggregate(
    (HighQ+MedQ+LowQ)/3~State, data=df, 
    FUN=function(df) c(mean=mean(df), count=length(df))))
  Mean_state$X.HighQ...MedQ...LowQ..3 = Mean_state$X.HighQ...MedQ...LowQ..3[1:51]
  Mean_state=Mean_state[order(Mean_state[,2]),]
  Q = Mean_state$X.HighQ...MedQ...LowQ..3
  title = "Average of the 3 qualities"
}


State = as.character(Mean_state$State)

label_max = paste(c(State[Q==max(Q)],":", ceiling(max(Q)),"$/oz."), collapse = " ")
label_mean = paste(c("Mean :", ceiling(mean(Q)),"$/oz."), collapse = " ")
label_min = paste(c(State[Q==min(Q)], ceiling(min(Q)),"$/oz."), collapse = " ")
label_median = paste(c("Median :", State[Q==median(Q)],"with", ceiling(median(Q)),"$/oz."), collapse = " ")
label_y = paste(c("Mean price per state :", title), collapse = " ")

Mean_state %>%
  arrange(Q) %>%
  mutate(State=factor(State,State)) %>%
  ggplot(aes(x=State, y=Q)) +
  geom_segment( aes(x=State, xend=State, y=0, yend=Q), 
                color=ifelse((State==State[Q==median(Q)]), "orange", "skyblue"), 
                  size=ifelse((State==State[Q==median(Q)]), 1.3, 0.7)) +
  geom_point( color=ifelse((State==State[Q==median(Q)]), "orange", "red"), 
              size=3, fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) +
  theme_light() +
  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  ylab(label_y) +
  
  annotate("text", x=State[Q==max(Q)], y=max(Q) - 130,
           label=label_max, color="red") +
  annotate(geom="point", State[Q==max(Q)], y=max(Q), 
           shape=21, size=10, fill="transparent", color="red") +
  
  annotate("text", x=State[Q==min(Q)], y=150,
           label=label_min, color="red") +
  annotate(geom="point", State[Q==min(Q)], y=min(Q), 
           shape=21, size=10, fill="transparent", color="red") +
  
  geom_hline(yintercept=mean(Q), color="blue", size=.5) +
  annotate("text", x=State[Q==min(Q)], y=max(Q) - 90,
           label=label_mean, color="blue", size=4, fontface="bold") +
  
  annotate("text", x = State[Q==median(Q)], y = median(Q)*0.8, 
    label=label_median, color="black", size=4 , angle=0, fontface="bold", hjust=0)






                                         



# IN CASE OF : Error in .Call.graphics(C_palette2, .Call(C_palette2, NULL)) : invalid graphics state
# USE : dev.off()


###### OTHER FUNCTIONS
sum(is.na(LowQN))
df[is.na(LowQ)]
summary(df)
head(df)
any(is.na(data))



States_list = c("Alabama","Indiana","Iowa","Wyoming","Mississippi")
# States_list = c("Indiana","Iowa","Wyoming","Mississippi")
# list_test = list()
# df_violin = data.frame(subset(df, State == "Alabama"))
# df_violin <- data.frame(State=character(),
                 # HighQ=double(),
                 # MedQ=double(),
                 # LowQ=double(),
                 # date=as.Date(character()), 
                 # stringsAsFactors=FALSE) 
State_list = data.frame(State=character(),stringsAsFactors = FALSE)
HighQ_list = data.frame(HighQ=double(),stringsAsFactors = FALSE)
MedQ_list = data.frame(MedQ=double(),stringsAsFactors = FALSE)
LowQ_list = data.frame(LowQ=double(),stringsAsFactors = FALSE)
date_list = data.frame(date=as.Date(character()),stringsAsFactors = FALSE)
                       
for(i in States_list)
{
  # df_violin = cbind(df_violin,subset(df, State == i))
  df_violin = subset(df, State == i)
  State_list = rbind(State_list,df_violin[1])
  HighQ_list = rbind(HighQ_list,df_violin[2])
  MedQ_list = rbind(MedQ_list,df_violin[4])
  LowQ_list = rbind(LowQ_list,df_violin[6])
  date_list = rbind(date_list,df_violin[8])
  # df_violin = df_violin[with(df_violin, order(df_violin$State,df_violin$date)),]
  # na.locf(df_violin)
  # State_list = list.append(State_list,df_violin[1])
  # HighQ_list = list.append(HighQ_list,df_violin[2])
  # MedQ_list = list.append(MedQ_list,df_violin[4])
  # LowQ_list = list.append(LowQ_list,df_violin[6])
  # date_list = list.append(date_list,df_violin[8])
  # list_test = list.append(list_test,subset(df, State == i))
}
# Mean_state = data.frame(aggregate(MedQ~State, data=df, 
                                  # FUN=function(df) c(mean=mean(df), count=length(df))))

# df_violin=df_violin[order(Violin[,2]),]

# df_violin = cbind(State_list,HighQ_list,MedQ_list,LowQ_list,date_list)
# names_list = c("State","HighQ","MedQ","LowQ","date")
# names(df_violin) = names_list
# attach(df_violin)
State = as.character(State)
date_list = unique(date_list)

Q = HighQ


###### Violin plot
df %>%
  mutate(date = fct_reorder(date, Q, .fun = median)) %>%
  ggplot(aes(x=date, y=Q, fill=date)) +
  geom_violin() +
  geom_jitter(color="grey", width=.2, size=.9, alpha=.8) +
  theme(
    legend.position = "none"
  ) +
  coord_flip()







# US map

library(plotly)
dfm <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_us_cities.csv')

dfm$q <- with(dfm, cut(pop, quantile(pop)))
levels(dfm$q) <- paste(c("1st", "2nd", "3rd", "4th", "5th"), "Quantile")
dfm$q <- as.ordered(dfm$q)

g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showland = TRUE,
  landcolor = toRGB("gray85"),
  subunitwidth = 1,
  countrywidth = 1,
  subunitcolor = toRGB("white"),
  countrycolor = toRGB("white")
)

p <- plot_geo(dfm, locationmode = 'USA-states', sizes = c(1, 250)) %>%
  add_markers(
    x = ~lon, y = ~lat, size = ~pop, color = ~q, hoverinfo = "text",
    text = ~paste(dfm$name, "<br />", dfm$pop/1e6, " million")
  ) %>%
  layout(title = '2014 US city populations<br>(Click legend to toggle)', geo = g)





#  Kangda graphs1(Tendance de la croissance Nevada HighQN 2014-01-01)

df %>%
  filter(State=="Nevada" ) %>%
  ggplot( aes(x=date, y=HighQN)) +
  geom_line()    
    
# Kangda graphs2(treemap 2014-01-01)
df1=read.table("Weed_Price.csv",header=TRUE,sep=",",dec=".")
df2=df1[c(1:51),]
df2$date <- as.Date(df2$date)

treemap(df2,
        index="State",
        vSize="HighQN",
        title="Treemap: HighQN of all the states in 2014-01-01",
        fontsize.title=12,
        cex = 0.5,
        type="index"
)
        
treemap(df2,
        index="State",
        vSize="LowQ",
        title="Treemap: LowQ of all the states in 2014-01-01",
        fontsize.title=12,
        cex = 0.5,
        type="index"
)

treemap(df2,
        index="State",
        vSize="HighQ",
        title="Treemap: HighQ of all the states in 2014-01-01",
        fontsize.title=12,
        cex = 0.5,
        type="index"
)

treemap(df2,
        index="State",
        vSize="MedQ",
        title="Treemap: MedQ of all the states in 2014-01-01",
        fontsize.title=12,
        cex = 0.5,
        type="index"
)



