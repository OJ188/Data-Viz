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
df = read.table("Weed_Price.csv",header=TRUE,sep=",",
                dec=".",stringsAsFactors = TRUE)
attach(df)
date <- as.Date(date)


###### COMPLETING MISSING VALUES WITH LAST NONE NA VALUE
df = df[with(df, order(State,date)),]
na.locf(df)


###### PLOTING Washington VALUES (for example)
# 
state = "Washington"

Chosen_state = cbind.data.frame(as.Date(df$date[df$State==state]), 
                              df$HighQ[df$State==state])
Chosen_state_names = c("Chosen_state_date","Chosen_state_HighQ")
names(Chosen_state) = Chosen_state_names
attach(Chosen_state)

label_Chosen_state = paste(c("Highest price in", state,
                             ": 1oz.",max(Chosen_state_HighQ),"$"),
                             collapse = " ")
label_dygraph = paste(c("Price fluctuation in", state, 
                      collapse = " "))
label_title = "Prices per once (in dollar)"

# Ploting Washington
ggplot(Chosen_state, aes(Chosen_state_date, Chosen_state_HighQ)) +
  geom_line() +
  ylim(min(Chosen_state_HighQ)-0.2*min(Chosen_state_HighQ),
       max(Chosen_state_HighQ)+0.2*max(Chosen_state_HighQ)) +
  geom_area(color="black", fill="red") +

  annotate("text",
           x=Chosen_state_date[Chosen_state_date=="2014-08-09"],
           y=max(Chosen_state_HighQ)*1.1, label=label_Chosen_state,
           color="red", fontface="bold") +
  annotate(geom="point",
           x=Chosen_state_date[Chosen_state_HighQ==max(Chosen_state_HighQ)],
           y=max(Chosen_state_HighQ), shape=21, size=10, fill="transparent") +
  xlab("Dates") +
  ylab(label_title)


# Interactive graph
don <- xts(x = Chosen_state_HighQ, order.by = Chosen_state_date)
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
# Choix = 2
# Choix = 3
Choix = 4

# Quality choice
if(Choix==1)
{
  Mean_state = data.frame(aggregate(HighQ~State, data=df, 
                                    FUN=function(df)
                                      c(mean=mean(df), count=length(df))))
  Mean_state$HighQ = Mean_state$HighQ[1:51]
  Mean_state=Mean_state[order(Mean_state[,2]),]
  Q = Mean_state$HighQ
  title = "High Quality"
}
if(Choix==2)
{
  Mean_state = data.frame(aggregate(MedQ~State, data=df, 
                                    FUN=function(df)
                                      c(mean=mean(df), count=length(df))))
  Mean_state$MedQ = Mean_state$MedQ[1:51]
  Mean_state=Mean_state[order(Mean_state[,2]),]
  Q = Mean_state$MedQ
  title = "Medium Quality"
} 
if(Choix==3)
{
  Mean_state = data.frame(aggregate(LowQ~State, data=df, 
                                    FUN=function(df)
                                      c(mean=mean(df), count=length(df))))
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

# Setting the labels
State = as.character(Mean_state$State)
label_max = paste(c(State[Q==max(Q)],":", ceiling(max(Q)),"$/oz."), collapse = " ")
label_mean = paste(c("Mean :", ceiling(mean(Q)),"$/oz."), collapse = " ")
label_min = paste(c(State[Q==min(Q)], ceiling(min(Q)),"$/oz."), collapse = " ")
label_median = paste(c("Median :", State[Q==median(Q)],
                       "with", ceiling(median(Q)),"$/oz."), collapse = " ")
label_y = paste(c("Mean price per state :", title), collapse = " ")

# Ploting
# Median state highlighted in orange
# Mean in blue
# Max and min circled in red
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






                                         



# IN CASE OF : Error in .Call.graphics(C_palette2, .Call(C_palette2, NULL)) : 
# invalid graphics state
# USE : dev.off()


###### OTHER FUNCTIONS
sum(is.na(LowQN))
df[is.na(LowQ)]
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
                                  # FUN=function(df)
                                      # c(mean=mean(df), count=length(df))))

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
        title="Treemap : Prices of low quality of weed 2014-01-01",
        fontsize.title=12,
        cex = 0.5,
        type="index"
)

treemap(df2,
        index="State",
        vSize="HighQ",
        title="Treemap : Prices of high quality of weed 2014-01-01",
        fontsize.title=12,
        cex = 0.5,
        type="index"
)

treemap(df2,
        index="State",
        vSize="MedQ",
        title="Treemap : Prices of med quality of weed 2014-01-01",
        fontsize.title=12,
        cex = 0.5,
        type="index"
)


###### US map (this code was shamefully copied from 
# https://plot.ly/r/choropleth-maps/)
# Showing mean of the 3 qualities and mean of each quality

# Reload to avoid problems
df = read.table("Weed_Price.csv",header=TRUE,sep=",",dec=".",stringsAsFactors = TRUE)
attach(df)
df = df[!(State =="District of Columbia"),]
State = unique(df$State)
date <- as.Date(date)

# COMPLETING MISSING VALUES WITH LAST NONE NA VALUE
df = df[with(df, order(State,df$date)),]
na.locf(df)

# Calculating means per state
Mean_state = data.frame(aggregate(df$HighQ~df$State, data=df, 
                                  FUN=function(df)
                                    c(mean=mean(df), count=length(df))))
HighQ = ceiling(Mean_state$df.HighQ[1:50])
Mean_state = data.frame(aggregate(df$MedQ~df$State, data=df, 
                                  FUN=function(df)
                                    c(mean=mean(df), count=length(df))))
MedQ = ceiling(Mean_state$df.MedQ[1:50])
Mean_state = data.frame(aggregate(df$LowQ~df$State, data=df, 
                                  FUN=function(df)
                                    c(mean=mean(df), count=length(df))))
LowQ = ceiling(Mean_state$df.LowQ[1:50])
Mean_state = data.frame(aggregate(
  (df$HighQ+df$MedQ+df$LowQ)/3~df$State, data=df, 
  FUN=function(df) c(mean=mean(df), count=length(df))))
MeanQ = ceiling(Mean_state$X.df.HighQ...df.MedQ...df.LowQ..3[1:50])


# Problem in State column with District of Columbia reappearing
df = df[!(State =="District of Columbia"),]
State = unique(df$State)

# Loading dataset from plotly
dfr <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")
dfr$hover = paste(State, "<br>", "High Quality", HighQ, "$,", 
                  "Medium Quality", MedQ, "$,",
                  "Low Quality", LowQ, "$")
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)
# Ploting values
p <- plot_geo(dfr, locationmode = 'USA-states') %>%
  add_trace(
    z = ~MeanQ, text = ~hover, locations = ~code,
    color = ~MeanQ, colors = 'Purples'
  ) %>%
  colorbar(title = "$/oz.") %>%
  layout(
    title = 'Mean of weed prices per state ($/oz.)',
    geo = g
  )
p
