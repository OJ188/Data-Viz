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
attach(df)
date <- as.Date(date)


###### COMPLETING MISSING VALUES WITH LAST NONE NA VALUE
df = df[with(df, order(State,date)),]
na.locf(df)


###### PLOTING North Dakota VALUES x=date[value==max(value)], y=max(value)
North_Dakota = cbind.data.frame(as.Date(date[State=="North Dakota"]), HighQ[State=="North Dakota"])
North_Dakota_names = c("North_Dakota_date","North_Dakota_HighQ")
names(North_Dakota) = North_Dakota_names
attach(North_Dakota)

# Ploting North Dakota
ggplot(North_Dakota, aes(North_Dakota_date, North_Dakota_HighQ)) +
  geom_line() +
  ylim(min(North_Dakota_HighQ)-0.2*min(North_Dakota_HighQ),max(North_Dakota_HighQ)+0.2*max(North_Dakota_HighQ)) +
  # ylim(0,max(North_Dakota_HighQ)+0.2*max(North_Dakota_HighQ)) +
  geom_area(color="black", fill="red") +
  annotate("text", x=date[HighQ==max(HighQ)]+190, y=max(HighQ)+20,
    # label=("test")) +
    label=("Highest price : North Dakota, 1 oz. (28g), 415$")) +
  annotate(geom="point", x=date[HighQ==max(HighQ)], y=max(HighQ), shape=21, size=10, fill="transparent")

plot_ly(x = date, y = HighQ, type="scatter", mode="markers", fill = "tozeroy")


# Interactive graph
don <- xts(x = North_Dakota_HighQ, order.by = North_Dakota_date)
# graph
dygraph(don) %>%
  dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors="#D8AE5A") %>%
  dyRangeSelector() %>%
  dyCrosshair(direction = "vertical") %>%
  dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1)




###### Lollipop chart : Mean by state, HighQ, MedQ and LowQ
Mean_HighQ_state = data.frame(aggregate(HighQ~State, data=df, FUN=function(df) c(mean=mean(df), count=length(df))))
Mean_HighQ_state$HighQ = Mean_HighQ_state$HighQ[1:51]
attach(Mean_HighQ_state)
# Mean_MedQ_state = aggregate(MedQ~State, data=df, FUN=function(df) c(mean=mean(df), count=length(df)))
# Mean_LowQ_state = aggregate(LowQ~State, data=df, FUN=function(df) c(mean=mean(df), count=length(df)))

geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) 

# Reorder
Mean_HighQ_state %>%
  arrange(HighQ) %>%
  mutate(State=factor(State,State)) %>%
  ggplot(aes(x=State, y=HighQ)) +
    geom_segment( aes(x=State, xend=State, y=0, yend=HighQ), color="skyblue") +
    geom_point( size=3, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) +
    theme_light() +
    coord_flip() +
    theme(
      panel.grid.major.y = element_blank(),
      panel.border = element_blank(),
      axis.ticks.y = element_blank()
    ) +
    xlab("") +
    ylab("Mean price per state") +
    
    annotate("text", x=State[HighQ==max(HighQ)], y=max(HighQ) - 160,
      label=("North Dakota : 415$/oz. (28g)"), color="red") +
    annotate(geom="point", State[HighQ==max(HighQ)], y=max(HighQ), shape=21, size=10, fill="transparent", color="red") +
    
    annotate("text", x=State[HighQ==min(HighQ)], y=min(HighQ) - 100,
      label=("Oregon : 208$/oz."), color="red") +
    annotate(geom="point", State[HighQ==min(HighQ)], y=min(HighQ), shape=21, size=10, fill="transparent", color="red") +
    
    geom_hline(yintercept=mean(HighQ), color="blue", size=.5) +
    annotate("text", x=State[HighQ==min(HighQ)], y=min(HighQ) + 100,
           label=("Mean : 330$/oz."), color="blue")




















# Reorder
p = Mean_HighQ_state %>%
  arrange(HighQ) %>%
  mutate(State=factor(State,State)) %>%
  ggplot(aes(x=State, y=HighQ)) +
  geom_segment( aes(x=State, xend=State, y=0, yend=HighQ), color=ifelse((State=="North Dakota" | State=="Oregon"), "orange", "grey"), size=ifelse((State=="North Dakota" | State=="Oregon"), 1.3, 0.7)) +
  geom_point( color=ifelse((State=="North Dakota" | State=="Oregon"), "orange", "grey"), size=ifelse((State=="North Dakota" | State=="Oregon"), 5, 2) ) +
  theme_light() +

  coord_flip() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  xlab("") +
  ylab("Mean price per state") +
  
  annotate("text", x=State[HighQ==max(HighQ)], y=max(HighQ) - 100,
    label=("North Dakota : 1 oz. (28g), 415$")) +
  annotate(geom="point", State[HighQ==max(HighQ)], y=max(HighQ), shape=21, size=10, fill="transparent") +
  
  annotate("text", x=State[HighQ==min(HighQ)], y=min(HighQ) - 100,
    label=("Oregon : 1 oz. (28g), 208$")) +
  annotate(geom="point", State[HighQ==min(HighQ)], y=min(HighQ), shape=21, size=10, fill="transparent") +
  
  geom_hline(yintercept=mean(HighQ), color="orange", size=.5)

dev.off()

p +
  annotate("text", x = State[HighQ==max(HighQ)], y = max(HighQ)*0.8, label = "Group D is very impressive", color="orange", size=4 , angle=0, fontface="bold", hjust=0)





p = ggplot(data, aes(x=x, y=y)) +
  geom_segment( aes(x=x, xend=x, y=0, yend=y ), color=ifelse(data$x %in% c("A","D"), "orange", "grey"), size=ifelse(data$x %in% c("A","D"), 1.3, 0.7) ) +
  geom_point( color=ifelse(data$x %in% c("A","D"), "orange", "grey"), size=ifelse(data$x %in% c("A","D"), 5, 2) ) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="none",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  xlab("") +
  ylab("Value of Y")
p
dev.off()

p +
  annotate("text", x = grep("D", data$x), y = data$y[which(data$x=="D")]*1.2, label = "Group D is very impressive", color="orange", size=4 , angle=0, fontface="bold", hjust=0) + 
  annotate("text", x = grep("A", data$x), y = data$y[which(data$x=="A")]*1.2, label = paste("Group A is not too bad\n (val=",data$y[which(data$x=="A")] %>% round(2),")",sep="" ) , color="orange", size=4 , angle=0, fontface="bold", hjust=0) +
  ggtitle("How did groups A and D perform?")

                                        



# IN CASE OF : Error in .Call.graphics(C_palette2, .Call(C_palette2, NULL)) : invalid graphics state
# USE : dev.off()


###### OTHER FUNCTIONS
sum(is.na(LowQN))
df[is.na(LowQ)]
summary(df)
head(df)
any(is.na(data))








###### Violin plot
data2 %>%
  mutate(text = fct_reorder(text, value, .fun = median)) %>%
  ggplot(aes(x=text, y=value, fill=text)) +
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
library(treemap)

treemap(df2,
        index="State",
        vSize="HighQN",
        type="index"
)