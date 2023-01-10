# Catherine Donner
# 14.1 Rich Web content using D3.js and htmlwidgets

# load packages
library(tidyverse)
library(mdsr)
library(babynames)
# static plot of Beatles using R
Beatles <- babynames %>%
  filter(name %in% c("John", "Paul", "George", "Ringo") & sex == "M") %>%
  mutate(name = factor(name, levels = c("John", "George", "Paul", "Ringo")))
beatles_plot <- ggplot(data = Beatles, aes(x = year, y = n)) +
  geom_line(aes(color = name), size = 2)
beatles_plot
# ggplotly graph
#install.packages("plotly")
library(plotly)
ggplotly(beatles_plot)
# DataTables package
#install.packages("DataTables")
library(DT)
datatable(Beatles, options = list(pageLength = 10))
# dygraphs package
#install.packages("dygraphs")
library(dplyr)
library(tidyr)
library(dygraphs)
# time series plot of Beatles names
Beatles %>% 
  filter(sex == "M") %>% 
  select(year, name, prop) %>%
  pivot_wider(names_from = name, values_from = prop) %>%
  dygraph(main = "Popularity of Beatles names over time") %>% 
  dyRangeSelector(dateWindow = c("1940", "1980"))
# streamgraph plot
#remotes::install_github("hrbrmstr/streamgraph")
library(streamgraph)
Beatles %>% 
  streamgraph(key = "name", value = "n", date = "year") %>%
  sg_fill_brewer("Accent")

# Chapter 11 exercises
#Start of exercise 11.1
library(macleish)
library(tidyverse)
library(lubridate)
library(ggplot2)
head(whately_2015)
whately_2015 %>%
  select(when, temperature)%>%
  group_by(when)%>%
  ggplot(aes(y= temperature, x= when))+ geom_line()+geom_smooth()
new<-whately_2015 %>% 
  mutate(date =format(as.POSIXct(when,format="%Y-%m-%d %H:%M:%S"),"%m/%d/%Y"), date= as.factor(date))
#install.packages("chron")
library(chron)
d <- as.Date(cut(as.Date(new$date, "%m/%d/%Y"), "month")) + 32
new$Season <- factor(quarters(d), levels = c("Q1", "Q2", "Q3", "Q4"), labels = c("winter", "spring", "summer", "fall"))
g <- new %>%
  select(when, temperature, Season)%>%
  group_by(when)%>%
  ggplot(aes(y= temperature, x= when, color= Season)) + geom_point()
g
#End of exercise 11.1
#Start of exercise 11.3
library(plotly)
ggplotly(g)
#End of exercise 11.3
#Start of exercise 11.4
library(dygraphs)
library(lubridate)
library(xts)
A <- new %>% select(when, Season, temperature)%>%
  tidyr::spread(key = Season, value = temperature) 
str(A)
A$when = ymd_hms(A$when)
whenxts <- xts(A[, -1], order.by = A$when)
dy <- dygraph(whenxts)
dyRangeSelector(dy, dateWindow = NULL)
#End of exercise 11.4