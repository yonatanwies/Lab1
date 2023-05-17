#To update our file, first save the changes locally on your computer, then click on the GIT button (next to "go to file/function)
# and click "commit". this will save the branch. Afterwards click on "Push" to merge changes to main file.

library(tidyverse) # This includes dplyr, stringr, ggplot2, .. 
library(data.table)
library(rworldmap) # world map
library(ggthemes)
library(reshape2) # melt: change data-frame format long/wide
library(e1071) # skewness and kurtosis
library(rvest)
#1.a

url = 'https://en.wikipedia.org/wiki/Democracy_Index'
source = read_html(url)
list.by.region <- source %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[3]') %>%
  html_table()
list.by.region <- as.data.frame(region_table)
list.by.country <- source %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[5]') %>%
  html_table(fill = TRUE)
list.by.country <- as.data.frame(country_table)
components <- source %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[6]') %>%
  html_table()
components <- as.data.frame(components_table)
head(list.by.region, 5)
head(list.by.country, 5)
head(components, 5)
#1.b
bottom_five <- list.by.country %>% 
  arrange(desc(`2022 rank`)) %>% 
  select(`Country`, `2022 rank`) %>% 
  head(5)

top_five <- list.by.country %>% 
  arrange(`2022 rank`) %>% 
  select(`Country`, `2022 rank`) %>% 
  head(5)

avg.list<-rowMeans(list.by.country[5:length(list.by.country)]) %>% sort(decreasing = TRUE)
top.average.five<-avg.list[1:5]
bottom.average.five<-sort(avg.list,decreasing = FALSE)[1:5]

#2.a

North.America<-list.by.country %>% filter(list.by.country$Region=="North America")
Western.Europe<-list.by.country %>% filter(list.by.country$Region=="Western Europe")
Central.Eastern.Europe<-list.by.country %>% filter(list.by.country$Region=="Central and Eastern Europe")
Latin.America<-list.by.country %>% filter(list.by.country$Region=="Latin America and the Caribbean")
Asia.Australasia<-list.by.country %>% filter(list.by.country$Region=="Asia and Australasia")
MiddleEast.NorthAfrica<-list.by.country %>% filter(list.by.country$Region=="Middle East and North Africa")
Africa<-list.by.country %>% filter(list.by.country$Region=="Sub-Saharan Africa") 


df<-c("North America"=North.America[5],"Western Europe" = Western.Europe[5],Central.Eastern.Europe[5],
      Latin.America[5],Asia.Australasia[5],MiddleEast.NorthAfrica[5],Africa[5])
boxplot(df)


