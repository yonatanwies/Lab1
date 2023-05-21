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
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[4]') %>%
  html_table()
list.by.region <- as.data.frame(list.by.region)
list.by.country <- source %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[6]') %>%
  html_table(fill = TRUE)
list.by.country <- as.data.frame(list.by.country)
components <- source %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[7]') %>%
  html_table()
components <- as.data.frame(components)
list.by.region
list.by.country
components

head(list.by.region, 5)
head(list.by.country, 5)
head(components, 5)

#1.b
bottom_five <- list.by.country %>% 
  arrange(desc("2022 rank")) %>% 
  select("Country", '2022 rank') %>% 
  head(5)

top_five <- list.by.country %>% 
  arrange(`2022 rank`) %>% 
  select(`Country`, `2022 rank`) %>% 
  head(5)

avg.list<-rowMeans(list.by.country[5:length(list.by.country)]) %>% sort(decreasing = TRUE)
top.average.five<-avg.list[1:5]
bottom.average.five<-sort(avg.list,decreasing = FALSE)[1:5]

#2.

boxplot(list.by.country$"2022" ~ list.by.country$Region)

regions <- list.by.country$Region %>% unique()
data_2022 <- list.by.country$`2022`

data.by.region <- list.by.country$"2022"
data.by.region



df <- data.frame(Region = character(), Data = list(), stringsAsFactors = FALSE)
data_by_region <- split(data_2022, regions)




st<-boxplot.stats(list.by.country$"2022" ~ list.by.country$Region)


