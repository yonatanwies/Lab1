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
all.tables = html_nodes(source, "table")  

# Use html_table to extract the individual tables from the all.tables object:
url = 'https://en.wikipedia.org/wiki/Democracy_Index'
source = read_html(url)
list.by.region <- source %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[4]') %>%
  html_table(fill =TRUE)
list.by.region <- as.data.frame(list.by.region[[1]])
list.by.country <- source %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[6]') %>%
  html_table(fill = TRUE)
list.by.country <- as.data.frame(list.by.country[[1]])
components <- source %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[7]') %>%
  html_table()
components <- as.data.frame(components[[1]])
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
x<-c(list.by.country[5,which(list.by.country$'Region'=="North America")])
x
avg.list<-rowMeans(list.by.country[5:length(list.by.country)]) %>% sort(decreasing = TRUE)
top.average.five<-avg.list[1:5]
bottom.average.five<-sort(avg.list,decreasing = FALSE)[1:5]

#2.a

boxplot(list.by.country$"2022" ~ list.by.country$Region)
#i want to create a data frame that the name is the region's name, and the value is the vector of
# values of 2022 for each region
regions<- list.by.country$Region %>% unique()

combined_list <- vector("list", length(regions))
for (i in 1:length(regions)) {
  region <- regions[i]
  data_vec <- list.by.country$"2022"[which(list.by.country$Region == region)]
  combined_list[[i]] <- data_vec
}
for(i in 1:length(regions)){
  if(boxplot.stats(combined_list[[i]])[4]!="numeric(0)"){
    for(j in boxplot.stats(combined_list[[i]])[4]){
      print(list.by.country$Country[which(list.by.country$"2022"==j)])
    }
  }
}
combined_list[[3]]
boxplot.stats(combined_list[[2]])[4] == "numeric(0)"

#2.b
ggplot(list.by.country, aes(x = `2022`, fill = Region)) +
  geom_density(alpha = 0.5) +
  labs(x = "Democracy Index (2022)", y = "Density") +
  ggtitle("Density Plots of Democracy Index by Region (2022)")
summary_stats <- list.by.country %>%
  group_by(Region) %>%
  summarise(
    Mean = mean(`2022`),
    Variance = var(`2022`),
    Skewness = skewness(`2022`),
    Kurtosis = kurtosis(`2022`)
  )

print(summary_stats)
