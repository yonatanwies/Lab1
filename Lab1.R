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

#3.a
plot_democracy_index <- function(data, countries) {
  # Subset the data for the specified countries
  subset_data <- data[data$Country %in% countries, ]
  
  # Convert year column to factor for plotting
  Year = c(2006:2022)
  subset_data$Year <- as.factor(subset_data$Year)
  
  # Plot the democracy index values for each country
  ggplot(subset_data, aes(x = Year, y = DemocracyIndex, color = Country)) +
    geom_line() +
    labs(x = "Year", y = "Democracy Index", color = "Country") +
    ggtitle("Democracy Index for Selected Countries Over Time") +
    theme(legend.title = element_blank())
}
#Write a function that receives as input a data-frame, and a vector of country names (as strings).
#The function plots the values of the democracy index of these countries in different colors as a function of the year (from 2006 to 2022),
#shown on the same graph as curves with different colors or symbols. Use meaningful axis and plot labels, and add an informative legend.
#Use the function and plot of the democracy index for five countries of your choice.
# Example country names
countries<-list.by.country$Country
# Plot the democracy index for the selected countries
plot_democracy_index(list.by.country, countries)