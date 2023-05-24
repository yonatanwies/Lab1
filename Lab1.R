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


# Use html_table to extract the individual tables from the all.tables object:
url = 'https://en.wikipedia.org/wiki/Democracy_Index'
source = read_html(url)
all.tables = html_nodes(source, "table")  
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

#3.a
#A function for listbycountry
plot_democracy_index_country <- function(data, names) {
  subset_data <- data[data$Country %in% names, ]
  grouping_column <- "Country"
  subset_data_long <- reshape2::melt(subset_data, id.vars = c("Country", "Region", "Regime type", "2022 rank"), variable.name = "Year", value.name = "DemocracyIndex")
  subset_data_long$Year <- as.integer(subset_data_long$Year)
  ggplot(subset_data_long, aes(x = Year, y = DemocracyIndex, color = get(grouping_column), linetype = get(grouping_column))) +
    geom_line() +
    labs(x = "Year", y = "Democracy Index", color = grouping_column, linetype = grouping_column) +
    ggtitle("Democracy Index Over Time") +
    theme(legend.title = element_blank())
  
}

#A function for listbyregion

plot_democracy_index_region <- function(data, names) {
  subset_data <- data[data$Region %in% names, ]
  grouping_column <- "Region"
  subset_data <- subset_data[, c("Region", "2022", "2021", "2020", "2019", "2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2008", "2006")]
  subset_data_long <- reshape2::melt(subset_data, id.vars = "Region", variable.name = "Year", value.name = "DemocracyIndex")
  subset_data_long$Year <- as.numeric(subset_data_long$Year)
  ggplot(subset_data_long, aes(x = Year, y = DemocracyIndex, color = get(grouping_column), linetype = get(grouping_column))) +
    geom_line() +
    labs(x = "Year", y = "Democracy Index", color = grouping_column, linetype = grouping_column) +
    ggtitle("Democracy Index Over Time") +
    theme(legend.title = element_blank()) 
}
plot_democracy_index_region(list.by.region,region_vector)
plot_democracy_index_country(list.by.country,c("England","Israel","Iraq"))
#3b
plot_democracy_index_clusters <- function(data) {
  data$Change <- data$`2022` - data$`2006`
  cluster1 <- data[data$Change >= 1.5, ]
  cluster2 <- data[data$Change <= -1.5, ]
  cluster3 <- data[data$Change >= 0.75 & data$Change <= 1.5, ]
  cluster4 <- data[data$Change <= -0.75 & data$Change >= -1.5, ]
  cluster5 <- data[data$Change <= -0.75 & data$`2022` - data$`Lowest drop` >= 0.75, ]
  cluster6 <- data[data$Change >= 0.75 & data$`2022` - data$`Highest point` <= -0.75, ]
  cluster7 <- data[data$`Highest` - data$`Lowest` < 0.5, ]
  cluster8 <- data[!(data %in% rbind(cluster1, cluster2, cluster3, cluster4, cluster5,cluster6,cluster7)), ]
}
plot_democracy_index_country(cluster1[,-length(cluster1)],cluster1$Country)
plot_democracy_index_country(cluster2[,-length(cluster2)], cluster2$Country)
plot_democracy_index_country(cluster3[,-length(cluster3)], cluster3$Country)
plot_democracy_index_country(cluster4[,-length(cluster4)], cluster4$Country)
plot_democracy_index_country(cluster5[,-length(cluster5)], cluster5$Country)
plot_democracy_index_country(cluster6[,-length(cluster6)], cluster6$Country)
plot_democracy_index_country(cluster7[,-length(cluster7)], cluster7$Country)
plot_democracy_index_country(cluster8[,-length(cluster8)], cluster8$Country)



#4
#helper function to determine type of regime for each country.
whichDemocracy<-function(num){
  ifelse(num>8 & num <=10,"Full democracy",
         ifelse(num>6 & num<=8, "Flawed democracy",
                ifelse(num>4 & num<=6,"Hybrid Regime","Authoritarian")))
    
}
data_2006 <- list.by.country$"2006"
data_2022 <- list.by.country$"2022"

empirical_changes<-function(old,new){
  regime.type<-c("Full democracy","Flawed democracy","Hybrid Regime","Authoritarian")
  result<-data.frame(matrix(0,nrow=4,ncol = 4,dimnames = c(regime.type %>% list(),regime.type %>% list())))
  for(i in 1:length(regime.type)){
    for(j in 1:length(regime.type)){
      to<-list.by.country$Country[which(whichDemocracy(data_2006)==regime.type[i])]
      from<-list.by.country$Country[which(whichDemocracy(data_2022)==regime.type[j])]
      val<-length(to[to%in%from])/length(to) 
      result[i,j]<-val
      
    }
  }
  return(result)
}

empirical_changes(data_2006,data_2022)
names <- c("Full democracy", "Flawed democracy", "Flawed democracy", "Authoritarian")

# Create the heatmap
heatmap(empirical_changes(data_2006,data_2022)%>%as.matrix(),
        Rowv = NA,      # Disable row clustering
        Colv = NA,      # Disable column clustering
        scale = "none", # Do not scale the values
        main = "Empirical Changes",
        colnames = names,
        rownames = names)



#Q5
#GDP
url = "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(PPP)_per_capita"
source = read_html(url)
all.tables = html_nodes(source, "table")  
gdp.countries <- source %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[1]') %>%
  html_table(fill =TRUE)
gdp.countries <- as.data.frame(gdp.countries[[1]])
gdp.countries
#INCARNATION
url = "https://en.wikipedia.org/wiki/List_of_countries_by_incarceration_rate"
source = read_html(url)
all.tables = html_nodes(source, "table")  
incarnation.rates <- source %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[2]') %>%
  html_table(fill =TRUE)
incarnation.rates <- as.data.frame(incarnation.rates[[1]])
#AREA

url = "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_area"
source = read_html(url)
all.tables = html_nodes(source, "table")  
area <- source %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[2]') %>%
  html_table(fill =TRUE)
area <- as.data.frame(area[[1]])

#POPULATION SIZE
url = "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"
source = read_html(url)
all.tables = html_nodes(source, "table")  
population <- source %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[2]') %>%
  html_table(fill =TRUE)
population <- as.data.frame(population)

names(gdp.countries)
# Check column names in population dataframe
print(colnames(population))
# Merge the two dataframes based on the common columns
merged_df <- merge(list.by.country, population, by = "Country",by.y = "Country...Dependency", all = TRUE)
merged_df <- merge(merged_df, area, by = "Country",by.y = "Country / Dependency", all = TRUE)
merged_df <- merge(merged_df, incarnation.rates, by = "Country",by.y = "Location", all = TRUE)
merged_df <- merge(merged_df, gdp.countries, by = "Country",by.y = "Country/Territory", all = TRUE)
# Print the merged dataframe
head(merged_df,5)



#5.b.
#Subset the dataframe to include only the necessary columns
data_subset <- merged_df[, c("CIA[8][9][10]", "2022 rank")]
data_subset%>%head()
x# Remove any rows with missing values
data_subset <- na.omit(data_subset)
data_subset$`CIA[8][9][10]` <- as.numeric(data_subset$`CIA[8][9][10]`)

# Fit a linear regression model
model <- lm(`CIA[8][9][10]` ~ `2022`, data = data_subset)
# Print the model summary
summary(model)
