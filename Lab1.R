#To update our file, first save the changes locally on your computer, then click on the GIT button (next to "go to file/function)
# and click "commit". this will save the branch. Afterwards click on "Push" to merge changes to main file.

library(tidyverse) # This includes dplyr, stringr, ggplot2, .. 
library(data.table)
library(rworldmap) # world map
library(ggthemes)
library(reshape2) # melt: change data-frame format long/wide
library(e1071) # skewness and kurtosis
library(rvest)
library(dplyr)
library(corrplot)
library(car)
#1.a


# Use html_table to extract the individual tables from the all.tables object:
url = 'https://en.wikipedia.org/wiki/The_Economist_Democracy_Index'
source = read_html(url)
all.tables = html_nodes(source, "table")  
list.by.region <- source %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[4]') %>%
  html_table(fill =TRUE)
list.by.region <- as.data.frame(list.by.region[[1]])
list.by.country <- source %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[5]') %>%
  html_table(fill = TRUE)
list.by.country <- as.data.frame(list.by.country[[1]])
components <- source %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[6]') %>%
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
    theme(legend.title = element_blank())}

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
list.by.country$Change <- list.by.country$`2022` - list.by.country$`2006`
cluster1 <- list.by.country[list.by.country$Change >= 1.5, ]
cluster2 <- list.by.country[list.by.country$Change <= -1.5, ]
cluster3 <- list.by.country[list.by.country$Change >= 0.75 & list.by.country$Change <= 1.5, ]
clust5<-filter(list.by.country,'2006'-apply(list.by.country[,6:18],1,min)>=0.75 & '2022'-apply(list.by.country[,6:18],1,min)>=0.75)
cluster4 <- list.by.country[list.by.country$Change <= -0.75 & list.by.country$Change >= -1.5, ]
cluster5 <- list.by.country[list.by.country$Change <= -0.75 & list.by.country$`2022` - list.by.country$`Lowest drop` >= 0.75, ]
cluster6 <- list.by.country[list.by.country$Change >= 0.75 & list.by.country$`2022` - list.by.country$`Highest point` <= -0.75, ]
cluster7 <- list.by.country[list.by.country$`Highest` - list.by.country$`Lowest` < 0.5, ]
cluster8 <- list.by.country[!(list.by.country %in% rbind(cluster1, cluster2, cluster3, cluster4, cluster5,cluster6,cluster7)), ]
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
colnames(gdp.countries)[1] <- "Country"
gdp.countries.clean<-gdp.countries[,c('Country',"CIA[8][9][10]")]
#INCARNATION
url = "https://en.wikipedia.org/wiki/List_of_countries_by_incarceration_rate"
source = read_html(url)
all.tables = html_nodes(source, "table")  
incarnation.rates <- source %>%
  html_nodes(xpath = '//*[@id="mw-content-text"]/div[1]/table[2]') %>%
  html_table(fill =TRUE)
incarnation.rates <- as.data.frame(incarnation.rates[[1]])
incarnation.rates
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
#Change the name of the column country
colnames(area)[2] = "Country"
colnames(population)[2] = "Country"
colnames(incarnation.rates)[1] = "Country"
colnames(gdp.countries)[1] = "Country"
#clean the names
incarnation.rates$Country= gsub("\\*", "", incarnation.rates$Country)
incarnation.rates$Country= gsub("\\ ","", incarnation.rates$Country)
incarnation.rates$Country= gsub("\\s", "", incarnation.rates$Country)
incarnation.rates$Country = gsub("\\[Note\\]", "", incarnation.rates$Country)
gdp.countries$Country= gsub("\\*", "", gdp.countries$Country)
gdp.countries$Country= gsub("\\ ","", gdp.countries$Country)
gdp.countries$Country= gsub("\\s","", gdp.countries$Country)
area$Country= gsub("\\ ", "", area$Country)
area$Country= gsub("\\(.*\\)", "", area$Country)
area$Country= gsub("\\s", "", area$Country)
population$Country= gsub("\\ ", "", population$Country)
population$Country= gsub("\\(.*\\)", "", population$Country)
population$Country= gsub("\\s", "", population$Country)
list.by.country$Country=gsub("\\ ", "", list.by.country$Country)
#list.by.country$Country=gsub("\\s", "", list.by.country$Country)
list.by.country$Country= gsub("\\(.*\\)", "", list.by.country$Country)


combined.table.GI=merge(gdp.countries,incarnation.rates)
combined.table.GIA=merge(combined.table.GI,area)
combined.table.GIAP=merge(combined.table.GIA,population,by="Country")
combined.table.GIAPL=merge(combined.table.GIAP,list.by.country,by="Country")
head(combined.table.GIAPL,5)


#5.b.
CIA_reported <- as.numeric(gsub(",","",combined.table.GIAPL$`CIA[8][9][10]`))
incar_rate <- as.numeric(combined.table.GIAPL$`Rate per 100,000 [3]`)
data_2022 <- as.numeric(combined.table.GIAPL$`2022`)

gdp.by.incar
gdp.by.rank<-lm(CIA_reported~data_2022) 
plot(data_2022,y = CIA_reported,xlab="2022 Rank",ylab = "GDP",main = "2022 Rank vs. CIA reported GDP")
abline(gdp.by.rank, col="Blue", lwd = 2)

gdp.by.incar<-lm(CIA_reported~incar_rate) 
plot(incar_rate,CIA_reported,xlab="Incarnation Rate",ylab = "GDP",main = "Incarnation Rate vs. CIA reported GDP")
abline(gdp.by.incar, col="Blue", lwd = 2)


#6.a.
# Compute the empirical CDF
combined.table.GIAPL$`CIA[8][9][10]`<-as.numeric(gsub("[^0-9.]","",combined.table.GIAPL$`CIA[8][9][10]`))
gdp=ecdf(combined.table.GIAPL$`CIA[8][9][10]`)
plot(gdp, main = "Empirical CDF of GDP (PPP) per capita", xlab = "GDP (PPP) per capita", ylab = "Empirical CDF")

#6.b.
combined.table.GIAPL$Population <- as.numeric(gsub("[^0-9.]","",combined.table.GIAPL$Population))
combined.table.GIAPL$`CIA[8][9][10]`<-as.numeric(gsub("[^0-9.]","",combined.table.GIAPL$`CIA[8][9][10]`))
gdp_per_capita <-combined.table.GIAPL$Population / combined.table.GIAPL$`CIA[8][9][10]`
gdp_per_capita.sort=sort(gdp_per_capita)
empirical_probs <- cumsum(combined.table.GIAPL$Population) / sum(combined.table.GIAPL$Population)
plot(gdp_per_capita.sort, empirical_probs, type = "s", main = "Empirical CDF of GDP per capita by population)",
     xlab = "GDP per capita", ylab = "Cumulative-Probability")

#7.
avg.list2<-rowMeans(list.by.country[5:length(list.by.country)])
list.by.country$Country
map_list=bind_cols(list.by.country$Country,avg.list2)

colnames(map_list)[colnames(map_list) == "...1"] <- "Country"
colnames(map_list)[colnames(map_list) == "...2"] <- "average_democracy_index"

data("countryExData", package = "rworldmap")
merged_data <- joinCountryData2Map(map_list, joinCode = "NAME", nameJoinColumn = "Country")
setdiff(map_list$Country,countryExData$Country)

world_map <- joinCountryData2Map(map_list, joinCode = 'NAME', nameJoinColumn = "Country",
                                 nameCountryColumn = "Country", suggestForFailedCodes = FALSE,
                                 mapResolution = "coarse", projection = NA, verbose = FALSE)

mapCountryData(world_map, nameColumnToPlot = "average_democracy_index", mapRegion = "world", colourPalette = "heat",
               addLegend = TRUE, borderCol = "black", mapTitle = "Average Democracy Index (2006-2022)",
               aspect = 1, missingCountryCol = NA, add = FALSE, nameColumnToHatch = "", lwd = 0.5)

#Q8a
combined.table.GIAPLC <-merge(combined.table.GIAP,components,by="Country")
combined.table.GIAPLC %>% head(5)
ElectoralProccess <- as.numeric(combined.table.GIAPLC$`Elec­toral pro­cessand plura­lism`)
FunctioningOfGovernment <- as.numeric(combined.table.GIAPLC$`Func­tioningof govern­ment`)
PoliticalParticipation <- as.numeric(combined.table.GIAPLC$`Poli­ticalpartici­pation`)
PoliticalCulture <- as.numeric(combined.table.GIAPLC$`Poli­ticalcul­ture`)
CivilLiberties <- as.numeric(combined.table.GIAPLC$`Civilliber­ties`)

DemocracyComponents<-cbind(ElectoralProccess,FunctioningOfGovernment,PoliticalCulture,PoliticalParticipation,CivilLiberties)
DemocracyComponentsNames<-c("ElectoralProccess","FunctioningOfGovernment","PoliticalCulture","PoliticalParticipation","CivilLiberties")

CalcCor <- function(vec,titles){
  result<-data.frame(matrix(0,nrow=5,ncol = 5,dimnames = list(c(titles),c(titles))))
  for(i in 1:5){
    for(j in 1:5){
      result[i,j] <- cor(vec[,i],vec[,j])
    }
  } 
  return(result)
}

CalcCor(DemocracyComponents,DemocracyComponentsNames)

heatmap(CalcCor(DemocracyComponents,DemocracyComponentsNames)%>%as.matrix(),
        Rowv = NA,      # Disable row clustering
        Colv = NA,      # Disable column clustering
        scale = "none", # Do not scale the values
        main = "Democracy Components Corrolations",
        colnames = DemocracyComponentsNames,
        rownames = DemocracyComponentsNames)



#Q8b

gdp.by.ElectoralProccess<-lm(CIA_reported~ ElectoralProccess+FunctioningOfGovernment+PoliticalCulture+PoliticalParticipation+CivilLiberties)
summary(gdp.by.ElectoralProccess)


coefficients <- coef(summary(gdp.by.ElectoralProccess))[, "Estimate"]
p_values <- coef(summary(gdp.by.ElectoralProccess))[, "Pr(>|t|)"]
significance_level <- 0.01

significant_coefficients <- coefficients[p_values < significance_level]
significant_coefficients


residuals <- rstudent(gdp.by.ElectoralProccess)
# Compute the 99th percentile of the residuals
percentile_99 <- quantile(residuals, 1-significance_level)
# Find the values in residuals that are bigger or smaller than the 99th percentile
outliers <- residuals[residuals > percentile_99 | residuals < -percentile_99]
outlier_countries <- combined.table.GIAPLC$Country[which(residuals>percentile_99 | residuals< -percentile_99)]
outlier_countries

residuals_table <- data.frame(Country = combined.table.GIAPLC$Country, Residuals = residuals)

top_outliers <- head(residuals_table[order(residuals_table$Residuals, decreasing = TRUE), ], 5)

bottom_outliers <- head(residuals_table[order(residuals_table$Residuals), ], 5)

outliers_table <- rbind(top_outliers, bottom_outliers)

#Other factors that maay contribute to a country's GDP score is being part of an alliance,
# Such as NATO, or the UN. In addition, amount of natural resources the country have.



outliers_table
