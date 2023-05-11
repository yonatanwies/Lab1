#To update our file, first save the changes locally on your computer, then click on the GIT button (next to "go to file/function)
# and click "commit". this will save the branch. Afterwards click on "Push" to merge changes to main file.

library(tidyverse)
library(rvest)
url <-"https://en.wikipedia.org/wiki/Democracy_Index"
democracy.index<-read_html(url)

list.by.region<- democracy.index %>% html_table()
head(list.by.region)
