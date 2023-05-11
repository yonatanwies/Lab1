#To update our file, first save the changes locally on your computer, then click on the GIT button (next to "go to file/function)
# and click "commit". this will save the branch. Afterwards click on "Push" to merge changes to main file.

library(tidyverse)
library(rvest)
url <-"https://en.wikipedia.org/wiki/Democracy_Index"
democracy.index<-read_html(url)
tables<- html_nodes(democracy.index,"table")
html_table(tables[4])

list.by.region <- as.data.frame(html_table(tables[4],fill = TRUE))
list.by.country <-as.data.frame(html_table(tables[6],fill = TRUE))
components <- as.data.frame(html_table(tables[7],fill = TRUE))
