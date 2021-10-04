library(ggplot2)
library(here)
library(tidyverse)
library(utils)
library(data.table)
library(plotly)
library(DT)

df<- read_csv(here("data", "presentations.csv"))
df<- df[-c(1,2,5),]
df$url <- paste0("<a href='",df$`Presentation Link`,"'>",df$`Presentation Title`,"</a>")

p<- df %>%  select(url) %>% 
  datatable()
p

datatable(select(df, url) , escape = F)

table<- datatable(select(df, Conference, url, Month, Year, Location, Reason), escape = F)
table
