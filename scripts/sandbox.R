setwd('~/Desktop/wiki/')

library(dplyr)
library(ggmap)
library(ggplot2)

drop_outliers <- function(geotags) {
  geotags %>% 
    filter(
      (gt_lon >= -180) & (gt_lon <= 180) &
        (gt_lat >= -90) & (gt_lat <= 90))
}

read_geotags <- function(path) {
  geotags <- read.csv(path, sep='\t', row.names = NULL, header = FALSE)
  colnames(geotags) <- c('gt_page_id', 'gt_lat', 'gt_lon')
  drop_outliers(geotags)
}

en.geotags <- read_geotags('data/csv/english.csv')
ru.geotags <- read_geotags('data/csv/russian.csv')
ua.geotags <- read_geotags('data/csv/ukrainian.csv')
zh.geotags <- read_geotags('data/csv/chinese.csv')

# ==================================================

subset <- geotags[1:1000,]

d <- data.frame(lat=subset$gt_lat,
                lon=subset$gt_lon)

map <- get_map("Ukraine,Lviv", zoom=3)

p <- ggmap(map)
p <- p + geom_point(data=d, aes(x=lon, y=lat), color="red", size=1, alpha=0.1)
p

write.table(geotags, file="lvivTagsUrl.csv")

geotags %>%
  mutate(lurl = sapply(url, length)) %>% 
  filter(lurl == 0) %>% 
  select(gt_id)
  
geotags[276,12][[1]] <- ''

geotags$url <- sapply(geotags$url, unlist)
geotags <- geotags[, !(names(geotags) %in% c('lurl'))]