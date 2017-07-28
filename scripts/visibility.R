setwd('~/Desktop/wiki/')

library(dplyr)
library(ggplot2)

#drop_outliers <- function(geotags) {
#  geotags %>% 
#    filter(
#      (gt_lon >= -180) & (gt_lon <= 180) &
#        (gt_lat >= -90) & (gt_lat <= 90))
#}

read_geotags <- function(path) {
  geotags <- read.csv(path, sep='\t', row.names = NULL, header = FALSE)
  colnames(geotags) <- c('gt_page_id', 'gt_lat', 'gt_lon')
  #drop_outliers(geotags)
  geotags
}

de.geotags <- read_geotags('data/csv/german.csv')
en.geotags <- read_geotags('data/csv/english.csv')
es.geotags <- read_geotags('data/csv/spanish.csv')
ja.geotags <- read_geotags('data/csv/japanese.csv')
pl.geotags <- read_geotags('data/csv/polish.csv')
ru.geotags <- read_geotags('data/csv/russian.csv')
ua.geotags <- read_geotags('data/csv/ukrainian.csv')
zh.geotags <- read_geotags('data/csv/chinese.csv')

draw_point <- function(data, color, alpha) {
  geom_jitter(
    data=data,
    aes(x=gt_lon, y=gt_lat),
    shape=".",
    size = 1,
    color=color, alpha = alpha)
}

plot_visibility <- function(geotags, alpha) {
  ggplot()+
    draw_point(geotags, geotags$color, alpha) + theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white"),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank())
}

plot_world <- function(geotags, alpha) {
  plot_visibility(geotags, alpha)
}

geotags_inside <- function(geotags, lon.min, lon.max, lat.min, lat.max) {
  geotags %>% 
    filter(
      (gt_lon >= lon.min) & (gt_lon <= lon.max) &
        (gt_lat >= lat.min) & (gt_lat <= lat.max))
}

plot_eu <- function(geotags, alpha) {
  geotags.eu <- geotags_inside(geotags, -25.0, 67.0, 28.0, 71.8)
  plot_visibility(geotags.eu, alpha)
}

plot_ua <- function(geotags, alpha) {
  geotags.eu <- geotags_inside(geotags, 21.8, 40.61, 44.15, 52.59)
  plot_visibility(geotags.eu, alpha)
}

de.color <- "#a6761d"
en.color <- "#1b9e77"
es.color <- "#e6ab02"
ja.color <- "#e7298a"
pl.color <- "#66a61e"
ru.color <- "#d95f02"
ua.color <- "#7570b3"
zh.color <- "#666666"

alpha = 0.3

# Adding color as a column.This way we can merge several data frames,
# shuffle them, and each language will still be plotted with its color
de.geotags$color <- rep(de.color, nrow(de.geotags))
en.geotags$color <- rep(en.color, nrow(en.geotags))
es.geotags$color <- rep(es.color, nrow(es.geotags))
ja.geotags$color <- rep(ja.color, nrow(ja.geotags))
pl.geotags$color <- rep(pl.color, nrow(pl.geotags))
ru.geotags$color <- rep(ru.color, nrow(ru.geotags))
ua.geotags$color <- rep(ua.color, nrow(ua.geotags))
zh.geotags$color <- rep(zh.color, nrow(zh.geotags))

# merging two data frames
de.es.geotags <- rbind(de.geotags, es.geotags)
de.ua.geotags <- rbind(de.geotags, ua.geotags)
en.es.geotags <- rbind(en.geotags, es.geotags)
ja.zh.geotags <- rbind(ja.geotags, zh.geotags)
pl.ua.geotags <- rbind(pl.geotags, ua.geotags)
ru.ua.geotags <- rbind(ru.geotags, ua.geotags)
ua.zh.geotags <- rbind(ua.geotags, zh.geotags)

# shufflung the rows
de.es.geotags <- de.es.geotags[sample(nrow(de.es.geotags)),]
de.ua.geotags <- de.ua.geotags[sample(nrow(de.ua.geotags)),]
en.es.geotags <- en.es.geotags[sample(nrow(en.es.geotags)),]
ja.zh.geotags <- ja.zh.geotags[sample(nrow(ja.zh.geotags)),]
pl.ua.geotags <- pl.ua.geotags[sample(nrow(pl.ua.geotags)),]
ru.ua.geotags <- ru.ua.geotags[sample(nrow(ru.ua.geotags)),]
ua.zh.geotags <- ua.zh.geotags[sample(nrow(ua.zh.geotags)),]

plot_world(de.geotags, alpha)
plot_world(en.geotags, alpha)
plot_world(es.geotags, alpha)
plot_world(ja.geotags, alpha)
plot_world(pl.geotags, alpha)
plot_world(ru.geotags, alpha)
plot_world(ua.geotags, alpha)
plot_world(zh.geotags, alpha)

plot_world(en.es.geotags, alpha)
plot_world(ja.zh.geotags, alpha)
plot_world(pl.ua.geotags, alpha)
plot_world(ru.ua.geotags, alpha)
plot_world(ua.zh.geotags, alpha)

plot_eu(de.geotags, alpha)
plot_eu(en.geotags, alpha)
plot_eu(es.geotags, alpha)
plot_eu(ja.geotags, alpha)
plot_eu(pl.geotags, alpha)
plot_eu(ru.geotags, alpha)
plot_eu(ua.geotags, alpha)
plot_eu(zh.geotags, alpha)

plot_eu(de.es.geotags, alpha)
plot_eu(de.ua.geotags, alpha)
plot_eu(ja.zh.geotags, alpha)
plot_eu(pl.ua.geotags, alpha)
plot_eu(ru.ua.geotags, alpha)

plot_ua(de.geotags, alpha)
plot_ua(en.geotags, alpha)
plot_ua(es.geotags, alpha)
plot_ua(ja.geotags, alpha)
plot_ua(pl.geotags, alpha)
plot_ua(ru.geotags, alpha)
plot_ua(ua.geotags, alpha)
plot_ua(zh.geotags, alpha)

plot_ua(ru.ua.geotags, alpha)
