setwd('~/Desktop/wiki/')

library(dplyr)
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

plot_visibility <- function(geotags, color, alpha) {
  ggplot()+
    draw_point(geotags, color, alpha) + theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white"),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank())
}

plot_world <- function(geotags, color, alpha) {
  plot_visibility(geotags, color, alpha)
}

geotags_inside <- function(geotags, lon.min, lon.max, lat.min, lat.max) {
  geotags %>% 
    filter(
      (gt_lon >= lon.min) & (gt_lon <= lon.max) &
        (gt_lat >= lat.min) & (gt_lat <= lat.max))
}

plot_eu <- function(geotags, color, alpha) {
  geotags.eu <- geotags_inside(geotags, -25.0, 67.0, 28.0, 71.8)
  plot_visibility(geotags.eu, color, alpha)
}

plot_ua <- function(geotags, color, alpha) {
  geotags.eu <- geotags_inside(geotags, 21.8, 40.61, 44.15, 52.59)
  plot_visibility(geotags.eu, color, alpha)
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

plot_world(de.geotags, de.color, alpha)
plot_world(en.geotags, en.color, alpha)
plot_world(es.geotags, es.color, alpha)
plot_world(ja.geotags, ja.color, alpha)
plot_world(pl.geotags, pl.color, alpha)
plot_world(ru.geotags, ru.color, alpha)
plot_world(ua.geotags, ua.color, alpha)
plot_world(zh.geotags, zh.color, alpha)

plot_eu(de.geotags, de.color, alpha)
plot_eu(en.geotags, en.color, alpha)
plot_eu(es.geotags, es.color, alpha)
plot_eu(ja.geotags, ja.color, alpha)
plot_eu(pl.geotags, pl.color, alpha)
plot_eu(ru.geotags, ru.color, alpha)
plot_eu(ua.geotags, ua.color, alpha)
plot_eu(zh.geotags, zh.color, alpha)

plot_ua(de.geotags, de.color, alpha)
plot_ua(en.geotags, en.color, alpha)
plot_ua(es.geotags, es.color, alpha)
plot_ua(ja.geotags, ja.color, alpha)
plot_ua(pl.geotags, pl.color, alpha)
plot_ua(ru.geotags, ru.color, alpha)
plot_ua(ua.geotags, ua.color, alpha)
plot_ua(zh.geotags, zh.color, alpha)
