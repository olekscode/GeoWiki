library(maps)
library(mapdata)
library(rworldmap)
library(rgdal)
library(rgeos)
library(sp)

newmap <- getMap(resolution = "low")
plot(newmap)

plot(newmap,
     xlim = c(-20, 59),
     ylim = c(35, 71),
     asp = 1)

#map("Europe",
#    fill=T,
#    col="azure3", bg="azure4",
#    mar = c(0,0,0,0))

sample.size <- 100000
ua.sample <- sample_n(ua.geotags, sample.size)
ru.sample <- sample_n(ru.geotags, sample.size)

#for (i in 1:(dim(ru.geotags)[1] / 20)) {
for (i in 1:sample.size) {
  each <- ru.sample[i,]
  points(each$gt_lon, each$gt_lat, col="firebrick1", pch=4, cex=0.00001)
}

#for (i in 1:(dim(ua.geotags)[1] / 20))  {
for (i in 1:sample.size) {
  each <- ua.sample[i,]
  points(each$gt_lon, each$gt_lat, col="royalblue2", pch=4, cex=0.00001)
}

#c("terrain", "terrain-background", "satellite",
#  "roadmap", "hybrid", "toner", "watercolor", "terrain-labels", "terrain-lines",
#  "toner-2010", "toner-2011", "toner-background", "toner-hybrid",
#  "toner-labels", "toner-lines", "toner-lite")

get_zoomed_map <- function(map.zoom) {
  get_map(
    location = c(lon = -50, lat = -10),
    scale = "auto",
    zoom = map.zoom,
    maptype = "terrain",
    source = "google",
    color = "bw")
}

draw_point <- function(data, color) {
  geom_jitter(
    data=data,
    aes(x=gt_lon, y=gt_lat),
    shape=".",
    size = 2,
    color=color, alpha = 0.3)
}

plot_samples <- function(sample.size, map.zoom) {
  map <- get_zoomed_map(map.zoom)
  
  ua.sample <- sample_n(ua.geotags, sample.size)
  ru.sample <- sample_n(ru.geotags, sample.size)
  
  #ggmap(map)+
  ggplot()+
    draw_point(ua.sample, "royalblue2")
    draw_point(ru.sample, "firebrick1")
}

plot_all <- function(geotags, color, map.zoom) {
  
  
  ggplot()+
    draw_point(geotags, color) + theme(
      panel.grid = element_blank(),
      panel.background = element_rect(fill = "white"),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank())
}

plot_samples(224606, 3)

plot_all(en.geotags, "darkolivegreen4", 3)

tags_in_ua <- function(geotags) {
  geotags %>% filter(
    (gt_lat > 44) & (gt_lat < 52.5) &
    (gt_lon > 21) & (gt_lon < 41))
}

sample.size <- 2000
ua.sample <- sample_n(tags_in_ua(ua.geotags), sample.size)
ru.sample <- sample_n(tags_in_ua(ru.geotags), sample.size)

ukraine<-readOGR("data/shape/UKR_adm_shp/UKR_adm1.shp")

ggplot()+
  #geom_polygon(data=ukraine, aes(x=long, y=lat, group=group), colour="grey90", fill="grey60")+ 
  geom_point(data=ua.sample, aes(x=gt_lon, y=gt_lat), color="grey30", alpha = 0.3, size = 1)+
  geom_point(data=ru.sample, aes(x=gt_lon, y=gt_lat), color="grey20", alpha = 0.3, size = 1)
