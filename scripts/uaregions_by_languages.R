library(data.table)
library(sp)
library(tmaptools)
library(tmap)
library(dplyr)
library(tidyr)

language_iso <- read.csv("../data/iso639.csv")
language_iso <- language_iso %>%
  gather(key = iso_type, value = language, iso639.2:iso639.1)

interested_languages <- c("en", "de", "uk", "ru", "zh", "hu", "be", "ro", "ja", "es")
languages_color <- c("Greens", "YlOrBr", "Blues", "Reds", "Greys", "Purples", "PuBuGn", "BuPu", "RdPu", "Oranges")

ukr_adm2 <- read_shape(file="../data/ukraine_shapefiles/UKR_adm1.shp")
ukr_adm2@data$NAME_1 <- gsub("Kiev", "Kyiv", ukr_adm2@data$NAME_1)
ukr_adm2@data$NAME_1 <- gsub("Odessa", "Odesa", ukr_adm2@data$NAME_1)
ukr_adm2@data$NAME_1 <- gsub("'", "", ukr_adm2@data$NAME_1)
ukr_adm2@data$NAME_1 <- gsub("Dnipropetrovsk", "Dnipro", ukr_adm2@data$NAME_1)

lang_mp <- read_shape("../data/World_Languages/World_Languages.shp")

geotags <-  fread("../data/all_on_ukraine.csv", data.table = FALSE)
geotags <- geotags[, c('gt_page_id', 'gt_lon', 'gt_lat', 'lang')]

ukr_adm2@data$territory <- paste(ukr_adm2@data$NAME_1, ukr_adm2@data$TYPE_1)

for (i in 1:length(interested_languages)) {
  l <- interested_languages[i]
  p <- languages_color[i]
  geotags_short <- geotags %>%  
    filter(lang %in% language_iso$language) %>% 
    filter(lang == l) %>% 
    filter(gt_lat >= -90 & gt_lat <= 90, gt_lon >= -180 & gt_lon <= 180)
  
  coordinates(geotags_short) <- c("gt_lon", "gt_lat")
  proj4string(geotags_short) <- proj4string(ukr_adm2)
  inside.ukraine <- !is.na(over(geotags_short, as(ukr_adm2, "SpatialPolygons")))
  geotags_short$ukraine <- inside.ukraine
  
  over_shape <- over(geotags_short, ukr_adm2)
  geotags_short$id <- over_shape$ID_1
  
  territory_n <- geotags_short %>% as("data.frame") %>% 
    filter(ukraine) %>% 
    group_by(id) %>% 
    summarise(geotags_short_number = n())
  
  regions_freq <- geotags_short %>% as("data.frame") %>% 
    filter(ukraine) %>% 
    select(id) %>% 
    table() %>% 
    prop.table() %>% 
    data.frame()
  
  names(regions_freq) <- c("id", "freq")
  freq_order <- order(regions_freq$freq, decreasing = TRUE)
  regions_freq$order <- NA
  for (i in 1:length(freq_order)) {
    regions_freq$order[freq_order[i]] <- i
  }
  
  
  if (l == "es") {
    regions_freq$text_color <- ifelse(regions_freq$id == "17", "red", "black")
  }
  
  if (l == "hu") {
    regions_freq$text_color <- ifelse(regions_freq$id == "23", "red", "black")
  }
  
  map_extended <- append_data(ukr_adm2, territory_n, key.shp = "ID_1", key.data = "id")
  
  
  m <- tm_shape(map_extended) +
    tm_fill("geotags_short_number", style = "cont")
  #tm_text("city_name", size = 0.5)

  map_extended <- append_data(ukr_adm2, regions_freq, key.shp = "ID_1", key.data = "id")
  map_extended$reg_name <- ifelse(map_extended$order <= 5, as.character(map_extended$NAME_1), NA)
  
  
  toPercent <- function(x) {
    paste0(as.character(round(x * 100, 0)), "%")
  }
  

  if (l %in% c("es", "hu")) {
    m2_fr <- tm_shape(map_extended) +
      tm_fill("freq", style = "cont", title = "% articles", legend.format = list(fun = toPercent), palette = p) + 
      tm_fill("darkolivegreen4") +
      tm_text("reg_name", size = 0.8, fontfamily = "PT Mono", remove.overlap = TRUE, col = "text_color") +
      tm_layout(frame = FALSE, outer.margins = rep(0.005, 4))
  } else {
    m2_fr <- tm_shape(map_extended) +
      tm_fill("freq", style = "cont", title = "% articles", legend.format = list(fun = toPercent), palette = p) + 
      tm_fill("darkolivegreen4") +
      tm_text("reg_name", size = 0.8, fontfamily = "PT Mono", remove.overlap = TRUE) +
      tm_layout(frame = FALSE, outer.margins = rep(0.005, 4))
  }
  
  map_filename <- paste0(l, "_ukraine_map.png")
  save_tmap(m2_fr, map_filename)
  
}




