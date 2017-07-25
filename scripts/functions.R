library(jsonlite)

extractData <- function(pageId, key) {
  json_file <- sprintf("https://uk.wikipedia.org/w/api.php?action=query&prop=info&pageids=%s&inprop=url&format=json", pageId)
  json_data <- fromJSON(paste(readLines(json_file), collapse=""))
  value <- json_data$query$pages[[as.character(pageId)]][[key]]
  
  if (is.null(value)) {
    value <- ""
  }
  value
}

getUrl <- function(pageId) { extractData(pageId, 'fullurl') }
getTime <- function(pageId) { extractData(pageId, 'touched') }
getTitle <- function(pageId) { extractData(pageId, 'title')}

geotags_inside <- function(geotags, lon.min, lon.max, lat.min, lat.max) {
  geotags %>% 
    filter(
      (gt_lon >= lon.min) & (gt_lon <= lon.max) &
      (gt_lat >= lat.min) & (gt_lat <= lat.max))
}

rand_titles_inside  <- function(geotags, lon.min, lon.max, lat.min, lat.max) {
  gt_inside <- geotags_inside(geotags, lon.min, lon.max, lat.min, lat.max)
  sample <- sample_n(gt_inside, 10)
  sapply(sample$gt_page_id, getTitle)
}

urls <- sapply(geotags[1:10,]$gt_page_id, getTime)

# ========================================

geotags$url <- sapply(geotags$gt_page_id, pageIdToUrl)

for (pageId in enGeotags$gt_page_id) {
  print(pageIdToUrl(as.character(pageId)))
}
