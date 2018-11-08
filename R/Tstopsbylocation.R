Tstopsbylocation <- function(lat, lon, radius = 0.01){
  # finds MBTA stops by latitude and longitude coordinates
  query <- "stops"
  base_url <- paste0("https://api-v3.mbta.com/",query)
  full_url <- paste0(base_url,"?latitude=",lat,"&longitude=",lon,"&radius=",radius)
  rawdata <- readLines(full_url, warn = F)
  dl <- jsonlite::fromJSON(txt=rawdata,simplifyDataFrame = T)
  allout <- NULL
  for(i in 1: length(dl$data$id)){
    stop_id <- dl$data$id[i]
    df <- dl$data$attributes[i,]
    parent_station <- dl$data$relationships$parent_station$data$id[i]
    this_station <- data.frame(stop_id, df, parent_station,row.names = NULL)
    allout <- rbind(allout,this_station)
  }
  #stop_id, stop_name, parent_station, parent_station_name, stop_lat, stop_lon, distance
  return(allout)
}
