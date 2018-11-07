Troutes <- function(api_key){
  # returns list of MBTA route ids and route names
  query <- "routes"
  base_url <- paste0("https://api-v3.mbta.com/",query) # api key not needed for API V3
  rawdata <- readLines(base_url, warn = F)
  dl <- jsonlite::fromJSON(txt=rawdata,simplifyDataFrame = T,flatten=F)
  allout <- NULL
  for(i in 1:length(dl$data$id)){
    route_types <- dl$data$attributes$type[i]
    mode_names <- dl$data$attributes$description[i]
    route_id <- dl$data$id[i]
    route_name <- dl$data$attributes$long_name[i]
    direction_names <- c(dl$data$attributes$direction_names[[i]])
    thisout <- data.frame(route_type=route_types, mode_name=mode_names, route_id, route_name, direction_0 = direction_names[1], direction_1 = direction_names[2])
    allout <- rbind(allout,thisout)
  }
  return(allout)
}

