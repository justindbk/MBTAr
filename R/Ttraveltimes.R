Ttraveltimes = function(from_stop_id, to_stop_id, route_id=NULL, from_datetime=(Sys.time()-3600),to_datetime=Sys.time(),api_key){
  query <- "traveltimes"
  from_datetime <- as.integer(from_datetime) # defaults to 30 minutes ago
  to_datetime <- as.integer(to_datetime) # defaults to now
  base_url <- paste0("http://realtime.mbta.com/developer/api/v2.1/",query,"?api_key=",api_key,"&format=json")
  full_url <- paste0(base_url,"&from_stop=",from_stop_id,
                     "&to_stop=",to_stop_id,
                     ifelse(length(route_id)>0, paste0("&route=",route_id),""),
                     "&from_datetime=",from_datetime,
                     "&to_datetime=",to_datetime)
  if(from_stop_id %in% c(70061,70105,70093,70036,70001,70060,70038) | to_stop_id %in% c(70061,70105,70093,70036,70001,70060,70038)){
    warning("Travel times not yet available for terminal stations.")
    allout <- NA
  }
  if(!(from_stop_id %in% c(70061,70105,70093,70036,70001,70060,70038)) & !(to_stop_id %in% c(70061,70105,70093,70036,70001,70060,70038))){ # check that not terminal stations
    rawdata <- readLines(full_url, warn = F)
    dl <- jsonlite::fromJSON(txt=rawdata,simplifyDataFrame = T,flatten=F)
    allout <- data.frame(route_id = dl$travel_times$route_id,
                         direction = dl$travel_times$direction,
                         dep_dt = dl$travel_times$dep_dt,
                         arr_dt = dl$travel_times$arr_dt,
                         travel_time_sec = dl$travel_times$travel_time_sec,
                         benchmark_travel_time_sec = dl$travel_times$benchmark_travel_time_sec
    )
    allout$dep_dt <- structure(as.numeric(as.character(allout$dep_dt)), class = c("POSIXt", "POSIXct"))
    allout$arr_dt <- structure(as.numeric(as.character(allout$arr_dt)), class = c("POSIXt", "POSIXct"))
    # route_id, direction, dep_dt, arr_dt, travel_time_sec, benchmark_travel_time_sec
  }
  return(allout)
}
