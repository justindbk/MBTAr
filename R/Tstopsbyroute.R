Tstopsbyroute <- function(route_id=NULL,route_name=NULL){
  # finds MBTA stops by GTFS route id number or route name
  query <- "stopsbyroute"
  base_url <- paste0("http://realtime.mbta.com/developer/api/v2/",query,"?api_key=",api_key,"&format=json")
  if(length(route_id)==0){ # checks if this is empty
    # routes <- Troutes(api_key) # preload from package instead
    route_id <- MBTAr::routes$route_id[which(MBTAr::routes$route_name==route_name)] # finds route_id from entered route_name
    if(length(route_id)==0){ # if nothing found
      stop("Please enter a valid GTFS-compatible route id or valid route name. Refer to the included dataframe 'routes' for this information.")
    }
  }
  if(length(route_id)>0){ # if either entered or found via name
    if((route_id %in% MBTAr::routes$route_id)==F){ # check if not in list
      stop("Please enter a valid GTFS-compatible route id or valid route name. Refer to the included dataframe 'routes' for this information.")
    }
  }
  full_url <- paste0(base_url,"&route=",route_id)
  rawdata <- readLines(full_url, warn = F)
  dl <- jsonlite::fromJSON(txt=rawdata,simplifyDataFrame = T,flatten=F)
  allout <- NULL
  for(i in 1: length(dl$direction$direction_id)){
    direction_ids <- dl$direction$direction_id[i]
    direction_names <- dl$direction$direction_name[i]
    stops <- dl$direction$stop[[i]][,c("stop_order","stop_id","stop_name","parent_station","parent_station_name","stop_lat","stop_lon")]
    thisout <- data.frame(direction_id=direction_ids,direction_name=direction_names,stops)
    allout <- rbind(allout,thisout)
  }
  return(allout)
}
