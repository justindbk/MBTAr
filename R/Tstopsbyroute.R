Tstopsbyroute <- function(route_id=NULL,route_name=NULL){
  # finds MBTA stops by GTFS route id number or route name
  query <- "stops"
  base_url <- paste0("https://api-v3.mbta.com/",query)
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
  full_url <- paste0(base_url,"?route=",route_id,"&api_key=8be3efdff2694d84aecb5cd16d0379ce")
  rawdata <- readLines(full_url, warn = F)
  dl <- jsonlite::fromJSON(txt=rawdata,simplifyDataFrame = T,flatten=F)
  allout <- NULL
  for(i in 1: length(dl$data$id)){
    stop_id <- dl$data$id[i]
    df <- dl$data$attributes[i,]
    parent_station <- ifelse("id" %in% names(dl$data$relationships$parent_station$data),dl$data$relationships$parent_station$data$id[i], NA)
    this_station <- data.frame(stop_id, df, parent_station,row.names = NULL)
    allout <- rbind(allout,this_station)
  }
  return(allout)
}
