Talertbyid <- function(alert_id){
  query <- "alerts"
  base_url <- paste0("https://api-v3.mbta.com/",query,"/",alert_id,"&api_key=8be3efdff2694d84aecb5cd16d0379ce")
  # full_url <- paste0(base_url,"&include_access_alerts=",include_access_alerts,"&include_service_alerts=",include_service_alerts)
  rawdata <- readLines(base_url, warn = F)
  dl <- jsonlite::fromJSON(txt=rawdata,simplifyDataFrame = T,flatten=F)
  ## old code:
  # allout <- NULL
  # for(i in 1:length(dl$data$id)){
  #   alltimes <- NULL
  #   for(j in 1:nrow(dl$data$attributes$active_period[[i]])){
  #     thistime <- data.frame(
  #       alert_id = dl$data$id[i],
  #       effect_name = dl$data$attributes$service_effect[i],
  #       effect = dl$data$attributes$effect[i],
  #       cause= dl$data$attributes$cause[i],
  #       header_text = dl$data$attributes$header[i],
  #       short_header_text = dl$data$attributes$short_header[i],
  #       description_text = dl$data$attributes$description[i],
  #       severity = dl$data$attributes$severity[i],
  #       created_dt = dl$data$attributes$created_dt[i],
  #       last_modified_dt = dl$data$attributes$last_modified_dt[i],
  #       timeframe_text = dl$data$attributes$timeframe[i],
  #       alert_lifecycle = dl$data$attributes$lifecycle[i],
  #       effect_start = dl$data$attributes$active_period$start[j],
  #       effect_end = dl$data$attributes$active_period$end[j],
  #       affected_activities = ifelse(length(dl$data$attributes$informed_entity[[i]]$activities)>0,dl$data$attributes$informed_entity[[i]]$route_type,NA),
  #       affected_route_type = ifelse(length(dl$data$attributes$informed_entity[[i]]$route_type)>0,dl$data$attributes$informed_entity[[i]]$route_type,NA),
  #       affected_mode_name = ifelse(length(dl$data$attributes$informed_entity[[i]]$mode_name)>0,dl$data$attributes$informed_entity[[i]]$mode_name,NA),
  #       affected_route_id = ifelse(length(dl$data$attributes$informed_entity[[i]]$route)>0,dl$data$attributes$informed_entity[[i]]$route,NA),
  #       # affected_direction_id = ifelse(length(dl$data$attributes$informed_entity[[i]]$direction_id)>0,dl$data$attributes$informed_entity[[i]]$direction_id,NA), # sometimes NA
  #       # affected_direction_name = ifelse(length(dl$data$attributes$informed_entity[[i]]$direction_name)>0,dl$data$attributes$informed_entity[[i]]$direction_name,NA), # sometimes NA
  #       affected_trip_id = ifelse(length(dl$data$attributes$informed_entity[[i]]$trip_id)>0,dl$data$attributes$informed_entity[[i]]$trip_id,NA), # sometimes NA
  #       affected_trip_name = ifelse(length(dl$data$attributes$informed_entity[[i]]$trip_name)>0,dl$data$attributes$informed_entity[[i]]$trip_name,NA), # sometimes NA
  #       affected_stop_id = ifelse(length(dl$data$attributes$informed_entity[[i]]$stop)>0,dl$data$attributes$informed_entity[[i]]$stop,NA), # sometimes NA
  #       affected_stop_name = ifelse(length(dl$data$attributes$informed_entity[[i]]$stop_name)>0,dl$data$attributes$informed_entity[[i]]$stop_name,NA), # sometimes NA
  #       affected_route_hide = ifelse(length(dl$data$attributes$informed_entity[[i]]$route_hide)>0,dl$data$attributes$informed_entity[[i]]$route_hide,NA), # sometimes NA
  #       # affected_elev_id = ifelse(length(dl$data$attributes$affected_services$elevators$elev_id)>0,dl$data$attributes$affected_services$elevators$elev_id,NA),
  #       # affected_elev_name = ifelse(length(dl$data$attributes$affected_services$elevators$elev_name)>0,dl$data$attributes$affected_services$elevators$elev_name,NA),
  #       # affected_elev_type = ifelse(length(dl$data$attributes$affected_services$elevators$elev_type)>0,dl$data$attributes$affected_services$elevators$elev_type,NA),
  #       # affected_elev_stop_id = ifelse(length(dl$data$attributes$affected_services$elevators$stop$stop_id)>0,dl$data$attributes$affected_services$elevators$stop$stop_id,NA),
  #       # affected_elev_stop_name = ifelse(length(dl$data$attributes$affected_services$elevators$stop$stop_name)>0,dl$data$attributes$affected_services$elevators$stop$stop_name,NA),
  #       # affected_elev_stop_parent_id = ifelse(length(dl$data$attributes$affected_services$elevators$stop$parent_station)>0,dl$data$attributes$affected_services$elevators$stop$parent_station,NA),
  #       # affected_elev_stop_parent_name = ifelse(length(dl$data$attributes$affected_services$elevators$stop$parent_station_name)>0,dl$data$attributes$affected_services$elevators$stop$parent_station_name,NA)
  #     )
  #     alltimes <- rbind(alltimes,thistime)
  #   }
  #   allout <- rbind(allout,alltimes)
  # }
  # return(allout)
  return(dl)
}
