Theadways = function(from_stop_id, to_stop_id=NULL, route_id=NULL, direction_id=NULL, from_datetime=(Sys.time()-1800), to_datetime=Sys.time(), api_key){
  query <- "headways"
  from_datetime <- as.integer(from_datetime) # defaults to 30 minutes ago
  to_datetime <- as.integer(to_datetime) # defaults to now
  base_url <- paste0("http://realtime.mbta.com/developer/api/v2.1/",query,"?api_key=",api_key,"&format=json")
  full_url <- paste0(base_url,
                     "&stop=",from_stop_id,
                     ifelse(length(to_stop_id)>0,paste0("&to_stop=",to_stop_id),""),
                     ifelse(length(route_id)>0,paste0("&route=",route_id),""),
                     ifelse(length(direction_id)>0,paste0("&direction=",direction_id),""),
                     "&from_datetime=",from_datetime,"&to_datetime=",to_datetime)
  if(from_stop_id %in% c(70061,70105,70093,70036,70001,70060,70038)){
    warning("Headways not yet available for terminal stations.")
    allout <- NA
  }
  if(!(from_stop_id %in% c(70061,70105,70093,70036,70001,70060,70038))){ # check that not terminal station
    rawdata <- readLines(full_url, warn = F)
    dl <- jsonlite::fromJSON(txt=rawdata,simplifyDataFrame = T,flatten=F)
    allout <- data.frame(route_id = ifelse(length(dl$headways$route_id)>0,dl$headways$route_id,NA),
                         direction = ifelse(length(dl$headways$direction)>0,dl$headways$direction,NA),
                         current_dep_dt = ifelse(length(dl$headways$current_dep_dt)>0,dl$headways$current_dep_dt,NA),
                         previous_dep_dt = ifelse(length(dl$headways$previous_dep_dt)>0,dl$headways$previous_dep_dt,NA),
                         headway_time_sec = ifelse(length(dl$headways$headway_time_sec)>0,dl$headways$headway_time_sec,NA),
                         benchmark_headway_time_sec = ifelse(length(dl$headways$benchmark_headway_time_sec)>0,dl$headways$benchmark_headway_time_sec,NA),
                         threshold_01_flag = ifelse(length(dl$headways$threshold_01_flag)>0,dl$headways$threshold_01_flag,NA),
                         threshold_02_flag = ifelse(length(dl$headways$threshold_02_flag)>0,dl$headways$threshold_02_flag,NA),
                         threshold_03_flag = ifelse(length(dl$headways$threshold_03_flag)>0,dl$headways$threshold_03_flag,NA)
    )
    allout$current_dep_dt <- structure(as.numeric(as.character(allout$current_dep_dt)), class = c("POSIXt", "POSIXct"))
    allout$previous_dep_dt <- structure(as.numeric(as.character(allout$previous_dep_dt)), class = c("POSIXt", "POSIXct"))

  }
  return(allout)
}
#
# ## Example:
# api_key <- "wX9NwuHnZU2ToO7GmGR9uw"
# from_stop_id <- 70007 # Jackson Square Northbound
# from_stop_id <- 70034 # Malden Southbound
# to_stop_id <- NULL
# route_id <- NULL
# direction_id <- 1 # northbound
# from_datetime <- as.POSIXct("2015-08-03 00:00")
# to_datetime <- as.POSIXct("2015-08-03 23:59")
#
# Theadways()
#
# write.csv(allout,"C:/Users/jdbk/Dropbox (MIT)/MBTA/OTP data/headways_jacksonsq_2015-08-03.csv",row.names = F)
# write.csv(allout,"C:/Users/jdbk/Dropbox (MIT)/MBTA/OTP data/headways_malden_2015-08-03.csv",row.names = F)



