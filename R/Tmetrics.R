Tmetrics = function(from_stop_id=NULL, to_stop_id=NULL, route_id=NULL, direction=NULL, from_datetime=(Sys.time()-3600),to_datetime=Sys.time(),api_key){
  query <- "metrics"
  from_datetime <- as.integer(from_datetime) # defaults to 30 minutes ago - careful not to do too long a time range
  to_datetime <- as.integer(to_datetime) # defaults to now
  base_url <- paste0("http://realtime.mbta.com/developer/api/v2.1/",query,"?api_key=",api_key,"&format=json")
  full_url <- paste0(base_url,ifelse(length(from_stop_id)>0,paste0("&from_stop=",from_stop_id),""),ifelse(length(to_stop_id)>0,paste0("&to_stop=",to_stop_id),""),ifelse(length(route_id)>0,paste0("&route=",route_id),""),"&from_datetime=",from_datetime,"&to_datetime=",to_datetime)
  if(length(from_stop_id) > 0){
    if(from_stop_id %in% c(70061,70105,70093,70036,70001,70060,70038) | to_stop_id %in% c(70061,70105,70093,70036,70001,70060,70038)){ # check if terminal station
      warning("Metrics not yet available for terminal stations.")
      allout <- NA
    }
    if(!(from_stop_id %in% c(70061,70105,70093,70036,70001,70060,70038)) & !(to_stop_id %in% c(70061,70105,70093,70036,70001,70060,70038))){
      rawdata <- readLines(full_url, warn = F)
      dl <- jsonlite::fromJSON(txt=rawdata,simplifyDataFrame = T,flatten=F)
      allout <- data.frame(threshold_id = dl$metrics$threshold_id,
                           threshold_type = dl$metrics$threshold_type,
                           threshold_name = dl$metrics$threshold_name,
                           metric_result = dl$metrics$metric_result
      )
    }
  }
  if(length(from_stop_id) < 1){
    rawdata <- readLines(full_url, warn = F)
    dl <- jsonlite::fromJSON(txt=rawdata,simplifyDataFrame = T,flatten=F)
    allout <- data.frame(threshold_id = dl$metrics$threshold_id,
                         threshold_type = dl$metrics$threshold_type,
                         threshold_name = dl$metrics$threshold_name,
                         metric_result = dl$metrics$metric_result
                         )
  }
  return(allout)
}
