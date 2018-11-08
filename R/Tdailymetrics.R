Tdailymetrics = function(route_id=NULL, from_date=(Sys.Date()-8), to_date=Sys.Date()-1, api_key){
  query <- "dailymetrics" # returns daily metrics for Red, Orange, Blue, Green lines
  # from_date defaults to 8 days ago, must be in YYYY-MM-DD format
  # to_date defaults to yesterday for last week of performance, must be in YYYY-MM-DD format
  base_url <- paste0("http://realtime.mbta.com/developer/api/v2.1/",query,"?api_key=",api_key,"&format=json")
  full_url <- paste0(base_url,ifelse(length(route_id)>0,paste0("&route=",route_id),""),"&from_service_date=",from_date,"&to_service_date=",to_date)

    rawdata <- readLines(full_url, warn = F)
    dl <- jsonlite::fromJSON(txt=rawdata,simplifyDataFrame = T,flatten=F)
    allout <- data.frame(service_date = dl$daily_metrics$service_date,
                         route_id = dl$daily_metrics$route_id,
                         threshold_id = dl$daily_metrics$threshold_id,
                         threshold_type = dl$daily_metrics$threshold_type,
                         threshold_name = dl$daily_metrics$threshold_name,
                         metric_result = dl$daily_metrics$metric_result
    )
  return(allout)
}

