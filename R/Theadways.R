#' To get headways and train arrive times for a given station/route/direction in
#' a certain time window.. Returns a list of headways at an origin station
#' during a particular time period on a particular route.
#'
#' @param from_stop_id GTFS-compatible stop_id for the origin stop for which
#'   headways should be returned.
#' @param to_stop_id GTFS-compatible stop_id for the destination stop for which
#'   headways should be returned. If empty will use departures from origin stop
#'   with any destination stop on that route (i.e. not distinguishing between
#'   branches).
#' @param route_id GTFS-compatible route_id value for which headways should be
#'   returned. If this is not included, headways for all routes between the from
#'   and to stop will be provided.
#' @param direction_id Direction of travel for headways to be returned. If
#'   empty, will rely on
#' @param from_datetime The start of the time period that the headways (arrival
#'   time at the origin stop) should fall within; converts to epoch time.
#' @param to_datetime The end of the time period that the headways (arrival time
#'   at the origin stop) should fall within; converts to epoch time.
#' @param api_key API key for MBTA Performance API. To obtain one, visit the
#'   MBTA Developer Portal (\url{https://mbta.com/developers/mbta-performance/})
#'
#' @return Arrival times for vehicles at the particular origin stop during the
#'   time window requested. \item{route_id}{GTFS-compatible route identifier for
#'   which headways are returned.} \item{direction}{Direction id (0/1) for which
#'   headways are returned.} \item{current_dep_dt}{Next departure time from
#'   origin stop during the time window specified.}
#'   \item{previous_dep_dt}{Previous departure time at destination stop during
#'   the time window specified.} \item{headway_time_sec}{Actual headway for next
#'   arriving vehicle during time window specified, in seconds.}
#'   \item{benchmark_headway_time_sec}{Benchmark headway for each trip, in
#'   seconds.} \item{threshold_01_flag}{Binary variable for whether a given
#'   headway was above the first threshold for delayed headways.}
#'   \item{threshold_02_flag}{Binary variable for whether a given headway was
#'   above the second threshold for delayed headways.}
#'   \item{threshold_03_flag}{Binary variable for whether a given headway was
#'   above the third threshold for delayed headways.}
#'
#' @export
#'
#' @examples
#' Theadways(from_stop_id = 70007, # Jackson Square Northbound
#' to_stop_id = 70034, # Malden Southbound
#' route_id = NULL,
#' direction_id = 1, # northbound
#' from_datetime = as.POSIXct("2015-08-03 00:00"),
#' to_datetime = as.POSIXct("2015-08-03 23:59"),
#' api_key = "wX9NwuHnZU2ToO7GmGR9uw")
#' #'
#'
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



