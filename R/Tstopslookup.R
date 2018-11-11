#' Search for information on a stop with a given name. Useful when stop_id is not known.
#'
#' @param stop_name Name of stop to search for.
#'
#' @return Dataframe with row for each potential matching stop and the following columns:
#' \item{route_id}{GTFS-compatible route_id for the stop match.}
#' \item{direction_id}{Direction ID (0/1)}
#' \item{direction_name}{Human-readable direction identifier}
#' \item{stop_order}{Order in given direction of stop}
#' \item{stop_id}{GTFS-compatible ID of stop}
#' \item{stop_name}{Name of stop}
#' \item{parent_station}{Parent station GTFS-compatible ID}
#' \item{parent_station_name}{Parent station name}
#' \item{stop_lat}{Stop latitude}
#' \item{stop_lon}{Stop longitude}
#' @export
#'
#' @examples
#' \dontrun{
#' Tstopslookup(stop_name = "Mattapan") # this would take a long tiem (~70 seconds) so is left out here
#' }
Tstopslookup = function(stop_name){
  # allroutes <- Troutes(api_key = api_key) # preload from package instead
  allroutes <- MBTAr::routes
  stopmatches <- NULL
  for(i in 1:length(allroutes$route_id)){
    allstops <- Tstopsbyroute(route_id = allroutes$route_id[i])
    if(length(allstops$parent_station_name)>0){
      match_index <- which(
        gsub("(\\w+)\\W+.*","\\1",x=allstops$name) == gsub("(\\w+)\\W+.*","\\1",x=stop_name) | # cuts name to first word
          gsub("(\\w+)\\W+.*","\\1",x=allstops$parent_station_name) == gsub("(\\w+)\\W+.*","\\1",x=stop_name))
    } else{
      match_index <- which(
        gsub("(\\w+)\\W+.*","\\1",x=allstops$name) == gsub("(\\w+)\\W+.*","\\1",x=stop_name)) # cuts name to first word
    }
    thismatches <- allstops[match_index,]
    if(nrow(thismatches)>0){
      thismatches <- data.frame(route_id=allroutes$route_id[i],thismatches)
      stopmatches <- rbind(stopmatches,thismatches)
      }
  }
  return(stopmatches)
}


## Example:
# api_key <- "wX9NwuHnZU2ToO7GmGR9uw"
# stop_name <- "Jackson Square"
# Tstopslookup("Jackson Square", api_key = api_key)
# route_id direction_id direction_name stop_order stop_id                        stop_name parent_station parent_station_name  stop_lat   stop_lon
# 17   Orange            0     Southbound        160   70006        Jackson Square - Outbound    place-jaksn      Jackson Square 42.323132 -71.099592
# 24   Orange            1     Northbound         30   70007         Jackson Square - Inbound    place-jaksn      Jackson Square 42.323132 -71.099592
# 8        14            0       Outbound          8   11531 Jackson Sq Station @ Orange Line    place-jaksn      Jackson Square 42.323132 -71.099592
# 93       14            1        Inbound         41   11531 Jackson Sq Station @ Orange Line    place-jaksn      Jackson Square 42.323132 -71.099592
# 18       22            0       Outbound          9   11531 Jackson Sq Station @ Orange Line    place-jaksn      Jackson Square 42.323132 -71.099592
# 67       22            1        Inbound         25   11531 Jackson Sq Station @ Orange Line    place-jaksn      Jackson Square 42.323132 -71.099592
# 6        29            0       Outbound          6   11531 Jackson Sq Station @ Orange Line    place-jaksn      Jackson Square 42.323132 -71.099592
# 70       29            1        Inbound         31   11531 Jackson Sq Station @ Orange Line    place-jaksn      Jackson Square 42.323132 -71.099592
# 23       41            0       Outbound         23   11531 Jackson Sq Station @ Orange Line    place-jaksn      Jackson Square 42.323132 -71.099592
# 43       41            1        Inbound         11   11531 Jackson Sq Station @ Orange Line    place-jaksn      Jackson Square 42.323132 -71.099592
# 29       44            0       Outbound         28   11531 Jackson Sq Station @ Orange Line    place-jaksn      Jackson Square 42.323132 -71.099592
# 30       44            1        Inbound          1   11531 Jackson Sq Station @ Orange Line    place-jaksn      Jackson Square 42.323132 -71.099592
# 20     9703            1        Inbound         20   11531 Jackson Sq Station @ Orange Line    place-jaksn      Jackson Square 42.323132 -71.099592
