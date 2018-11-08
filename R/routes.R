#' Routes and respective information for the MBTA system
#'
#' @source From the MBTA GTFS API V3 in November 2018. \url{}
#' @format A data frame with columns:
#' \describe{
#'  \item{route_type}{A value between 0 and 4 indicating the type of vehicle use on this route. 0 is light rail, 1 is heavy rail, 2 is commuter rail, 3 is bus, and 4 is ferry.}
#'  \item{mode_name}{Official name for route: Rapid Transit, Commuter Rail, Limited Service, Local Bus, Key Bus Route (Frequent Service), Express Bus, or Ferry.}
#'  \item{route_id}{GTFS-compatible id (text or number) for route.}
#'  \item{route_name}{Longform name for route (text).}
#'  \item{direction_0}{First direction of travel.}
#'  \item{direction_1}{Second direction of travel.}
#' }
#' @examples
#' \dontrun{
#'  routes
#' }
"routes"
