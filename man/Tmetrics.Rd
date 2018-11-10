\name{Tmetrics}
\alias{Tmetrics}
\title{
Query real-time performance statistics.
}
\description{
Returns real-time performance statistics for an origin-destination pair or systemwide for a specified time range.
}
\usage{
Tmetrics(from_stop_id = NULL, to_stop_id = NULL, route_id = NULL, direction = NULL,
from_datetime = (Sys.time() - 3600), to_datetime = Sys.time(), api_key)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{from_stop_id}{
GTFS-compatible stop_id for the origin stop for which metrics should be returned. If not specified, will return metrics for all routes.
}
  \item{to_stop_id}{
GTFS-compatible stop_id for the destination stop for which metrics should be returned.
}
 \item{route_id}{
GTFS-compatible route_id value for which metrics should be returned. If not specified, will return metrics for all routes.
}
  \item{direction}{
GTFS-compatible direction value (1 or 0) for which metrics in a particular direction should be returned. If this is not included, metrics will be aggregated for all directions.
}
  \item{from_datetime}{
The start of the time period that the metrics should be aggregated for; converts to epoch time. Defaults to 30 minutes ago.
}
  \item{to_datetime}{
The end of the time period that the metrics should be aggregated for; converts to epoch time. Defaults to now (system time).
}
\item{api_key}{API key for MBTA API. To obtain one, visit the MBTA Developer
Portal (\url{https://mbta.com/developers/mbta-performance})}
}

\value{
\item{threshold_id}{Reference number for this type of performance metric}
\item{threshold_type}{Type of performance metric (wait time-based or travel time-based)}
\item{threshold_name}{Text name for this type of performance metric}
\item{metric_result}{Corresponding result for this performance metric (percentage of trips that met or performed better than this threshold)}
}

\seealso{
\code{\link{Tdailymetrics}}
}
\examples{
  ## Authenticate:
  \dontshow{
  test_key <- "Zt0g4Tkp2kaLNxoKFXI_Kg"
  }
  mykey <- NULL
  mykey <- assign("mykey", value = test_key, envir = .GlobalEnv)
  ## use your own key from http://realtime.mbta.com/Portal/ instead of the test key

  Tmetrics(from_stop_id = NULL, # entire line
  to_stop_id = NULL, # entire line
  route_id = "Red", # Red Line
  direction = 0, # southbound only
  from_datetime = (Sys.time() - 3600), # 30 minutes ago
  to_datetime = Sys.time(), # now
api_key = mykey) # user-defined API key

## Returns:
#      threshold_id threshold_type   threshold_name metric_result
# 1 threshold_id_01      wait_time          Headway        0.8395
# 2 threshold_id_02      wait_time          Big Gap        0.9395
# 3 threshold_id_03      wait_time       2X Headway        0.9809
# 4 threshold_id_04    travel_time delayed < 3 min.        0.9792
# 5 threshold_id_05    travel_time delayed < 6 min.        0.9963

}
