\name{Ttraveltimes}
\alias{Ttraveltimes}
\title{
Query travel times and travel time performance}
\description{
Returns a list of travel times between an origin destination
pair during a particular time period.
}
\usage{
Ttraveltimes(from_stop_id, to_stop_id, route_id = NULL,
  from_datetime = (Sys.time() - 3600),
  to_datetime = Sys.time(),
  api_key)
}
\arguments{
  \item{from_stop_id}{
GTFS-compatible stop_id for the origin stop for which travel times should be returned.
}
  \item{to_stop_id}{
GTFS-compatible stop_id for the destination stop for which travel times should be returned.
}
  \item{route_id}{
GTFS-compatible route_id value for which travel times should be returned. If this is not included, travel times for all routes between the from and to stop will be provided.
}
  \item{from_datetime}{
The start of the time period that the travel time (arrival time at the destination stop) should fall within; converts to epoch time.
}
  \item{to_datetime}{
The end of the time period that the travel time (arrival time at the destination stop) should fall within; converts to epoch time.
}
  \item{api_key}{
API key for MBTA API. To obtain one, visit the MBTA Developer
Portal (\url{https://mbta.com/developers/mbta-performance})}
}

\value{
\item{route_id}{Route ID for which travel time are returned.}
\item{direction}{Direction id for which travel times are returned.}
\item{dep_dt}{Actual departure times from origin stop during the time window specified.}
\item{arr_dt}{Actual arrival times at destination stop during the time window specified.}
\item{travel_time_sec}{Actual travel times for all trips during time window specified, in seconds.}
\item{benchmark_travel_time_sec}{Benchmark travel times for each trip, in seconds. Benchmarks are based on median travel times during the last 30 days.}
}

