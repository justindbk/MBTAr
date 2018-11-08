\name{Ttravelperformance}
\alias{Ttravelperformance}
\title{
Query performance statistics for many trips contained in a dataframe.
}
\description{
Returns travel time performance statistics for many sets of trips with specified origin-destination pairs and specific time periods.
}
\usage{
Ttravelperformance(enter_time, exit_time = NULL, enter_route_name, exit_route_name,
enter_stop_name, exit_stop_name, data, api_key)
}
\arguments{
  \item{enter_time}{
Name of column in data that specifies entrance times for the trips for which performance is to be returned.
}
  \item{exit_time}{
Name of column in data that specifies exit times for the trips for which performance is to be returned.
Defaults to 30 minutes from the enter_time value.
}
  \item{enter_route_name}{
Name of column in data that specifies GTFS-compatible route ID for entrance stations.
}
  \item{exit_route_name}{
Name of column in data that specifies GTFS-compatible route ID for exit stations.
Travel time API request only available for single unlinked rides as of Release v2.
}
  \item{enter_stop_name}{
Name of column in data that specifies human-readable text name for the stop where trips began (origin name).
}
  \item{exit_stop_name}{
Name of column in data that specifies human-readable text name for the stop where trips ended (destination name).
}
  \item{data}{
Dataframe from which holds the previously named arguments.
}
\item{api_key}{API key for MBTA API. To obtain one, visit the MBTA Developer
Portal (\url{http://realtime.mbta.com/Portal/})}
}

\value{
Dataframe containing the following:
\item{traveltime_mean}{Mean travel time between origin and destination stops during the time period specified.}
\item{traveltime_bench}{Benchmark travel times between origin and destination stops during the time period specified.
Benchmarks come from historical data over the past 30 days for travel times.}
\item{traveltime_perf}{Average travel time as a proportion of the benchmark time, or traveltime_mean/traveltime_bench.}
}

\seealso{
\code{\link{Ttraveltimes}}
}

