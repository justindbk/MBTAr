\name{Tschedulebytrip} \alias{Tschedulebytrip}
\title{ Query schedule by trip }
\description{ Returns scheduled arrival and departure times for a particular
trip. } \usage{ Tschedulebytrip(trip_id, datetime = Sys.time(), api_key) }
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{trip_id}{ GTFS-compatible trip_id value for which schedule should be
returned. Example: "CR-Providence-CR-Weekday-807" } \item{datetime}{ Epoch
time after which schedule should be returned. If included then must be within
the next seven (7) days. If not included then schedule starting from the
current datetime will be returned, using Sys.time converted to epoch time.
Example: "1361989200"} \item{api_key}{ API key for MBTA API. To obtain one,
visit the MBTA Developer Portal (\url{http://realtime.mbta.com/Portal/}) } }

\value{ \item{route_id}{The unique GTFS-compatible identifier for the route
for which schedule is returned. Example: "CR-Providence"}
\item{route_name}{The human-readable name for the route for which schedule is
returned. Example: "Providence/Stoughton Line"} \item{trip_id}{The unique
GTFS-compatible identifier for the trip for which schedule is returned.
Example: "CR-Providence-CR-Weekday-815"} \item{trip_name}{The human-readable
for the trip for which schedule is returned. Example: "815 (4:35 pm from South
Station)"} \item{direction_id}{The GTFS-compatible identifier for the
direction. Example: "0"} \item{direction_name}{The human-readable name for the
direction. Example: "Outbound"} \item{stop_sequence}{Identifies where the stop
comes in the sequence of stops for this trip. Example: "2"} \item{stop_id}{The
GTFS-compatible unique identifier for the stop. Example: "Back Bay"}
\item{stop_name}{The GTFS-compatible name for the stop. Example: "Back Bay"}
\item{sch_arr_dt}{Scheduled arrival time at the stop for the trip, in epoch
time. Example: "1361986080"} \item{sch_dep_dt}{Scheduled departure time at the
stop for the trip, in epoch time. Example: "1361986080"} }



\seealso{ %% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{Tpredictionsbytrip}} \code{\link{Tschedulebystop}}
\code{\link{Tschedulebyroute}} }
