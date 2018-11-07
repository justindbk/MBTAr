\name{Tschedulebyroute} \alias{Tschedulebyroute}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Query the schedule by route }
\description{ Returns the scheduled arrivals and departures in a direction for
a particular route. } \usage{ Tschedulebyroute(route_id, direction = NULL,
datetime = Sys.time(), max_time = 60, max_trips = 5, api_key) }
%- maybe also 'usage' for other objects documented here.
\arguments{ \item{route_id}{
GTFS-compatible route_id value for which schedule should be returned. Example:
"CR-Providence" } \item{direction}{ GTFS-compatible direction_id value on
route for which schedule should be returned. If not included then schedule for
all directions of the route will be returned. Example: "0" } \item{datetime}{
Epoch time after which schedule should be returned. If included then must be
within the next seven (7) days. If not included then schedule starting from
the current datetime will be returned. Example: "1361989200" }
\item{max_time}{ Defines maximum range of time (in minutes) within which trips
will be returned. Integer between 1 and 1440 (24 hours). If not included
defaults to 60. Example: "120" } \item{max_trips}{ Defines number of trips to
return. Integer between 1 and 100. If not included defaults to 5. Example:
"100" } \item{api_key}{ API key for MBTA API. To obtain one, visit the MBTA
Developer Portal (\url{http://realtime.mbta.com/Portal/}) } }

\value{ \item{route_id}{The unique GTFS-compatible identifier for the route
for which the schedule is returned. Example: "CR-Providence"}
\item{route_name}{The human-readable name for the route for which the schedule
is returned. Example: "Providence/Stoughton Line"} \item{direction_id}{The
GTFS-compatible identifier for the direction. Example: "0"}
\item{direction_name}{The human-readable name for the direction. Example:
"Outbound"} \item{trip_id}{The unique GTFS-compatible identifier for the trip.
Example: "CR-Providence-CR-Weekday-815"} \item{trip_name}{The human-readable
name for the trip. Example: "815 (4:35 pm from South Station)"}
\item{stop_sequence}{Identifies where the stop comes in the sequence of stops
for this trip. Example: "2"} \item{stop_id}{The GTFS-compatible unique
identifier for the stop. Example: "Back Bay"} \item{stop_name}{The
GTFS-compatible name for the stop. Example: "Back Bay"}
\item{sch_arr_dt}{Scheduled arrival time at the stop for the trip, in epoch
time. Example: "1361986080"} \item{sch_dep_dt}{Scheduled departure time at the
stop for the trip, in epoch time. Example: "1361986080"} }



\seealso{ %% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{Tpredictionsbyroute}} \code{\link{Tschedulebystop}}
\code{\link{Tschedulebytrip}} }
