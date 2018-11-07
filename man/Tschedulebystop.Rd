\name{Tschedulebystop} \alias{Tschedulebystop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Query schedule by stop }
\description{ Returns scheduled arrival and departure times for a direction
and rotue for a particular stop. } \usage{ Tschedulebystop(stop_id, route_id,
direction = NULL, datetime = Sys.time(), max_time = 60, max_trips = 5,
api_key) }
%- maybe also 'usage' for other objects documented here.
\arguments{ \item{stop_id}{ GTFS-compatible stop_id value for which schedule
should be returned. Example: "Back Bay" } \item{route_id}{GTFS-compatible
route_id value on the stop for which schedule should be returned. If not
included then schedule for all routes serving the stop will be returned.
Example: "CR-Providence" } \item{direction}{ GTFS-compatible direction_id
value on route of the stop for which schedule should be returned. Bit (0 or
1). If included then route must also be included. If not included then
schedule for all directions of the route serving the stop will be returned.
Example: "0" } \item{datetime}{ Epoch time after which schedule should be
returned. If included then must be within the next seven (7) days. If not
included then schedule starting from the current datetime will be returned.
Example: "1361989200" } \item{max_time}{ Defines maximum range of time (in
minutes) within which trips will be returned. Integer between 1 and 1440 (24
hours). If not included defaults to 60. Example: "120" } \item{max_trips}{
Defines number of trips to return. Integer between 1 and 100. If not included
defaults to 5. Example: "100" } \item{api_key}{ API key for MBTA API. To
obtain one, visit the MBTA Developer Portal
(\url{http://realtime.mbta.com/Portal/}) } }

\value{ \item{stop_id}{The GTFS-compatible unique identifier for the stop for
which the schedule is returned. Example: "Back Bay"} \item{stop_name}{The
GTFS-compatible name for the stop for which the schedule is returned. Example:
"Back Bay"} \item{route_type}{The GTFS-compatible identifier for the type of
service (mode). Example: "2"} \item{mode_name}{The human-readable name for the
type of service (mode). Example: "Commuter Rail"} \item{route_id}{The unique
GTFS-compatible identifier for the route. Example: "CR-Providence"}
\item{route_name}{The human-readable name for the route. Example:
"Providence/Stoughton Line"} \item{direction_id}{The GTFS-compatible
identifier for the direction. Example: "0"} \item{direction_name}{The
human-readable name for the direction. Example: "Outbound"} \item{trip_id}{The
unique GTFS-compatible identifier for the trip. Example:
"CR-Providence-CR-Weekday-815"} \item{trip_name}{The human-readable name for
the trip. Example: "815 (4:35 pm from South Station)"}
\item{sch_arr_dt}{Scheduled arrival time at the stop for the trip, in epoch
time. Example: "1361986080"} \item{sch_dep_dt}{Scheduled departure time at the
stop for the trip, in epoch time. Example: "1361986080"} }

\note{ If the GTFS-compatible stop_id value in the stop parameter in the
request is for a parent station then all routes that serve that parent station
are returned. }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ %% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{Tpredictionsbystop}} \code{\link{Tschedulebyroute}}
\code{\link{Tschedulebytrip}} }
