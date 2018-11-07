\name{Tpredictionsbytrip} \alias{Tpredictionsbytrip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Query predictions by trip }
\description{ Returns the predicted arrival and departure times for a given
trip.} \usage{ Tpredictionsbytrip(trip_id, api_key) }
%- maybe also 'usage' for other objects documented here.
\arguments{ \item{trip_id}{ GTFS-compatible
trip_id value for which vehicle positions should be returned. Data type:
String. Example: "CR-Providence-CR-Weekday-807" } \item{api_key}{ API key for
MBTA API. To obtain one, visit the MBTA Developer Portal
(\url{http://realtime.mbta.com/Portal/}) } }

\value{ \item{route_id}{The unique GTFS-compatible identifier for the route
for which vehicle positions are returned. Example: "CR-Providence"}
\item{route_name}{The human-readable name for the route for which vehicle
positions are returned. Example: "Providence/Stoughton Line"}
\item{route_type}{The GTFS-compatible identifier for the type of service
(mode). Example: "2"} \item{mode_name}{The human-readable name for the type of
service (mode). Example: "Commuter Rail"} \item{trip_id}{The unique
GTFS-compatible identifier for the trip for which vehicle positions are
returned. Example: "CR-Providence-CR-Weekday-815"} \item{trip_name}{The
human-readable for the trip for which schedule is returned. Example: "815
(4:35 pm from South Station)"} \item{trip_headsign}{The text that identifies
the trip's destination to passengers. Example: "North Station"}
\item{direction_id}{The GTFS-compatible identifier for the direction. Example:
"0"} \item{direction_name}{The human-readable name for the direction. Example:
"Outbound"} \item{vehicle_id}{The GTFS-compatible unique identifier for the
vehicle. Example: "1531"} \item{vehicle_lat}{The GTFS-compatible latitude of
the vehicle. Example: "42.08997"} \item{vehicle_lon}{The GTFS-compatible
longitude of the vehicle. Example: "-71.4388"}
\item{vehicle_bearing}{GTFS-compatable bearing of the vehicle. This can be the
compass bearing, or the direction towards the next stop or intermediate
location. May be empty. Example: "259"} \item{vehicle_speed}{Identifies the
vehicle's momentary speed, in meters per second. Example: "21"}
\item{vehicle_timestamp}{Identifies the moment when the content of this feed
has been created, in epoch time. Example: "1400855704"}
\item{stop_sequence}{Identifies where the stop comes in the sequence of stops
for this trip. Example: "2"} \item{stop_id}{The GTFS-compatible unique
identifier for the stop. Example: "Back Bay"} \item{stop_name}{The
GTFS-compatible name for the stop. Example: "Back Bay"}
\item{sch_arr_dt}{Scheduled arrival time at the stop for the trip, in epoch
time. Example: "1361986080"} \item{sch_dep_dt}{Scheduled departure time at the
stop for the trip, in epoch time. Example: "1361986080"}
\item{pre_dt}{Predicted time at the stop - departure time for origin stop and
arrival time for all other stops - for the trip, in epoch time. Example:
"1400855700"} \item{pre_away}{Predicted amount of time until the vehicle
arrives at the stop, in seconds. Example: "339"} }




\seealso{ %% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{Tpredictionsbyroute}} \code{\link{Tpredictionsbystop}} }
