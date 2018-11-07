\name{Tpredictionsbyroute} \alias{Tpredictionsbyroute}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Query predictions by
route } \description{ Returns predicted upcoming arrivals and departures in
the next hour for a particular route. } \usage{ Tpredictionsbyroute(route_id,
include_access_alerts = FALSE, include_service_alerts = TRUE, api_key) }
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{route_id}{ GTFS-compatible route_id value for which predictions should
be returned. Example: "Orange"} \item{include_access_alerts}{ Whether or not
alerts pertaining to accessibility (elevators, escalators) should be returned.
Data type: logical. Defaults to FALSE. } \item{include_service_alerts}{
Whether or not service alerts should be returned. Data type: logical. Defaults
to TRUE. } \item{api_key}{ API key for MBTA API. To obtain one, visit the MBTA
Developer Portal (\url{http://realtime.mbta.com/Portal/}) } }

\value{ \item{route_id}{The unique GTFS-compatible identifier for the route
for which predictions are returned. Example: "CR-Franklin"}
\item{route_name}{The human-readable name for the route for which predictions
are returned. Example: "Franklin Line"} \item{route_type}{The GTFS-compatible
identifier for the type of service (mode). Example: "2"} \item{mode_name}{The
human-readable name for the type of service (mode). Example: "Commuter Rail"}
\item{direction_id}{The GTFS-compatible identifier for the direction. Example:
"0"} \item{direction_name}{The human-readable name for the direction. Example:
"Outbound"} \item{trip_id}{The unique GTFS-compatible identifier for the trip.
Example: "CR-Providence-CR-Weekday-815"} \item{trip_name}{The human-readable
name for the trip. Example: "815 (4:35 pm from South Station)"}
\item{trip_headsign}{The text that identifies the trip's destination to
passengers. Example: "North Station"} \item{vehicle_id}{The GTFS-compatible
unique identifier for the vehicle. Example: "1531"} \item{vehicle_lat}{The
GTFS-compatible latitude of the vehicle. Example: "42.08997"}
\item{vehicle_lon}{The GTFS-compatible longitude of the vehicle. Example:
"-71.4388"} \item{vehicle_bearing}{GTFS-compatable bearing of the vehicle.
This can be the compass bearing, or the direction towards the next stop or
intermediate location. May be empty. Example: "259"}
\item{vehicle_speed}{Identifies the vehicle's momentary speed, in meters per
second. Example: "21"} \item{vehicle_timestamp}{Identifies the moment when the
content of this feed has been created, in epoch time. Example: "1400855704"}
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
