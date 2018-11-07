\name{Tvehiclesbyroute} \alias{Tvehiclesbyroute}
\title{ Query vehicles by a route }
\description{ Returns vehicle positions for upcoming trips (including trips
already underway) in a direction for a particular route. } \usage{
Tvehiclesbyroute(route_id, api_key) }
\arguments{ \item{route_id}{ GTFS-compatible route_id value
for which vehicle positions should be returned. Example: "Red" }
\item{api_key}{ API key for MBTA API. To obtain one, visit the MBTA Developer
Portal (\url{http://realtime.mbta.com/Portal/}) } }

\value{ \item{route_id}{The unique GTFS-compatible identifier for the route
for which vehicle positions are returned. Example: "CR-Franklin"}
\item{route_name}{The human-readable name for the route for which vehicle
positions are returned. Example: "Franklin Line"} \item{route_type}{The
GTFS-compatible identifier for the type of service (mode). Example: "2"}
\item{mode_name}{The human-readable name for the type of service (mode).
Example: "Commuter Rail"} \item{direction_id}{The GTFS-compatible identifier
for the direction. Example: "0"} \item{direction_name}{The human-readable name
for the direction. Example: "Outbound"} \item{trip_id}{The unique
GTFS-compatible identifier for the trip. Example:
"CR-Providence-CR-Weekday-815"} \item{trip_name}{The human-readable name for
the trip. Example: "815 (4:35 pm from South Station)"}
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
content of this feed has been created, in epoch time. Example: "1400855704"} }



\seealso{ %% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{Tvehiclesbytrip}} }

