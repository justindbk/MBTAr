\name{Tstopsbyroute} \alias{Tstopsbyroute}
\title{ Query stops by route } \description{
Returns the stops for a particular route, specified by id or by name. }
\usage{ Tstopsbyroute(route_id = NULL, route_name = NULL, api_key) }
\arguments{ \item{route_id}{
GTFS-compatible route_id value for which stops should be returned. Example:
"Red" } \item{route_name}{ The full human-readable name for the route for
which vehicle positions are returned. Example: "Red Line" } \item{api_key}{
API key for MBTA API. To obtain one, visit the MBTA Developer Portal
(\url{http://realtime.mbta.com/Portal/}) } }

\value{ \item{route_id}{The unique GTFS-compatible identifier for the route
for which stops are returned. Example: "Orange"} \item{route_name}{The
human-readable name for the route for which stops are returned. Example:
"Orange Line"} \item{direction_id}{The GTFS-compatible identifier for the
direction. Example: "0"} \item{direction_name}{The human-readable name for the
direction. Example: "Southbound"} \item{stop_order}{Identifies where the stop
comes in the order of stops for this route and direction (note: not guaranteed
to be unique). Example: "1"} \item{stop_id}{The GTFS-compatible unique
identifier for the stop. Example: "70063"} \item{stop_name}{The
GTFS-compatible name for the stop (not unique). Example: "Davis Sq - Inbound"}
\item{parent_station}{The GTFS-compatible unique identifier for the station
associated with the stop. (note: can be empty if stop does not have an
associated station). Example: "place-davis"} \item{parent_station_name}{The
human-readable name for the larger station associated with the stop. (note:
can be empty if stop does not have an associated station). Example: "Davis
Station"} \item{stop_lat}{The GTFS-compatible latitude of the station.
Example: "42.3967399597168"} \item{stop_lon}{The GTFS-compatible longitude of
the station. Example: "-71.1218185424805"} }


