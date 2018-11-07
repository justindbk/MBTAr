\name{Troutesbystop} \alias{Troutesbystop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Query all routes at a stop }
\description{ Returns routes that serve a particular stop. } \usage{
Troutesbystop(stop_id = NULL, api_key) }
%- maybe also 'usage' for other objects documented here.
\arguments{ \item{stop_id}{ GTFS-compatible stop_id
value for which routes should be returned. Example: "70065" } \item{api_key}{
API key for MBTA API. To obtain one, visit the MBTA Developer Portal
(\url{http://realtime.mbta.com/Portal/}) } }

\value{ \item{stop_id}{The GTFS-compatible unique identifier for the stop for
which routes are returned. Example: "70065"} \item{stop_name}{The
GTFS-compatible name for the stop for which routes are returned. Example:
"Porter Sq - Inbound"} \item{route_type}{The GTFS-compatible identifier for
the type of service (mode). Example: "2"} \item{mode_name}{The human-readable
name for the type of service (mode). Example: "Commuter Rail"}
\item{route_id}{The unique GTFS-compatible identifier for the route. Example:
"CR-Providence"} \item{route_name}{The human-readable name for the route.
Example: "Providence/Stoughton Line"} }



\seealso{ %% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{Troutes}} }
