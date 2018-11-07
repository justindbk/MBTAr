\name{Tstopsbylocation} \alias{Tstopsbylocation}
\title{ Query stops by goegraphic location }
\description{ Returns information about the nearest stops to a particular
location. Up to 15 are returned, within a 1-mile radius. } \usage{
Tstopsbylocation(lat, lon, api_key) }
\arguments{ \item{lat}{ The latitude for location near which
stops should be returned. Example: "42.352913" } \item{lon}{ The longitude for
location near which stops should be returned. Example: "-71.064648" }
\item{api_key}{ API key for MBTA API. To obtain one, visit the MBTA Developer
Portal (\url{http://realtime.mbta.com/Portal/}) } }

\value{ \item{stop_id}{The GTFS-compatible unique identifier for the stop.
Example: "70063"} \item{stop_name}{The GTFS-compatible name for the stop (not
unique). Example: "Davis Sq - Inbound"} \item{parent_station}{The
GTFS-compatible unique identifier for the station associated with the stop.
(note: can be empty if stop does not have an associated station). Example:
"place-davis"} \item{parent_station_name}{The human-readable name for the
larger station associated with the stop. (note: can be empty if stop does not
have an associated station). Example: "Davis Station"} \item{stop_lat}{The
GTFS-compatible latitude of the station. Example: "42.3967399597168"}
\item{stop_lon}{The GTFS-compatible longitude of the station. Example:
"-71.1218185424805"} \item{distance}{The distance of the stop from the
requested location in miles. Example: "0.00800655130296946"} }

