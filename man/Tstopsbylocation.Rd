\name{Tstopsbylocation} \alias{Tstopsbylocation}
\title{ Query stops by goegraphic location }
\description{ Returns information about the nearest stops to a particular
location.} \usage{
Tstopsbylocation(lat, lon, radius = 0.01) }
\arguments{ \item{lat}{ The latitude for location near which
stops should be returned. Example: "42.352913" } \item{lon}{ The longitude for
location near which stops should be returned. Example: "-71.064648" }
\item{radius}{ Radius in degrees around the coordinates in which to search.
Defaults to 0.01 degrees, or approximately half a mile.} }

\value{ \item{stop_id}{The GTFS-compatible unique identifier for the stop.
Example: "70063"} \item{address}{Street address or intersection at which
stop is located. (note: can be empty for non-parent stations).}
\item{description}{Full description of stop or entrance/exit.} \item{latitude}
{The GTFS-compatible latitude of the station. Example: "42.3967399597168"}
\item{location_type}{Takes value of 0, 1, or 2, corresponding to stops, parent
stations, and entrances/exits, respectively.} \item{longitude}{The GTFS-
compatible longitude of the station. Example: "-71.1218185424805"}
\item{name}{The GTFS-compatible name for the stop (not unique). Example:
"State Street"} \item{platform_code}{Number, 1 through 13, indicating platform
 within station serving commuter rail.} \item{platform_name}{Direction towards
 which vehicles from that platform go.} \item{wheelchair_boarding}{Code for
 whether or not platform allows for wheelchair access. tkaes values of 0, 1,
 or 2, corresponding to no information, accessible, or inaccessible,
 respectively.} \item{parent_station}{The GTFS-compatible unique
identifier for the station associated with the stop. (note: can be empty
if stop does not have an associated station). Example: "place-davis"}
}

