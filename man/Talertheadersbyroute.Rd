\name{Talertheadersbyroute}
\alias{Talertheadersbyroute}
%- Also NEED an'\alias' for EACH other topic documented here.
\title{ Queries all active and upcoming alerts for a particular route. }
\description{ Returns the alert id
numbers and text descriptions of alerts. } \usage{
Talertheadersbyroute(route_id, include_access_alerts = FALSE,
include_service_alerts = TRUE, api_key) }
%- maybe also 'usage' for other objects documented here.
\arguments{ \item{route_id}{GTFS-compatible route_id
value for which alert headers should be returned. Data type: String. Example:
"Red"} \item{include_access_alerts}{ Whether or not alerts pertaining to
accessibility (elevators, escalators) should be returned. Data type: logical.
Defaults to FALSE. } \item{include_service_alerts}{ Whether or not service
alerts should be returned. Data type: logical. Defaults to TRUE. }
\item{api_key}{ API key for MBTA API. To obtain one, visit the MBTA Developer
Portal (\url{http://realtime.mbta.com/Portal/}) } }

\value{ \item{route_id}{The unique GTFS-compatible identifier for the route
for which alert headers are returned. Data type: String. Example: "Red"}
\item{route_name}{The human-readable name for the route for which alert
headers are returned. Example: "Red Line"} \item{alert_id}{The unique
identifier for the alert} \item{header_text}{A brief summary of the situation
(GTFS-realtime-compatible) Example: "Shuttle buses replacing Red Line service
from Sat Jun 01, 2013 to Sun Jun 30, 2013 every Saturday and Sunday from 09:00
PM to end of service due to tie replacement"} }

