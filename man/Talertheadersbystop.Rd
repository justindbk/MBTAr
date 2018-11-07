\name{Talertheadersbystop} \alias{Talertheadersbystop}
%- Also NEED an'\alias' for EACH other topic documented here.
\title{ Queries alert header
text by stop. } \description{ Returns text description headers for all active
and upcoming alerts affecting a particular stop. } \usage{
Talertheadersbystop(stop_id, include_access_alerts = FALSE,
include_service_alerts = TRUE, api_key) }
%- maybe also 'usage' for other objects documented here.
\arguments{ \item{stop_id}{GTFS-compatible stop_id
value for which alert headers should be returned. Data type: String. Example:
"place-portr"} \item{include_access_alerts}{ Whether or not alerts pertaining
to accessibility (elevators, escalators) should be returned. Data type:
logical. Defaults to FALSE. } \item{include_service_alerts}{ Whether or not
service alerts should be returned. Data type: logical. Defaults to TRUE. }
\item{api_key}{ API key for MBTA API. To obtain one, visit the MBTA Developer
Portal (\url{http://realtime.mbta.com/Portal/}) } }

\value{ \item{stop_id}{String. The GTFS-compatible unique identifier for the
stop for which alert headers are returned. Example: "place-portr"}
\item{stop_name}{The GTFS-compatible name for the stop for which alert headers
are returned. Example: "Porter Square Station"} \item{alert_id}{The unique
identifier for the alert} \item{header_text}{A brief summary of the situation
(GTFS-realtime-compatible) Example: "Shuttle buses replacing Red Line service
from Sat Jun 01, 2013 to Sun Jun 30, 2013 every Saturday and Sunday from 09:00
PM to end of service due to tie replacement"} }

