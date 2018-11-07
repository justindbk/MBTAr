\name{Talertheaders} \alias{Talertheaders}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Queries the header text for all active
and upcoming alerts. } \description{ Returns alert id numbers and text
descriptions of alerts. } \usage{ Talertheaders(include_access_alerts = FALSE,
include_service_alerts = TRUE, api_key) }
%- maybe also 'usage' for other objects documented here.
\arguments{ \item{include_access_alerts}{ Whether or
not alerts pertaining to accessibility (elevators, escalators) should be
returned. Data type: logical. Defaults to FALSE. }
\item{include_service_alerts}{ Whether or not service alerts should be
returned. Data type: logical. Defaults to TRUE. } \item{api_key}{ API key for
MBTA API. To obtain one, visit the MBTA Developer Portal
(\url{http://realtime.mbta.com/Portal/}) } }

\value{ \item{alert_id}{The unique identifier for the alert}
\item{header_text}{A brief summary of the situation (GTFS-realtime-compatible)
Example: "Shuttle buses replacing Red Line service from Sat Jun 01, 2013 to
Sun Jun 30, 2013 every Saturday and Sunday from 09:00 PM to end of service due
to tie replacement"} }


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ %% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{Talerts}} \code{\link{Talertheadersbyroute}}
\code{\link{Talertheadersbystop}} }
