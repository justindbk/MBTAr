\name{Talertsbystop} \alias{Talertsbystop}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Queries all active and upcoming alerts
for a particular stop. } \description{ Returns all information on alerts
affecting a given stop. May include some empty fields.} \usage{
Talertsbystop(stop_id, include_access_alerts = FALSE, include_service_alerts =
TRUE, api_key) } %- maybe also 'usage' for other objects documented here.
\arguments{ \item{stop_id}{GTFS-compatible stop_id value for which alert
information should be returned. Data type: String. Example: "place-portr"}
\item{include_access_alerts}{ Whether or not alerts pertaining to
accessibility (elevators, escalators) should be returned. Data type: logical.
Defaults to FALSE. } \item{include_service_alerts}{ Whether or not service
alerts should be returned. Data type: logical. Defaults to TRUE. }
\item{api_key}{ API key for MBTA API. To obtain one, visit the MBTA Developer
Portal (\url{http://realtime.mbta.com/Portal/}) } }

\value{ \item{stop_id}{The GTFS-compatible unique identifier for the stop for
which alerts are returned. Data type: String. Example: "place-portr"}
\item{stop_name}{The GTFS-compatible name for the stop for which alerts are
returned. Data type: String. Example: "Porter Square Station"}
\item{alert_id}{The unique identifier for the alert} \item{effect_name}{The
human-readable name for the effect. Example: "Shuttle bus"} \item{effect}{The
GTFS-realtime-compatible code for the effect. Example: "DETOUR"}
\item{cause}{The human-readable name for the cause. Seomtimes empty. Example:
"maintenance"} \item{header_text}{A brief summary of the situation
(GTFS-realtime-compatible). Example: "Shuttle buses replacing Red Line service
from Sat Jun 01, 2013 to Sun Jun 30, 2013 every Saturday and Sunday from 09:00
PM to end of service due to tie replacement"} \item{short_header_text}{A
shortened version of header_text. Example: "Shuttle buses replacing Red Line
service from Sat Apr 27 to Sun May 26 every Saturday and Sunday due to
maintenance"} \item{description_text}{Additional details
(GTFS-realtime-compatible). Example: "Affected stops: Alewife Station Davis
Station Porter Square Station Harvard Square Station"}
\item{severity}{Possible values: "Severe", "Moderate", "Minor"}
\item{created_dt}{Date and time the alert was created, in epoch time. Example:
"1361395938"} \item{last_modified_dt}{Date and time the alert was last
modified, in epoch time. Example: "1361395938"}
\item{service_effect_text}{Summarizes the service and the impact to that
service. Example: "Minor Route 1 delay"} \item{timeframe_text}{Summarizes when
an alert is in effect. Example: "starting Saturday"}
\item{alert_lifecycle}{Identifies whether alert is a new or old, in effect or
upcoming. Not intended to be human-readable. Possible values: "Upcoming",
"New", "Ongoing", "Ongoing-Upcoming." See notes.} \item{effect_start}{Date and
time of the start of the effect period, in epoch time. Example: "1367110800"}
\item{effect_end}{Date and time of the end of the effect period, in epoch
time. Can be empty if effect end is not known. Example: "1367130600"}
\item{affected_route_type}{GTFS-compatible code for route type (i.e. mode).
Appears for service alerts only (not access alerts). Example: "1"}
\item{affected_mode_name}{Human-readable name for the mode. Example: "Subway"}
\item{affected_route_id}{The unique GTFS-compatible identifier for the route.
Example: "Red"} \item{affected_route_name}{The human-readable name for the
route. Example: "Red Line"} \item{affected_direction_id}{The GTFS-compatible
identifier for the direction. Example: "0"}
\item{affected_direction_name}{Human-readable direction name. Example:
"Westbound"} \item{affected_trip_id}{The GTFS-compatible unique identifier for
the trip. Example: "CR-Newburyport-CR-Weekday-129"}
\item{affected_trip_name}{Human-readable trip name. Example: "129 (5:00 pm
from North Station)"} \item{affected_route_hide}{Whether this route should be
hidden from users. See notes. Possible values: "true". Only included if
"true."} \item{affected_elev_id}{Unique identifier for the elevator/escalator.
Example: "926"} \item{affected_elev_name}{Human-readable name for the
elevator/escalator. Example: "SOUTH STATION - Lobby to Street"}
\item{affected_elev_type}{Type of the elevator/escalator. Possible values:
"Elevator", "Escalator", "Lift"} \item{affected_elev_stop_id}{The
GTFS-compatible unique identifier for the stop. Example: "70080"}
\item{affected_elev_stop_name}{The GTFS-compatible name for the stop (not
unique). Example: "South Station - Inbound"}
\item{affected_elev_stop_parent_id}{The GTFS-compatible unique identifier for
the larger station associated with the stop, if one exists. Can be empty if
parent station does not exist. Example: "place_sstat"} }


%% ~Make other sections like Warning with \section{Warning }{....} ~

