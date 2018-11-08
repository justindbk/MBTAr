\name{Talerts} \alias{Talerts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Query all active and upcoming alerts. } \description{
Find current list of all MBTA alerts, including all routes and stops they
affect, along with timeframes that the alert is in effect. Returns all
possible information for a given alert, often with empty fields.} \usage{
Talerts()
} %- maybe also 'usage' for other objects documented here.
\value{ \item{alert_id}{The unique identifier for the alert}
\item{effect_name}{The human-readable name for the effect. Example: "Shuttle
bus"} \item{effect}{The GTFS-realtime-compatible code for the effect. Example:
"DETOUR"} \item{cause}{The human-readable name for the cause. Seomtimes empty.
Example: "maintenance"} \item{header_text}{A brief summary of the situation
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
from North Station)"} \item{affected_stop_id}{The GTFS-compatible unique
identifier for the stop. Example: "70061"} \item{affected_stop_name}{The
GTFS-compatible name for the stop (not unique). Example: "Alewife Station Red
Line"} \item{affected_route_hide}{Whether this route should be hidden from
users. See notes. Possible values: "true". Only included if "true."}
\item{affected_elev_id}{Unique identifier for the elevator/escalator. Example:
"926"} \item{affected_elev_name}{Human-readable name for the
elevator/escalator. Example: "SOUTH STATION - Lobby to Street"}
\item{affected_elev_type}{Type of the elevator/escalator. Possible values:
"Elevator", "Escalator", "Lift"} \item{affected_elev_stop_id}{The
GTFS-compatible unique identifier for the stop. Example: "70080"}
\item{affected_elev_stop_name}{The GTFS-compatible name for the stop (not
unique). Example: "South Station - Inbound"}
\item{affected_elev_stop_parent_id}{The GTFS-compatible unique identifier for
the larger station associated with the stop, if one exists. Can be empty if
parent station does not exist. Example: "place_sstat"} } \note{ Severity:
\itemize{ \item{"Severity" was created with the intent that it could drive
presentation of alerts in a variety of ways - ordering, coloring, icons - and
not with the intent that the words "severity," "mild," "moderate," or "severe"
would necessarily be shown directly to customers.} } Effect Periods: \itemize{
\item{More than one 'effect_period' object can be present.} \item{'effect_end'
can be empty if the end time for an alert is not known.} } Alert Lifecycle:
\itemize{ \item{"New" and "Ongoing" refer to alerts that are in effect now.}
\item{"Upcoming" and "Ongoing-Upcoming" refer to alerts that will be in effect
in the future.} \item{"Ongoing" and "Ongoing-Upcoming" refer to alerts that
are "old news," like a station that is closed and has been for weeks.}
\item{An example of an "Ongoing-Upcoming" alert would be a shuttle that has
been happening every weekend for a month so far (if you retrieve the data on a
weekday.)} } Affected Services: \itemize{ \item{The affected services for an
alert can include either services or elevators/escalators but NOT both. If the
'services' object is empty (i.e. 'service' objects are not present) then the
'elevators' object will not be empty (i.e. 'elevator' objects will be present)
and vice versa.} \item{More than one 'service' object can be present.}
\item{Different service objects can have different combinations of attributes.
They may have just a mode and route (affects an entire route), or mode and
stop (affects all service at the stop.) Or be much more specific - including
mode, route, direction, trip, and stop, indicating that it applies to one
scheduled stop on one trip.} \item{Currently, the system does not allow
creation of an alert that applies to multiple elevators/escalators. Therefore,
only one 'elevator' object can be present. This may change in the future.}
\item{For alerts that apply to elevators/escalators, 'parent_station' and
'parent_station_name' properties on the 'stop' object can be empty if parent
station does not exist.} } }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{Talertbyid}} }
