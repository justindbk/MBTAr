\name{Tservertime} \alias{Tservertime}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Queries the current MBTA API server time. }
\description{ Returns the server time. Useful if needing to sync user computer
time with the epoch time of MBTA events. } \usage{ Tservertime(api_key) }
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{api_key}{ API key for MBTA API. To obtain one, visit the MBTA Developer
Portal (\url{http://realtime.mbta.com/Portal/}) } }

\value{ \item{server_dt}{Server time, in epoch time. Example: "1361996667"} }


