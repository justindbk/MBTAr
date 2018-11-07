\name{Troutes} \alias{Troutes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Query all T routes } \description{ Returns
information about all routes for which information can be requested. }
\usage{
Troutes(api_key) }
%- maybe also 'usage' for other objects documented here.
\arguments{ \item{api_key}{ API key for MBTA API. To obtain one, visit the
MBTA Developer Portal (\url{http://realtime.mbta.com/Portal/}) } }

\value{ \item{route_type}{The GTFS-compatible identifier for the type of
service (mode). Example: "2"} \item{mode_name}{The human-readable name for the
type of service (mode). Example: "Commuter Rail"} \item{route_id}{The unique
GTFS-compatible identifier for the route. Example: "CR-Providence"}
\item{route_name}{The human-readable name for the route. Example:
"Providence/Stoughton Line"} }


\examples{
  ## Authenticate:
  \dontshow{
  test_key <- "hUOvci6zZkihtonl1_yDkg"
  }
  mykey <- NULL
  mykey <- assign("mykey", value = test_key, envir = .GlobalEnv)
  ## use your own key from http://realtime.mbta.com/Portal/ instead of the test key

	Troutes(api_key = mykey)

	## returns:
	    # route_type     mode_name         route_id                   route_name
# 1            0        Subway          Green-B                 Green Line B
# 2            0        Subway          Green-C                 Green Line C
# 3            0        Subway          Green-D                 Green Line D
# 4            0        Subway          Green-E                 Green Line E
# 5            0        Subway         Mattapan             Mattapan Trolley
# 6            1        Subway             Blue                    Blue Line
# 7            1        Subway           Orange                  Orange Line
# 8            1        Subway              Red                     Red Line
# 9            2 Commuter Rail     CR-Fairmount               Fairmount Line
# 10           2 Commuter Rail     CR-Fitchburg               Fitchburg Line
# ...
# 207          3           Bus             9702                         9702
# 208          3           Bus             9703                         9703
# 209          4          Boat          Boat-F4            Charlestown Ferry
# 210          4          Boat          Boat-F1                Hingham Ferry
# 211          4          Boat          Boat-F3                   Hull Ferry
}

\seealso{ %% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{Troutesbystop}} }

