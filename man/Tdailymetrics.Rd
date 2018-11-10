\name{Tdailymetrics}
\alias{Tdailymetrics}
\title{
Query daily performance statistics.}
\description{
Returns day-level summary performance statistics for a specified date or range of dates.
}
\usage{
Tdailymetrics(route_id=NULL,
from_date=(Sys.Date()-8),
to_date=Sys.Date()-1,
api_key)
}
\arguments{
 \item{route_id}{
GTFS-compatible route_id value for which metrics should be returned. If not specified, will return metrics for all routes.
}
\item{from_date}{
The start of the time period that the metrics should be returned for. Must be in YYYY-MM-DD format. Defaults to 8 days ago.
}
  \item{to_date}{
The end of the time period that the metrics should be returned for. Must be in YYYY-MM-DD format. Defaults to yesterday.
}
\item{api_key}{API key for MBTA API. To obtain one, visit the MBTA Developer
Portal (\url{https://mbta.com/developers/mbta-performance/})}
}

\value{
\item{service_date}{Date of data for this performance metric}
\item{route_id}{Route of data for this performance metric}
\item{threshold_id}{Reference number for this type of performance metric}
\item{threshold_type}{Type of performance metric (wait time-based or travel time-based)}
\item{threshold_name}{Text name for this type of performance metric}
\item{metric_result}{Corresponding result for this performance metric (percentage of trips that met or performed better than this threshold)}
}

\seealso{
\code{\link{Tmetrics}}
}
\examples{
  ## Authenticate:
  \dontshow{
  test_key <- "Zt0g4Tkp2kaLNxoKFXI_Kg"
  }
  mykey <- NULL
  mykey <- assign("mykey", value = test_key, envir = .GlobalEnv)
  ## use your own key from http://realtime.mbta.com/Portal/ instead of the test key

  Tdailymetrics(route_id="Red", # metrics for the Red Line
from_date=(Sys.Date()-8), # 8 days ago
to_date=Sys.Date()-1, # to yesterday (a full week of performance)
api_key = mykey) # user-defined API key

## Returns:
#
# service_date route_id    threshold_id threshold_type   threshold_name metric_result
# 1    2015-11-05      Red threshold_id_01      wait_time          Headway        0.8623
# 2    2015-11-05      Red threshold_id_02      wait_time          Big Gap        0.9398
# 3    2015-11-05      Red threshold_id_03      wait_time       2X Headway        0.9769
# 4    2015-11-05      Red threshold_id_04    travel_time delayed < 3 min.        0.9653
# 5    2015-11-05      Red threshold_id_05    travel_time delayed < 6 min.         0.996
# 6    2015-11-06      Red threshold_id_01      wait_time          Headway         0.864
# 7    2015-11-06      Red threshold_id_02      wait_time          Big Gap        0.9443
# 8    2015-11-06      Red threshold_id_03      wait_time       2X Headway        0.9814
# 9    2015-11-06      Red threshold_id_04    travel_time delayed < 3 min.        0.9549
# 10   2015-11-06      Red threshold_id_05    travel_time delayed < 6 min.        0.9928
# 11   2015-11-07      Red threshold_id_01      wait_time          Headway        0.7365
# 12   2015-11-07      Red threshold_id_02      wait_time          Big Gap          0.89
# 13   2015-11-07      Red threshold_id_03      wait_time       2X Headway         0.952
# 14   2015-11-07      Red threshold_id_04    travel_time delayed < 3 min.        0.9853
# 15   2015-11-07      Red threshold_id_05    travel_time delayed < 6 min.        0.9979
# 16   2015-11-08      Red threshold_id_01      wait_time          Headway        0.7808
# 17   2015-11-08      Red threshold_id_02      wait_time          Big Gap        0.8972
# 18   2015-11-08      Red threshold_id_03      wait_time       2X Headway        0.9582
# 19   2015-11-08      Red threshold_id_04    travel_time delayed < 3 min.        0.9997
# 20   2015-11-08      Red threshold_id_05    travel_time delayed < 6 min.        0.9999
# 21   2015-11-09      Red threshold_id_01      wait_time          Headway        0.8515
# 22   2015-11-09      Red threshold_id_02      wait_time          Big Gap        0.9344
# 23   2015-11-09      Red threshold_id_03      wait_time       2X Headway        0.9768
# 24   2015-11-09      Red threshold_id_04    travel_time delayed < 3 min.        0.9403
# 25   2015-11-09      Red threshold_id_05    travel_time delayed < 6 min.         0.977
# 26   2015-11-10      Red threshold_id_01      wait_time          Headway        0.8698
# 27   2015-11-10      Red threshold_id_02      wait_time          Big Gap        0.9479
# 28   2015-11-10      Red threshold_id_03      wait_time       2X Headway        0.9805
# 29   2015-11-10      Red threshold_id_04    travel_time delayed < 3 min.        0.9725
# 30   2015-11-10      Red threshold_id_05    travel_time delayed < 6 min.        0.9976
# 31   2015-11-11      Red threshold_id_01      wait_time          Headway        0.8854
# 32   2015-11-11      Red threshold_id_02      wait_time          Big Gap        0.9605
# 33   2015-11-11      Red threshold_id_03      wait_time       2X Headway        0.9874
# 34   2015-11-11      Red threshold_id_04    travel_time delayed < 3 min.        0.9856
# 35   2015-11-11      Red threshold_id_05    travel_time delayed < 6 min.        0.9995
# 36   2015-11-12      Red threshold_id_01      wait_time          Headway        0.8573
# 37   2015-11-12      Red threshold_id_02      wait_time          Big Gap        0.9399
# 38   2015-11-12      Red threshold_id_03      wait_time       2X Headway        0.9772
# 39   2015-11-12      Red threshold_id_04    travel_time delayed < 3 min.        0.9315
# 40   2015-11-12      Red threshold_id_05    travel_time delayed < 6 min.        0.9806

}
