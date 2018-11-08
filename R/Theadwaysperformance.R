Theadwaysperformance <- function(enter_time,exit_time=NULL,enter_route_name,exit_route_name,enter_stop_name,exit_stop_name,data,api_key){

  if(length(exit_time)<1){
    exit_time <- "exit_time_imputed"
    data$exit_time_imputed <- NULL
    data[,c(exit_time)] <- as.POSIXlt(data[,c(enter_time)]) + 1800 # add 30 minutes by default
  }
  route_table <- routes # has all routes but Green with separate letter identifiers
  data$route_id_enter <- route_table$route_id[match(x = data[,c(enter_route_name)], table = route_table$route_name)]
  data$route_id_exit <- route_table$route_id[match(x = data[,c(exit_route_name)], table = route_table$route_name)]

  data$enter_stop_id <- NA
  data$exit_stop_id <- NA
  for(i in 1:nrow(data)){
    print(paste("Finding stop for row ",i," of ",nrow(data),sep=""))
    if(((data[i,c(enter_route_name)] %in% c("Red Line","Orange Line","Blue Line")) |
          (data[i,c(exit_route_name)] %in% c("Red Line","Orange Line","Blue Line"))) & # no other travel time data yet
         data[i,c(enter_stop_name)] != "" & data[i,c(exit_stop_name)] != "" & # non-missing
         !is.na(data[i,c(enter_stop_name)]) & !is.na(data[i,c(exit_stop_name)]) & # non-missing
         data[i,c(enter_stop_name)] != "Mattapan Trolley" & data[i,c(exit_stop_name)] != "Mattapan Trolley"){ # error-prone

      stop_table <- NULL
      enter_stop_table <- NULL
      exit_stop_table <- NULL
      if(!is.na(data$route_id_enter[i])){
        stop_table <- Tstopsbyroute(route_id = data$route_id_enter[i])
        enter_stop_table <- stop_table[which(gsub("(\\w+)\\W+.*","\\1",x=stop_table$parent_station_name) == gsub("(\\w+)\\W+.*","\\1",x=data[i,c(enter_stop_name)])),]
        exit_stop_table <- stop_table[which(gsub("(\\w+)\\W+.*","\\1",x=stop_table$parent_station_name) == gsub("(\\w+)\\W+.*","\\1",x=data[i,c(exit_stop_name)])),]

        if(nrow(enter_stop_table)>2){ # for Quincy Center/Quincy Adams duplicate of first word
          enter_stop_table <- enter_stop_table[which(enter_stop_table$parent_station_name == data[i,c(enter_stop_name)]),]
        }
        if(nrow(exit_stop_table)>2){
          exit_stop_table <- exit_stop_table[which(exit_stop_table$parent_station_name == data[i,c(exit_stop_name)]),]
        }
        if((nrow(enter_stop_table) < 1 | nrow(exit_stop_table) < 1 ) & !is.na(data$route_id_exit[i])){
          stop_table <- Tstopsbyroute(route_id = data$route_id_exit[i])

          enter_stop_table <- stop_table[which(gsub("(\\w+)\\W+.*","\\1",x=stop_table$parent_station_name) == gsub("(\\w+)\\W+.*","\\1",x=data[i,c(enter_stop_name)])),]
          exit_stop_table <- stop_table[which(gsub("(\\w+)\\W+.*","\\1",x=stop_table$parent_station_name) == gsub("(\\w+)\\W+.*","\\1",x=data[i,c(exit_stop_name)])),]
        }
      }
      # check other route stops in case user inputted wrong line but correct stop:
      if(is.na(data$route_id_enter[i]) & !is.na(data$route_id_exit[i])){
        stop_table <- Tstopsbyroute(route_id = data$route_id_exit[i])
        enter_stop_table <- stop_table[which(gsub("(\\w+)\\W+.*","\\1",x=stop_table$parent_station_name) == gsub("(\\w+)\\W+.*","\\1",x=data[i,c(enter_stop_name)])),]
        exit_stop_table <- stop_table[which(gsub("(\\w+)\\W+.*","\\1",x=stop_table$parent_station_name) == gsub("(\\w+)\\W+.*","\\1",x=data[i,c(exit_stop_name)])),]
      }

      if(nrow(enter_stop_table) > 0 & nrow(exit_stop_table) > 0){
        # pick entrance stop id that comes before stop id in same direction
        if(as.numeric(enter_stop_table$stop_order[enter_stop_table$direction_id==1]) < as.numeric(exit_stop_table$stop_order[exit_stop_table$direction_id==1])){
          data$enter_stop_id[i] <- enter_stop_table$stop_id[enter_stop_table$direction_id==1]
        }
        if(as.numeric(enter_stop_table$stop_order[enter_stop_table$direction_id==1]) > as.numeric(exit_stop_table$stop_order[exit_stop_table$direction_id==1])){
          data$enter_stop_id[i] <- enter_stop_table$stop_id[enter_stop_table$direction_id==0]
        }
        # pick exit stop id that comes after stop id in same direction
        if(as.numeric(exit_stop_table$stop_order[exit_stop_table$direction_id==1]) > as.numeric(enter_stop_table$stop_order[enter_stop_table$direction_id==1])){
          data$exit_stop_id[i] <- exit_stop_table$stop_id[exit_stop_table$direction_id==1]
        }
        if(as.numeric(exit_stop_table$stop_order[exit_stop_table$direction_id==1]) < as.numeric(enter_stop_table$stop_order[enter_stop_table$direction_id==1])){
          data$exit_stop_id[i] <- exit_stop_table$stop_id[exit_stop_table$direction_id==0]
        }
      }
      if(nrow(enter_stop_table) < 1 | nrow(exit_stop_table) < 1){
        data$enter_stop_id[i] <- NA
        data$exit_stop_id[i] <- NA
      }
    }
    if(is.na(data$route_id_enter[i]) & is.na(data$route_id_exit[i])){
      data$enter_stop_id[i] <- NA
      data$exit_stop_id[i] <- NA
    }

    if(!((data[i,c(enter_route_name)] %in% c("Red Line","Orange Line","Blue Line")) |
           (data[i,c(exit_route_name)] %in% c("Red Line","Orange Line","Blue Line"))) |
         data[i,c(enter_stop_name)] == "" | data[i,c(exit_stop_name)] == "" |
         is.na(data[i,c(enter_stop_name)]) | is.na(data[i,c(exit_stop_name)]) | # non-missing
         data[i,c(enter_stop_name)] == "Mattapan Trolley" | data[i,c(exit_stop_name)] == "Mattapan Trolley"){
      data$enter_stop_id[i] <- NA
      data$exit_stop_id[i] <- NA
    }
  }

  # convert enter and exit times to character so that they can later be sent to POSIXct
  data[,c(enter_time)] <- as.character(data[,c(enter_time)])
  data[,c(exit_time)] <- as.character(data[,c(exit_time)])

  headway_mean <- NULL
  headway_bench <- NULL
  headway_perf <- NULL
  next_train <- NULL
  prev_train <- NULL
  headway_guess <- NULL
  headway_bench_guess <- NULL

  for(k in 1:nrow(data)){
    print(paste("Finding headways for row ",k," of ",nrow(data),sep=""))

    hw <- NULL
    if(!is.na(data$enter_stop_id[k]) & !is.na(data$exit_stop_id[k]) &
         ((data[k,c(enter_route_name)] %in% c("Red Line","Orange Line","Blue Line")) |
            (data[k,c(exit_route_name)] %in% c("Red Line","Orange Line","Blue Line"))) &
       !is.na(data[k,c(enter_time)]) & !is.na(data[k,c(exit_time)])){
      hw <- Theadways(from_stop_id = data$enter_stop_id[k], to_stop_id = data$exit_stop_id[k],
                              route_id = NULL, direction_id = NULL,
                              from_datetime = as.POSIXct(data[k,c(enter_time)]),
                              to_datetime = as.POSIXct(data[k,c(exit_time)]),
                              api_key = api_key)

      if(is.data.frame(hw)){ # if returned results:
        headway_mean[k] <- ifelse(sum(is.na(as.numeric(as.character(hw$headway_time_sec)))) !=
                                    length(as.numeric(as.character(hw$headway_time_sec))),
          mean(as.numeric(as.character(hw$headway_time_sec)),na.rm=T),
          NA)
        headway_bench[k] <- ifelse(sum(is.na(as.numeric(hw$benchmark_headway_time_sec))) !=
                                        length(hw$benchmark_headway_time_sec),
                                      mean(as.numeric(as.character(hw$benchmark_headway_time_sec)),na.rm=T),
                                      NA)
        headway_perf[k] <- ifelse(sum(is.na(as.numeric(hw$benchmark_headway_time_sec))) !=
                                       length(hw$benchmark_headway_time_sec),
                                     mean((as.numeric(as.character(hw$headway_time_sec))-as.numeric(as.character(hw$benchmark_headway_time_sec)))>60,na.rm=T),
                                     NA)
        thismatch <- hw$current_dep_dt[order(abs(as.numeric(difftime(hw$current_dep_dt,as.POSIXct(data[k,c(enter_time)]), units="secs"))),decreasing = F)][1]
        next_train[k] <- as.character(thismatch)
        prev_train[k] <- as.character(hw$previous_dep_dt[order(abs(as.numeric(difftime(hw$current_dep_dt,as.POSIXct(data[k,c(enter_time)]), units="secs"))),decreasing = F)][1])
        headway_guess[k] <- as.numeric(difftime(thismatch,as.POSIXct(data[k,c(enter_time)]), units="secs"))
        headway_bench_guess[k] <- hw$benchmark_headway_time_sec[order(abs(as.numeric(difftime(hw$current_dep_dt,as.POSIXct(data[k,c(enter_time)]), units="secs"))),decreasing = F)][1]
      }

      if(!is.data.frame(hw)){
        headway_mean[k] <- NA
        headway_bench[k] <- NA
        headway_perf[k] <- NA
        next_train[k] <- NA
        prev_train[k] <- NA
        headway_guess[k] <- NA
        headway_bench_guess[k] <- NA
      }

    }

    if(is.na(data$enter_stop_id[k]) |
         is.na(data$exit_stop_id[k]) |
         !((data[k,c(enter_route_name)] %in% c("Red Line","Orange Line","Blue Line")) |
             (data[k,c(exit_route_name)] %in% c("Red Line","Orange Line","Blue Line"))) |
         is.na(data[k,c(enter_time)]) | is.na(data[k,c(exit_time)])){
      headway_mean[k] <- NA
      headway_bench[k] <- NA
      headway_perf[k] <- NA
      next_train[k] <- NA
      prev_train[k] <- NA
      headway_guess[k] <- NA
      headway_bench_guess[k] <- NA
    }

  }

  return(data.frame(headway_mean=headway_mean,headway_bench=headway_bench,headway_perf=headway_perf,next_train=next_train,prev_train=prev_train,headway_guess=headway_guess,headway_bench_guess=headway_bench_guess))

}
