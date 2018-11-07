## Check individual transit records:

Ttravelperformance = function(enter_time,exit_time=NULL,enter_route_name,exit_route_name,enter_stop_name,exit_stop_name,data,api_key){
  if(length(exit_time)<1){
    exit_time <- "exit_time_imputed"
    data$exit_time_imputed <- NULL
    data[,c(exit_time)] <- as.POSIXct(data[,c(enter_time)]) + 1800 # add 30 minutes by default
  }
#   if(enter_route_name %in% names(data)){ # if "enter_route_name" specifies a column in dataset...
    route_table <- Troutes(api_key)
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
          stop_table <- Tstopsbyroute(route_id = data$route_id_enter[i], api_key = api_key)
          enter_stop_table <- stop_table[which(gsub("(\\w+)\\W+.*","\\1",x=stop_table$parent_station_name) == gsub("(\\w+)\\W+.*","\\1",x=data[i,c(enter_stop_name)])),]
          exit_stop_table <- stop_table[which(gsub("(\\w+)\\W+.*","\\1",x=stop_table$parent_station_name) == gsub("(\\w+)\\W+.*","\\1",x=data[i,c(exit_stop_name)])),]
          
          if(nrow(enter_stop_table)>2){ # for Quincy Center/Quincy Adams duplicate of first word
            enter_stop_table <- enter_stop_table[which(enter_stop_table$parent_station_name == data[i,c(enter_stop_name)]),]
          }
          if(nrow(exit_stop_table)>2){
            exit_stop_table <- exit_stop_table[which(exit_stop_table$parent_station_name == data[i,c(exit_stop_name)]),]
          }
          if((nrow(enter_stop_table) < 1 | nrow(exit_stop_table) < 1 ) & !is.na(data$route_id_exit[i])){
            stop_table <- Tstopsbyroute(route_id = data$route_id_exit[i], api_key = api_key)
            
            enter_stop_table <- stop_table[which(gsub("(\\w+)\\W+.*","\\1",x=stop_table$parent_station_name) == gsub("(\\w+)\\W+.*","\\1",x=data[i,c(enter_stop_name)])),]
            exit_stop_table <- stop_table[which(gsub("(\\w+)\\W+.*","\\1",x=stop_table$parent_station_name) == gsub("(\\w+)\\W+.*","\\1",x=data[i,c(exit_stop_name)])),]
          }
        }
          # check other route stops in case user inputted wrong line but correct stop:
          if(is.na(data$route_id_enter[i]) & !is.na(data$route_id_exit[i])){
            stop_table <- Tstopsbyroute(route_id = data$route_id_exit[i], api_key = api_key)
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
#   }
  
  # convert enter and exit times to character so that they can later be sent to POSIXct
  data[,c(enter_time)] <- as.character(data[,c(enter_time)])
  data[,c(exit_time)] <- as.character(data[,c(exit_time)])
  
  traveltime_mean <- NULL
  traveltime_bench_mean <- NULL
  traveltime_perf <- NULL
  traveltime_guess <- NULL
  traveltime_bench_guess <- NULL
  for(k in 1:nrow(data)){
    print(paste("Finding travel time for row ",k," of ",nrow(data),sep=""))
    
    travelt <- NULL
    if(!is.na(data$enter_stop_id[k]) & !is.na(data$exit_stop_id[k]) &
         ((data[k,c(enter_route_name)] %in% c("Red Line","Orange Line","Blue Line")) |
             (data[k,c(exit_route_name)] %in% c("Red Line","Orange Line","Blue Line"))) &
         !is.na(data[k,c(enter_time)]) & !is.na(data[k,c(exit_time)])){
      travelt <- Ttraveltimes(from_stop_id = data$enter_stop_id[k], to_stop_id = data$exit_stop_id[k],
                              route_id = NULL,
                              from_datetime = as.POSIXct(data[k,c(enter_time)]),
                              to_datetime = as.POSIXct(data[k,c(exit_time)]),
                              api_key = api_key)
      
      if(is.data.frame(travelt)){
        traveltime_mean[k] <- mean(as.numeric(as.character(travelt$travel_time_sec)),na.rm=T)
        traveltime_bench_mean[k] <- ifelse(sum(travelt$benchmark_travel_time_sec != "") ==
                                             length(travelt$benchmark_travel_time_sec),
                                           mean(as.numeric(as.character(travelt$benchmark_travel_time_sec)),na.rm=T),
                                           NA)
        traveltime_perf[k] <- ifelse(sum(travelt$benchmark_travel_time_sec != "") ==
                                       length(travelt$benchmark_travel_time_sec),
                                     mean(as.numeric(as.character(travelt$travel_time_sec)),na.rm=T)/
                                       mean(as.numeric(as.character(travelt$benchmark_travel_time_sec)),na.rm=T),
                                     NA)
        
        travelt <- travelt[which(travelt$dep_dt>as.POSIXct(data[k,c(enter_time)])),] # subset to after reported depart time
        
        if(nrow(travelt)>0){
          traveltime_guess[k] <- as.numeric(as.character(travelt$travel_time_sec[order(abs(difftime(as.POSIXct(travelt$dep_dt), as.POSIXct(data[k,c(enter_time)]),units="secs")) )][1]))
          traveltime_bench_guess[k] <- as.numeric(as.character(travelt$benchmark_travel_time_sec[order(abs(difftime(as.POSIXct(travelt$dep_dt), as.POSIXct(data[k,c(enter_time)]),units="secs")) )][1]))
        }
        if(nrow(travelt)<1){
          traveltime_guess[k] <- NA
          traveltime_bench_guess[k] <- NA
        }
      }
      
      if(!is.data.frame(travelt)){
        traveltime_mean[k] <- NA
        traveltime_bench_mean[k] <- NA
        traveltime_perf[k] <- NA
        traveltime_guess[k] <- NA
        traveltime_bench_guess[k] <- NA
      }
      
    }
    
    if(is.na(data$enter_stop_id[k]) |
         is.na(data$exit_stop_id[k]) |
         !((data[k,c(enter_route_name)] %in% c("Red Line","Orange Line","Blue Line")) |
             (data[k,c(exit_route_name)] %in% c("Red Line","Orange Line","Blue Line"))) |
         is.na(data[k,c(enter_time)]) | is.na(data[k,c(exit_time)])){
      traveltime_mean[k] <- NA
      traveltime_bench_mean[k] <- NA
      traveltime_perf[k] <- NA
      traveltime_guess[k] <- NA
      traveltime_bench_guess[k] <- NA
    }
    
  }
  
  return(data.frame(traveltime_mean=traveltime_mean,traveltime_bench_mean=traveltime_bench_mean,traveltime_perf=traveltime_perf,traveltime_guess=traveltime_guess,traveltime_bench_guess=traveltime_bench_guess))
  
}


