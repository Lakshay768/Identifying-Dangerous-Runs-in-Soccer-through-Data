


#### This function finds if there was any run around the times specified by the previous function. It returns the start, end times of the runs detected; the playerid of the player making that run and assigns two scores (space gained and formation disruption)


find_runs_and_players<-function(data,attack=F){
  find_eval_runs<-function(frame_number,sequence_number,attack=F){
    
    nash <- subset(tracking_data, frameIdx >= frame_number - 750 & frameIdx <= frame_number + 200)
    nash$team = as.numeric(nash$team)
    
    if(attack==T){
      if(filtered_calendar[[1,3]]== defending_teamid){
        nash <- nash %>% group_by(playerId) %>% mutate(new_speed = sgolayfilt(speed), accel = (new_speed - lag(new_speed))/0.04, new_accel = sgolayfilt(accel),
                                                       dummy = ifelse(new_speed < 3.75 | new_accel < -2.5  | live == 'False' | lastTouch != 'home' | team != 2, 1, 0),
                                                       run_start = min(which(new_speed > 4.5 & new_accel > 2.5)))
      }else{
        nash <- nash %>% group_by(playerId) %>% mutate(new_speed = sgolayfilt(speed), accel = (new_speed - lag(new_speed))/0.04, new_accel = sgolayfilt(accel),
                                                       dummy = ifelse(new_speed < 3.75 | new_accel < -2.5  | live == 'False' | lastTouch != 'away' | team != 1, 1, 0),
                                                       run_start = min(which(new_speed > 4.5 & new_accel > 2.5)))
      }  
    }else{
      if(filtered_calendar[[1,3]]== defending_teamid){
        nash <- nash %>% group_by(playerId) %>% mutate(new_speed = sgolayfilt(speed), accel = (new_speed - lag(new_speed))/0.04, new_accel = sgolayfilt(accel),
                                                       dummy = ifelse(new_speed < 3.75 | new_accel < -2.5  | live == 'False' | lastTouch != 'away' | team != 1, 1, 0),
                                                       run_start = min(which(new_speed > 4.5 & new_accel > 2.5)))
      }else{
        nash <- nash %>% group_by(playerId) %>% mutate(new_speed = sgolayfilt(speed), accel = (new_speed - lag(new_speed))/0.04, new_accel = sgolayfilt(accel),
                                                       dummy = ifelse(new_speed < 3.75 | new_accel < -2.5  | live == 'False' | lastTouch != 'home' | team != 2, 1, 0),
                                                       run_start = min(which(new_speed > 4.5 & new_accel > 2.5)))
      }
    }
    nash <- nash %>% group_by(playerId) %>% mutate(run = ifelse(dummy == 0 & row_number() > run_start, 1, 0))
    nash$start_frame_run <- nash[nash$run_start*22, 'frameIdx']
    
    
    nash2 <- center_tracking(nash)
    
    runs <- subset(nash2, run == 1)
    runs$lastframe <- lag(runs$frameIdx)
    runs$frame_diff <- runs$frameIdx - runs$lastframe
    runs$start_of_run <- ifelse(runs$frame_diff >= 10, runs$frameIdx, NA)
    runs$start_of_run<-as.integer(runs$start_of_run)
    runs[1, 'start_of_run'] <- runs[1, 'frameIdx']
    xa <- nrow(runs)
    runs$frame_last <- lead(runs$frame_diff)
    runs$end_of_run <- ifelse(runs$frame_last >= 10, runs$frameIdx, NA)
    runs$end_of_run<-as.integer(runs$end_of_run)
    runs[xa, 'end_of_run'] <- runs[xa, 'frameIdx']
    
    
    #tracking_data %>% dplyr::filter(team=='away') %>% pull(number) %>% unique()
    
    runs_time_data<-data.frame(na.omit(runs$start_of_run), na.omit(runs$end_of_run))
    
    if(nrow(runs_time_data)==0){
      return(NULL)
    }
    
    
    ############### Calculates Space gained value for each identified run #####################################
    
    runs_time_data$points_gained_space<-0
    for(j in 1:nrow(runs_time_data)){
      
      frame1<-which(data[[sequence_number]][[4]]>runs_time_data[j,1])[1]
      frame2<-which(data[[sequence_number]][[4]]>runs_time_data[j,2])[1]
      # get space created vector
      if((!is.na(frame1)) & (!is.na(frame2))){
        vector_v<-(data[[sequence_number]][[3]][frame1:frame2])
        
        # 170 points is no significant space gained
        
        # 270 points gained is a decent score
        runs_time_data[j,3]<-first(vector_v)-last(vector_v)
      }
    }
    
    
    
    ################## Calculates shape disruption value for each run #############################
    
    runs_time_data$points_gained_disruption<-0
    for(k in 1:nrow(runs_time_data)){
      if(attack==T){
        if(filtered_calendar[[1,3]]== defending_teamid){
          form1<-tracking_data %>% center_tracking() %>%  dplyr::filter((frameIdx==runs_time_data[k,1])) %>% dplyr::filter(team=='away' & number!=1)
          
          form2<-tracking_data %>% center_tracking() %>%  dplyr::filter((frameIdx==runs_time_data[k,2])) %>% dplyr::filter(team=='away' & number!=1)
        }else{
          form1<-tracking_data %>% center_tracking() %>%  dplyr::filter((frameIdx==runs_time_data[k,1])) %>% dplyr::filter(team=='home' & number!=1)
          
          form2<-tracking_data %>% center_tracking() %>%  dplyr::filter((frameIdx==runs_time_data[k,2])) %>% dplyr::filter(team=='home' & number!=1)
        }
        
        
      }else{
        if(filtered_calendar[[1,3]]== defending_teamid){
          form1<-tracking_data %>% center_tracking() %>%  dplyr::filter((frameIdx==runs_time_data[k,1])) %>% dplyr::filter(team=='home' & number!=1)
          
          form2<-tracking_data %>% center_tracking() %>%  dplyr::filter((frameIdx==runs_time_data[k,2])) %>% dplyr::filter(team=='home' & number!=1)
        }else{
          form1<-tracking_data %>% center_tracking() %>%  dplyr::filter((frameIdx==runs_time_data[k,1])) %>% dplyr::filter(team=='away' & number!=1)
          
          form2<-tracking_data %>% center_tracking() %>%  dplyr::filter((frameIdx==runs_time_data[k,2])) %>% dplyr::filter(team=='away' & number!=1)
        }
      }
      form1_shape_matrix<-matrix(rep(NA,20),ncol=2,byrow=TRUE)
      form2_shape_matrix<-matrix(rep(NA,20),ncol=2,byrow=TRUE)
      
      form1_shape_matrix[,1]<-form1$x
      form1_shape_matrix[,2]<-form1$y
      
      
      form2_shape_matrix[,1]<-form2$x
      form2_shape_matrix[,2]<-form2$y
      
      dist_matrix<-cdist(form1_shape_matrix,form2_shape_matrix)
      
      ## 55 value of disruption is basically no disruption(On visual inspection)
      
      
      if((!is.na(lp.assign(dist_matrix))) & (!is.null(lp.assign(dist_matrix)))){
        runs_time_data[k,4]<-sum(dist_matrix*(lp.assign(dist_matrix)$solution))
      }
      else{
        runs_time_data[k,4]<-0
      }
      
    }
    
    
    dataframes_return<-list()
    dataframes_return[[1]]<-runs_time_data
    dataframes_return[[2]]<-runs
    return(dataframes_return)
  }
  
  
  data_empty1<-NULL
  data_empty2<-NULL
  
  
  for(i in 1:length(data)){
    if(is.null(data[[i]])){
      
      next()
      
    }
    if(is.null(find_eval_runs(data[[i]][[2]],i))){
      
      next()
    }
    else{
      data_empty1<-rbind(data_empty1,find_eval_runs(data[[i]][[2]],i)[[1]])
      data_empty2<-rbind(data_empty2,find_eval_runs(data[[i]][[2]],i)[[2]])
    }
  }
  players_involved<-as.data.frame(data_empty2)
  combined_runs<-as.data.frame(data_empty1)
  final_runs<-combined_runs %>% filter_func()
  
  return(list(final_runs,players_involved))
}





#### This function filters the runs using a thresholding rule which only keeps either those runs whose value of space gained >500 or those whose formation disruption score is >100


filter_func<-function(data){
  filtered_data<-data %>% dplyr::filter(((points_gained_space>=500) & (points_gained_disruption>100))) %>% rename(start_of_run=na.omit.runs.start_of_run.,
                                                                                                                  end_of_run=na.omit.runs.end_of_run.)
  return(filtered_data)
}




#### Function which helps plot all the runs from left to right and reflects the coordinates 

reflect_track<-function(data){
  if(data$x[[1]]< 50 & tail(data$x,1) >50){
    return(data)
  }
  if(data$x[[1]]> 50 & tail(data$x,1) >50){
    return(data)
  }
  if(data$x[1]< 50 & tail(data$x,1) < 50){
    data$x<- 100 - data$x
    data$y<- 100 - data$y
    return(data)
  }
  if(data$x[1]> 50 & tail(data$x,1) < 50){
    data$x<- 100 - data$x
    data$y<- 100 - data$y
    return(data)
  }
}




#### Function which helps identify multiple runs by the same player and seperates those runs




adjusted_coordinates<-function(data){
  center_x<-data %>% pull(x) %>% mean()
  center_y<-data %>% pull(y) %>% mean()
  adjust_x<-50- center_x 
  adjust_y<-50- center_y 
  data$x<-data$x + adjust_x
  data$y<-data$y +adjust_y
  
  return(data)
}



#### This function plots all the runs on a pitch, it returns the xy location data of each run which is used in the clustering step.

plot_runs_function<-function(data,adjust_coordinates=T){
  
  plot_data<-NULL
  
  if(nrow(data[[1]]) >=1){
    for(i in 1:nrow(data[[1]])){
      
      p_ID<-data[[2]] %>% dplyr::filter((frameIdx>=data[[1]][i,1]) & (frameIdx< data[[1]][i,2])) %>% pull(playerId) %>% unique()
      
      if(adjust_coordinates==T){
        out<-tracking_data %>% dplyr::filter((frameIdx>=data[[1]][i,1]) &(frameIdx<data[[1]][i,2]))
        out$x<-out$x %>% rescale_x()
        out$y<-out$y %>% rescale_y()
        out<- out %>% adjusted_coordinates()
        out<- out %>% dplyr::filter(playerId %in% p_ID)
        
      }else{
        out<-tracking_data %>% dplyr::filter((playerId %in% p_ID) & (frameIdx>=data[[1]][i,1]) &(frameIdx<data[[1]][i,2]))
        #
        out$x<-out$x %>% rescale_x()
        out$y<-out$y %>% rescale_y()
      }
      out<-out %>% group_split(playerId) %>% lapply(reflect_track) %>% bind_rows()
      
      
      out <-transform(out, frameIdx = (frameIdx - min(frameIdx)) / (max(frameIdx) - min(frameIdx)))
      
      #out<- out %>% group_split(playerId) %>% lapply(run_seperator) %>% bind_rows()
      plot_data<-rbind(plot_data,out)
      
      # out<-out %>% reflect_track()
      z<-soccermatics::soccerPitch(lengthPitch = 100,widthPitch = 100)
      a2<- z+geom_point(data = out,aes(x,y,fill=team,alpha=frameIdx),size=1,shape=21)
      print(a2)
      
    }
    
    # plot_data<-plot_data %>% reflect_track()
    plot_data$gameid<-gameid
    z<-soccermatics::soccerPitch(lengthPitch = 100,widthPitch = 100)
    a1<- z+geom_point(data = plot_data,aes(x,y,fill=team,alpha=frameIdx),size=1,shape=21)
    #print(a1)
    return(plot_data)
  }
  else{
    empty_data<-data.frame()
    return(empty_data)
  }
}