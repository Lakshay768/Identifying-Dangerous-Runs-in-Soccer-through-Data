

########################  This function estimates a curve made by plotting the xy points of a players run into 3 points. It returns the 3 points (x1,y1) (x2,y2) (x3,y3). These are temporally related to each other in the sense that (x1,y1) marks start of a run and (x3,y3) marks the end of a run  ################################################################

make_bezier_points<-function(data){
  p<-data %>% select(x,y) %>% as.matrix
  m<-bezierCurveFit(p,min.control.points = 3,max.control.points = 3)
  
  cc2 <- data.frame(m[[1]][[1]][[1]],m[[1]][[1]][[2]],m[[1]][[1]][[3]],m[[1]][[2]][[1]],m[[1]][[2]][[2]], m[[1]][[2]][[3]],unique(data$start_run_time),unique(data$end_run_time),unique(data$period),unique(data$gameid))
  colnames(cc2) <- c('x1','x2','x3','y1','y2','y3','start_of_run','end_of_run','period','gameid')
  
  return(cc2)
}





############################ This function makes a distance function which takes the 3 estimated points for each run and finds distance between them. This is necessary so that these runs can be clustered.The function returns the cluster number and the runid ####################################################################################################

cluster_curves<- function(data,num_clusters){
  
  curve_euclidean_dist<-function(data1,data2){
    value<- (((data1$x1 -data2$x1)^2) + ((data1$x2 -data2$x2)^2) + ((data1$x3 -data2$x3)^2) + ((data1$y1 -data2$y1)^2) + ((data1$y2 -data2$y2)^2) + ((data1$y3 -data2$y3)^2) )
    
    return(value)
  }
  
  df2 <- data.frame(matrix(ncol = nrow(data), nrow = nrow(data)))
  for (x in 1:nrow(data)) {
    for (y in 1:nrow(data)) {
      
      # can use scaled similarity score instead of sim formations to create distance matrix
      df2[x,y] <- ifelse(x >= y, curve_euclidean_dist(data[x,],data[y,]), NA)
    }
  }
  #scaled_similarity_score(formation_data[[1]],formation_data[[2]])
  
  m <- as.matrix(df2)
  m <- as.dist(m)
  hclust_avg <- hclust(m, method = 'centroid')
  
  b <- data.frame(1:nrow(data), cutree(hclust_avg, k = num_clusters))
  colnames(b) <- c("Run", "Cluster")
  
  return(b)
}






##################### This function adds the start and end time of the run which is used by the video analyst. The earlier dataframes had information only about the xy locations of the run. #######################################

add_times_of_runs<-function(data){
  gameClock_start <- head(data,1) %>% pull(gameClock)
  gameClock_end <- tail(data,1) %>% pull(gameClock)
  period <- head(data,1) %>% pull(period)
  data$start_run_time<- ifelse(period == 1,
                               paste(as.integer(gameClock_start/60),as.integer((gameClock_start/60-as.integer(gameClock_start/60))*60),sep = ":"),
                               paste(as.integer(gameClock_start/60)+45,as.integer((gameClock_start/60-as.integer(gameClock_start/60))*60),sep = ":"))
  
  # data$start_run_time_s<-(((head(data,1) %>% pull(gameClock))/60) - round(((head(data,1) %>% pull(gameClock))/60),0))*60
  data$end_run_time<-ifelse(period == 1,
                            paste(as.integer(gameClock_end/60),as.integer((gameClock_end/60-as.integer(gameClock_end/60))*60),sep = ":"),
                            paste(as.integer(gameClock_end/60)+45,as.integer((gameClock_end/60-as.integer(gameClock_end/60))*60),sep = ":"))
  # data$end_run_time_secs<-(((tail(data,1) %>% pull(gameClock))/60) - round(((tail(data,1) %>% pull(gameClock))/60),0))*60
  data$period<-head(data,1) %>% pull(period)
  return(data)
}






########################### This functions returns the cluster of runs with necessary information ####################

make_cluster_points<-function(data){
  
  result<-data  %>% group_split(run_id) %>% lapply(add_times_of_runs) %>%  lapply(make_bezier_points) %>% bind_rows()
  
  return(result)
}




