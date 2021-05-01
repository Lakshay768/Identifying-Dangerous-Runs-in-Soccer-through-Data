


################# Importing necessary libraries
library(readr)
library(nashvillesc)
library(tidyverse)
library(transport)
library(lpSolve)
library(readr)
library(rdist)
library(factoextra)
library(fpc)
library(dbscan)
library(dplyr)
library(bezier)
library(flexdashboard)
library(shiny)
library(shinydashboard)
library(readr)
library(tidyverse)
library(nashvillesc)
library(soccermatics)
library(signal)
library(DT)


#### Function to calculate space created by offensive team at any given frame
find_space_created<-function(track_data,attack=F){
  
  #### Thresholding the data to prevent any coordinates which are outside the pitch dimensions  
  
  
  track_data$x[which(track_data$x>52.5)]<-52
  track_data$y[which(track_data$y>34)]<-34
  
  track_data$x[which(track_data$x< -52.5)]<- -52.5
  track_data$y[which(track_data$y< -34)]<- -34
  
  
  
  #### Create deldir object
  
  deldir_obj<-deldir(track_data$x,track_data$y,list(ndx=2,ndy=2),rw=c(-52.5,52.5,-34,34))
  
  
  track_data<-track_data %>% arrange(x,y)
  df_deldir<-deldir_obj$summary
  df_deldir<-df_deldir %>% dplyr::filter(pt.type=='data')
  
  df_deldir<-df_deldir %>% arrange(x,y)
  df_deldir$team<-track_data$team
  
  
  
  # Space creation total area for away team 
  
  if(attack==T){
    if(filtered_calendar[[1,3]]== defending_teamid){
      value<- df_deldir %>% dplyr::filter(team=='away') %>% pull(del.area) %>% sum()
    }else{
      value<- df_deldir %>% dplyr::filter(team=='home') %>% pull(del.area) %>% sum()
    }
    
  }else{
    if(filtered_calendar[[1,3]]== defending_teamid){
      value<- df_deldir %>% dplyr::filter(team=='home') %>% pull(del.area) %>% sum()
    }else{
      value<- df_deldir %>% dplyr::filter(team=='away') %>% pull(del.area) %>% sum()
    }
  }
  return(value)
}




#### Functions to rescale the coordinates of the tracking data to 

rescale_x<-function(x){
  result<-scales::rescale(x,to=c(0,100),from=c(-52.5,52.5))
  return(result)
}

rescale_y<-function(x){
  result<-scales::rescale(x,to=c(0,100),from=c(-34,34))
  return(result)
}






#### Function to create gaussian normals for each player 

make_gauss<-function(data){
  for (i in 1:nrow(data)){
    cov_mat<-matrix(rep(1,4),ncol=2,byrow=T)
    cov_mat[1,2]<-cov_mat[2,1]<-data$sd_x[i]*data$sd_y[i]
    
    cov_mat[1,1]<-(data$sd_x[i])^2
    cov_mat[2,2]<-(data$sd_y[i])^2
    
    test1<-MASS::mvrnorm(1000,mu = c(data$avg_x[i],data$avg_y[i]),Sigma = cov_mat)
    
    data$gauss_data[i]<-list(test1)
  }
  
  return(data)
  
}








#### Function to center the tracking data to the center circle

center_tracking<-function(data){
  center_x<-data %>% pull(x) %>% mean()
  center_y<-data %>% pull(y) %>% mean()
  nudge_x<-0-center_x
  nudge_y<-0-center_y
  
  data$x<-data$x + nudge_x
  data$y<-data$y +nudge_y
  
  return(data)
}


library(lqmm)





#### Function to scale a formation to take care of cases when a formation might be an expanded version of another formation

scale_min_func<-function(k,form1,form2){
  
  #creating the scaled co-variance matrix again for formation passed in first in "form1"
  
  for(i in 1:nrow(form1)){
    cov_mat<-matrix(rep(1,4),ncol=2,byrow=T)
    cov_mat[1,2]<-cov_mat[2,1]<-form1$sd_x[i]*form1$sd_y[i]*k
    
    cov_mat[1,1]<-((form1$sd_x[i])^2) *(k^2)
    cov_mat[2,2]<-((form1$sd_y[i])^2)*(k^2)
    cov_mat <- make.positive.definite(cov_mat, tol=1e-3)
    test1<-MASS::mvrnorm(1000,mu = c(form1$avg_x[i],form1$avg_y[i]),Sigma = cov_mat)
    
    form1$gauss_data[i]<-list(test1)
  }
  
  dist_matrix<-matrix(rep(NA,100),ncol=10,byrow = T)
  for(i in 1:nrow(form1)){
    
    for (j in 1:nrow(form2)){
      dist_matrix[i,j]<-(wasserstein1d(form1[i,7][[1]][[1]],form2[j,7][[1]][[1]],p=2))^2
      
    }
  }
  
  sum(dist_matrix*(lp.assign(dist_matrix)$solution))
  
}




#### Function to find similarity between two formations/shapes

sim_formations<-function(form1,form2){
  dist_matrix<-matrix(rep(NA,100),ncol=10,byrow = T)
  for( i in 1:nrow(form1)){
    for (j in 1:nrow(form2)){
      dist_matrix[i,j]<-(wasserstein1d(form1[i,7][[1]][[1]],form2[j,7][[1]][[1]],p=2))^2
      
    }
  }
  
  if((!is.na(lp.assign(dist_matrix))) & (!is.null(lp.assign(dist_matrix)))){
    return(sum(dist_matrix*(lp.assign(dist_matrix)$solution)) )
  }else{
    return(10000)
  }
  
  
}





scaled_similarity_score<-function(form1,form2){
  return(min(optim(1,scale_min_func,form1=form1,form2=form2)$value,optim(1,scale_min_func,form1=form2,form2=form1)$value))
}




center_tracking2<-function(data){
  center_x<-data %>% pull(avg_x) %>% mean()
  center_y<-data %>% pull(avg_y) %>% mean()
  nudge_x<-0-center_x
  nudge_y<-0-center_y
  
  data$avg_x<-data$avg_x + nudge_x
  data$avg_y<-data$avg_y +nudge_y
  
  return(data)
}









