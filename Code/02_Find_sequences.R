

################ Function to get the coordinates of each run identified for a single game ##############################

#### Parameters:

###  Game id : Identifier for the match for which to produce results

###  Defending_teamid : Identifier for the team who would be on defense in all the sequences. In other words the runs would be found against this team.

###  Attack : If True then the team id provided above will be for offense.

###  Box_entry : If 1 then only those sequences are counted which lead to box entry, if 0 then all sequences will be counted



find_sequences<-function(gameid,defending_teamid,attack=F,from_seq){
  
  
  
  #### Read in event data which contains information about all on ball events during a match for eg. every pass, dribble, tackle etc
  gameid<<-gameid
  event_data<<-nashvillesc::parse_f24(gameid)
  
  defending_teamid<<-defending_teamid
  
  #### Adding a new column to convert the time on the clock to only seconds 
  
  event_data$total_secs<<-(event_data$min*60) + event_data$sec
  
  
  
  #### Getting the season calendar of Nashville to check for a particular game if Nashville was the home or the away team
  
  calendar_season<-nashvillesc::getSeasonCalendar()
  filtered_calendar<<-calendar_season %>% dplyr::filter(game_id==gameid)
  
  
  
  
  #### Get tracking data of the game identified by the gameid
  
  tracking_data<<- nashvillesc::get_parsed_2S_tracking_data(gameid,return_data = T)
  tracking_data$team<<-as.factor(tracking_data$team)
  
  
  
  #### Get Insights data of the game identified by gameid
  
  insight_data<-nashvillesc::get_parsed_2S_Insight_data(gameid,return_data = T)
  insight_data$diff_secs<-insight_data$endClock - insight_data$startClock
  
  
  
  
  #### How to get relevant sequences
  ## 1) Sequences which started after a Throw in, Goal Kick, Kick off, Tackle Recovery, Tackle, Pass interception
  ## 2) Sequences were atleast 15 secs long
  ## 3) Had atleast 5 passes inside the offensive team's own half (This condition is checked later)
  
  #### Chosen sequences which were atleast 15 seconds long and came from these start types.
  
  if(attack==T){
    timestamps_data<<-insight_data %>% dplyr::filter(markingType=='possession' & diff_secs>=15 & startType %in% from_seq & defTeamId!=as.character(defending_teamid)) %>% select(startFrameIdx,endFrameIdx,startClock,endClock,diff_secs,startType)
  }else{
    timestamps_data<<-insight_data %>% dplyr::filter(markingType=='possession' & diff_secs>=15 & startType %in% from_seq & defTeamId==as.character(defending_teamid)) %>% select(startFrameIdx,endFrameIdx,startClock,endClock,diff_secs,startType)
  }
  
  timestamps_data$sequence_id <<- 1:nrow(timestamps_data)
  library(deldir)
  
  
  
  
  #### Getting the formation/ shape data for the defending team for each sequence in the 'timestamps data'
  
  formation_data<-list()
  for(i in 1:nrow(timestamps_data)){
    if(attack==T){
      if(filtered_calendar[[1,3]]== defending_teamid){
        players_track<-tracking_data %>% dplyr::filter((frameIdx>=timestamps_data[[i,1]]) & (frameIdx<=timestamps_data[[i,2]]) & (team=='away') & (number!=1))
      }
      else{
        players_track<-tracking_data %>% dplyr::filter((frameIdx>=timestamps_data[[i,1]]) & (frameIdx<=timestamps_data[[i,2]]) & (team=='home') & (number!=1))
      }
    }else{
      if(filtered_calendar[[1,3]]== defending_teamid){
        players_track<-tracking_data %>% dplyr::filter((frameIdx>=timestamps_data[[i,1]]) & (frameIdx<=timestamps_data[[i,2]]) & (team=='home') & (number!=1))
      }
      else{
        players_track<-tracking_data %>% dplyr::filter((frameIdx>=timestamps_data[[i,1]]) & (frameIdx<=timestamps_data[[i,2]]) & (team=='away') & (number!=1))
      }
    }
    players_track<- players_track %>% group_by(playerId) %>% summarise(avg_x=mean(x),avg_y=mean(y),sd_x=sd(x),sd_y=sd(y))
    
    players_track$sequence_id<- timestamps_data[[i,7]]
    
    formation_data[[i]]<- players_track
    
  }
  
  
  
  
  
  #### Creates the gaussian normals for each player in the formation data (This step is a pre-requisite to find similarity in two different shapes) and also centers the data for consistency(to keep the point of reference same for every shape)
  
  formation_data<-lapply(formation_data, make_gauss) %>% lapply(center_tracking2)
  
  
  
  
  #### Calculates the similarity score for every formation for all the sequences and stores it in a distance matrix to prepare it for clustering
  
  df2 <- data.frame(matrix(ncol = length(formation_data), nrow = length(formation_data)))
  for (x in 1:length(formation_data)) {
    for (y in 1:length(formation_data)) {
      
      # can use scaled similarity score instead of sim formations to create distance matrix
      df2[x,y] <- ifelse(x >= y, sim_formations(formation_data[[x]],formation_data[[y]]), NA)
      if(nrow(df2)<=0){
        empty_data<-data.frame()
        return(empty_data)
      }
    }
  }
  
  #scaled_similarity_score(formation_data[[1]],formation_data[[2]])
  
  
  #### Hierarchical clustering is done on the formations to find the biggest cluster which we can use to back track the sequences involved in the biggest cluster
  
  
  m <- as.matrix(df2)
  m <- as.dist(m)
  hclust_avg <- hclust(m, method = 'centroid')
  
  #b <- data.frame(c(1:24,cutree(hclust_avg, k = 8)))
  b <- data.frame(1:length(formation_data), cutree(hclust_avg, k = 6))
  colnames(b) <- c("Formation", "Cluster")
  
  # find the biggest cluster and then formations in that cluster
  cluster_biggest<-tail(names(sort(table(b$Cluster))), 1) %>% as.integer()
  
  
  #### Gives the sequences which had the most frequent formations of the defending team.(THIS IS HOW WE GET RELEVANT SEQUENCES ) 
  
  sequences<-b %>% dplyr::filter(Cluster==cluster_biggest) %>% select(Formation) %>% pull()
  
  return(sequences)
}

