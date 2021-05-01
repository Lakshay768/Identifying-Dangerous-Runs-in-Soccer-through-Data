
library(rdist)
library(lpSolve)
library(BBmisc)


#### This functions finds the time in each sequence when the space gained by the attacking team was maximum or where the space controlled by defending team was minimum . It returns the frameids or the times during each sequence which can then be used to find if there was any 'run' or not around that time.

find_max_space_created<-function(vector_seq_numbers,box_entry=0,attack=F){
  
  data_disruption<-list()
  
  classification_vector<-rep(0,length(vector_seq_numbers))
  passes_vector<-rep(0,length(vector_seq_numbers))
  timestamps_frequent_sequences<-timestamps_data %>% dplyr::filter(sequence_id %in% vector_seq_numbers)
  
  timestamps_frequent_sequences<- timestamps_frequent_sequences %>% as.data.frame() 
  
  
  for (i in 1:nrow(timestamps_frequent_sequences)){
    pass_counter<-0
    
    # getting the start and end events of the sequence
    sequence_events<-event_data %>% dplyr::filter(total_secs>= (timestamps_frequent_sequences[i,3]) & total_secs <= (timestamps_frequent_sequences[i,4]))
    
    # Checking if the ball ended in the box during this sequence
    
    # if it does then the classification vector will have a value 1 for that sequence number.
    if(nrow(sequence_events)==0){
      next()
    }else{
      for(j in 1:nrow(sequence_events)){
        
        if(is.na(sequence_events$`140`[j]) | is.na(sequence_events$`141`[j]))
        {
          next()
          
        }else{
          
          if((sequence_events$type_id[j]=='1') & (sequence_events$`140`[j]<50)){
            
            pass_counter<-pass_counter+1
          }
          
          if((sequence_events$`140`[j]>=83) & ((sequence_events$`141`[j]>21) & (sequence_events$`141`[j] < 79)))
          {
            classification_vector[i]<-1
            
          }else{
            next()
          }
        }
        
      }
    }
    if(pass_counter>=3){
      passes_vector[i]<-1
    }
  }
  
  
  
  ### keep only those sequences which had atleast 3 passes in defensive half.
  
  vector_seq_numbers<-vector_seq_numbers[which(passes_vector==1)]
  
  
  # To get only sequences where there was box entry use the classification vector line
  if(box_entry==1){
    box_entry_sequence_numbers<-vector_seq_numbers[which(classification_vector==1)]
  }else{
    box_entry_sequence_numbers<-vector_seq_numbers
  }
  
  
  frame_disruption_max<-rep(0,length(box_entry_sequence_numbers))
  
  box_entry_sequences_timestamp<-timestamps_data %>% dplyr::filter(sequence_id %in% box_entry_sequence_numbers)
  
  box_entry_sequences_timestamp<-as.data.frame(box_entry_sequences_timestamp)
  
  for(i in 1:length(box_entry_sequence_numbers)){
    
    if((!is.na(box_entry_sequences_timestamp[i,1])) & (!is.na(box_entry_sequences_timestamp[i,2]))){
      
      frame_numbers<-seq(box_entry_sequences_timestamp[i,1],box_entry_sequences_timestamp[i,2],5)
      
      space_creation<-seq(box_entry_sequences_timestamp[i,1],box_entry_sequences_timestamp[i,2],5)
      
      
      
      for(j in 1:length(space_creation)){
        
        
        space_creation[j]<- find_space_created(tracking_data %>% dplyr::filter(frameIdx==frame_numbers[j]))
        
      }
      frame_ids<-frame_numbers[which.min(space_creation)]
      delta_max<-frame_numbers[which.min(diff(space_creation))]
      data_disruption[[i]]<-list(frame_ids,delta_max,space_creation,frame_numbers)
    }
  }
  return(data_disruption)
}


