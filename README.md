# Identifying-Dangerous-Runs-in-Soccer-through-Data
This project aims to help video analysts and coaches in football clubs in Identifying Dangerous Runs in Soccer through a data driven approach. 


## Prerequisites

R and Flexdashboard was used to do visualization,modeling of algorithm and making dashboard for this project.

## Installation

Nashvillesc package (private), Tidyverse, signal, readr, shiny, shinydashboard, flexdashboard, bezier, rdist, transport, lpsolve, fpc, DT   are needed for R. The instructions for installing these packages are provided below:

```
install.packages('package name')
```

In order to run the dashboard you will need to install Flexdashboard, shiny and shinydashboard. 


## Datasets and Feature Sources

Data was given to me by a third party which gives us GPS location data for all players during a match (Tracking data) and data for all on the ball events (Event data).

The data is extracted using functions from the nashvillesc package which require the gameid of the match for which the data is needed. The data cannot be shared due to confidentiality reasons.


`Insight_data.rds`:  Has information about the sequences in a game and where they came from


`Tracking_data.csv` Gps locations of all 22 players and the ball which is provided at 25frames per second.

`event_data.csv`  Data with record of all on ball events during a game like tackles, interceptions, passes, shots and information about the player and receiver and also the start and end location coordinates of the ball in this event.


## Pipeline


#### Data processing 
    
  The file [01_Load_utility_functions](Code/01_Load_utility_functions.R) contains all the necessary functions to help manipulate the data to run clustering and other algorithms on it. It also contains the function to find space created value of a team at any point 't'. This file is necessary to load to make sure everything works smoothly. It also contains the necessary libraries.
     
  The file [02_Find_sequences](Code/02_Find_sequences.R) contains a function which takes the gameid of the game for which the result needs to be produced, the defending team's id and the start type of sequences allowed for eg. sequences which started from Goalkicks or Corner Kick or Throw-in or a Tackle etc. The code gets the necessary data for the game and finds the sequences where the defending team were in their desired shape. The file then returns the sequence numbers where the team was in the desire shape and these sequences would then be used to find space created by the team. 
  
  The file [03_Find_max_space_gained](Code/03_Find_max_space_gained.R) contains a function which gives the space gained value of a team at any given time and also gives the the time 't' where the space gained between time 't' and 't-1' was maximum in the whole sequence.
  
  
#### Finding and Filtering Runs

   The file [04_runs_identification_and_filtering](Code/04_runs_identification_and_filtering.R) contains a function which gives out the XY locations of all the runs identified and which pass the threshold value. The function also assigns each run a value for Space gained and Shape disruption which is then passed to a filtering function. The file returns the location data of each run.

 
#### Clustering

  The file [05_runs_clustering_funcs](Code/05_runs_clustering_funcs.R) contains functions to cluster the runs produced in the previous step

#### Dashboard
  Run the file [Runs_dashboard.Rmd](Code/Runs_dashboard.Rmd) it uses the previous files as source and contains functions to produce results for multiple games and also produces a dashboard which can be used in the following ways:

 1. Where the Dangerous runs came from
 2. Look at the times in video when each run happened 
 3. From which type of sequence did most of the dangerous runs come from.



