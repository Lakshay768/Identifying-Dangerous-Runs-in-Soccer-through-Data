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

  The file [runs_data_preprocessing](Code/runs_data_preprocessing.R) process the data and produces a data frame which calculates space gained value and shape disruption value for all sequences and gives the XY locations of the runs identified in all the sequences in a game.

#### Clustering

  The file [runs_clustering_funcs](Code/runs_clustering_funcs.R) contains functions to cluster the runs produced in the Data processing step.

#### Dashboard
  Run the file [Runs_dashboard.Rmd](Code/Runs_dashboard.Rmd) it uses the previous files as source and contains functions to produce results for multiple games and also produces a dashboard which can be used in the following ways:

 1. Where the Dangerous runs came from
 2. Look at the times in video when each run happened 
 3. From which type of sequence did most of the dangerous runs come from.



