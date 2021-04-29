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

Run through [Runs_dashboard.rmd](Runs_dashboard.rmd). The Runs_dashboard.rmd contains code to take data using the package pre-process it, apply the algorithm to generate the runs data, cluster them and produce a visualization.


### Dashboard

I have also created a dashboard that helps executives visualize:
1. Where the Dangerous runs came from
2. Look at the times in video when each run happened 
3. From which type of sequence did most of the dangerous runs come from.



