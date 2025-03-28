# Pupillometer Processing Pipeline

## Overview

The `pupillometer_pipeline` package is a processing pipeline for pupillometer data, designed to clean and smooth data using functional data analysis. It is specifically built for longitudinal studies where pupil trajectories are recorded from both eyes at multiple timepoints.

Due to technological limitations, human error, and other factors, pupil trajectories may not always be accurately captured. As a result, multiple trials may be recorded for a single eye at a given timepoint. Our pipeline incorporates a novel method to classify the "best" pupil trajectory and applies smoothing accordingly. Additionally, users can manually review trajectories and adjust classifications as needed.

The pipeline is detailed in (insert Suni’s paper here), where it is applied to analyze pupillary trajectories of participants from three cannabis use groups before and after cannabis consumption.


## Installation

You can install this package like so:


....


## Example

This step reads in a selection of sample trajectory data collected from a handheld pupillometer. The functions provided in this R package will assume that the input data is in long format (i.e. one row per timepoint in the pupil trajectory for each participant trial). The required input variables are as follows:

1. Ptid: A numeric vector. This is a variable that identifies each unique participant in the dataset
2. pupil_measured: A character vector. This variable defines which pupil the trajectory was collected on. Will take on either a value of "Right" or "Left".
3. time: A numeric vector. This is a variable that contains each unique time where a measurement was collected in the pupillary trajectory. This value should strictly be positive, and should be in long format.
4. mtm: A character vector. This is a variable that uniquely identifies each timepoint where pupillary trajectories were assessed in the dataset. In the case of our dataset, the mtm variable takes on values of 0_Pre, 1_Post1, and 2_Post2.
5. pct_EyeObstructed: (OPTIONAL) A numeric vector. This variable contains summary information for each participant-trial identifying the percent of pupillary obstruction that occurs during the whole length of the pupil trajectory. It is recommended that users remove trials where there is greater than 60\% obstruction, which is demonstrated in the first step below.
6. mtmTrial: A numeric vector. In the case of multiple trials at a particular timepoint for one eye, this is an identifier that orders the trials in order of when they were conducted. 
7. pd: A numeric vector. This variable contains measurements of pupil diameter at each unique time in the trajectory as specified by the 'time' variable.
8. bin: A numeric vector. This variable takes on a value of either 0 or 1. A value of 1 indicates that at the particular moment in the trajectory specified by the 'time' variable, the pupil was obstructed. A value of 0 indicates that the pupil was not obstructed. 


To produce the smoothed trajectories, the user should first remove trials that have high pupil obstruction. Then, the following functions should be run to obtain the smoothed trajectories or scalar summary data:

1. trajectory_cleaning()
2. smooth_trajectories()
3. pupil_trajectory_metrics()










