##########################################################################
#
#   Function: identify_spikes
#   Purpose:  The purpose of this function is to identify points that are
#     drastically outside of the expected range of pupil diameter in 
#     pupil trajectory
#
#########################################################################
#
# Inputs:
#         - ids: Numeric vector, variable that identifies each participant
#             in the dataset
#         - pupil: Character vector, variable that identifies which pupil
#             the observation is collected from
#         - mtm: Character/numeric vector, variable that identifies which
#             timepoint the observation is collected from (pre, post1, post2)
#         - trial: Character vector, variable that identifies each unique combination
#             of participant ID, pupil measured, timepoint, timing of observation
#             collected, and number of observation collected if multiple trajectories
#             were collected for a unique ID + pupil measured + timepoint combination
#             (i.e. For the right pupil, two measurements were collected for participant 20
#             at the post-consumption 1 timpoint)
#         - times: Numeric vector, variable that identifies the time in the pupil trajectory
#             that the observation was recorded at
#         - pd: Numeric vector, variable identifying the pupil diameter size, (mm)
#
#########################################################################
#
# Outputs:  A dataframe that contains all original input information and
#     also identifies which points in the trajectory were identified as 
#     a "drop point"
#   
#########################################################################
identify_spikes <- function(ids, pupil, mtm, trial, times, pd) {
  
  # Combine supplied variables into a single dataset
  dat <- data.frame(ptid = ids, pupil_measured = pupil,
                    mtm, trial, time = times, pd)
  
  ## lag difference: create a variable that captures the difference in the pd value from the
  #     prev timepoint to the current one
  ### order data by ptid, pupil, measurement time point, trial and time
  long_dat <- dat[order(dat$ptid, dat$pupil_measured, 
                            dat$mtm, dat$trial, dat$time), ]
  
  long_dat$lagDiff <- c(0, diff(long_dat$pd, lag = 1))
  
  # For each new eye, visit, or trial type, we'll reset the lagged diff
  for(i in 2:nrow(long_dat)){
    if(long_dat$ptid[i-1] != long_dat$ptid[i] |
       long_dat$pupil_measured[i-1] != long_dat$pupil_measured[i] | 
       long_dat$mtm[i-1] != long_dat$mtm[i] |
       long_dat$trial[i-1] != long_dat$trial[i]){
      long_dat$lagDiff[i] <- 0
    }
  }
  
  long_dat$leadDiff <- NA
  
  # This section will create the leadDiff variable, which increments the lagDiff variable
  #   effectively bringing it up one entire row
  for(i in 1:(nrow(long_dat)-1)){
    if(long_dat$ptid[i+1] == long_dat$ptid[i] &
       long_dat$pupil_measured[i+1] == long_dat$pupil_measured[i] & 
       long_dat$mtm[i+1] == long_dat$mtm[i] &
       long_dat$trial[i+1] == long_dat$trial[i]){
      long_dat$leadDiff[i] <- long_dat$pd[i+1] - long_dat$pd[i]
    }
    if(long_dat$ptid[i+1] != long_dat$ptid[i] |
       long_dat$pupil_measured[i+1] != long_dat$pupil_measured[i] | 
       long_dat$mtm[i+1] != long_dat$mtm[i] |
       long_dat$trial[i+1] != long_dat$trial[i]){
      long_dat$leadDiff[i] <- 0
    }
  }
  
  ## Mark any points with a lead and lag difference > 0.26 as a drop point. 
  ## If first or last point use only lead or lag difference respectively
  long_dat$dropPt <- ifelse((abs(long_dat$lagDiff) > 0.26 & abs(long_dat$leadDiff) > 0.15) |
                             (abs(long_dat$lagDiff) > 0.15 & abs(long_dat$leadDiff) > 0.26) |
                             (abs(long_dat$lagDiff) > 0.26 & abs(long_dat$leadDiff) == 0) |
                             (abs(long_dat$lagDiff) == 0 & abs(long_dat$leadDiff) > 0.26),
                           1, 0)
  # Return the completed dataset
  return(long_dat)
}
