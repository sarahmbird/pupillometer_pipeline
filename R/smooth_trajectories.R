##########################################################################
#
#   Function: smooth_trajectories
#   Purpose:  The purpose of this function is to take in the raw trajectories
#               from the pupillometer, after best trials have been identified,
#               and to smooth them using the qgam() function
#
#########################################################################
#
# Inputs:
#         - ptids: Numeric vector, variable that identifies each participant
#             in the dataset
#         - time: Numeric vector, variable that identifies the time in the pupil trajectory
#             that the observation was recorded at
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
#         - pd: Numeric vector, variable identifying the pupil diameter size, (mm)
#         - pupilsize_bl: Numeric vector, variable identifying the average baseline
#             pupil diameter size (mm).
#
#
#########################################################################
#
# Outputs:  A dataframe that contains all variables that were in the original
#               input dataframe along with a new column, pred, that contains
#               the newly calculated predicted trajectories using the 
#               results from the qgam() function
#   
#########################################################################
smooth_trajectories <- function(ptids, time, pupil, mtm, trial,
                                pd, pupilsize_bl) {
  # Create dataframe out of provided objects
  dat <- data.frame(ptid = ptids, trial, mtm, eye = pupil, pupilsize_bl, time, pd)
  #return(dat)
  # Out of the cleaned pupillary data, extract all trials we'll use
  trial_ids <- unique(dat$trial)
  
  # Create a vector that contains all times that we'll be ordering and applying
  #   the smoothing function to 
  time_vec.df <- data.frame(time = sort(unique(dat$time)))
  
  # Create an empty list to store the smoothed trajectories
  trajectory_smooth <- list()
  
  # For each participant, apply the qgam_smoothing() function
  #   and obtain the smoothed data
  for(i in trial_ids){
    trial_df <- dat[dat$trial == i, ]
    # gqam_smoothing() requires the following variables to run:
    #     ptid, pupil_measured, mtm, trial_id, trial, pd
    trajectory_smooth[[i]] <- qgam_smoothing(trial_df, time_vec.df, knots = 14)
  }
  # Combine all smoothed trajectories into one data frame
  trajectory_smooth <- do.call("rbind", trajectory_smooth)
  return(trajectory_smooth)
}









########################################################################
#
# Scratch code...
#
########################################################################





# smooth_trajectories <- function(ptids, trial_id, time, pupil, mtm, trial,
#                                 pd) {
#   # Create dataframe out of provided objects
#   dat <- data.frame(ptid = ptids, trial_id, trial, mtm, eye = pupil, time, pd)
#   #return(dat)
#   # Out of the cleaned pupillary data, extract all trials we'll use
#   trial_ids <- unique(dat$trial_id)
#   
#   # Create a vector that contains all times that we'll be ordering and applying
#   #   the smoothing function to 
#   time_vec.df <- data.frame(time = sort(unique(dat$time)))
# 
#   # Create an empty list to store the smoothed trajectories
#   trajectory_smooth <- list()
#   
#   # For each participant, apply the qgam_smoothing() function
#   #   and obtain the smoothed data
#   for(i in trial_ids){
#     trial_df <- dat[dat$trial_id == i, ]
#     # gqam_smoothing() requires the following variables to run:
#     #     ptid, pupil_measured, mtm, trial_id, trial, pd
#     trajectory_smooth[[i]] <- qgam_smoothing(trial_df, time_vec.df, knots = 14)
#   }
#   # Combine all smoothed trajectories into one data frame
#   trajectory_smooth <- do.call("rbind", trajectory_smooth)
#   return(trajectory_smooth)
# }



