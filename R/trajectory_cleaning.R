##########################################################################
#
#   Function: data_cleaning
#   Purpose:  The purpose of this function is to take in an input dataset
#     that contains the raw pupil trajectories, and to clean them based
#     on ...
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
#         - times: Numeric vector, variable that identifies the time in the pupil trajectory
#             that the observation was recorded at
#         - pd: Numeric vector, variable identifying the pupil diameter size, (mm)
#         - bin: Numeric vector, variable identifying whether or not the participant's eye was 
#             obstructed at the associated time provided in the "times" vector
#         - bestTrials: Boolean value, variable identifying if only the best trials classified
#             by the internal algorithm are retained in the final trajectory provided by the 
#             function.
#         - removeBaselineJumps: Boolean value, variable identifying if baseline jumps should be r
#             removed from the analysis
#         - plotBaselineJumps: Boolean value, variable identifying if observations with baseline jumps
#             should be printed to the console for manual verification
#         - graphingDset: Boolean value, variable identifying if a separate dataset, compatible with the 
#             plot_pupil_trajectories() function should be returned. 
#
#########################################################################
#
# Outputs:  There are two possible pieces of output to this function:
#     1) pupil_clean_trajectory: A dataset that contains cleaned pupil trajectories
#         that are ready to be smoothed.
#     2) If graphingDset is set to TRUE, a second object is returned as the second
#         element in the list function. graphing_dataset is a dataframe that contains
#         the raw pupil trajectories with drop points identified and all trials, 
#         regardless of bestTrial status, so the user can view all trajectories and
#         assign trajectories the BestTrial status based on visual judgement.
#   
#########################################################################
trajectory_cleaning <- function(ids, pupil, mtm, mtmTrial, times, pd, bin,
                                bestTrials = T, removeBaselineJumps = T, 
                                plotBaselineJumps = F, graphingDset = F) {
  
  # Create dataframe containing all information needed to run the functions below
  dat <- data.frame(ptid = ids, pupil_measured = pupil, mtm, mtmTrial, 
                    time = times, pd, bin)
  
  dat$trial <- paste0(dat$ptid, "_", dat$pupil_measured,
                        "_", dat$mtm, "_",
                        dat$mtmTrial)
  
  ### Eye obstruction sets pupil diameter value to 0. 
  dat <- dat[dat$bin == 1, ]
  
  # Identify the drop points
  drop_points <- identify_spikes(ids = dat$ptid,
                                 pupil = dat$pupil_measured,
                                 mtm = dat$mtm,
                                 trial = dat$trial,
                                 times = dat$time,
                                 pd = dat$pd)
  
  # First, let's obtain the quality metrics 
  quality_metrics <- obtain_quality_metrics(ids = drop_points$ptid,
                                            trial = drop_points$trial,
                                            mtm = drop_points$mtm,
                                            pupil = drop_points$pupil_measured,
                                            dropPt = drop_points$dropPt,
                                            lagDiff = drop_points$lagDiff,
                                            time = drop_points$time)
  
  # Merge the quality metrics to the long pupil trajectory dataset that we have
  pupil_l2 <- merge(drop_points, quality_metrics[, c("trial", "bestTrial")],
                    by = "trial")
  
  # Assuming that the best trials ARE the best trials, we can save the data accordingly
  if (bestTrials == T) {
    pupil_trajectories <- pupil_l2[pupil_l2$bestTrial == 1 & pupil_l2$dropPt == 0 & pupil_l2$pd < 10, ]
  } else {
    pupil_trajectories <- pupil_l2[pupil_l2$dropPt == 0 & pupil_l2$pd < 10, ]
  }

  # Identify the baseline jumps
  baselinejumps <- identify_baseline_jumps(ptids = pupil_trajectories$ptid,
                                           trial = pupil_trajectories$trial,
                                           pupil = pupil_trajectories$pupil_measured,
                                           mtm = pupil_trajectories$mtm,
                                           time = pupil_trajectories$time,
                                           pd = pupil_trajectories$pd, 
                                           plot = plotBaselineJumps)
  
  # Remove points that are baseline jumps, if specified by the user
  if (removeBaselineJumps == T) {
    baselinejumps <- baselinejumps[baselinejumps$median_baseline_jump == 0, ]
  } 

  # Calculate the baseline diameter
  baseline_diameter <- calculate_baseline_pd(ptids = baselinejumps$ptid,
                                             trial = baselinejumps$trial,
                                             pupil = baselinejumps$pupil_measured,
                                             mtm = baselinejumps$mtm,
                                             time = baselinejumps$time,
                                             pd = baselinejumps$pd,
                                             baseline_jump = baselinejumps$median_baseline_jump)
  # Merge the quality metrics to the baseline jump dataset
  baselinejumps <- merge(baselinejumps, quality_metrics[, c("trial", "bestTrial")],
                          by = "trial", all.x = TRUE)
  # Merge the baseline pupil size to the baseline jump dataset
  baselinejumps <- merge(baselinejumps, baseline_diameter[,c("trial", "pupilSize_bl")],
                         by = "trial", all.x = TRUE)
  # Subset to only relevant variables
  #pupil_clean_trajectory <- baselinejumps[,c("ptid", "pupil_measured", "mtm", "trial", "time", "pd")]
  pupil_clean_trajectory <- baselinejumps[,c("ptid", "pupil_measured", "mtm", "trial", "time", "pd", "bestTrial",
                                             "pupilSize_bl")]
  # Create a trial id variable that identifies the final participant id, measured pupil, and timepoint
  pupil_clean_trajectory$trial_id <- paste(as.character(pupil_clean_trajectory$ptid), 
                                           substr(pupil_clean_trajectory$pupil_measured, 1,1), 
                                           substr(pupil_clean_trajectory$mtm, 1,1), sep = "_")
  
  # Truncate start to 0.6 sec and after; light stimulus at 1 sec.
  pupil_clean_trajectory <- pupil_clean_trajectory[pupil_clean_trajectory$time >= 0.6, ]
  
  if (graphingDset == T) {
    return(list(pupil_clean_trajectory = pupil_clean_trajectory, graphing_dataset = pupil_l2))
    
  } else {
    return(pupil_clean_trajectory)
  }
}


