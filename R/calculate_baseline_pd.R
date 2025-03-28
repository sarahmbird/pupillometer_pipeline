##########################################################################
#
#   Function: calculate_baseline_pd
#   Purpose:  The purpose of this function is to calculate the average
#       baseline pupil diameter before the light stimulus is flashed.
#
#########################################################################
#
# Inputs:
#         - ptids: Numeric vector, variable that identifies each participant
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
#         - time: Numeric vector, variable that identifies the time in the pupil trajectory
#             that the observation was recorded at
#         - pd: Numeric vector, variable identifying the pupil diameter size, (mm)
#         - baseline_jump: Numeric vector, Indicator (0/1) which indicates whether the
#             current observation is a baseline jump (1) or is not a baseline jump (0).
#
#########################################################################
#
# Outputs:  A dataframe that contains one row per participant trial. The dataset
#     contains the variable "bl_pupilSize" which identifies the baseline
#     pupil diameter for each participant trial in the study.
#   
#########################################################################
calculate_baseline_pd <- function(ptids, trial, pupil, mtm, time, pd, baseline_jump) {
  # Create dataset out of input vectors
  dat <- data.frame(ptid = ptids, trial, pupil_measured = pupil, 
                    mtm, time, pd, baseline_jump)
  # Create baseline 
  dat$bl <- ifelse( (dat$time > 0.75) & (dat$time < 1.0) & (dat$baseline_jump == 0), 1, 0)
  bl_pupilSize <- as.data.frame(aggregate(dat$pd, 
                                          by = list(dat$ptid, 
                                                    dat$pupil_measured, 
                                                    dat$mtm,
                                                    dat$trial,
                                                    dat$bl), 
                                          FUN = function(x) {round(mean(x, na.rm = T), 2)}))
  names(bl_pupilSize) <- c("ptid", "eye", "mtm", "trial", "bl", "pupilSize_bl")
  bl_pupilSize <- bl_pupilSize[bl_pupilSize$bl == 1, ]
  bl_pupilSize$bl <- NULL
  
  return(bl_pupilSize)
}



