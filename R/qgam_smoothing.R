##########################################################################
#
#   Function: qgam_smoothing
#   Purpose:  The purpose of this function is to take a participants 
#     pupil trajectory and smooth them using the function qgam() with
#     a specified number of knots
#
#########################################################################
#
# Inputs:
#         - df: A dataframe that contains a participant identifier, the
#             unique trial identifier, a variable identifying timepoint
#             of the pupil measurement, a variable specifying which pupil
#             the trajectory was collected from, the baseline pupil diameter,
#             and the time/pupil diameter variables that compose the pupil
#             trajectory. This function is used in the smooth_trajectories()
#             function, which is fed the necessary data for this function to 
#             run.
#
#########################################################################
#
# Outputs:  A dataframe containing all variables in the original dataframe
#             with an additional column, pred, that contains the predicted
#             pupil diameter using the results from the qgam() function.
#   
#########################################################################

qgam_smoothing <- function(df, time_vec, knots = 25){
  qgam_trial = qgam(pd ~ s(time, k=knots, bs="cs"),
                    data = df,
                    qu = 0.5)
  p <- predict(qgam_trial, newdata = time_vec, 
               type = "link", se.fit = TRUE)# predicted values
  
  pred.df <- cbind(time_vec, "pred" = p$fit)
  
  df <- merge(df, pred.df, 
              by = "time", 
              all.y = T)
  
  df$ptid[is.na(df$ptid)] <- unique(df$ptid[!(is.na(df$ptid))])
  df$eye[is.na(df$eye)] <- unique(df$eye[!(is.na(df$eye))])
  df$mtm[is.na(df$mtm)] <- unique(df$mtm[!(is.na(df$mtm))])
  #df$trial_id[is.na(df$trial_id)] <- unique(df$trial_id[!(is.na(df$trial_id))])
  df$trial[is.na(df$trial)] <- unique(df$trial[!(is.na(df$trial))])
  
  return(df)
}
