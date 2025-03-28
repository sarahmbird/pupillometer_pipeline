##########################################################################
#
#   Function: auc_calc
#   Purpose:  The purpose of this function is to take in the raw trajectories
#               from the pupillometer, after trials have been identified,
#               and to make them longer
#
#########################################################################
#
# Inputs:
#         - time: A vector that contains the sequence of times for the 
#             pupil trajectory
#         - pmc: A vector of values that indicate whether the point in the 
#             pupil trajectory is the PMC
#         - pred: A vector of values represented the smoothed pupil diameter
#             at each point in the pupil trajectory
#         - bl: A numeric value representing the baseline pupil diameter
#
#########################################################################
#
# Outputs:  A numeric value, the AUC of the pupil trajectory
#   
#########################################################################
auc_calc <- function(time, pmc, pred, bl){
  # Create the dataframe using these features
  dat <- data.frame(time, pmc, pred)
  
  # Subset the dataframe to only observations that occur after the PMC
  df_trunc <- dat[dat$time >= dat$time[dat$pmc == 1],  ]
  
  # Calculate the AUC using the desctools function
  auc <- AUC(x=df_trunc$time, y = df_trunc$pred-bl,
             method = "spline",
             absolutearea = T)
  return(auc)
}








