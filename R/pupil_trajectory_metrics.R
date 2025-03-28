##########################################################################
#
#   Function: pupil_trajectory_metrics
#   Purpose:  The purpose of this function is to take in the raw trajectories
#               from the pupillometer, after trials have been identified,
#               and to make them longer
#
#########################################################################
#
# Inputs:
#         - dat: A dataframe that contains the wide raw pupillometer
#             trajectories that need to be converted to long format
#         - times: variable names of the 150 variables that contain the 
#             times associated with the pupil trajectories
#         - pd: variable names of the 150 variables that contain the 
#             pupil diameter values for each participant in the 
#             pupil trajectory
#         - bin: variable names of the 150 variables that identify 
#             whether or not there was eye obstruction at a specific
#             point in the pupil trajectory
#         - output: Numeric variable, can only take value of 1 or 2. A
#             value of 1 indicates that the smoothed pupil trajectory will be
#             output. A value of 2 indicates that the summary metrics will
#             be output.
#
#########################################################################
#
# Outputs:  The output will depend on the value provided by the 
#             'output' argument:
#       1) If the user provides a value of 1, then a dataset containing
#           the smoothed pupil trajectory will be output with the following
#           variables:
#             - ptid: A unique identifier for each participant
#             - eye: An identifier for the pupil measured (either Left or Right)
#             - mtm: An identifier for timepoint (0_pre, 1_post1, or 2_post2)
#             - time: The variable identifying the timepoint within the pupil
#                 trajectory
#             - trial: A unique identifier for the combination of ptid, eye, 
#                 timepoint (mtm), and # of trial (if there were repeated trials
#                 at a specific timepoint for a participant-eye combination, 
#                 potentially collected due to eye obstruction, etc.)
#             - pupil_sm_mm: The smoothed value of pupil diameter at the time 
#                 specified by the 'time' variable. 
#             - pupil_sm_pctChg: A variable that characterizes the percent change in
#                 pupillary constriction at the specific time specified by 'time'.
#       2) If the user provides a value of 2, then a dataset containing scalar 
#           pupillary metrics will be provided with the following columns:
#             - ptid      - eye       -mtm        -trial
#             - PMC
#             - bl.calc
#             - slope2PMC
#             - slope_rebound
#             - auc
#             - PMC_pctChg
#             - slope2PMC_pctChg
#             - slope_rebound_pctChg
#             - auc_pctChg
#             
#             
#   
#########################################################################

pupil_trajectory_metrics <- function(ptids, pupil, mtm, time, trial, pred,
                                     pd, pupilSize_bl, plot = NULL, output = 1) {
  # Create dataframe that we'll use through this function
  dat <- data.frame(ptid = ptids, eye = pupil, mtm, trial, time, pd, pred, pupilSize_bl)
  # Order by participant, eye, timepoint, and time
  dat <- dat[order(dat$ptid, dat$eye, dat$mtm, dat$time), ]
  # Extract the unique trials that we'll go through individually
  trial <- unique(dat$trial)
  # Create list to store the finalized pupil metrics in 
  pupil_marked <- list()
  for(i in trial){
    # Subset the dataframe to just look at the trial we're interested in 
    df <- dat[dat$trial == i, ]
    # Calculate the first derivative of the curve
    fda.deriv <- calculate.fda(signal = df$pred, 
                               time = df$time)
    # Convert the first and second derivatives into a dataframe and name them accordingly
    gold.df <- as.data.frame(cbind("time"=fda.deriv$dtime, fda.deriv$dsignal))
    names(gold.df) <- c("time", "signal", "d1", "d2")
    # Create a flag that indicates the time BEFORE the light stimulus is shown 
    gold.df$bl_flag <- ifelse(gold.df$time >= 0.75 & gold.df$time < 1.0, 1, 0)
    # Ready the dataframe for the calculation of the PMC
    gold.df$pmc <- NA
    # Examine where the direction of the first derivative switches and the sign of the second derivative
    #   to determine where the minimum of the pupillary response curve is
    for(j in 2:nrow(gold.df)){
      # gold.df$bl_flag[j] <- ifelse(gold.df$d1[j] > -0.05 & gold.df$d1[j+1]<0 & gold.df$time[j] < 1.5, 1, 0)
      gold.df$pmc[j] <- ifelse(gold.df$d1[j]<0 & gold.df$d1[j+1]>0 & gold.df$d2[j] > 0, 1, 0)
    }
    
    # Indicate which observations are part of the baseline
    bl.ind <- which(gold.df$bl_flag == 1)
    bl.val <- mean(gold.df$signal[ c(bl.ind-1, bl.ind, bl.ind+1)], na.rm = T)
    
    if (plot == T) {
      plot(gold.df$time, gold.df$signal, "l", main = i)
      points(df$time, df$pd, col = "orange")
      points(gold.df$time[gold.df$bl_flag == 1], gold.df$signal[gold.df$bl_flag == 1], col = "blue", pch = 19)
      points(gold.df$time[gold.df$pmc == 1], gold.df$signal[gold.df$pmc == 1], col = "red", pch = 19)
      lines(gold.df$time, rep(bl.val, length(gold.df$time)), col = "blue", pch = 19)
      abline(v = 1)
    }
    # Ready the dataframe for the calculation of the baseline value
    bl.val <-  NULL
    # Merge the newly calculated values to the dataframe
    df <- merge(df, gold.df[, c("time", "bl_flag", "pmc")], 
                by = "time", 
                all.x = T)
    # Save the dataframe in the list object for trial i
    pupil_marked[[i]] <- df
    # Remove the dataframe
    rm(df)
  }

  # Merge the list objects together into one dataset
  pupil_marked <- do.call("rbind", pupil_marked)
  
  # Aggregate all pmcs together and subset to the set of times that are past 1 second (when the light was flashed),
  #   and less than second 3.5
  pmcs.id <- as.data.frame(aggregate(pupil_marked$pmc[pupil_marked$time > 1 & pupil_marked$time < 3.5], 
                                     by = list(pupil_marked$trial[pupil_marked$time > 1 & 
                                                                  pupil_marked$time < 3.5]), 
                                     FUN = sum))
  names(pmcs.id) <- c("trial", "numMarkedPts")

  # Identify observations who have more than one point of minimal constriction
  two.pmc <- pmcs.id$trial[pmcs.id$numMarkedPts > 1]
  
  if (plot == T) {
    for(i in two.pmc){
      df <- pupil_marked[pupil_marked$trial == i, ]
      
      print(ggplot(df, aes(x = time, y = pred))+
              geom_line()+
              geom_point(data = df[df$pmc == 1, ], col = "red")+
              labs(title = i)+
              theme_bw()
      )
    } 
  }

  # Remove potential minima before the light actually flashed
  pupil_marked$pmc[(pupil_marked$pmc == 1 & pupil_marked$time < 1)] <- 0
  # Remove any potential minima after the 3.5 second window has passed
  pupil_marked$pmc[(pupil_marked$pmc == 1 & pupil_marked$time > 3.5)] <- 0
  # Replace NAs (the first observation) with a zero
  pupil_marked$pmc[is.na(pupil_marked$pmc)] <- 0
  
  ## Select marked value of PMC for trials where only 1 PMC point met the time criteria
  one.pmc.dfs <- pupil_marked[!(pupil_marked$trial %in% two.pmc), ]
  
  ## Select the PMC point with the minimum pupil size as the "valid" PMC.
  two.pmc.dfs <- pupil_marked[pupil_marked$trial %in% two.pmc,]
  
  # If the dataframe for observations with two pmcs is empty, we implement more logic
  if (nrow(two.pmc.dfs) != 0) {
    # Identify observations with two pmc values, if any exist
    two.pmc.vals <- as.data.frame(aggregate(two.pmc.dfs$pred[two.pmc.dfs$pmc == 1],
                                            by = list(two.pmc.dfs$trial[two.pmc.dfs$pmc == 1]), 
                                            FUN = function(x) min(x, na.rm = T)))
    names(two.pmc.vals) <- c("trial", "min_PMCval")
    
    ## Select the PMC point with the minimum pupil size as the "valid" PMC.
    two.pmc.dfs <- pupil_marked[pupil_marked$trial %in% two.pmc,]
    # Identify observations with two pmc values
    two.pmc.vals <- as.data.frame(aggregate(two.pmc.dfs$pred[two.pmc.dfs$pmc == 1],
                                            by = list(two.pmc.dfs$trial[two.pmc.dfs$pmc == 1]), 
                                            FUN = function(x) min(x, na.rm = T)))
    names(two.pmc.vals) <- c("trial", "min_PMCval")
    # Merge the smallest pmc value to the original dataset
    two.pmc.dfs <- merge(two.pmc.dfs, two.pmc.vals, 
                         by = "trial", 
                         all.x = T)
    # Ensure that the smallest PMC value is retained as the pmc
    two.pmc.dfs$pmc <- ifelse(two.pmc.dfs$pred == two.pmc.dfs$min_PMCval & two.pmc.dfs$pmc == 1, 
                              1,  0)
    # Concatenate the datasets together
    two.pmc.dfs$min_PMCval <- NULL
    pupil_marked <- rbind(one.pmc.dfs, two.pmc.dfs)
  } 
  
  # Calculate the baseline pupil diameter size
  bl.calc <- as.data.frame(aggregate(pupil_marked$pred[pupil_marked$bl_flag == 1], 
                                     by = list(pupil_marked$trial[pupil_marked$bl_flag == 1]), 
                                     FUN = function(x) mean(x, na.rm = T)))
  names(bl.calc) <- c("trial", "bl.calc")
  PMC.df <- pupil_marked[pupil_marked$pmc == 1, c("ptid", "eye", "mtm", "trial", "time", "pred", "pupilSize_bl")]
  names(PMC.df)[names(PMC.df) == "pred"] <- "PMC"
  names(PMC.df)[names(PMC.df) == "time"] <- "PMC_time"
  
  # Extract the last pupil diameter measured and remove the time associated with the entry (unneeded)
  #     the last time data was collected with these pupil trajectories was at 4.98 seconds
  last.pupilSize <- pupil_marked[pupil_marked$time == 4.980, c("ptid", "eye", "mtm", "trial", "time", "pred")]
  names(last.pupilSize)[names(last.pupilSize) == "pred"] <- "lastPupilSize"
  last.pupilSize$time <- NULL

  # Combine the datasets together to obtain the pupillometer summary statistics
  Summ_vals <- merge(PMC.df, bl.calc, 
                     by = "trial")
  Summ_vals <- merge(Summ_vals, last.pupilSize[, c("trial", "lastPupilSize")], 
                     by = "trial")
  Summ_vals$slope2PMC <- (Summ_vals$bl.calc - Summ_vals$PMC)/(1 - Summ_vals$PMC_time)
  Summ_vals$slope_rebound <- (Summ_vals$PMC - Summ_vals$lastPupilSize)/(Summ_vals$PMC_time - 4.980)

  # Merge the pupil marked trajectories with the baseline calculation
  pupil_marked <- merge(pupil_marked, Summ_vals[, c("trial", "bl.calc")], 
                        by = "trial", 
                        all.x = T)
  # Calculate the percent change in pupil diameter
  pupil_marked$pupil_pctChg <- round((pupil_marked$pred - pupil_marked$bl.calc)/pupil_marked$bl.calc*100, 3)

  # Create a bunch of blank summary values for each variable
  Summ_vals$auc <- NA
  Summ_vals$auc_pctChg <- NA
  Summ_vals$PMC_pctChg <- NA
  Summ_vals$lastPupilSize_pctChg <- NA
  # Create a vector of unique trial IDs
  trial_ids <- unique(pupil_marked$trial)
  
  for(id in trial_ids){
    Summ_vals$auc[Summ_vals$trial == id] <- auc_calc(time = pupil_marked$time[pupil_marked$trial == id], 
                                                        pmc = pupil_marked$pmc[pupil_marked$trial == id],
                                                        pred = pupil_marked$pred[pupil_marked$trial == id],
                                                        bl = Summ_vals$bl.calc[Summ_vals$trial == id])
    
    Summ_vals$auc_pctChg[Summ_vals$trial == id] <- AUC(pupil_marked$time[pupil_marked$trial == id], 
                                                          pupil_marked$pupil_pctChg[pupil_marked$trial == id], 
                                                          method = "spline", 
                                                          absolutearea = T)
    pmc <- NA
    pmc <- Summ_vals$PMC[Summ_vals$trial == id]
    Summ_vals$PMC_pctChg[Summ_vals$trial == id] <- pupil_marked$pupil_pctChg[pupil_marked$pred == pmc & 
                                                                                  pupil_marked$trial == id]
    
    Summ_vals$lastPupilSize_pctChg[Summ_vals$trial == id] <- 
      pupil_marked$pupil_pctChg[pupil_marked$time == 4.980 & pupil_marked$trial == id]
  }
  Summ_vals$slope2PMC_pctChg <- (0-Summ_vals$PMC_pctChg)/(1 - Summ_vals$PMC_time)
  Summ_vals$slope_rebound_pctChg <- (Summ_vals$PMC_pctChg - Summ_vals$lastPupilSize_pctChg)/(Summ_vals$PMC_time - 4.980)
  
  ### Cleaning data file for saving
  pupil_marked <- pupil_marked[ , c("ptid", "trial", "eye",  "mtm", "time", "pd", "pred", 
                                    "pmc", "pupil_pctChg")]
  names(pupil_marked)[names(pupil_marked) == "pred"] <- "pd_sm"
  names(pupil_marked)[names(pupil_marked) == "pd"] <- "pd_og"
  
  #####################################################################################
  #
  # Calculate Latency & Max constriction velocity
  #
  #####################################################################################
  
  # Obtain the unique trial information
  trials <- unique(pupil_marked$trial)
  # Set up the data frame
  lat_velo <- data.frame("trial" = character(), 
                         "max_constrict_velo" = numeric(),
                         "latency" = numeric())
  
  for(i in trials){
    df <- pupil_marked[pupil_marked$trial ==  i, ] # Subset to the current trial
    df <- df[df$time >= 0.99 & df$time < df$time[df$pmc == 1] & !(is.na(df$pd_og)), ] 
    df$constrict_velo <- round(c(diff(df$pd_sm), 0)/c(diff(df$time), 1), 2)
    
    
    res_df <- data.frame("trial" = i,
                         "max_constrict_velo" = min(df$constrict_velo),
                         "latency" = df$time[which.max(df$constrict_velo < -0.8)] - 1)
    lat_velo <- rbind(lat_velo, res_df)
  }
  
  Summ_vals <- merge(Summ_vals, lat_velo, 
                     by.x = "trial", 
                     by.y = "trial")
  
  # 
  if (output == 1) {
    pupillometer_df <- pupil_marked[pupil_marked$time > 0.99, 
                                       c("ptid", "eye", "mtm", "trial", "time", "pd_sm", "pupil_pctChg")]
    names(pupillometer_df)[names(pupillometer_df) == "pd_sm"] <- "pupil_sm_mm"
    names(pupillometer_df)[names(pupillometer_df) == "pupil_pctChg"] <- "pupil_sm_pctChg"
    pupillometer_df$time <- pupillometer_df$time - 1
    return(smoothed_trajectories = pupillometer_df)
  } else if (output == 2) {
    pupillometer_summ <- Summ_vals[, c("ptid", "eye", "mtm", "trial", "PMC", "bl.calc", 
                                             "slope2PMC", "slope_rebound", "auc", 
                                             "PMC_pctChg", "slope2PMC_pctChg", 
                                             "slope_rebound_pctChg", "auc_pctChg")]
    return(metrics = pupillometer_summ)
  }
  
  return(list(pupil_marked = pupil_marked,
              summary_values = Summ_vals))
}




# 
# pupil_trajectory_metrics <- function(ptids, pupil, mtm, time, trial_id, pred,
#                                      pd, pupilSize_bl, plot = NULL) {
#   # Create dataframe that we'll use through this function
#   dat <- data.frame(ptid = ptids, eye = pupil, mtm, trial_id, time, pd, pred, pupilSize_bl)
#   # Order by participant, eye, timepoint, and time
#   dat <- dat[order(dat$ptid, dat$eye, dat$mtm, dat$time), ]
#   # Extract the unique trials that we'll go through individually
#   trial <- unique(dat$trial_id)
#   # Create list to store the finalized pupil metrics in 
#   pupil_marked <- list()
#   for(i in trial){
#     # Subset the dataframe to just look at the trial we're interested in 
#     df <- dat[dat$trial_id == i, ]
#     # Calculate the first derivative of the curve
#     fda.deriv <- calculate.fda(signal = df$pred, 
#                                time = df$time)
#     # Convert the first and second derivatives into a dataframe and name them accordingly
#     gold.df <- as.data.frame(cbind("time"=fda.deriv$dtime, fda.deriv$dsignal))
#     names(gold.df) <- c("time", "signal", "d1", "d2")
#     # Create a flag that indicates the time BEFORE the light stimulus is shown 
#     gold.df$bl_flag <- ifelse(gold.df$time >= 0.75 & gold.df$time < 1.0, 1, 0)
#     # Ready the dataframe for the calculation of the PMC
#     gold.df$pmc <- NA
#     # Examine where the direction of the first derivative switches and the sign of the second derivative
#     #   to determine where the minimum of the pupillary response curve is
#     for(j in 2:nrow(gold.df)){
#       # gold.df$bl_flag[j] <- ifelse(gold.df$d1[j] > -0.05 & gold.df$d1[j+1]<0 & gold.df$time[j] < 1.5, 1, 0)
#       gold.df$pmc[j] <- ifelse(gold.df$d1[j]<0 & gold.df$d1[j+1]>0 & gold.df$d2[j] > 0, 1, 0)
#     }
#     # Indicate which observations are part of the baseline
#     bl.ind <- which(gold.df$bl_flag == 1)
#     bl.val <- mean(gold.df$signal[ c(bl.ind-1, bl.ind, bl.ind+1)], na.rm = T)
# 
#     if (plot == T) {
#       plot(gold.df$time, gold.df$signal, "l", main = i)
#       points(df$time, df$pd, col = "orange")
#       points(gold.df$time[gold.df$bl_flag == 1], gold.df$signal[gold.df$bl_flag == 1], col = "blue", pch = 19)
#       points(gold.df$time[gold.df$pmc == 1], gold.df$signal[gold.df$pmc == 1], col = "red", pch = 19)
#       lines(gold.df$time, rep(bl.val, length(gold.df$time)), col = "blue", pch = 19)
#       abline(v = 1)
#     }
#     # Ready the dataframe for the calculation of the baseline value
#     bl.val <-  NULL
#     # Merge the newly calculated values to the dataframe
#     df <- merge(df, gold.df[, c("time", "bl_flag", "pmc")], 
#                 by = "time", 
#                 all.x = T)
#     # Save the dataframe in the list object for trial i
#     pupil_marked[[i]] <- df
#     # Remove the dataframe
#     rm(df)
#   }
#   # Merge the list objects together into one dataset
#   pupil_marked <- do.call("rbind", pupil_marked)
# 
#   # Aggregate all pmcs together and subset to the set of times that are past 1 second (when the light was flashed),
#   #   and less than second 3.5
#   pmcs.id <- as.data.frame(aggregate(pupil_marked$pmc[pupil_marked$time > 1 & pupil_marked$time < 3.5], 
#                                      by = list(pupil_marked$trial_id[pupil_marked$time > 1 & 
#                                                                        pupil_marked$time < 3.5]), 
#                                      FUN = sum))
#   names(pmcs.id) <- c("trial_id", "numMarkedPts")
#   
#   # Identify observations who have more than one point of minimal constriction
#   two.pmc <- pmcs.id$trial_id[pmcs.id$numMarkedPts > 1]
#   
#   if (plot == T) {
#     for(i in two.pmc){
#       df <- pupil_marked[pupil_marked$trial_id == i, ]
#       
#       print(ggplot(df, aes(x = time, y = pred))+
#               geom_line()+
#               geom_point(data = df[df$pmc == 1, ], col = "red")+
#               labs(title = i)+
#               theme_bw()
#       )
#     } 
#   }
#   
#   # Remove potential minima before the light actually flashed
#   pupil_marked$pmc[(pupil_marked$pmc == 1 & pupil_marked$time < 1)] <- 0
#   # Remove any potential minima after the 3.5 second window has passed
#   pupil_marked$pmc[(pupil_marked$pmc == 1 & pupil_marked$time > 3.5)] <- 0
#   # Replace NAs (the first observation) with a zero
#   pupil_marked$pmc[is.na(pupil_marked$pmc)] <- 0
#   
#   ## Select marked value of PMC for trials where only 1 PMC point met the time criteria
#   one.pmc.dfs <- pupil_marked[!(pupil_marked$trial_id %in% two.pmc), ]
#   
#   ## Select the PMC point with the minimum pupil size as the "valid" PMC.
#   two.pmc.dfs <- pupil_marked[pupil_marked$trial_id %in% two.pmc,]
#   
#   # Identify observations with two pmc values
#   two.pmc.vals <- as.data.frame(aggregate(two.pmc.dfs$pred[two.pmc.dfs$pmc == 1],
#                                           by = list(two.pmc.dfs$trial_id[two.pmc.dfs$pmc == 1]), 
#                                           FUN = function(x) min(x, na.rm = T)))
#   names(two.pmc.vals) <- c("trial_id", "min_PMCval")
#   
#   ## Select the PMC point with the minimum pupil size as the "valid" PMC.
#   two.pmc.dfs <- pupil_marked[pupil_marked$trial_id %in% two.pmc,]
#   # Identify observations with two pmc values
#   two.pmc.vals <- as.data.frame(aggregate(two.pmc.dfs$pred[two.pmc.dfs$pmc == 1],
#                                           by = list(two.pmc.dfs$trial_id[two.pmc.dfs$pmc == 1]), 
#                                           FUN = function(x) min(x, na.rm = T)))
#   names(two.pmc.vals) <- c("trial_id", "min_PMCval")
#   # Merge the smallest pmc value to the original dataset
#   two.pmc.dfs <- merge(two.pmc.dfs, two.pmc.vals, 
#                        by = "trial_id", 
#                        all.x = T)
#   # Ensure that the smallest PMC value is retained as the pmc
#   two.pmc.dfs$pmc <- ifelse(two.pmc.dfs$pred == two.pmc.dfs$min_PMCval & two.pmc.dfs$pmc == 1, 
#                             1,  0)
#   # Concatenate the datasets together
#   two.pmc.dfs$min_PMCval <- NULL
#   pupil_marked <- rbind(one.pmc.dfs, two.pmc.dfs)
#   
#   # Calculate the baseline pupil diameter size
#   bl.calc <- as.data.frame(aggregate(pupil_marked$pred[pupil_marked$bl_flag == 1], 
#                                      by = list(pupil_marked$trial_id[pupil_marked$bl_flag == 1]), 
#                                      FUN = function(x) mean(x, na.rm = T)))
#   names(bl.calc) <- c("trial_id", "bl.calc")
#   PMC.df <- pupil_marked[pupil_marked$pmc == 1, c("ptid", "eye", "mtm", "trial_id", "time", "pred", "pupilSize_bl")]
#   names(PMC.df)[names(PMC.df) == "pred"] <- "PMC"
#   names(PMC.df)[names(PMC.df) == "time"] <- "PMC_time"
#   
#   # Extract the last pupil diameter measured and remove the time associated with the entry (unneeded)
#   #     the last time data was collected with these pupil trajectories was at 4.98 seconds
#   last.pupilSize <- pupil_marked[pupil_marked$time == 4.980, c("ptid", "eye", "mtm", "trial_id", "time", "pred")]
#   names(last.pupilSize)[names(last.pupilSize) == "pred"] <- "lastPupilSize"
#   last.pupilSize$time <- NULL
# 
#   # Combine the datasets together to obtain the pupillometer summary statistics
#   Summ_vals <- merge(PMC.df, bl.calc, 
#                      by = "trial_id")
#   Summ_vals <- merge(Summ_vals, last.pupilSize[, c("trial_id", "lastPupilSize")], 
#                      by = "trial_id")
#   Summ_vals$slope2PMC <- (Summ_vals$bl.calc - Summ_vals$PMC)/(1 - Summ_vals$PMC_time)
#   Summ_vals$slope_rebound <- (Summ_vals$PMC - Summ_vals$lastPupilSize)/(Summ_vals$PMC_time - 4.980)
#   
#   # Merge the pupil marked trajectories with the baseline calculation
#   pupil_marked <- merge(pupil_marked, Summ_vals[, c("trial_id", "bl.calc")], 
#                         by = "trial_id", 
#                         all.x = T)
#   # Calculate the percent change in pupil diameter
#   pupil_marked$pupil_pctChg <- round((pupil_marked$pred - pupil_marked$bl.calc)/pupil_marked$bl.calc*100, 3)
#   
#   # Create a bunch of blank summary values for each variable
#   Summ_vals$auc <- NA
#   Summ_vals$auc_pctChg <- NA
#   Summ_vals$PMC_pctChg <- NA
#   Summ_vals$lastPupilSize_pctChg <- NA
#   
#   # Create a vector of unique trial IDs
#   trial_ids <- unique(pupil_marked$trial_id)
#   
#   for(id in trial_ids){
#     Summ_vals$auc[Summ_vals$trial_id == id] <- auc_calc(time = pupil_marked$time[pupil_marked$trial_id == id, ], 
#                                                         pmc = pupil_marked$pmc[pupil_marked$trial_id == id, ],
#                                                         pred = pupil_marked$pred[pupil_marked$trial_id == id, ],
#                                                         bl = Summ_vals$bl.calc[Summ_vals$trial_id == id])
#     
#     Summ_vals$auc_pctChg[Summ_vals$trial_id == id] <- AUC(pupil_marked$time[pupil_marked$trial_id == id], 
#                                                           pupil_marked$pupil_pctChg[pupil_marked$trial_id == id], 
#                                                           method = "spline", 
#                                                           absolutearea = T)
#     pmc <- NA
#     pmc <- Summ_vals$PMC[Summ_vals$trial_id == id]
#     Summ_vals$PMC_pctChg[Summ_vals$trial_id == id] <- pupil_marked$pupil_pctChg[pupil_marked$pred == pmc & 
#                                                                                   pupil_marked$trial_id == id]
#     
#     Summ_vals$lastPupilSize_pctChg[Summ_vals$trial_id == id] <- 
#       pupil_marked$pupil_pctChg[pupil_marked$time == 4.980 & pupil_marked$trial_id == id]
#   }
#   Summ_vals$slope2PMC_pctChg <- (0-Summ_vals$PMC_pctChg)/(1 - Summ_vals$PMC_time)
#   Summ_vals$slope_rebound_pctChg <- (Summ_vals$PMC_pctChg - Summ_vals$lastPupilSize_pctChg)/(Summ_vals$PMC_time - 4.980)
#   
#   ### Cleaning data file for saving
#   pupil_marked <- pupil_marked[ , c("ptid", "trial_id", "eye",  "mtm", "time", "pd", "pred", 
#                                     "pmc", "pupil_pctChg")]
#   names(pupil_marked)[names(pupil_marked) == "pred"] <- "pd_sm"
#   names(pupil_marked)[names(pupil_marked) == "pd"] <- "pd_og"
#   
#   #####################################################################################
#   #
#   # Calculate Latency & Max constriction velocity
#   #
#   #####################################################################################
#   
#   # Obtain the unique trial information
#   trials <- unique(pupil_marked$trial_id)
#   # Set up the data frame
#   lat_velo <- data.frame("trial" = character(), 
#                          "max_constrict_velo" = numeric(),
#                          "latency" = numeric())
#   
#   for(i in trials){
#     df <- pupil_marked[pupil_marked$trial_id ==  i, ] # Subset to the current trial
#     df <- df[df$time >= 0.99 & df$time < df$time[df$pmc == 1] & !(is.na(df$pd_og)), ] 
#     df$constrict_velo <- round(c(diff(df$pd_sm), 0)/c(diff(df$time), 1), 2)
#     
#     
#     res_df <- data.frame("trial" = i,
#                          "max_constrict_velo" = min(df$constrict_velo),
#                          "latency" = df$time[which.max(df$constrict_velo < -0.8)] - 1)
#     lat_velo <- rbind(lat_velo, res_df)
#   }
#   
#   Summ_vals <- merge(Summ_vals, lat_velo, 
#                      by.x = "trial_id", 
#                      by.y = "trial")
#   
#   return(list(pupil_marked = pupil_marked,
#               summary_values = Summ_vals))
# }












