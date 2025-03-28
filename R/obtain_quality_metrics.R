##########################################################################
#
#   Function: qual_lagDiff
#   Purpose:  The purpose of this function is to assemble a dataframe
#     of quality metrics for which we can assess each trial. These
#     metrics will be the basis for which we select the final observation.
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
#         - time: Numeric vector, variable that identifies the time in the pupil trajectory
#             that the observation was recorded at
#         - dropPt: Numeric vector, indicator variable that identifies if the specific point 
#             in the pupil trajectory is a "drop" point (i.e. a point that is significantly
#             larger or smaller than the adjacent measurements).
#         - lagDiff: Numeric vector, variable that contains the lagged difference between the previous
#             point and the current point. This value is calculated and returned in the 
#             "identify_spikes" function.
#
#########################################################################
#
# Outputs:  A dataframe that contains one row per participant trial. This 
#     dataframe contains each trial that occured along with summary statistics
#     on the best trial. The best trial for each participant
#     trial is identified by the 'bestTrial' variable, which takes on the value
#     of 1 if the trial was classified as the best, and a value of 0 if the trial
#     was not classified as the best.
#   
#########################################################################
obtain_quality_metrics <- function(ids, trial, mtm, pupil, dropPt, 
                                   lagDiff, time) {
  qual_lagDiff <- function(df){
    # spikes <- sum((abs(df$lagDiff) > 0.18) & (abs(df$leadDiff) > 0.18))
    spikes <- sum(df$dropPt == 1)
    df <- df[df$dropPt == 0, ]
    df$lagdiff <- c(0, diff(df$time, lag = 1))
    pctTestTimeDiff <- round((5-df$time[nrow(df)])/5, 4)
    sumDiff <- round(sum(df$lagdiff[df$lagdiff > 0.034], na.rm = T), 2)

    res <- c(unique(df$ptid), unique(df$pupil_measured),
             unique(df$mtm),
             unique(df$trial), spikes, pctTestTimeDiff, sumDiff)

    return(res)
  }
  # Combine all variables into one dataframe
  dat <- data.frame(ptid = ids, trial, mtm, pupil_measured = pupil, dropPt, lagDiff, time)
  # Obtain unique trials that are in the dataset
  testTrials <- unique(dat$trial)
  # Apply the quality assessment to each trial in the raw dataset
  qualMetrics <- lapply(testTrials, 
                        FUN = function(x) qual_lagDiff(dat[dat$trial  == x,]))

  # Combine all quality metrics together and supply them with names
  qualMetrics <- as.data.frame(do.call("rbind", qualMetrics))
  names(qualMetrics) <- c("ptid", "pupil_measured", "mtm", "trial", "spikes", 
                          "pctTestTimeDiff", "SumLagDiff")
  # Convert all created variables to numeric quantities
  qualMetrics$spikes <- as.numeric(qualMetrics$spikes)
  qualMetrics$pctTestTimeDiff <- as.numeric(qualMetrics$pctTestTimeDiff)
  # qualMetrics$TestTimeDiff_gt0.1 <- ifelse(qualMetrics$pctTestTimeDiff > 0.2, 1, 0)
  qualMetrics$SumLagDiff <- as.numeric(qualMetrics$SumLagDiff)
  # We'll assign a "ranking" based on
  #   1) The total number of spikes in the trajectory
  #   2) Total seconds of time missing in trajectory
  #   3) An indication of if the trial ended early or not
  qualMetrics$qualAssess <- ifelse(qualMetrics$spikes <= 2, 0, qualMetrics$spikes) +
    ifelse(qualMetrics$pctTestTimeDiff > 0.2, 1, 0)+ 
    ifelse(qualMetrics$SumLagDiff < 0.5, 0, qualMetrics$SumLagDiff)
  # Aggregate the quality metrics and extract the trial, for each participant
  #   pupil, and timepoint combination, that has the lowest score. This indicates
  #   that trial was the "best". 
  qual_agg <- as.data.frame(aggregate(qualMetrics$qualAssess,
                                      by = list(qualMetrics$ptid,
                                                qualMetrics$pupil_measured,
                                                qualMetrics$mtm),
                                      FUN = min))
  # Supply column names
  names(qual_agg) <- c("ptid", 
                       "pupil_measured", 
                       "mtm", 
                       "minQual")
  # Merge the quality metrics along with the aggregated rankings
  qualMetrics <- merge(qualMetrics, qual_agg, 
                       by = c("ptid", 
                              "pupil_measured", 
                              "mtm"), 
                       all.x = T)
  # Create a vector that stores all duplicated variables
  dup.vars <- c("ptid", "pupil_measured", "mtm", "qualAssess", "minQual")
  # Remove all duplicated variables
  qualMetrics$dup <- duplicated(qualMetrics[, dup.vars])
  # Identify the best trial
  qualMetrics$bestTrial <- ifelse(qualMetrics$qualAssess == qualMetrics$minQual & qualMetrics$dup == FALSE,
                                  1, 0)
  return(qualMetrics)
}


