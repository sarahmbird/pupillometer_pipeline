##########################################################################
#
#   Function: lengthen_trajectories
#   Purpose:  The purpose of this function is to take in the raw trajectories
#               from the pupillometer, after trials have been identified,
#               and to make them longer
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
#         - pd: Numeric vector, variable identifying the pupil diameter size, (mm)
#         - plot: Boolean (T/F), indicates whether or not a plot should be printed to the 
#             console that plots all trajectories where a baseline jump was identified
#
#########################################################################
#
# Outputs:  A dataframe that contains all variables that were in the original
#               input dataframe. This dataframe also identifies which points in 
#               the pupil trajectory were identified as "baseline jumps" which are 
#               points, similar to a drop point, that are outside of the expected
#               range for the participant's pupil trajectory (which is defined as 
#               being >0.3mm from the median pupil diameter during the baseline
#               period).
#   
#########################################################################
identify_baseline_jumps <- function(ptids, trial, pupil, mtm, time, pd, plot = NULL) {
  
  clean <- data.frame(ptid = ptids, trial, pupil_measured = pupil,
                      mtm, time, pd)
  
  # Filter to right BEFORE the light is flashed
  filtered_data <- clean[clean$time <= 1.003,]
  # Find the mean, max, min, and median pupil diameter for each pupil trajectory right
  #     before the light stimulus is shown in each participant's eyes
  summarystats <- aggregate(pd ~ trial, data = filtered_data,
                            FUN = function(x) c(mean = mean(x, na.rm = TRUE),
                                                max = max(x, na.rm = TRUE),
                                                min = min(x, na.rm = TRUE),
                                                median = median(x, na.rm = TRUE)))
  # Convert the list columns into separate variables
  summarystats <- do.call(data.frame, summarystats)
  # Rename the variables in the summarystats column
  colnames(summarystats) <- c("trial", "mean_pd", "max_pd", "min_pd", "median_pd")
  # Create a variable called the extreme max, which looks at 
  summarystats$extreme_max <- (summarystats$max_pd - summarystats$median_pd) / summarystats$median_pd
  
  clean2 <- merge(x = clean, 
                        y = summarystats[, c("trial", "median_pd")], 
                        by = "trial",
                        all.x = TRUE)
  # Order by trial and time
  clean2 <- clean2[order(clean2$trial, clean2$time), ]
  # Create baseline indicator
  clean2$baseline <- ifelse(clean2$time <= 1.003, 1, 0)
  clean2$median_pctChg <- (clean2$pd - clean2$median_pd)/clean2$median_pd * clean2$baseline
  clean2$median_baseline_jump <- ifelse( abs(clean2$median_pctChg) > 0.30, 1, 0)
  # Order by mean_baseline_jump (descending) and mean_pctChg (descending)
  #clean2 <- clean2[order(-clean2$median_baseline_jump, -clean2$median_pctChg), ]
  
  if (plot == TRUE) {
    # Separate out those who had baseline jumps
    ids_baseJumps_median <- unique(clean2$trial[clean2$median_baseline_jump == 1])
    
    for(i in ids_baseJumps_median){
      df <- clean2[clean2$trial == i, ]
      print(ggplot(df, aes(x = time, y = pd,
                           group = trial)) +
              geom_point() +
              geom_point(data = df[df$median_baseline_jump == 1, ], 
                         aes(x = time, y = pd, group = trial), col = "blue")+
              facet_grid(rows = vars(pupil_measured), cols = vars(mtm))+
              labs(title = i) +
              theme_bw() +
              theme(legend.position = "bottom"))
    }
  }

  return(clean2)
}






