##########################################################################
#
#   Function: plot_pupil_trajectories
#   Purpose:  This function takes raw and smoothed pupil trajectories and 
#     plots pupil trajectories for two scenarios:
#       1) All trials for each person, timepoint, and pupil measured
#       2) The raw pupil trajectory
#
#########################################################################
#
# Inputs:
#         - dat: This parameter accepts two types of inputs:
#             1) The second list object returned from the trajectory_cleaning()
#                 function, which is returned when graphingDset is set to TRUE.
#             2) The cleaned pupil trajectories returned from the trajectory_cleaning()
#                 function
#         - subset: OPTIONAL character/numeric that identifies a selection of participants
#             from the dataset whose trajectories will be plotted.
#         - evaluateBestTrials: Boolean (T/F), Indicates if the user would like to 
#             plot all trials for each combination of participant, timepoint, and
#             pupil measured. Note: there will, potentially, be multiple pupil
#             trajectories on one graph, color coded to represent which trials
#             were deemed to be the "best" ones according to the metrics calculated
#             by the algorithm.
#
#########################################################################
#
# Outputs:  The function will return images of the smoothed (or raw) pupil
#     trajectories for either 1) all trials for each combination of participant
#     timepoint, and pupil measured or 2) the raw/smoothed pupil trajecory.
#   
#########################################################################
plot_pupil_trajectories <- function(dat,
                                    subset = NULL,
                                    evaluateBestTrials = NULL) {

  # If the user is only interested in a subset of the trajectories, then this
  #   if statement will subset the dataframe to only include the desired participants
  if (length(subset) != 0) {
    dat <- dat[dat$ptid == subset,]
  }
  
  if (evaluateBestTrials) {

    ptids <- unique(dat$ptid)
    #[1:27]
    for(i in ptids){
      df <- dat[dat$ptid == i, ]
      df$bestTrial <- factor(df$bestTrial, levels = c(0, 1), labels = c("Not Best Trial", "Best Trial"))
      print(ggplot(df,
                   aes(x = time, y = pd, group = trial, color = as.factor(bestTrial)))+
              geom_line(data = df[df$dropPt == 0,],
                        aes(x = time, y = pd, group = trial, col = as.factor(bestTrial)))+
              geom_point(data = df[df$dropPt == 1, ],
                         aes(x = time, y = pd, group = trial, color = as.factor(bestTrial)),
                         size = 2)+
              facet_grid(rows = vars(pupil_measured), cols = vars(mtm))+
              labs(title = i)+
              theme_bw() +
              theme(legend.position = "bottom") +
              scale_color_discrete(name = "Identified as best trial"))
    }
  } else {
    # Print pupil trajectories for all observations
    ptids <- unique(dat$ptid)
    for(i in ptids){
      df <- dat[dat$ptid == i, ]
      print(ggplot(df, aes(x = time, y = pd,
                           group = trial, color = as.factor(bestTrial)))+
              geom_point()+
              facet_grid(rows = vars(pupil_measured), cols = vars(mtm) )+
              labs(title = i)+
              theme_bw() +
              theme(legend.position = "bottom"))
    }
  }
}


