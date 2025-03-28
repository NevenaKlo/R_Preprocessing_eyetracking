# This script preprocesses eye-tracking data from a visual world experiment  
# on pronouns and anaphors in German
# Created by: Nevena Klobučar.
# Updated: 27.01.2025.

# ..............................................................................

# Load packages.

library(tidyverse)
library(dplyr)
library(Matrix)
library(eyetrackingR)

#update eyetrackingR package
remotes::install_github("ropensci/bib2df")

# set wd
setwd("C:/Users/klobu/Dropbox/DPBE_German3_JB2_deploy/myfiles")


# Import eye tracking data
eyedata <- read.table("dataviewerdata_interest_period.txt", 
                      sep = ",", 
                      header = TRUE, 
                      stringsAsFactors = TRUE, 
                      strip.white = TRUE, 
                      fill = TRUE, 
                      quote = "")


# ..............................................................................
# Reshape data ----
# ..............................................................................

#create 'group' column ('adults', 'children')
eyedata <- eyedata %>%
  mutate(group = ifelse(grepl("^A", RECORDING_SESSION_LABEL), "adults", 
                        ifelse(grepl("^B", RECORDING_SESSION_LABEL), "children", NA)))


#create AOI columns, which are 1 when the sample is a look to Target or Competitor
eyedata <- eyedata %>%
  mutate(
    Target = ifelse(AVERAGE_INTEREST_AREA_LABEL == "Target", 1, 0),
    Competitor = ifelse(AVERAGE_INTEREST_AREA_LABEL == "Competitor", 1, 0)
  )

#create children data
prefilter_eyedata_children <- eyedata %>%
  filter(group != "adults")

#create adult data
prefilter_eyedata_adults <- eyedata %>%
  filter(group != "children")

# Calculate the percentage of looks outside AOIs in children
children_missingLooks <- prefilter_eyedata_children %>%
  filter((is.na(Target) | Target == 0) & (is.na(Competitor) | Competitor == 0)) %>%
  summarise(percent = n() / nrow(eyedata) * 100) %>%
  pull(percent)

print(children_missingLooks)

# Calculate the percentage of looks outside AOIs in adults
adults_missingLooks <- prefilter_eyedata_adults %>%
  filter((is.na(Target) | Target == 0) & (is.na(Competitor) | Competitor == 0)) %>%
  summarise(percent = n() / nrow(eyedata) * 100) %>%
  pull(percent)

print(adults_missingLooks)

# ..............................................................................
# rename columns ----
# ..............................................................................

eyedata <- eyedata %>%
  rename(
    ParticipantName = RECORDING_SESSION_LABEL,
    Trial = TRIAL_INDEX,
    TimeFromTrialOnset = TIMESTAMP,
    Items = audiofile,
  )

# ..............................................................................
# Calculate trackloss ----
# ..............................................................................

# create trackloss column, which is 1 when the sample is a blink or saccade
eyedata <-  eyedata %>%
  mutate(TrackLoss= ifelse(AVERAGE_IN_BLINK == 1 | AVERAGE_IN_SACCADE == 1, 1, 0)) 


# Calculate trackloss percentage per participant per trial and group
Trackloss_trial_by_group <- eyedata %>%
  group_by(group, ParticipantName, Trial) %>%  # Include 'group' in the grouping
  summarize(trackloss_percentage = mean(TrackLoss) * 100, .groups = "drop")

#view average trackloss per group
Trackloss_summary <- Trackloss_trial_by_group %>%
  group_by(group) %>%
  summarise(
    mean = mean(trackloss_percentage),
    median = median(trackloss_percentage),
    sd = sd(trackloss_percentage),
    min = min(trackloss_percentage),
    max = max(trackloss_percentage),
    .groups = "drop"
  )

# View the summary
Trackloss_summary

# ..............................................................................
# Create data options for eyetrackingR ----
# ..............................................................................

eyedata <- make_eyetrackingr_data(eyedata, 
                               participant_column = "ParticipantName",
                               trial_column = "Trial",
                               time_column = "TimeFromTrialOnset",
                               trackloss_column = "TrackLoss",
                               item_column = "Items",
                               aoi_columns = c('Target','Competitor'),
                               treat_non_aoi_looks_as_missing = TRUE
)



# set critical word onset = 0
eyedata <- subset_by_window(eyedata, window_start_msg = "TIMER_CRIT_WORD",
                            msg_col = "SAMPLE_MESSAGE", rezero= TRUE, remove = FALSE)

# ..............................................................................
# Clean by trackloss ----
# ..............................................................................


# Filter data set to include only adults
eyedata_adults <- eyedata %>%
  filter(group == "adults")


# exclude participants and/or trials in the adult group with a trackloss higher than 60%
data_clean_adults <- clean_by_trackloss(eyedata_adults,
participant_prop_thresh = .60, 
trial_prop_thresh = .60,
window_start_time = 0, 
window_end_time = Inf
)

# Filter data set to include only children
eyedata_children <- eyedata  %>%
  filter(group == "children")

#exclude participants and/or trials in the children group with a trackloss higher than 70%
data_clean_children <- clean_by_trackloss(eyedata_children,
                                 participant_prop_thresh = .70, 
                                 trial_prop_thresh = .70,
                                 window_start_time = 0, 
                                 window_end_time = Inf
)

# Merge the two data sets (adults and children)
analysis <- bind_rows(data_clean_adults, data_clean_children)

# Keep only necessary columns
analysis <- analysis %>%
  select(
    ParticipantName,
    Trial,
    TimeFromTrialOnset,
    Items,
    condition,
    group,
    Target,
    Competitor
  )

# Rename columns
analysis <- analysis %>%
  rename(
    Participant = ParticipantName,
    Time = TimeFromTrialOnset
  )

analysis <- analysis %>%
  mutate(Items = sub("\\.wav$", "", Items))

# Export data 
write.table(analysis, 
            file = "analysis.txt", 
            sep = ",", 
            row.names = FALSE,
            quote = FALSE,
            fileEncoding = "UTF-8" 
            )



