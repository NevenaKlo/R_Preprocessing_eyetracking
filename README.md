# R_Preprocessing_eyetracking
This repository contains the R script for data cleaning and preprocessing of a visual world eye-tracking experiment.

The raw file reports information about 60 participants and their fixations, saccades and blinks every 20 ms over a time frame of 2500 ms. 

The package eyetrackingR is used to clean the data and prepare it for an analysis of fixation patterns (linear and non-linear regression analyses, cluster-based permutation analysis). 

The script is used to:
- Filter unnecessary information (e.g., saccades, blinks, response time, pupil dimension);
- Filter low-qaulity data (e.g., participants and trials with a trackloss record over a defined threshold);
- Add new columns to group relevant information (e.g., participants groups, interest areas). 
