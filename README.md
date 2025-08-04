This repository contains the data and code used in the manuscript entitled " Testing the impact of prey presentation method on the feeding kinematics of terrestrial and aquatic Ambystomatidae." More precisely:

• ‘PPMI_landmarks_over_time_dlc_csv_files.zip’ contains the CSV files created by DeepLabCut with the 2D coordinates of the landmarks over time for each video sequence.

• ‘Aquatic_and_terrestrial_dataset_SCALES.ods’ contains the time and spatial scales for each aquatic and terrestrial video.

• ‘Toussaint-Larde_et_al_SCRIPT_TERRESTRIAL_FEEDING_data_extraction.R’ and ‘Toussaint-Larde_et_al_SCRIPT_SUCTION_data_extraction.R’  are respectively the R scripts used to facilitate the extraction of the terrestrial and aquatic kinematic variable measurements from the CSV files created by DeepLabCut (DLC).

• ‘Aquatic_dataset.csv’, ‘Terrestrial_dataset.csv’, contain respectively the extracted kinematic variables and other variable of interest of the aquatic feeding, of the terrestrial feeding.

• ‘Mixed_dataset.csv’, is the dataset gathering the terrestrial and aquatic values for the 25 common feeding kinematic variables

• ‘Aquatic_dataset_analysis.R’, ‘Terrestrial_dataset_analysis.R’, ‘Mixed_dataset_analysis.R’ are respectively the code used to perform the statistical analysis on ‘Aquatic_dataset.csv’, ‘Terrestrial_dataset.csv’, and ‘Mixed_dataset.csv’

All code is written in R (R Core Development Team, 2024). If you use these data, please cite Toussaint-Lardé et al., 2025.
