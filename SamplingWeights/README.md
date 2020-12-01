# Sampling Weights

# Responsibles

* Ronan Le Gleut
* Turid Frahnow
* Yannik Schälte
* Mercè Garí

# Content

The sampling weights scripts consists of the following files:

* Contiguity_matrices_Constituencies.R: The script generates a contiguity matrices with different assumptions for the neighboring constituencies.
* Sampling_weights_study_pop.R: The script computes the sampling weights for each household and individual according to the sampling design.
* calibrated_weights_study_pop.R: The script computes the calibrated weights and calculate the variance associated to the seroprevalence estimates. These estimates are adjusted for specificity and sensitivity of the different tests.
* Sampling_weights_long.R: A version of Sampling_weights.R adapted to run the code for a time trend analysis based on four time intervals.
* calibrated_weights_long.R: A version of calibrated_weights.R adapted to run the code for a time trend analysis based on four time intervals. Uses the output of Sampling_weights_long.R, but will check if it exists or run the other script automatically.
* Sampling_weights_study_pop_long.R: A version of Sampling_weights_long.R adapted to run for the study population without ppl younger than 14 years.
* calibrated_weights_long.R: A version of calibrated_weights_long.R adapted to run for the study population without ppl younger than 14 years. Uses the output of Sampling_weights_studi_pop_long.R, but will check if it exists or run the other script automatically.

TBC...

Variations of the above scripts to test some ideas:

* Calibrated_weights_study_pop_long_wo_migration.R: A version of Calibrated_weights_study_pop_long ignoring the migration background (surrogate "country of birth") as margin. Uses Sampling_weights_study_pop_long.R as source.
* Study_pop_long_2tp.R: Stand alone script. Combination of Sampling_weights_study_pop_long.R and Calibrated_weights_study_pop_long.R reducing the number of time intervals (n=2 atm). The median is used as cutting point to balance the subsample size. 
* Study_pop_long_aggregated.R: Stand alone script. Combination of Sampling_weights_study_pop_long.R and Calibrated_weights_study_pop_long.R aggregating the time intervalls (t1_n = t1_o, t2_n = t1_n + t2_o, t3_n= t2_n + t3_o, t4_n= t3_n + t4_o).
* Study_pop_long_simple.R : Stand alone script. We "ignore" the first phase of the study design and decide that the household was selected randomly from all households in Munich.

# Requirements

Necessary data sets: 

* neighbourhoods.geojson (for generating contiguity matrices)
* Koco_baseline.csv (for Sampling_weights_study_pop.R Sampling_weights_long.R, Sampling_weights_study_pop_long.R)
* cutoffs.RData (for Sampling_weights_study_pop.R, Sampling_weights_long.R, Sampling_weights_study_pop_long.R)
* KoCo19_Haushalte4Modeler_wRRstartConstituency_20200910.csv (for Sampling_weights_study_pop.R, Calibrated_weights_study_pop.R, Sampling_weights_long.R, calibrated_weights_long.R, Sampling_weights_study_pop_long.R)
* muc_hh_structure_KoCo_bis.csv (for Sampling_weights_study_pop.R, Calibrated_weights_study_pop.R, Sampling_weights_long.R, calibrated_weights_long.R, Sampling_weights_study_pop_long.R, calibrated_weights_study_pop_long.R)
* contiguity_matrices-munich-constituencies_200914.RData (for Sampling_weights_study_pop.R, Sampling_weights_long.R, calibrated_weights_long.R, Sampling_weights_study_pop_long.R, calibrated_weights_study_pop_long.R)
* KoCo19_BaselineLAB.csv (for calibrated_weights_long.R)
* muc_age_structure_KoCo_bis.csv (for calibrated_weights_long.R)
* specificity_sensitivity_classifier.RData (for Calibrated_weights_study_pop.R calibrated_weights_long.R, calibrated_weights_study_pop_long.R)
* ind_lab_baseline_new.csv (for Sampling_weights_study_pop.R, Calibrated_weights_study_pop.R, Sampling_weights_study_pop_long.R, calibrated_weights_study_pop_long.R)
* muc_age_structure_KoCo_bis_study_pop.csv (for Sampling_weights_study_pop.R, Calibrated_weights_study_pop.R, calibrated_weights_study_pop_long.R)
* ind_characteristics_new.csv (for Calibrated_weights_study_pop.R)
TBC...

# Run

All scripts mentioned in the README are written with global paths.

# To do
* TBC...

# Results
Using the according scripts you get the following output.

* Sampling_weights_study_pop.R -> data frame "KoCo_BLab" containing with the original data on the participants and the sampling weights.
* Calibrated_weights_study_pop.R -> csv file "KoCo_weights" with the calibrated weights of each household/individual. csv file "All_estimates" containing the weighted/unweighted seroprevalence estimates adjusted or not for specificity and sensitivity. csv file "pcr_pos" with the estimated number of PCR positive cases in private households based on our survey.

* Sampling_weights_long.R -> list of data frames "KoCo_BLab_split" in workspace. Same output as Sampling_weights.R
* Calibrated_weights_long.R -> saves two output files (csv) for each time interval [x]. "Estimates_t[x].csv" saves the unweighted and weighted prevalence of Roche and Roche manufacturer ignoring sensitivity and specificity. "Estimates_adjusted_t[x].csv" saves the unweighted and weightes prevalence of Roche and Roche manufacturer also adjusting for sensitivity and specificity using the optimized cutoff.

* Sampling_weights_study_pop_long.R -> list of data frames "KoCo_BLab_split_study_pop" in workspace. Same output as Sampling_weights_long.R just with changed study population (ppl <14 not included)
* Calibrated_weights_study_pop_long.R -> saves two output files (csv) for each time interval [x]. "Estimates_study_pop_t[x].csv" saves the unweighted and weighted prevalence of Roche and Roche manufacturer ignoring sensitivity and specificity and ppl younger than 14 years in the study population. "Estimates_study_pop_adjusted_t[x].csv" saves the unweighted and weightes prevalence of Roche and Roche manufacturer also adjusting for sensitivity and specificity using the optimized cutoff ignoring ppl younger than 14 years in the study population.
