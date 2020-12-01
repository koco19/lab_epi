# Sampling Weights

# Responsibles

* Ronan Le Gleut
* Turid Frahnow
* Yannik Schälte
* Mercè Garí

# Content

The scripts consist of the following files:

* Contiguity_matrices_Constituencies_200914.R: The script generates a contiguity matrices with different assumptions for the neighboring constituencies.
* Stimmbezirkssampling_COVID_MonteCarloEstimate.R: The script runs a Monte Carlo simulation to estimate the first order inclusion probabilities of the constituencies.
* Sampling_weights_study_pop.R: The script computes the sampling weights for each household and individual according to the sampling design.
* calibrated_weights_study_pop.R: The script computes the calibrated weights and calculate the variance associated to the seroprevalence estimates. These estimates are adjusted for specificity and sensitivity of the different tests. It also computes the estimated number of registered cases (PCR positive) in private households in Munich based on our survey.


# Requirements

Necessary data sets: 

* neighbourhoods.geojson (for Contiguity_matrices_Constituencies_200914.R) - NOT AVAILABLE
* Kopie von Wahlbezirke_2020_Strukturindikatoren_mstatistik.csv (for Stimmbezirkssampling_COVID_MonteCarloEstimate.R) - NOT AVAILABLE
* contiguity_matrices-munich-constituencies_200914.RData (for Sampling_weights_study_pop.R)
* Koco_baseline.csv (for Sampling_weights_study_pop.R, Calibrated_weights_study_pop.R)
* cutoffs.RData (for Sampling_weights_study_pop.R)
* KoCo19_Haushalte4Modeler_wRRstartConstituency_20200910.csv (for Sampling_weights_study_pop.R, Calibrated_weights_study_pop.R)
* muc_hh_structure_KoCo_bis.csv (for Sampling_weights_study_pop.R, Calibrated_weights_study_pop.R)
* muc_age_structure_KoCo_bis_study_pop.csv (for Calibrated_weights_study_pop.R)
* specificity_sensitivity_classifier.RData (for Calibrated_weights_study_pop.R)
* ind_lab_baseline_new.csv (for Sampling_weights_study_pop.R, Calibrated_weights_study_pop.R)
* ind_characteristics_new.csv (for Calibrated_weights_study_pop.R)


# Run

All scripts mentioned in the README are written with global paths.


# Results
Using the according scripts you get the following output.

* Contiguity_matrices_Constituencies_200914.R -> RData file "contiguity_matrices-munich-constituencies_200914".
* Stimmbezirkssampling_COVID_MonteCarloEstimate.R -> RData file "MC-Estimation-Sampling-Probabilities" - NOT AVAILABLE.
* Sampling_weights_study_pop.R -> data frame "KoCo_BLab" containing with the original data on the participants and the sampling weights.
* Calibrated_weights_study_pop.R -> csv file "KoCo_weights" with the calibrated weights of each household/individual. csv file "All_estimates" containing the weighted/unweighted seroprevalence estimates adjusted or not for specificity and sensitivity. csv file "pcr_pos" with the estimated number of PCR positive cases in private households based on our survey.
