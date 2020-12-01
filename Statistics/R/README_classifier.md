# Classifier

Cutoff optimization for single tests, performance (specificity, sensitivity and overall accuracy) calculation for different classifiers.

## Responsibles

* Peter Pütz

## Data

Initial data are not provided due to data protection. The scripts expect them to be in the standard folders (indicated in the scripts).
But intermediate results can be accessed from the `AlgorithmResults` folder and used to generate the outputs as shown in the paper. 

## Run

The analysis (in R) can be performed via `classifier.R` which calls the scripts `algorithm_manucutoff_rf_svm.R` and `algorithm_optcutoff.R`.

## Results

The results can be viewed in the `Lab_paper2_output` folder.

## Requirements

The analysis has been run in R version 4.0.2 (64 bit), packages will be installed automatically if necessary.

## Notes

Running the scripts `algorithm_manucutoff_rf_svm.R` and `algorithm_optcutoff.R` on their own or by calling them via
`classifier.R` might take a few hours, instead the results of these scripts can be loaded directly as indicated in `classifier.R`.
