# Statistical Analysis

This folder contains most of the code for the statistical analysis for the
Lab and the Epi data. Some parts (e.g. the weighting and the permutation
tests) are in dedicated folders instead.

The main folder contains a few `run_...` scripts for automatically running
various analyses, while the actual code is in the `R` subfolder.

## Responsibles

* Yannik Schälte
* Mercè Garí
* Abhishek Bakuli
* Peter Pütz

## Installation

Most R packages that are required can be found in `requirements.txt`.
Some routines install their required packages automatically, if not yet
existent.

## Run

The scripts use `R::here()` to identify paths and should thus be system-independent.

Most scripts use the [here](https://github.com/jennybc/here_here) package
to define locations, such that no path adjustments should be required.

Some figure generation and analysis routines can be automatically run via
`run_...` scripts in this folder, however this does unfortunately not apply
to all, especially as some contain highly time-intensive parts and need
manual intervention. In other routines, some lines may need to be uncommented
in order to re-run time-intensive parts.

## Further Readmes

There are several readmes in the folder R that elaborate on the R scripts included in that folder. The following list indicates what the readmes and the corresponding R script are about:

* `Readme_classifier.md`: Cutoff optimization for single tests, performance (specificity, sensitivity and overall accuracy) calculation for different classifiers, and adjustment of seroprevalence estimates for the Munich population by accounting for the performance.
* `Readme_maps.md`: Requirements and instructions for creating the maps featured in the manuscripts.
