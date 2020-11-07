# KoCo19

Code for laboratory and epidemiological analysis for the initial examination
of the prospective Covid-19 cohort study Munich (KoCo19).
The study website is [http://koco19.de](http://koco19.de), see there for
further information.

## Structure

The code is located in the following folders:

* `Statistics` contains the main code for statistical tests and visualization
  of results, except for some parts located in the below folders.
* `PermutationStudies` contains the code for performing a permutation test
  assessing geospatial clustering.
* `SamplingWeights` contains the code for performing sampling weight
  correction and calibration.

Further:

* `AlgorithmResults` contains results of the test strategy optimization
  performed in `Statistics/R/classifier.R`.
* `TemplateData` contains a template of the data sources used in this study
  to allow assessing the code better.
* `Icons` contains a few icons used for visualization.
