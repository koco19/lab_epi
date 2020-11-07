#!/usr/bin/Rscript

#' Run R/lab_figures_merce.Rmd

here_r = function (...) here::here(
  "Statistics", "R", ...)
here_lab_output = function (...) here::here(
  "Lab_paper2_output", "lab_figures_merce", ...)

# Unset warnings (run manually if you want to see them)
options(warn=-1)

# Copy Rmd R code to temporary file
temp_r = tempfile(fileext = ".R")
knitr::purl(here_r("lab_figures_merce.Rmd"), output = temp_r)

# Run code
source(temp_r)

# Remove temporary file again
file.remove(temp_r)
