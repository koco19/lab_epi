#!/usr/bin/Rscript

#' Run and render R/lab_figures_yannik.Rmd

here_r = function (...) here::here(
  "Statistics", "R", ...)
here_out = function (...) here::here(
  "Lab_paper2_output", "lab_figures_yannik", ...)

# Create output folder if inexistent
dir.create(here_out(), showWarnings = FALSE)

# Run and render code
rmarkdown::render(
  input=here_r("lab_figures_yannik.Rmd"),
  output_format="all",
  output_dir=here_out()
)
