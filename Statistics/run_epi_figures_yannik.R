#!/usr/bin/Rscript

#' Run and render R/epi_figures_yannik.Rmd

here_r = function (...) here::here(
  "Statistics", "R", ...)
here_out = function (...) here::here(
  "Epi_out", "epi_figures_yannik", ...)

# Create output folder if inexistent
dir.create(here_out(), showWarnings = FALSE)

# Run and render code
rmarkdown::render(
  input=here_r("epi_figures_yannik.Rmd"),
  output_format="all",
  output_dir=here_out()
)
