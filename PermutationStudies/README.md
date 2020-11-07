# Permutation studies

Study on the impact of geospatial proximity clusters on antibody levels.

## Responsibles

* Yannik Schaelte

## Data

Data are not provided due to data protection. The scripts expect them to be in the standard folders (indicated in the scripts).

## Run

1) Template notebooks containing variables are located in the folder `template`. These can be transformed to executable notebooks via `bash sc_templates_to_notebooks.sh`. The executable notebooks are then located in the folder `notebook_empty`.
2) The executable notebooks can be executed via `bash sc_run_notebooks.sh`. This runs all notebooks in the `notebook_empty` folder and writes filled notebooks to the `notebook` folder. Further, the notebooks write data to the `data` folder and images to the `img` folder.
3) The notebooks can be converted to the shareable html and pdf formats via `bash sc_convert_notebooks.sh`. The converted notebooks are in the `html` and `pdf` folders.
4) The final analysis (in R) can be performed via `bash sc_analyze.sh`, which executes the `Perm-analysis.Rmd` notebook, creating figures as well as html and pdf conversions of the notebook in the `out` folder, as well as a shareable `out-Perm.zip` zip file of the `out` folder.

## Results

The reports can be viewed in the `out` folder.

## Requirements

The shell scripts require an appropriate interpreter (bash). The analysis was for efficiency reasons done in Python(>=3.8.5, with requirements as specified in `requirements.txt`). The figure generation is run in R (>=3.6.3), with `ggplot2` and `here` as only package requirements.

## Notes

Permutation studies are computationally expensive, therefore we employed optimized vectorized implementations. The whole study takes a few hours to run all scenarios.
