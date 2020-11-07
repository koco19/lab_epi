#!/bin/bash

# Generate figures in R

# Cutoffs
cutoffs=("cutold" "cutnew")
# Measurements to consider
data_keys=("R" "IgG" "IgA")
# Number of permutations to perform
n_perm=10000

mkdir -p out

for cutoff in "${cutoffs[@]}"; do
  for data_key in "${data_keys[@]}"; do	
	  Rscript -e "rmarkdown::render('Perm-analysis.Rmd', output_format='html_document', params=list(cutoff='${cutoff}', data_key='${data_key}', n_perm='${n_perm}'), output_file='out/Perm-analysis-${cutoff}-${data_key}-${n_perm}.html')"
  done
done

# Package
rm out-Perm.zip
zip -r out-Perm.zip out
