#!/bin/bash

# Convert template notebooks to executable notebooks

# Cutoffs
cutoffs=("cutold" "cutnew")
# Measurements to consider
data_keys=("R" "IgG" "IgA")
# Number of permutations to perform
n_perm=10000
# Thresholds for the geospatial clustering
thresholds=("0.05" "0.1" "0.2" "0.5" "1" "2" "4")

households=true
buildings=true
locations=true

mkdir -p notebook_empty

# households
if $households; then
  echo "Households"
  for cutoff in "${cutoffs[@]}"; do
    for data_key in "${data_keys[@]}"; do
      sed -e "s/%%data_key%%/${data_key}/g" \
        -e "s/%%n_perm%%/${n_perm}/g" \
        -e "s/%%cutoff%%/${cutoff}/g" \
        template/Perm-hh.ipynb > notebook_empty/Perm-hh-${cutoff}-${data_key}-${n_perm}.ipynb
    done
  done
fi

# buildings
if $buildings; then
  echo "Buildings"
  for cutoff in "${cutoffs[@]}"; do
    for data_key in "${data_keys[@]}"; do
      sed -e "s/%%data_key%%/${data_key}/g" \
        -e "s/%%n_perm%%/${n_perm}/g" \
        -e "s/%%cutoff%%/${cutoff}/g" \
        template/Perm-bd.ipynb > notebook_empty/Perm-bd-${cutoff}-${data_key}-${n_perm}.ipynb
    done
  done
fi

# locations
if $locations; then
  echo "Locations"
  for threshold in "${thresholds[@]}"; do
    for cutoff in "${cutoffs[@]}"; do
      for data_key in "${data_keys[@]}"; do
        sed -e "s/%%threshold%%/${threshold}/g" \
          -e "s/%%data_key%%/${data_key}/g" \
          -e "s/%%n_perm%%/${n_perm}/g" \
          -e "s/%%cutoff%%/${cutoff}/g" \
          template/Perm-lc.ipynb > notebook_empty/Perm-lc-${cutoff}-${threshold}-${data_key}-${n_perm}.ipynb
      done
    done
  done
fi
