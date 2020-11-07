#!/bin/bash

# Run all notebooks

households=true
buildings=true
locations=true

mkdir -p notebook
mkdir -p data
mkdir -p data_private
mkdir -p img

function run(){
  nb=$1
  if [ ! -f notebook/${nb} ]; then
    echo $nb
    jupyter nbconvert --to notebook --execute notebook_empty/${nb} \
      --ExecutePreprocessor.timeout=86400 \
      --stdout > notebook/${nb}
  fi
}

# households
if $households; then
  echo "Households"
  for nb in `cd notebook_empty && ls Perm-hh-*.ipynb`; do
    run $nb
  done
fi

# buildings
if $buildings; then
  echo "Buildings"
  for nb in `cd notebook_empty && ls Perm-bd-*.ipynb`; do
    run $nb
  done
fi

# locations
if $locations; then
  echo "Locations"
  for nb in `cd notebook_empty && ls Perm-lc-*.ipynb`; do
    run $nb
  done
fi
