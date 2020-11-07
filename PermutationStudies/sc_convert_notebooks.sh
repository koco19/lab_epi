#!/bin/bash

# Convert run notebooks to shareable html and pdf formats

mkdir -p html
mkdir -p pdf

for nb in `cd notebook; ls *.ipynb`; do
  jupyter nbconvert --to html notebook/${nb} \
    --stdout > html/${nb%.*}.html
  jupyter nbconvert --to pdf notebook/${nb} \
    && mv notebook/${nb%.*}.pdf pdf/${nb%.*}.pdf
done
