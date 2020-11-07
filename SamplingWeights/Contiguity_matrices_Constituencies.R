#!/usr/bin/Rscript

# Mercè Garí
# dt. d’ag.  4 11:15:05 CEST 2020

library(ggmap)
library(ggplot2)
library(dplyr)
library(tidyr)


#######################################################
### Follows https://juanitorduz.github.io/germany <- plots/
### Data from http://insideairbnb.com/get-the-data.html
### file http://data.insideairbnb.com/germany/bv/munich/2020-06-20/visualisations/neighbourhoods.geojson

library(rgdal)
library(spdep)

m.sh <- readOGR("neighbourhoods.geojson")

rd <- tibble(District = attributes(m.sh)$data$neighbourhood) %>%
  mutate(region = as.character(0:(n()-1)))


map.m <- map_data(m.sh)

map.centroids <- map.m %>%
  left_join(rd) %>%
  group_by(District) %>%
  summarize(long = mean(long), lat = mean(lat))

ggplot(map.m, aes(x = long, y = lat)) +
  geom_polygon(aes(group = region), fill = "white", color = "black") +
  geom_text(data = map.centroids, aes(label = District)) +
  coord_fixed() +
  theme_bw()


# Neighbours list
nl <- poly2nb(m.sh, row.names = attributes(m.sh)$data$neighbourhood)

class(nl)
str(nl)




# Neighbours matrix, check help:
# "Starting from a binary neighbours list, in which regions are either 
# listed as neighbours or are absent (thus not in the set of neighbours 
# for some definition), the function creates an n by n weights matrix 
# with values given by the coding scheme style chosen. 
# B is the basic binary coding, W is row standardised, C is globally 
# standardised, while S is the variance-stabilizing coding scheme 
# proposed by Tiefelsdorf et al. 1999, p. 167-168. "


# Weights by the number of neighbours
M.w.by.neighbours <- nb2mat(nl, style = "W")
hist(M.w.by.neighbours)

# Contiguity, binary
M.binary <- nb2mat(nl, style = "B")
hist(M.binary)

# Weights by distance shared
M.w.by.shared.neighbours <- nb2mat(nl, style = "S")
hist(M.w.by.shared.neighbours)


dimnames(M.binary)[[2]] <- dimnames(M.binary)[[1]]
dimnames(M.w.by.neighbours)[[2]] <- dimnames(M.w.by.neighbours)[[1]]
dimnames(M.w.by.shared.neighbours)[[2]] <- dimnames(M.w.by.shared.neighbours)[[1]]


save(M.binary, M.w.by.neighbours, M.w.by.shared.neighbours,
     file = "contiguity_matrices-munich.RData")




# Graphical figures in tiles
d.binary <- as.data.frame.table(M.binary)
ggplot(d.binary, aes(x = Var1, y = Var2, fill = Freq)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  geom_raster()

ggsave("Binary_Contiguity.pdf", height=8, width=8)

# Graphical figures in networks
library(igraph)

net <- graph_from_adjacency_matrix(M.binary,
                                   mode = "directed",
                                   weighted = TRUE,
                                   diag = FALSE)
plot(net)
