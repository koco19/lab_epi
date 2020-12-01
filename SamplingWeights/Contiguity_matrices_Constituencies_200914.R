#!/usr/bin/Rscript

# Mercè Garí
# 200914

library(ggmap)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rgdal)
library(spdep)
library(stringr)
library(sf)

#######################################################
### Follows https://juanitorduz.github.io/germany <- plots/
### Data from http://insideairbnb.com/get-the-data.html
### file http://data.insideairbnb.com/germany/bv/munich/2020-06-20/visualisations/neighbourhoods.geojson


m.sh <- readOGR("neighbourhoods.geojson")


S.original <- st_read("KW_2020_Stimmbezirke_V2.shp")

ggplot() + 
  geom_sf(data = S.original, size = 0.1, color = "black", fill = "white") + 
  ggtitle("Munich Constituencies") + 
  coord_sf() +
  theme_bw()


S <- S.original %>%
  mutate(Constituency_ID = KW_SB_20_2,
         District_ID = as.numeric(str_sub(Constituency_ID, 1, 2)))

rd <- tibble(District = attributes(m.sh)$data$neighbourhood) %>%
  mutate(District_ID = (1:(n()))) 
rd

S <- S %>%
  left_join(rd)

ggplot() + 
  geom_sf(data = S, size = 0.1, color = "black", aes(fill=District)) + 
  ggtitle("Munich Constituencies by District") + 
  coord_sf() +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(legend.position = "none")
#ggsave("Munich_constituencies.pdf", height=8, width=8)


which(duplicated(S$KW_SB_2020))
S$KW_SB_2020[c(362, 499)]

# Two constituencies are duplicated: 1909 and 1828.
# It means there are two shapes for each, instead of one.

ggplot() + 
  geom_sf(data = S.original, size = 0.1, color = "black", fill = "white") + 
  geom_sf(data = filter(S.original, KW_SB_2020 %in% c("1909", "1828")),
          color = "blue", fill="blue") +
  ggtitle("Munich Constituencies") + 
  coord_sf() +
  theme_bw()

S.original %>% filter(KW_SB_2020 %in% c("1909", "1828")) 

S.rm <- S %>%
  mutate(shape.rm = ifelse((KW_SB_2020 == 1828 & SHAPE_AREA < 7000) |
                            (KW_SB_2020 == 1909 & SHAPE_AREA < 7000),
                           "yes", "no"))
table(S.rm$shape.rm)
S.rm %>% filter(KW_SB_2020 %in% c("1909", "1828")) 
S <- S.rm %>%
  filter(shape.rm %in% "no")

# Extract one of the two polygons from those constituencies
ggplot() + 
  geom_sf(data = S.original, size = 0.1, color = "black", fill = "white") + 
  #  geom_sf(data = filter(S.original, SHAPE_AREA %in% c("882", "6586")),
  #          color = "blue", fill="blue") +
  #  geom_sf(data = filter(S.original, SHAPE_AREA %in% c(514227, 598672)),
  #          color = "red", fill="red") +
  geom_sf(data = filter(S.rm, shape.rm %in% "yes"),
          color = "red", fill="red") +
  geom_sf(data = filter(S.rm, shape.rm %in% "no" & KW_SB_2020 %in% c("1909", "1828")),
          color = "blue", fill="blue") +
    ggtitle("Munich Constituencies") + 
  coord_sf() +
  theme_bw()
ggsave("Map-shapes-to-remove.pdf", width=6, height=4)

# Neighbours list
dimnames(S)[[1]] <- S$KW_SB_2020

nl <- poly2nb(S)#, row.names = attributes(m.sh)$data$neighbourhood)



# Neighbours matrix, check help:
# "Starting from a binary neighbours list, in which regions are either 
# listed as neighbours or are absent (thus not in the set of neighbours 
# for some definition), the function creates an n by n weights matrix 
# with values given by the coding scheme style chosen. 
# B is the basic binary coding, W is row standardised, C is globally 
# standardised, while S is the variance-stabilizing coding scheme 
# proposed by Tiefelsdorf et al. 1999, p. 167-168. "


# Weights by the number of neighbours
M.w.by.neighbours <- nb2mat(nl, style = "W", zero.policy = TRUE)
hist(M.w.by.neighbours)

# Contiguity, binary
M.binary <- nb2mat(nl, style = "B", zero.policy = TRUE)
hist(M.binary)

# Weights by distance shared
M.w.by.shared.neighbours <- nb2mat(nl, style = "S", zero.policy = TRUE)
hist(M.w.by.shared.neighbours)



dimnames(M.binary)[[2]] <- dimnames(M.binary)[[1]]
dimnames(M.w.by.neighbours)[[2]] <- dimnames(M.w.by.neighbours)[[1]]
dimnames(M.w.by.shared.neighbours)[[2]] <- dimnames(M.w.by.shared.neighbours)[[1]]


save(M.binary, M.w.by.neighbours, M.w.by.shared.neighbours,
     file = "contiguity_matrices-munich-constituencies_200914.RData")




# Graphical figures in tiles
# Binary matrix
d.binary <- as.data.frame.table(M.binary) %>%
  mutate(Freq = ifelse(Var1 == Var2, NA, Freq))

ggplot(d.binary, aes(x = Var2, y = Var1, 
                     fill = Freq)) +
  geom_raster() +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 0, vjust = 0),
        legend.position = "none") +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = rev(levels(d.binary$Var1))) +
  scale_fill_continuous(na.value="white") +
  ggtitle("Binary Contiguity Matrix") +
  xlab("") + ylab("") 
#ggsave("Constituencies_Binary_Contiguity.pdf", height=12, width=12)

# Neighbours matrix
d.nei <- as.data.frame.table(M.w.by.neighbours) %>%
  mutate(Freq = ifelse(Var1 == Var2, NA, Freq))

ggplot(d.nei, aes(x = Var2, y = Var1, 
                     fill = Freq)) +
  geom_raster() +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 0, vjust = 0),
        legend.position = "none") +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = rev(levels(d.binary$Var1))) +
  scale_fill_continuous(na.value="white") +
  ggtitle("Neighbours Contiguity Matrix") +
  xlab("") + ylab("") 
#ggsave("Constituencies_Neighbours_Contiguity.pdf", height=8, width=8)


# Shared Neighbours matrix
d.nei.sh <- as.data.frame.table(M.w.by.shared.neighbours) %>%
  mutate(Freq = ifelse(Var1 == Var2, NA, Freq))

ggplot(d.nei.sh, aes(x = Var2, y = Var1, 
                  fill = Freq)) +
  geom_raster() +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 0, vjust = 0),
        legend.position = "none") +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = rev(levels(d.binary$Var1))) +
  scale_fill_continuous(na.value="white") +
  ggtitle("Shared Neighbours Contiguity Matrix") +
  xlab("") + ylab("") 
#ggsave("Constituencies_Shared_Neighbours_Contiguity.pdf", height=8, width=8)



# Generate a spatial distance matrix (takes a while)
#library(geospacom)
M.dist <- st_distance(S, S)
dimnames(M.dist)[[1]] <- dimnames(M.dist)[[2]] <- S$Constituency_ID

dim(M.dist)
M.dist[1:5,1:5]
hist(M.dist)

# From the spatial distance matrix, normalize it by row,
# so that every origin constituency (in the row) is related to every destination
# constituency (in the column) by adding up to 1
M.w.dist <- M.dist / apply(M.dist, 1, sum)
hist(M.w.dist)

M.dist[1:5,1:5]
  # constituency 1 to 2 are separated by 12096, both ways
M.w.dist[1:5,1:5]
  # constituency 1 to 2 are separated by 0.00215, 
  # while constituency 2 to 1 are separated by 0.002355

# For a matrix of spatial association, simply invert the distance
M.w.closeness <- 1 / M.w.dist
hist(M.w.closeness)
M.w.closeness[1:5,1:5]
diag(M.w.closeness) <- NA

save(M.dist, M.w.dist, M.w.closeness, 
     file = "spatial_distance-matrices-munich-constituencies_200914.RData")


# Graphical figures in tiles
# Distance matrix
d.distance <- as.data.frame.table(M.dist) %>%
  mutate(Freq = ifelse(Var1 == Var2, NA, Freq))

ggplot(d.distance, aes(x = Var2, y = Var1, 
                     fill = Freq)) +
  geom_raster() +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 0, vjust = 0),
        legend.position = "none") +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = rev(levels(d.distance$Var1))) +
  scale_fill_continuous(na.value="white") +
  ggtitle("Distance Matrix") +
  xlab("") + ylab("") 
ggsave("Constituencies_Distance.pdf", height=12, width=12)

# Normalized or weighted distance matrix
d.w.dist <- as.data.frame.table(M.w.dist) %>%
  mutate(Freq = ifelse(Var1 == Var2, NA, Freq))

ggplot(d.w.dist, aes(x = Var2, y = Var1, 
                  fill = Freq)) +
  geom_raster() +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 0, vjust = 0),
        legend.position = "none") +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = rev(levels(d.w.dist$Var1))) +
  scale_fill_continuous(na.value="white") +
  ggtitle("Normalized Distance Matrix") +
  xlab("") + ylab("") 
ggsave("Constituencies_Normalized_Distance.pdf", height=8, width=8)


# Closeness matrix
d.closeness <- as.data.frame.table(M.w.closeness) %>%
  mutate(Freq = ifelse(Var1 == Var2, NA, Freq))

ggplot(d.closeness, aes(x = Var2, y = Var1, 
                     fill = Freq)) +
  geom_raster() +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust = 0, vjust = 0),
        legend.position = "none") +
  scale_x_discrete(position = "top") +
  scale_y_discrete(limits = rev(levels(d.closeness$Var1))) +
  scale_fill_continuous(na.value="white") +
  ggtitle("Shared Neighbours Contiguity Matrix") +
  xlab("") + ylab("") 
ggsave("Constituencies_Closeness.pdf", height=8, width=8)

