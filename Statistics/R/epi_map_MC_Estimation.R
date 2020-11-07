#!/usr/bin/Rscript

# Mercè Garí
# 201013

library(ggmap)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rgdal)
library(spdep)
library(stringr)
library(sf)
library(colorspace)
library(cowplot)

#######################################################
### Follows https://juanitorduz.github.io/germany <- plots/
### Data from http://insideairbnb.com/get-the-data.html
### file http://data.insideairbnb.com/germany/bv/munich/2020-06-20/visualisations/neighbourhoods.geojson

# Input and output locations used in this script
here_maps_data = function (...) here::here("maps_data", ...)
here_maps_out = function (...) here::here("maps_output", ...)

# Polygon of districts
m.sh <- readOGR(here_maps_data("neighbourhoods.geojson"))

# Polygon of constituencies
S.original <- st_read(here_maps_data("KW_2020_Stimmbezirke_V2.shp"))

# Polygon of constituencies clean
S <- S.original %>%
   mutate(Constituency_ID = (KW_SB_20_2),
          District_ID = as.numeric(str_sub(Constituency_ID, 1, 2)),
          Constituency_ID = as.numeric(Constituency_ID))

# Match district with district-id
rd <- tibble(District = attributes(m.sh)$data$neighbourhood) %>%
  mutate(District_ID = (1:(n())))
rd

# Join polygon constituencies with districts by District_ID
S <- S %>%
  left_join(rd)

# Dataset of districts and constituencies
dc <- S %>%
  as_tibble() %>%
  select(Constituency_ID, District_ID, District) %>%
  unique()

# Plot map
ggplot() +
  geom_sf(data = S, size = 0.1, color = "black", aes(fill=District)) +
  ggtitle("Munich Constituencies by District") +
  coord_sf() +
  theme_bw() +
  theme(legend.position = "none")

# Check real output
load(here_maps_data("output.RData"))
output <- output %>%
  select(Gebietsnummer) %>%
  rename(Constituency_ID = Gebietsnummer) %>%
  mutate(selected = "Yes")
# Join polygon constituencies with output by Constituency_ID
real <- left_join(S, output)
ggplot() +
  geom_sf(data = real, size = 0.1, color = "grey", aes(fill = selected)) +
  coord_sf() +
  scale_fill_discrete(na.value="white") +
  theme_bw()

# MC Sampling Probabilities
load(here_maps_data("MC-Estimation-Sampling-Probabilities.RData"))
sampling.prob.mc

s.sampling <- left_join(S, sampling.prob.mc)

ggplot() +
  geom_sf(data = s.sampling, size = 0.1, color = "grey",
          aes(fill = Sampling_Probabilities)) +
  coord_sf() +
  scale_fill_continuous_sequential(palette = "Viridis", na.value = "white",
                                   name = "Sampling Probability") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(here_maps_out("Map-Sampling-Probability-MC.pdf"), height=6, width=10)

# Comparison between the selected and not selected constituencies
comparison <- sampling.prob.mc %>%
  left_join(output) %>%
  mutate(selected = ifelse(is.na(selected), "No", selected)) %>%
  rename(`Sampling probability` = Sampling_Probabilities)
ggplot(comparison, aes(x=`Sampling probability`,
                       y=reorder(Constituency_ID, `Sampling probability`),
                       color=selected, alpha=0.4)) +
  geom_point(alpha = 0.4) +
  ylab("Constituency") +
  geom_vline(xintercept = 0.1324503, lty=2) +
  theme(axis.text.y = element_blank())

ggsave(
  here_maps_out("Plot-Sampling-Probability-MC-vs-chosen-constituencies.pdf"),
  width=6, height=4)
