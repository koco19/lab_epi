#!/usr/bin/Rscript

# Mercè Garí
# 210317

library(ggmap)
library(ggplot2)
library(dplyr)
library(tidyr)
library(rgdal)
library(spdep)
library(stringr)
library(sf)
library(sp)
library(colorspace)
library(cowplot)
library(geojson)
library(grid)
library(spatial)
library(ggspatial)
library(xlsx)

#######################################################
### Follows https://juanitorduz.github.io/germany <- plots/
### Data from http://insideairbnb.com/get-the-data.html
### file http://data.insideairbnb.com/germany/bv/munich/2020-06-20/visualisations/neighbourhoods.geojson

# Input and output locations used in this script
here_maps_data = function (...) here::here("maps_data", ...)
here_maps_out = function (...) here::here("Epi_out", "epi_figures_merce", ...)
here_koco_data = function (...) here::here("KoCo19_Datasets", ...)

# Polygon of districts
m.sh <- readOGR(here_maps_data("neighbourhoods.geojson"))
class(m.sh)
head(m.sh)

districts <- m.sh

# Polygon of constituencies
S.original <- st_read(here_maps_data("KW_2020_Stimmbezirke_V2.shp"))

constituencies <- S.original

# Depending on the data, we would need 4 or 5 digits constituency ID (cid)
# 4 digits refers to: last two digits are the constituencies and first one
#  or first two are the districts
# 5 digits refers to: 2+3, 2 for the district, and 3 with the leading 0 for
#  the constituencies
cid <- S.original %>%
  as_tibble() %>%
  select(KW_SB_2020, KW_SB_20_2) %>%
  unique() %>%
  rename(cid.4 = KW_SB_2020,  # 4 digits
         cid.5 = KW_SB_20_2)  # 5 digits

# Plot the underlying map with constituency borders
ggplot() + 
  geom_sf(data = S.original, size = 0.1, color = "black", fill = "white") + 
  ggtitle("Munich Constituencies") + 
  coord_sf() +
  theme_bw()

# Polygon of constituencies clean
S <- S.original %>%
  mutate(cid.4 = KW_SB_2020,
         cid.5 = KW_SB_20_2,
         District_ID = as.numeric(str_sub(cid.5, 1, 2)))

# Match district with district-id
rd <- tibble(District = attributes(m.sh)$data$neighbourhood) %>%
  mutate(District_ID = (1:(n()))) 
rd
# Joining polygon constituencies with districts
S <- S %>%
  left_join(rd)  # joining by District_ID

# Create and write table of 4/5 CIDs with district IDs and district names
dc <- S %>%
  as_tibble() %>%
  select(cid.4, cid.5, District_ID, District) %>%
  unique()
#write.csv(dc, here_maps_out("District-Constituency-IDs.csv"))

# Plot map, colored by district
ggplot() + 
  geom_sf(data = S, size = 0.1, color = "black", aes(fill=District)) + 
  ggtitle("Munich Constituencies by District") + 
  coord_sf() +
  theme_bw() +
  theme(legend.position = "none")

# Load prevalence for districts
prev.d <- read.xlsx(here_maps_data("prevalence_by_district_munich.xlsx"), 1)
head(prev.d)
prev.d <- prev.d %>%
  rename(District_ID = dist) %>%
  mutate(`Prevalence (%)` = prev*100,
         `Prevalence (%)\nCI low` = prev_0.025*100,
         `Prevalence (%)\nCI high` = prev_0.975*100) %>%
  rename(`N. Participants` = Tested_Ind)
prev.d.S <- left_join(S, prev.d)
library(viridis)
map.prev.mean <- ggplot(data=prev.d.S, size=0.1, 
                        aes(fill=`Prevalence (%)`)) + 
  geom_sf(size=0) +
  coord_sf() + theme_bw() +
  scale_fill_continuous_sequential(palette = "Viridis") +
  theme(panel.background = element_blank(),
        axis.text=element_blank(), panel.grid.major = element_blank()) +
  annotation_scale()
map.prev.mean  

map.prev.low <- ggplot(data=prev.d.S, size=0.1, 
                       aes(fill=`Prevalence (%)\nCI low`)) + 
  geom_sf(size=0) +
  coord_sf() + theme_bw() +
  scale_fill_continuous_sequential(palette = "Viridis",
                                   limits=c(0, 1),
                                   breaks=c(0, 0.5, 1),
                                   labels=c(0, 0.5, 1)) +
  theme(panel.background = element_blank(),
        axis.text=element_blank(), panel.grid.major = element_blank()) +
  annotation_scale()

map.prev.low

map.prev.high <- ggplot(data=prev.d.S, size=0.1, 
                        aes(fill=`Prevalence (%)\nCI high`)) + 
  geom_sf(size=0) +
  coord_sf() + theme_bw() +
  scale_fill_continuous_sequential(palette = "Viridis",
                                   breaks=c(0, 5, 10, 15),
                                   labels=c(0, 5, 10, 15)) +
  theme(panel.background = element_blank(),
        axis.text=element_blank(), panel.grid.major = element_blank()) +
  annotation_scale()
map.prev.high

map.individuals <- ggplot(data = prev.d.S, 
                          aes(fill=`N. Participants`),
                          colour="grey", size=0.1) + 
  geom_sf(size=0) +
  coord_sf() + theme_bw() +
  scale_fill_continuous_sequential(palette = "Viridis",
                                   breaks=c(50, 250, 500)) +
  theme(panel.background = element_blank(),
        axis.text=element_blank(), panel.grid.major = element_blank()) +
  annotation_scale()

map.individuals



plot_grid(
  plot_grid(map.individuals, map.prev.mean, nrow=1, labels=c("A", "B")),
  plot_grid(map.prev.low, map.prev.high,
          labels=c("C", "D"), nrow=1),  nrow=2)
ggsave(here_maps_out("Epi-Fig-PrevMap-SI.pdf"), height=8, width=10)
ggsave(here_maps_out("Epi-Fig-PrevMap-SI.png"), height=8, width=10)


# Load prevalence for districts with Weights
prev.d <- read.xlsx(here_maps_data("prevalence_by_district_munich_weighted.xlsx"), 1)
head(prev.d)
prev.d <- prev.d %>%
  rename(District_ID = dist) %>%
  mutate(`Prevalence (%)` = prev*100,
         `Prevalence (%)\nCI low` = prev_0.025*100,
         `Prevalence (%)\nCI high` = prev_0.975*100) %>%
  rename(`N. Participants` = Tested_Ind)
prev.d.S <- left_join(S, prev.d)
library(viridis)
map.prev.mean <- ggplot(data=prev.d.S, size=0.1, 
                        aes(fill=`Prevalence (%)`)) + 
  geom_sf(size=0) +
  coord_sf() + theme_bw() +
  scale_fill_continuous_sequential(palette = "Viridis") +
  theme(panel.background = element_blank(),
        axis.text=element_blank(), panel.grid.major = element_blank()) +
  annotation_scale()
map.prev.mean  

map.prev.low <- ggplot(data=prev.d.S, size=0.1, 
                       aes(fill=`Prevalence (%)\nCI low`)) + 
  geom_sf(size=0) +
  coord_sf() + theme_bw() +
  scale_fill_continuous_sequential(palette = "Viridis",
                                   breaks=c(0, 0.75, 1.5)) +
  theme(panel.background = element_blank(),
        axis.text=element_blank(), panel.grid.major = element_blank()) +
  annotation_scale()
map.prev.low
map.prev.high <- ggplot(data=prev.d.S, size=0.1, 
                        aes(fill=`Prevalence (%)\nCI high`)) + 
  geom_sf(size=0) +
  coord_sf() + theme_bw() +
  scale_fill_continuous_sequential(palette = "Viridis") +
  theme(panel.background = element_blank(),
        axis.text=element_blank(), panel.grid.major = element_blank()) +
  annotation_scale()
map.prev.high

map.individuals <- ggplot(data = prev.d.S, 
                          aes(fill=`N. Participants`),
                          colour="grey", size=0.1) + 
  geom_sf(size=0) +
  coord_sf() + theme_bw() +
  scale_fill_continuous_sequential(palette = "Viridis",
                                   breaks=c(100, 250, 500)) +
  theme(panel.background = element_blank(),
        axis.text=element_blank(), panel.grid.major = element_blank()) +
  annotation_scale()

map.individuals



plot_grid(
  plot_grid(map.individuals, map.prev.mean, nrow=1, labels=c("A", "B")),
  plot_grid(map.prev.low, map.prev.high,
            labels=c("C", "D"), nrow=1),  nrow=2)
ggsave(here_maps_out("Epi-Fig-PrevMap-Weighted-SI.pdf"), height=8, width=10)
ggsave(here_maps_out("Epi-Fig-PrevMap-Weighted-SI.png"), height=8, width=10)

  