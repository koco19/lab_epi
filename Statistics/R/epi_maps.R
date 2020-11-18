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
library(sp)
library(colorspace)
library(cowplot)
library(geojson)
library(grid)

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

# Polygons of districts
rd1 <- tibble(District = attributes(m.sh)$data$neighbourhood) %>%
  mutate(region = as.character(0:(n()-1)))
rd1

# Create table with longitude, latitude, group, region
map.m <- map_data(m.sh)

# Group by district, with longitude and latitude as the means
map.centroids <- map.m %>%
  left_join(rd1) %>%
  group_by(District) %>%
  summarize(long = mean(long), lat = mean(lat))

# Plot district labels
ggplot(map.m, aes(x = long, y = lat)) +
  geom_polygon(aes(group = region), fill = "white", color = "black") +
  geom_text(data = map.centroids, aes(label = District)) +
  coord_fixed() +
  theme_bw()


#------------------------------------------------------------------------------
# Load data on in which district a household is

# Details of HH constituencies KoCo19 start/reside
d.orig <- read.csv(here_koco_data(
  "Geocodes", "KoCo19_Haushalte4Modeler_wRRstartConstituency_20200910.csv"),
  header=TRUE, na.strings = c("-9999", "NotInMunich"))
# HH constituencies KoCo19 clean
d <- d.orig %>%
  as_tibble() %>%
  select(hht_ID, address_id, constituency, constituency_RRstart) %>%
  rename(hh_id = hht_ID,
         ConstIdStart = constituency_RRstart,
         ConstIdReside = constituency)

#------------------------------------------------------------------------------
# Load individual characteristics dataset

ind.orig <- read.csv(here_koco_data(
  "Analysis Data Sets", "ind_characteristics_new.csv"),
  header = TRUE, na.string = c("-9999", "NA"))
ind.orig <- ind.orig[,-1]
head(ind.orig)
table(ind.orig$inclusion_criteria)

# Filter for inclusion, select ind_id and hh_id
ind <- ind.orig %>%
  filter(inclusion_criteria %in% "Included") %>%
  select(ind_id, hh_id)

#------------------------------------------------------------------------------
# Load household characteristics dataset

hh.orig <- read.csv(here_koco_data(
  "Analysis Data Sets", "hh_characteristics_new.csv"), 
  header = TRUE, na.string = "-9999")
hh.orig <- hh.orig[,-1]
dim(hh.orig)
table(hh.orig$inclusion_criteria)

# Filter for inclusion, select hh_id and size
hh <- hh.orig %>%
  filter(inclusion_criteria %in% "Included") %>%
  select(hh_id, HouseholdSize) %>%
  mutate(HouseholdSize = ifelse(HouseholdSize %in% "NA", NA, 
                                HouseholdSize),
         HouseholdSize = as.numeric(HouseholdSize))

#------------------------------------------------------------------------------
# Left joins

# I have three datasets:
# * d with hh_id, address_id, ConstIdReside, ConstIdStart
# * ind with ind_id and hh_id
# * hh with hh_id and HouseholdSize

hh.d <- left_join(hh, d)

length(unique(hh.d$ConstIdStart))
length(unique(hh.d$ConstIdReside))

#------------------------------------------------------------------------------
# Epi 2a

# Figure 2a
#  100 randomly selected constituencies for Munich with districts 
#  (districts not coloured, only contours, 
#  constituencies included in different colours per district, 
#  the other constituencies only contours, slightly less than districts)

# Number of households per start constituency
d.start <- hh.d %>%
  group_by(ConstIdStart) %>%
  summarize(num.constituencies = n()) %>%
  rename(cid.4 = ConstIdStart)
# Merge with geographical info (join by cid.4)
s.hh.start <- left_join(S, d.start) %>%
  mutate(
    constituencies.present = ifelse(is.na(num.constituencies), FALSE, TRUE))

# Plot as indicated above
p.start.district <- ggplot(data = s.hh.start,
                           aes(fill = District), colour = "grey", size=0.1) +
  # Other constituencies
  geom_sf(size = 0, data = filter(s.hh.start, !constituencies.present), 
          alpha=0.1, show.legend=FALSE) +
  # Starting constituencies
  geom_sf(size = 0, data = filter(s.hh.start, constituencies.present),
          show.legend=FALSE) +
  coord_sf() +
  theme_bw() +
  labs(caption="\nRandom selection of constituencies (n=100)") +
  theme(legend.position="bottom", panel.background = element_blank(),
        axis.text=element_blank(), panel.grid.major = element_blank(),
        plot.margin=margin(0,0.45,0.25,0.45, unit="in"),
        plot.caption = element_text(hjust=0.5, size=11))
p.start.district

#ggsave(here_map_out("Epi-Fig2a-200914.pdf"), height=6, width=8)
#ggsave(here_map_out("Epi-Fig2a-200914.png"), height=6, width=8)

#------------------------------------------------------------------------------
# Fig 2b
#  Number of households by residence

# Number of households per constituency
d.reside <- hh.d %>%
  group_by(ConstIdReside) %>%
  summarize(num.hh = n()) %>%
  rename(cid.4 = ConstIdReside)
# Merge with geographical info (join by cid.4)
s.hh.reside <- left_join(S, d.reside) %>%
  mutate(hh.present = ifelse(is.na(num.hh), FALSE, TRUE)) 

# Plot as indicated above
p.reside <- ggplot() +
  geom_sf(data = s.hh.reside, size = 0.1, color = "grey", aes(fill = num.hh)) +
  coord_sf() +
  scale_fill_continuous_sequential(palette = "Viridis", na.value="white",
                                   name = "Number of Households") + 
  theme_bw() +
  theme(legend.position = "bottom", panel.background = element_blank(),
        axis.text=element_blank(), panel.grid.major = element_blank())
p.reside

#ggsave(here_maps_out("Epi-Fig2b-200914.pdf"), height=6, width=8)
#ggsave(here_maps_out("Epi-Fig2b-200914.png"), height=6, width=8)

#------------------------------------------------------------------------------
# Fig 2c
#  Number of hh in the same building sampled
#  Visualization of number of houses (or buildings) included via colour scheme

dim(d)
length(unique(hh.d$address_id))
length(hh.d$address_id)
dim(hh.d)

# Plot number of households considered in the same building
hh.d %>%
  group_by(address_id) %>%
  summarise(num.h = n()) %>%
  ggplot(aes(x=num.h)) +
  geom_bar(alpha=0.7) +
  theme_bw() +
  xlab("Number of households in the same building") +
  ylab("Cases") +
  scale_x_continuous(breaks=1:10)
#ggsave(here_maps_out("Barplot-Number-hh-same-building-200922.pdf"), width=6, height=4)
#ggsave(here_maps_out("Barplot-Number-hh-same-building-200922.png"), width=6, height=4)

# Average num households per house (building) per residence constituency
hh.houses <- hh.d %>%
  group_by(ConstIdReside, address_id) %>%
  summarize(num.h = n()) %>%
  ungroup() %>%
  group_by(ConstIdReside) %>%
  summarize(average.hh.house = mean(num.h, na.rm=TRUE)) %>%
  mutate(cid.4 = ConstIdReside)
hh.houses
s.hh.houses <- left_join(S, hh.houses)

# Plot the average number of households per residence constituency
p.houses <- ggplot() +
  geom_sf(data = s.hh.houses, size = 0.1, color = "grey", 
          aes(fill = average.hh.house)) +
  coord_sf() +
  scale_fill_continuous_sequential(palette = "Viridis", na.value="white",
                                   name = "Average number of households per house") +
  theme_bw() +
  theme(legend.position = "bottom", panel.background = element_blank(),
        axis.text=element_blank(), panel.grid.major = element_blank())
p.houses
#ggsave("Epi-Fig2c-average-hh-house.pdf", height=6, width=8)
#ggsave("Epi-Fig2c-average-hh-house.png", height=6, width=8)

#------------------------------------------------------------------------------
# Fig 2d
#  369 constituencies finally sampled, 
#  visualization of average household size via colour scheme

# Average HouseholdSize per residence constituency
hh.reside <- hh.d %>%
  group_by(ConstIdReside) %>%
  summarize(num.hh = mean(HouseholdSize, na.rm=TRUE)) %>%
  rename(cid.4 = ConstIdReside)
s.hh.reside <- left_join(S, hh.reside) %>%
  mutate(hh.present = ifelse(is.na(num.hh), FALSE, TRUE)) 

# Plot the average household size per residence constituency
p.size <- ggplot() +
  geom_sf(data = s.hh.reside, size = 0.1, color = "grey", aes(fill = num.hh)) +
  coord_sf() +
  scale_fill_continuous_sequential(palette = "Viridis", na.value="white",
                                   name = "Average household size") + 
  theme_bw() +
  theme(legend.position = "bottom", panel.background = element_blank(),
        axis.text=element_blank(), panel.grid.major = element_blank())
p.size

#ggsave(here_maps_out("Epi-Fig2f.pdf"), height=6, width=8)
#ggsave(here_maps_out("Epi-Fig2f.png"), height=6, width=8)

#------------------------------------------------------------------------------
# Combine and save subplots

plot_grid(p.start.district, p.reside,
          p.houses, p.size, 
          labels=c("A", "B", "C", "D"), 
          nrow=2)

ggsave(here_maps_out("Epi-Fig1.pdf"), height=8, width=10)
ggsave(here_maps_out("Epi-Fig1.png"), height=8, width=10)

