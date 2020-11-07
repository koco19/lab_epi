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
here_maps_out = function (...) here::here("maps_output", ...)
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
write.csv(dc, here_maps_out("District-Constituency-IDs.csv"))

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
ggsave(here_maps_out("Barplot-Number-hh-same-building-200922.pdf"), width=6, height=4)
ggsave(here_maps_out("Barplot-Number-hh-same-building-200922.png"), width=6, height=4)

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
#ggsave(here_maps_out("Epi-Figure2-200930.pdf"), height=8, width=10)
ggsave(here_maps_out("Epi-Figure2-200922.pdf"), height=8, width=10)
ggsave(here_maps_out("Epi-Figure2-200922.png"), height=8, width=10)


###############################################################################
# Hereafter: Old code
###############################################################################

stop()

# The same as before (average hh size) but compared to Munich
# load data from Munich (excel sent by Jan and modified by me)
library(readxl)
av.hh.size <- read_xls(here_maps_data("constituencies-munich-dept-statistics-KoCo19.xls"),
                        skip=1, sheet=11)
head(av.hh.size)
av.hh.size <- av.hh.size[,1:2]
names(av.hh.size) <- c("cid.5", "Average HH Size Munich")
av.hh.size

hh.reside <- hh.d %>%
  group_by(ConstIdReside) %>%
  summarize(num.hh = mean(HouseholdSize, na.rm=TRUE)) %>%
  rename(cid.4 = ConstIdReside)
s.hh.reside <- left_join(S, hh.reside) %>%
  mutate(hh.present = ifelse(is.na(num.hh), FALSE, TRUE)) 

s.hh.reside.muc <- left_join(s.hh.reside, av.hh.size) %>%
  mutate('Average hh size (KoCo19 - Munich)' = num.hh - `Average HH Size Munich`)

p.size.muc <- ggplot() +
  geom_sf(data = s.hh.reside.muc, size = 0.1, color = "grey", 
          aes(fill = `Average hh size (KoCo19 - Munich)`)) +
  coord_sf() +
  scale_fill_continuous_sequential(palette = "Viridis", na.value="white",
                                   name = "Average household size \n(Difference KoCo19 - Munich)") + 
  theme_bw() +
  theme(legend.position = "bottom", panel.background = element_blank(),
        axis.text=element_blank(), panel.grid.major = element_blank())
p.size.muc


library(cowplot)
plot_grid(p.start.district, p.reside,
          p.houses, p.size.muc, 
          labels=c("A", "B", "C", "D"), 
          nrow=2)
ggsave(here_maps_out("Epi-Figure2b-200930.pdf"), height=8, width=10)

#------------------------------------------------------------

# Figure 2d
# 368 constituencies finally sampled, 
# visualization of number of households included via colour scheme

d.reside <- hh.d %>%
  group_by(ConstIdResideStr) %>%
  summarize(num.hh = n()) %>%
  rename(cid.5 = ConstIdResideStr)
s.hh.reside <- left_join(S, d.reside) %>%
  mutate(hh.present = ifelse(is.na(num.hh), FALSE, TRUE)) 

p.reside <- ggplot() +
  geom_sf(data = s.hh.reside, size = 0.1, color = "grey", aes(fill = num.hh)) +
  coord_sf() +
  scale_fill_continuous_sequential(palette = "Viridis", na.value="white",
                                   name = "Number of Households") + 
  theme_bw() +
  theme(legend.position = "bottom")
p.reside


# Fig 2e
# 368 constituencies finally sampled, 
# visualization of number of participants included via colour scheme

ind <- ind.orig %>%
  as_tibble() %>%
  mutate(HouseholdSize = as.integer(as.numeric(HouseholdSize)),
         duplicated_hh = as.logical(duplicated_hh)) %>%
  left_join(select(hh, hh_id, ConstIdReside))

dim(ind)
length(unique(ind$hh_id))

# Num participants per constituency
hh.reside <- ind %>%
  group_by(ConstIdReside) %>%
  summarize(num.hh = n()) %>%
  rename(Constituency_ID = ConstIdReside)
s.hh.reside <- left_join(S, hh.reside) %>%
  mutate(hh.present = ifelse(is.na(num.hh), FALSE, TRUE)) 

ggplot() +
  geom_sf(data = s.hh.reside, size = 0.1, color = "grey", aes(fill = num.hh)) +
  coord_sf() +
  scale_fill_continuous_sequential(palette = "Viridis", na.value="white",
                                   name = "Number of participants") + 
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(here_maps_out("Epi-Fig2e.pdf"), height=6, width=8)
ggsave(here_maps_out("Epi-Fig2e.png"), height=6, width=8)

# Fig 2f
# 368 constituencies finally sampled, 
# visualization of average household size via colour scheme
# Average HouseholdSize per constituency
hh.reside <- hh.d %>%
  group_by(ConstIdResideStr) %>%
  summarize(num.hh = mean(HouseholdSize, na.rm=TRUE)) %>%
  rename(cid.5 = ConstIdResideStr)
s.hh.reside <- left_join(S, hh.reside) %>%
  mutate(hh.present = ifelse(is.na(num.hh), FALSE, TRUE)) 

ggplot() +
  geom_sf(data = s.hh.reside, size = 0.1, color = "grey", aes(fill = num.hh)) +
  coord_sf() +
  scale_fill_continuous_sequential(palette = "Viridis", na.value="white",
                                   name = "Average household size") + 
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(here_maps_out("Epi-Fig2f.pdf"), height=6, width=8)
ggsave(here_maps_out("Epi-Fig2f.png"), height=6, width=8)








# Fig 2e
# Euroimmun IgG, continuous variables and positivity, 
# maybe with smaller tiles via colour scheme 
# (similar to slide 27 but with more exact geo data) 
# or density map (similar to slide 28b but with more exact geo data)


# Fig 2f
# Roche, continuous variables and positivity, 
# maybe with smaller tiles via colour scheme 
# (similar to slide 27 but with more exact geo data) 
# or density map (similar to slide 28b but with more exact geo data)













# Improve maps with district colors
# For start
p.start.district <- ggplot(data = s.hh.start, 
       aes(fill=District)) +
  geom_sf(size = 0, data = filter(s.hh.start, !hh.present), alpha=0.2) +
  geom_sf(size = 0, data = filter(s.hh.start, hh.present)) +
  coord_sf() +
  theme_bw() +
  theme(legend.position = "none")
p.start.district

# Original of constituency per district
p.start.district <- ggplot(data = s.hh.start, 
                           aes(fill = District)) +
  geom_sf(size = 0, data = filter(s.hh.start, !hh.present), alpha=0.2) +
  geom_sf(size = 0, data = filter(s.hh.start, hh.present)) +
  coord_sf() +
  theme_bw() +
  theme(legend.position = "none")
p.start.district


# Checkings
table(s.hh.start$District, s.hh.start$hh.present)
table(s.hh.reside$District, s.hh.reside$hh.present)
table(s.hh.start$hh.present)
table(s.hh.reside$hh.present)


############################################# Map with arrows
d.pre.post <- hh.d %>%
  group_by(ConstIdStart, ConstIdReside) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  group_by(ConstIdStart) %>%
  mutate(p = n / sum(n) * 100)

# Percentage HH in the same constituency at start and reside
d.pre.post %>%
#  left_join(dc, by = c("ConstIdStart" = "Constituency_ID")) %>%
  filter(ConstIdStart == ConstIdReside) %>%
  ggplot(aes(x=p, y=reorder(ConstIdStart, desc(p)))) +
  geom_point() +
  theme_bw() +
  xlab("Households located in the same Constituency as start (%)") + 
  ylab("")
#ggsave(here_maps_out("Proportion-hh-same-constituency.pdf"), width=8, height=6)

# By districts
d.pre.post %>%
  left_join(dc, by = c("ConstIdStart" = "cid.4")) %>%
  filter(ConstIdStart == ConstIdReside) %>%
  ggplot(aes(x=p, y=reorder(ConstIdStart, desc(p)))) +
  geom_point() +
  theme_bw() +
  xlab("Households located in the same Constituency as expected (%)") + 
  ylab("") +
  facet_wrap(~District, scales="free_y") 
#ggsave(here_maps_out("Proportion-hh-same-constituency-district.pdf"), width=8, height=6)

# Arrange the data from sf() into another system, to be able to merge twice
# based on the starting an finishing point
# Do it both for centroids and also for the polygons
S.polygons <- st_coordinates(S)

# This assumes that the position is held constant
S.coords <- as.data.frame(st_coordinates(st_centroid(S))) %>%
  as_tibble() %>%
  bind_cols(cid.4 = S$cid.4) %>%
  rename(lon = Y, lat = X)

S.coords.polygons <- as.data.frame(st_coordinates(S)) %>%
  as_tibble() %>%
  rename(lat = Y, lon = X, id = L2)

match.id.constit <- tibble(id = unique(S.coords.polygons$id),
                           cid.4 = S$cid.4)

S.coords.polygons <- S.coords.polygons %>%
  left_join(match.id.constit)

S.coords.polygons.centroids <- S.coords.polygons %>%
  group_by(cid.4) %>%
  summarize(lat = mean(lat), lon = mean(lon))

S.pre.post <- d.pre.post %>%
  # For the origin of the constituency, make a join
  left_join(S.coords.polygons.centroids, by = c("ConstIdStart" = "cid.4")) %>%
  # For the destination, make another join, now with a different variable
  left_join(S.coords.polygons.centroids, by = c("ConstIdReside" = "cid.4"))

ggplot(S.coords.polygons, aes(x = lon, y = lat, group = cid.4)) +
  geom_polygon(fill = "white", color = "grey") +
  theme_bw()


d.pre.post.geo <- d.pre.post %>%
  # For the origin of the constituency, make a join
  left_join(S.coords, by = c("ConstIdStart" = "cid.4")) %>%
  # For the destination, make another join, now with a different variable
  left_join(S.coords, by = c("ConstIdReside" = "cid.4")) %>%
  left_join(match.id.constit, by = c("ConstIdReside" = "cid.4"))


ggplot() +
  geom_polygon(data = S.coords.polygons, aes(x = lon, y = lat, 
                                             group = cid.4), 
                                             fill = "white", 
                                             color = "grey",
                                             size = 0.1) +
  geom_point(data = filter(d.pre.post.geo, ConstIdStart == ConstIdReside),
             aes(x=lat.x, y = lon.x, size = p, color = as.factor(ConstIdStart))) +
  theme_bw() +
  scale_color_discrete(guide = "none") +
  scale_size_continuous(name = "HH same const. (%)")
#ggsave(here_maps_out("Sampling-proportion-same-constituency.pdf"), height=6, width=10)


ggplot() +
  geom_polygon(data = S.coords.polygons, aes(x = lon, y = lat, 
                                             group = cid.4), 
               fill = "white", 
               color = "grey",
               size = 0.1) +
  geom_segment(data = d.pre.post.geo,
               aes(x = lat.x, y = lon.x,
                   xend = lat.y, yend = lon.y,
                   color = as.factor(ConstIdStart)),#,
               #alpha = p),
               arrow = arrow(length = unit(0.01, "npc"))) +
  geom_point(data = filter(d.pre.post.geo, ConstIdStart == ConstIdReside),
             aes(x=lat.x, y = lon.x, color = as.factor(ConstIdStart), size = p)) +
  theme_bw() +
  theme(legend.position = "none")

#ggsave(here_maps_out("Sampling-Theoretical-Real-200922.pdf"), height=6, width=10)
#ggsave(here_maps_out("Sampling-Theoretical-Real-200914.pdf"), height=6, width=10)

ggplot() +
  geom_polygon(data = S.coords.polygons, aes(x = lon, y = lat, 
                                             group = cid.4), 
               fill = "white", 
               color = "grey",
               size = 0.1) +
  geom_segment(data = d.pre.post.geo,
               aes(x = lat.x, y = lon.x,
                   xend = lat.y, yend = lon.y,
                   color = as.factor(ConstIdStart)),#,
               #alpha = p),
               arrow = arrow(length = unit(0.01, "npc"))) +
  theme_bw() +
  theme(legend.position = "none")
ggsave(here_maps_out("Sampling-Theoretical-Real-without-points-200922.pdf"), height=6, width=10)
ggsave(here_maps_out("Sampling-Theoretical-Real-without-points-200914.pdf"), height=6, width=10)




####################### Detect IDs long arrows
ggplot() +
  geom_polygon(data = S.coords.polygons, aes(x = lon, y = lat, 
                                             group = cid.4), 
               fill = "white", 
               color = "grey",
               size = 0.1) +
  geom_segment(data = d.pre.post.geo,
               aes(x = lat.x, y = lon.x,
                   xend = lat.y, yend = lon.y,
                   color = as.factor(ConstIdStart)),#,
               #alpha = p),
               arrow = arrow(length = unit(0.01, "npc"))) +
  geom_text(data = d.pre.post.geo,
            aes(x = lat.x, y = lon.x,
                label = as.factor(ConstIdStart))) +
  theme_bw() +
  theme(legend.position = "none")

ggplot() +
  geom_polygon(data = S.coords.polygons, aes(x = lon, y = lat, 
                                             group = cid.4), 
               fill = "white", 
               color = "grey",
               size = 0.1) +
  geom_segment(data = d.pre.post.geo,
               aes(x = lat.x, y = lon.x,
                   xend = lat.y, yend = lon.y,
                   color = as.factor(ConstIdStart)),#,
               #alpha = p),
               arrow = arrow(length = unit(0.01, "npc"))) +
  geom_text(data = d.pre.post.geo, aes(x = lat.y, y = lon.y,
                label = as.factor(ConstIdReside))) +
  theme_bw() +
  theme(legend.position = "none")



hh.d %>%
  filter(ConstIdReside %in% 311)
hh.d %>%
  filter(ConstIdReside %in% 203)
hh.d %>%
  filter(ConstIdReside %in% "1923")
hh.d %>%
  filter(ConstIdReside %in% "1234")
hh.d %>%
  filter(ConstIdReside %in% "923")
hh.d %>%
  filter(ConstIdReside %in% "923")

# 5145B00
# 4548G00
# 4800R00
# 4664A00
# 3904G00



ggplot() +
  geom_polygon(data = S.coords.polygons, aes(x = lon, y = lat, 
                                             group = cid.4), 
               fill = "white", 
               color = "grey",
               size = 0.1) +
  geom_segment(data = d.pre.post.geo,
               aes(x = lat.x, y = lon.x,
                   xend = lat.y, yend = lon.y,
                   color = as.factor(ConstIdStart)),#,
               #alpha = p),
               arrow = arrow(length = unit(0.01, "npc"))) +
  geom_text(data = d.pre.post.geo %>%
              select(ConstIdStart, lon.x, lat.x) %>%
              unique(), 
            aes(x = lat.x, y = lon.x,
                label = as.factor(ConstIdStart)),
            color="black") +
  geom_text(data = d.pre.post.geo %>%
              select(ConstIdReside, lon.y, lat.y) %>%
              unique(), 
            aes(x = lat.y, y = lon.y,
                label = as.factor(ConstIdReside)),
            color="grey30") +
  theme_bw() +
  theme(legend.position = "none")
ggsave(here_maps_out("Localization-IDs-map.pdf"), width = 30, height = 30)


d.pre.post.geo.2 <- d.pre.post.geo %>%
  mutate(select = ifelse(ConstIdStart == 2506 & ConstIdReside == 311,
                         "Const1",
                  ifelse(ConstIdStart == 1011 & ConstIdReside == 203,
                                "Const2",
                  ifelse(ConstIdStart == 223 & ConstIdReside == 1923,
                         "Const3",
                  ifelse(ConstIdStart == 1618 & ConstIdReside == 1234,
                                "Const4",
                  ifelse(ConstIdStart == 427 & ConstIdReside == 923,
                                "Const5", "NA"))))))

ggplot() +
  geom_polygon(data = S.coords.polygons, aes(x = lon, y = lat, 
                                             group = cid.4), 
               fill = "white", 
               color = "grey",
               size = 0.1) +
  geom_segment(data = d.pre.post.geo.2,
               aes(x = lat.x, y = lon.x,
                   xend = lat.y, yend = lon.y,
                   color = as.factor(ConstIdStart)),#,
               #alpha = p),
               arrow = arrow(length = unit(0.01, "npc"))) +
  geom_segment(data = filter(d.pre.post.geo.2,
                             select != "NA"),
               aes(x = lat.x, y = lon.x,
    xend = lat.y, yend = lon.y),
    color = "black",
    arrow = arrow(length = unit(0.01, "npc"))) +
  geom_text(data = d.pre.post.geo.2 %>%
              select(ConstIdStart, lon.x, lat.x) %>%
              unique(), 
            aes(x = lat.x, y = lon.x,
                label = as.factor(ConstIdStart)),
            color="black") +
  geom_text(data = d.pre.post.geo.2 %>%
              select(ConstIdReside, lon.y, lat.y) %>%
              unique(), 
            aes(x = lat.y, y = lon.y,
                label = as.factor(ConstIdReside)),
            color="grey30") +
  theme_bw() +
  theme(legend.position = "none")
ggsave(here_maps_out("Localization-IDs-map-long-arrows.pdf"), width = 30, height = 30)


head(hh.orig)
dim(hh.orig)


library(geosphere)
dist.geo <- d.pre.post.geo %>%
  mutate(distance = distm(c(lon.x, lat.x), c(lon.y, lat.y), fun = distGeo))

test <- d.pre.post.geo[1,]
distm(c(test$lon.x, test$lat.x), 
       c(test$lon.y, test$lat.y), 
       fun = distHaversine())
distGeo(c(test$lon.x, test$lat.x), 
      c(test$lon.y, test$lat.y))

##############################################################


# Num HH reside
hh.reside <- hh %>%
  group_by(ConstIdReside) %>%
  summarize(num.hh = n()) %>%
  rename(Constituency_ID = ConstIdReside)
s.hh.reside <- left_join(S, hh.reside) %>%
  mutate(hh.present = ifelse(is.na(num.hh), FALSE, TRUE)) 

ggplot() +
  geom_sf(data = s.hh.reside, size = 0.1, color = "grey", aes(fill = num.hh)) +
  coord_sf() +
  scale_fill_continuous_sequential(palette = "Viridis", na.value="white",
                                   name = "Num. HH") + 
  theme_bw() +
  ggtitle("Number of households sampled")
ggsave(here_maps_out("Households-sampled.pdf"), height=6, width=10)



# Average Number of Rooms per constituency (as a proxy of Big houses??)
hh.reside <- hh %>%
  group_by(ConstIdReside) %>%
  summarize(num.hh = mean(as.numeric(num_rooms), na.rm=TRUE)) %>%
  rename(Constituency_ID = ConstIdReside)
s.hh.reside <- left_join(S, hh.reside)

ggplot() +
  geom_sf(data = s.hh.reside, size = 0.1, color = "grey", aes(fill = num.hh)) +
  coord_sf() +
  scale_fill_continuous_sequential(palette = "Viridis", na.value="white",
                                   name = "Num. rooms") + 
  theme_bw() +
  ggtitle("Average number of rooms")
ggsave(here_maps_out("HH-Num-Rooms.pdf"), height=6, width=10)

#------------------------------------------------------------------ Ind
# Load ind characteristics dataset
ind.orig <- read.csv(here_koco_data("Analysis Data Sets", "ind_characteristics_new.csv"), 
                    header = TRUE, na.string = c("-9999", "NA"))
ind.orig <- ind.orig[,-1]
head(ind.orig)

ind <- ind.orig %>%
  as_tibble() %>%
  mutate(HouseholdSize = as.integer(as.numeric(HouseholdSize)),
         duplicated_hh = as.logical(duplicated_hh)) %>%
  left_join(select(hh, hh_id, ConstIdReside))

dim(ind)
length(unique(ind$hh_id))


# Num participants per constituency
hh.reside <- ind %>%
  group_by(ConstIdReside) %>%
  summarize(num.hh = n()) %>%
  rename(Constituency_ID = ConstIdReside)
s.hh.reside <- left_join(S, hh.reside) %>%
  mutate(hh.present = ifelse(is.na(num.hh), FALSE, TRUE)) 

ggplot() +
  geom_sf(data = s.hh.reside, size = 0.1, color = "grey", aes(fill = num.hh)) +
  coord_sf() +
  scale_fill_continuous_sequential(palette = "Viridis", na.value="white",
                                   name = "Num. Individuals") + 
  theme_bw() +
  ggtitle("Number of individuals sampled")
ggsave(here_maps_out("Num-individuals.pdf"), height=6, width=10)

# Average age per constituency
hh.reside <- ind %>%
  group_by(ConstIdReside) %>%
  summarize(num.hh = mean(Age, na.rm=TRUE)) %>%
  rename(Constituency_ID = ConstIdReside)
s.hh.reside <- left_join(S, hh.reside) 

ggplot() +
  geom_sf(data = s.hh.reside, size = 0.1, color = "grey", aes(fill = num.hh)) +
  coord_sf() +
  scale_fill_continuous_sequential(palette = "Viridis", na.value="white",
                                   name = "Age") + 
  theme_bw() +
  ggtitle("Average age")
ggsave(here_maps_out("Age-mean.pdf"), height=6, width=10)

# Percentage women per constituency
hh.reside <- ind %>%
  group_by(ConstIdReside) %>%
  summarize(num.hh = (length(which(Sex == "Female")) / length(which(!is.na(Sex))) * 100),
            size = length(which(!is.na(Sex)))) %>%
  rename(Constituency_ID = ConstIdReside)
s.hh.reside <- left_join(S, hh.reside) 

ggplot() +
  geom_sf(data = s.hh.reside, size = 0.1, color = "grey", aes(fill = num.hh)) +
  coord_sf() +
  scale_fill_continuous_diverging(palette = "Blue-Red", na.value="white",
                                  name = "Women (%)", mid = 50) + 
  theme_bw() +
  ggtitle("Percentage of women")
ggsave(here_maps_out("Women-percentage.pdf"), height=6, width=10)

ggplot(hh.reside, aes(x=size, y=num.hh)) +
  geom_jitter(alpha=0.5) +
  theme_bw() +
  ylab("Proportion of women (%)") + 
  xlab("Sample size at each constituency")
ggsave(here_maps_out("Women-percentage-plot.pdf"), height=4, width=6)

# Percentage children per constituency
hh.reside <- ind %>%
  group_by(ConstIdReside) %>%
  summarize(num.hh = (length(which(Age <= 14)) / length(which(!is.na(Age))) * 100),
            size = length(which(!is.na(Age)))) %>%
  rename(Constituency_ID = ConstIdReside)
s.hh.reside <- left_join(S, hh.reside) 

ggplot() +
  geom_sf(data = s.hh.reside, size = 0.1, color = "grey", aes(fill = num.hh)) +
  coord_sf() +
  scale_fill_continuous_sequential(palette = "Viridis", na.value="white",
                                   name = "Children (%)") + 
  theme_bw() +
  ggtitle("Percentage of children (<14 years)")
ggsave(here_maps_out("Children-percentage.pdf"), height=6, width=10)

ggplot(hh.reside, aes(x=size, y=num.hh)) +
  geom_jitter(alpha=0.5) +
  theme_bw() +
  ylab("Proportion of children (%)") + 
  xlab("Sample size at each constituency")
ggsave(here_maps_out("Children-percentage-plot.pdf"), height=4, width=6)

#------------------------------------------------------------------ Lab
# Load ind characteristics dataset
lab.orig <- read.csv(here_koco_data("Analysis Data Sets", "ind_lab_baseline_new.csv"),
                     header = TRUE, na.string = c("-9999", "NA"))
lab.orig <- lab.orig[,-1]
dim(lab.orig)

lab <- lab.orig %>%
  as_tibble() %>%
  left_join(select(hh, hh_id, ConstIdReside))

dim(lab)
length(unique(lab$hh_id))


# Calculate prevalence
prev.const <- lab %>%
  select(ind_id, hh_id, IgA_result, IgG_result, R_Result) %>%
  mutate(IgA = ifelse(IgA_result == "Positive", "Positive", "Negative"),
         IgG = ifelse(IgG_result == "Positive", "Positive", "Negative"),
         Roche = ifelse(R_Result == "reactive", "Positive", "Negative")) %>%
  select(-IgA_result, -IgG_result, -R_Result) %>%
  gather(Test, value, -ind_id, -hh_id) %>%
  left_join(select(hh, hh_id, ConstIdReside)) %>%
  group_by(ConstIdReside, Test) %>%
  summarize(Prevalence = ((length(which(value == "Positive")) / length(which(!is.na(value)))) * 100)) %>%
  spread(Test, Prevalence) %>%
  rename(Constituency_ID = ConstIdReside)

         
hist(prev.const$IgA)  
hist(prev.const$IgG)
hist(prev.const$Roche)

s.prev.const <- left_join(S, prev.const)

# Prevalence IgA
ggplot() +
  geom_sf(data = s.prev.const, 
          size = 0.1, color = "grey", aes(fill = IgA)) +
  coord_sf() +
  scale_fill_continuous_sequential(palette = "Viridis", na.value="white",
                                   name = "Prevalence IgA") + 
  theme_bw() +
  ggtitle("Prevalence IgA (irrespective of sample size)")
ggsave(here_maps_out("Prevalence-IgA.pdf"), height=6, width=10)

# Prevalence IgG
ggplot() +
  geom_sf(data = s.prev.const, 
          size = 0.1, color = "grey", aes(fill = IgG)) +
  coord_sf() +
  scale_fill_continuous_sequential(palette = "Viridis", na.value="white",
                                   name = "Prevalence IgG") + 
  theme_bw() +
  ggtitle("Prevalence IgG (irrespective of sample size)")
ggsave(here_maps_out("Prevalence-IgG.pdf"), height=6, width=10)

# Prevalence Roche
ggplot() +
  geom_sf(data = s.prev.const, 
          size = 0.1, color = "grey", aes(fill = Roche)) +
  coord_sf() +
  scale_fill_continuous_sequential(palette = "Viridis", na.value="white",
                                   name = "Prevalence Roche") + 
  theme_bw() +
  ggtitle("Prevalence Roche (irrespective of sample size)")
ggsave(here_maps_out("Prevalence-Roche.pdf"), height=6, width=10)

# Prevalence Roche with number of individuals.
# Use centroids instead of polygons for the map circles
# Centroids are in S.coords.polygons.centroids

# Calculate prevalence
prev.roche.size <- lab %>%
  mutate(Roche = ifelse(R_Result == "reactive", "Positive", "Negative")) %>%
  select(hh_id, Roche) %>%
  left_join(select(hh, hh_id, ConstIdReside)) %>%
  group_by(ConstIdReside) %>%
  summarize(Prevalence = ((length(which(Roche == "Positive")) / length(which(!is.na(Roche)))) * 100),
            Size = n(),
            Size.Roche = length(which(!is.na(Roche)))) %>%
  rename(Constituency_ID = ConstIdReside)

S.prev.roche.size <- prev.roche.size %>%
  left_join(S.coords.polygons.centroids)

ggplot() +
  geom_polygon(data = S.coords.polygons, aes(x = lon, y = lat, 
                                             group = Constituency_ID), 
               fill = "white", 
               color = "grey",
               size = 0.1) +
  geom_point(data = S.prev.roche.size,
             aes(y=lat, x = lon, size = Size.Roche, color = Prevalence)) +
  theme_bw() +
  ggtitle("Roche prevalence (%) and sample size (n)")
ggsave(here_maps_out("Prevalence-Roche-sample-size.pdf"), height=6, width=10)

# Showing prevalence by constituency is misleading, because it does
# not show prevalence but rare events in small sample size constituencies.
ggplot(prev.roche.size, aes(x=Size.Roche, y=Prevalence)) +
  geom_jitter(alpha=0.5) +
  theme_bw() +
  ylab("Prevalence Roche (%)") + xlab("Sample size at each constituency")
ggsave(here_maps_out("Prevalence-Roche-sample-size-plot.pdf"), height=4, width=6)

# If we want a geographical pattern, it is better to work with higher sample
# sizes (by district instead of by constituency) or some sort of geospatial
# model.

# Calculate prevalence
prev.roche.size.district <- lab %>%
  mutate(Roche = ifelse(R_Result == "reactive", "Positive", "Negative")) %>%
  select(hh_id, Roche) %>%
  left_join(select(hh, hh_id, ConstIdReside)) %>%
  rename(Constituency_ID = ConstIdReside) %>%
  left_join(dc) %>%
  group_by(District) %>%
  summarize(Prevalence = ((length(which(Roche == "Positive")) / length(which(!is.na(Roche)))) * 100),
            Size = n(),
            Size.Roche = length(which(!is.na(Roche))))
  
ggplot(prev.roche.size.district, aes(x=Size.Roche, y=Prevalence)) +
  geom_jitter(alpha=0.5) +
  theme_bw() +
  ylab("Prevalence Roche (%)") + xlab("Sample Size at each district")
ggsave(here_maps_out("Prevalence-Roche-district-plot.pdf"), height=4, width=6)
# Now we don't see anymore a clear pattern between size and prevalence


map.m <- map_data(m.sh)
rdp <- tibble(District = attributes(m.sh)$data$neighbourhood) %>%
  mutate(region = as.character(0:(n()-1)))

map.centroids <- map.m %>%
  left_join(rdp) %>%
  group_by(District) %>%
  summarize(long = mean(range(long)), lat = mean(range(lat))) %>%
  left_join(prev.roche.size.district)


ggplot(map.m, aes(x = long, y = lat)) +
  geom_polygon(aes(group = region), fill = "white", color = "grey") +
  geom_point(data = map.centroids, 
             aes(y=lat, x = long, size = Size.Roche, color = Prevalence)) +
  coord_fixed() +
  theme_bw() +
  ggtitle("Roche prevalence (%) and sample size (n)")
ggsave(here_maps_out("Prevalence-Roche-district.pdf"), height=6, width=10)


# Density estimation map of prevalence for Roche
# Unfortunately, we don't know the exact spot of the results, but only
# the constituencies. Maybe later we can do that with the geospatial 
# location. Now we work with constituencies.


# Calculate prevalence
prev.roche.ind <- lab %>%
  mutate(Roche = ifelse(R_Result == "reactive", 1, 0)) %>%
  filter(!is.na(Roche)) %>%
  select(hh_id, Roche) %>%
  left_join(select(hh, hh_id, ConstIdReside)) %>%
  rename(Constituency_ID = ConstIdReside)

S.prev.roche.ind <- prev.roche.ind %>%
  left_join(S.coords.polygons.centroids)

ggplot() +
  geom_polygon(data = S.coords.polygons, aes(x = lon, y = lat, 
                                             group = Constituency_ID), 
               fill = "white", 
               color = "grey",
               size = 0.1) +
   theme_bw() +
  ggtitle("Sampling density estimation") +
  #geom_density2d(data = S.prev.roche.ind, aes(x = lon, y = lat), size = 0.3) +
  stat_density2d(data = S.prev.roche.ind, aes(x = lon, y = lat, fill = ..level.., 
                                              alpha = ..level..), size = 0.01,
                 bins = 16, geom = "polygon") +
  theme(legend.position = "none")
# Light blue means higher individuals sampled.
ggsave(here_maps_out("Sampling-density-estimation.pdf"), height=6, width=10)

ggplot() +
  geom_polygon(data = S.coords.polygons, aes(x = lon, y = lat, 
                                             group = Constituency_ID), 
               fill = "white", color = "grey", size = 0.1) +
  theme_bw() +
  ggtitle("Roche positive density estimation (tiles)") +
  stat_summary_2d(data = S.prev.roche.ind, aes(x = lon, y = lat, z = Roche)) +
  theme(legend.position = "none") +
  scale_fill_gradient(low = "yellow", high = "red", guide=FALSE) +
  scale_alpha(range = c(0.02, 0.8), guide = FALSE)
ggsave(here_maps_out("Density-estimation-roche-positive-tiles.pdf"), height=6, width=10)
# Red means higher rate of positives taking into account sample size.
  
ggplot() +
  geom_polygon(data = S.coords.polygons, aes(x = lon, y = lat, 
                                             group = Constituency_ID), 
               fill = "white", color = "grey", size = 0.1) +
  theme_bw() +
  ggtitle("Roche density estimation for negatives (0) and positives (1)") +
  stat_density_2d(data = S.prev.roche.ind, geom = "polygon", bins = 30,
                  aes(x = lon, y = lat, alpha = ..level.., fill = ..level..)) +
  theme(legend.position = "none") +
  facet_wrap(~Roche) +
  scale_fill_gradient(low = "yellow", high = "red", guide=FALSE) +
  scale_alpha(range = c(0.02, 0.8), guide = FALSE)
ggsave(here_maps_out("Density-estimation-roche-pos-neg.pdf"), height=4, width=10)

# With more precise location of participants, those maps would be more informative,
# because the density estimation would be based on a specific dot and not on the
# constituency centroid level.
  
  