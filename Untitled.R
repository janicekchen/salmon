# DATA TIDYING

library(tidyverse)
library(magrittr)
library(tidygeocoder)
library(tigris)
library(sf)
library(geosphere)
library(leaflet)
library(rnaturalearth)

# read in data
import <- read.csv("data/processed/salmon_imports.csv") %>%
  select(1:14)
export <- read.csv("data/processed/salmon_exports.csv")
ports <- read.csv("data/processed/salmon_ports.csv")

# renaming columns
names(import) <- c("year", "hs10", "hs10_formatted", "hs10_datekey", "hs10_desc", "country_origin", "country_origin_datekey", "us_stateorigin", "clearance_province", "clearance_port", "uom", "uom_datekey", "quant", "value")

names(export) <- c("year", "hs8", "hs8_formatted", "hs8_datekey", "hs8_desc", "country_dest", "country_dest_datekey", "us_statedest", "origin_province", "clearance_province", "clearance_port", "uom", "uom_datekey", "quant", "value")

names(ports) <- c("port_code", "start_date", "end_date", "port_eng", "port_fr")

# removing commas and $ signs from quant and value columns and converting to numeric

df_list <- list(export, import)
df_list2 <- lapply(df_list, function(x) {
  x$quant <- as.numeric(gsub(",", "", x$quant))
  x$value <- gsub(",", "", x$value)
  x$value <- as.numeric(gsub("\\$", "", x$value))
  return(x)
})

export <- df_list2[[1]]
import <- df_list2[[2]]

# start with all U.S. ports, geocode, and then geocode the other places, and then rbind
export_us <- export %>%
  filter(country_dest == "United States")

import_us <- import %>%
  filter(country_origin == "United States")

# summarize by year, departure and arrival
export_sum <- export_us %>%
  group_by(year, us_statedest, clearance_port) %>%
  summarize(quant = sum(quant), value = sum(value))

export_sum$us_statedest <- gsub(", State", "", export_sum$us_statedest) # converting "Washington, State" to "Washington to match centroid data frame)

import_sum <- import_us %>%
  group_by(year, us_stateorigin, clearance_port) %>%
  summarize(quant = sum(quant), value = sum(value))

import_sum$us_stateorigin <- gsub(", State", "", import_sum$us_stateorigin)

# STATE CENTROIDS
# creating centroid data frame of US states
us_states <- states(cb = TRUE, resolution = "20m") %>%
  mutate(lon = st_coordinates(st_centroid(.))[, 1], lat = st_coordinates(st_centroid(.))[, 2]) %>%
  select(c(NAME, lon, lat)) %>%
  st_set_geometry(NULL)

# joining centroid data frame to export/import data frame
import_sum %<>% left_join(us_states, by = c("us_stateorigin" = "NAME"))
names(import_sum)[6:7] <- c("or_lon", "or_lat")

export_sum %<>% left_join(us_states, by = c("us_statedest" = "NAME"))
names(export_sum)[6:7] <- c("dest_lon", "dest_lat")

# MATCHING PORT COORDINATES
# creating list of unique ports
unique_ports <- unique(c(import_sum$clearance_port, export_sum$clearance_port))
# filtering Canadian ports to this list
# ports %<>% filter(port_code %in% unique_ports)
# write.csv(ports, "data/processed/ports_filtered.csv") # write out to geocode manually

ports_filt <- read.csv("data/processed/ports_filtered.csv")
ports_filt <- ports_filt[, c(1:4, 6, 5)]

import_sum %<>% left_join(ports_filt[c(1, 5:6)], by = c("clearance_port" = "port_code"))
names(import_sum)[8:9] <- c("dest_lon", "dest_lat")

export_sum %<>% left_join(ports_filt[c(1, 5:6)], by = c("clearance_port" = "port_code"))
names(export_sum)[8:9] <- c("or_lon", "or_lat")
  
names(import_sum)[c(2, 4:5)] <- c("us_state", "imp_quant", "imp_value")
names(export_sum)[c(2, 4:5)] <- c("us_state", "exp_quant", "exp_value")

net <- full_join(import_sum[1:5], export_sum[1:5], 
                 by = c("year", "us_state", "clearance_port")) %>%
  left_join(us_states, by = c("us_state" = "NAME")) %>%
  left_join(ports_filt[c(1, 4:6)], by = c("clearance_port" = "port_code"))

names(net)[c(8:9, 11:12)] <- c("state_lon", "state_lat", "port_lon", "port_lat")
net %<>% mutate_at(c(1:7), ~replace(., is.na(.), 0))
write.csv(net, "data/processed/net_salmon.csv")


# doing world things now
export_world <- export %>%
  filter(country_dest != "United States")

import_world <- import %>%
  filter(country_origin != "United States")

export_worldsum <- export_world %>%
  group_by(year, country_dest, clearance_port) %>%
  summarize(quant = sum(quant), value = sum(value))

names(export_worldsum)[c(2, 4:5)] <- c("country", "exp_quant", "exp_value") # changing column name to make net data frame later

import_worldsum <- import_world %>%
  group_by(year, country_origin, clearance_port) %>%
  summarize(quant = sum(quant), value = sum(value))

names(import_worldsum)[c(2, 4:5)] <- c("country", "imp_quant", "imp_value")


# retrieving country boundaries & creating data frame of country centroids
countries <- ne_countries(returnclass = "sf")
countries_cent <- st_coordinates(st_centroid(countries))  %>% 
  cbind(countries["name_long"]) %>%
  st_set_geometry(NULL)

# filtering ports and geocoding manually
unique_portsworld <- unique(c(import_worldsum$clearance_port, export_worldsum$clearance_port))
# ports %<>% filter(port_code %in% unique_portsworld)
# write.csv(ports, "data/processed/ports_filteredworld.csv") # write out to geocode manually
ports_filtworld <- read.csv("data/processed/ports_filteredworld.csv")


# creating net trade data frame
net_world <- full_join(import_worldsum, export_worldsum, by = c("year", "country", "clearance_port")) %>%
  left_join(countries_cent, by = c("country" = "name_long")) %>%
  left_join(ports_filtworld[c(1, 4:6)], by = c("clearance_port" = "port_code"))

# some countries have not been matched with coordinates. finding NA rows.
nwNA <- net_world %>%
  filter(is.na(X))
uniqueNA <- unique(nwNA$country)

net_world <- full_join(import_worldsum, export_worldsum, by = c("year", "country", "clearance_port"))

net_world$country <- gsub("Viet Nam", "Vietnam", net_world$country)
countries_cent$name_long <- gsub("Republic of Korea", "South Korea", countries_cent$name_long)
net_world$country <- gsub("Korea, South", "South Korea", net_world$country)
net_world$country <- gsub("Congo, Republic of the", "Republic of Congo", net_world$country)
net_world$country <- gsub("Macedonia, North", "North Macedonia", net_world$country)
countries_cent$name_long <- gsub("Macedonia", "North Macedonia", countries_cent$name_long)
countries_cent$name_long <- gsub("Lao PDR", "Laos", countries_cent$name_long)
net_world$country <- gsub("Moldova, Republic of", "Moldova", net_world$country)
countries_cent$name_long <- gsub("Swaziland", "Eswatini", countries_cent$name_long)
net_world$country <- gsub("Moldova, Republic of", "Moldova", net_world$country)
net_world$country <- gsub("South Africa, Republic of", "South Africa", net_world$country)

# run data frame join again
net_world %<>%
  left_join(countries_cent, by = c("country" = "name_long")) %>%
  left_join(ports_filtworld[c(1, 4:6)], by = c("clearance_port" = "port_code"))

nwNA <- net_world %>%
  filter(is.na(X))
uniqueNA <- unique(nwNA$country)

net_world[net_world$country == "Faroe Islands", 8] <- -6.9118
net_world[net_world$country == "Faroe Islands", 9] <- 61.8926
net_world[net_world$country == "Hong Kong", 8] <- 114.1694
net_world[net_world$country == "Hong Kong", 9] <- 22.3193
net_world[net_world$country == "Singapore", 8] <- 103.8198
net_world[net_world$country == "Singapore", 9] <- 1.3521
net_world[net_world$country == "Netherlands Antilles", 8] <- -69.060087
net_world[net_world$country == "Netherlands Antilles", 9] <- 12.226079
net_world[net_world$country == "Bermuda", 8] <- -64.7505
net_world[net_world$country == "Bermuda", 9] <- 32.3078
net_world[net_world$country == "Macao", 8] <- 113.5439
net_world[net_world$country == "Macao", 9] <- 22.1987
net_world[net_world$country == "Cabo Verde", 8] <- -23.0418
net_world[net_world$country == "Cabo Verde", 9] <- 16.5388
# mapping
# flow <- function(x) {
#   gcIntermediate(as.data.frame(x)[complete.cases(x), 6:7], as.data.frame(x)[complete.cases(x), 8:9],
#                  sp = TRUE, addStartEnd = TRUE)}
# 
# df_list <- list(import_sum, export_sum)
# flow_list <- lapply(df_list, function(x) {
#   flow(x)
#   # for(i in 1999:2020) {
#   #   t <- x %>%
#   #     filter(year == i)
#   #   flows <- flow(t)
#   #   assign(paste0(x, i), flows, envir = .GlobalEnv)
#   # }
# })
# 
# 
# for(j in 2000:2020) {
#   t <- export_sum[complete.cases(export_sum), ] %>%
#     filter(year == j) 
#   flows <- flow(t) %>%
#     st_as_sf() %>%
#     cbind(t) %>%
#     st_as_sf() %>%
#     st_transform(4326)
# 
#   assign(paste0("export", j), flows, envir = .GlobalEnv)
# }
# 
# for(j in 2000:2020) {
#   t <- import_sum[complete.cases(import_sum), ] %>%
#     filter(year == j) 
#   flows <- flow(t) %>%
#     st_as_sf() %>%
#     cbind(t) %>%
#     st_as_sf() %>%
#     st_transform(4326)
#   
#   assign(paste0("import", j), flows, envir = .GlobalEnv)
# }
# 
# 
# 
# leaflet() %>%
#   addProviderTiles('CartoDB.Positron') %>%
#   addPolylines(data = export2019, color = "blue", weight = export2020$value / 5000000) %>%
#   addPolylines(data = import2019, color = "black", weight = import2020$value / 5000000) 
