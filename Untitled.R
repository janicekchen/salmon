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
us_states <- states(cb = TRUE, resolution = "20m")
state_cents <- data.frame(lon = st_coordinates(st_centroid(us_states))[, 1], lat = st_coordinates(st_centroid(us_states))[, 2])
us_states <- cbind(us_states, state_cents) %>%
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

# import_sum %<>% left_join(ports_filt[c(1, 4, 7:9)], by = c("clearance_port" = "port_code"))
# # names(import_sum)[8:9] <- c("dest_lon", "dest_lat")
# 
# export_sum %<>% left_join(ports_filt[c(1, 4, 7:9)], by = c("clearance_port" = "port_code"))
# # names(export_sum)[8:9] <- c("or_lon", "or_lat")
#   
names(import_sum)[c(2, 4:5)] <- c("us_state", "imp_quant", "imp_value")
names(export_sum)[c(2, 4:5)] <- c("us_state", "exp_quant", "exp_value")

net <- full_join(import_sum[1:5], export_sum[1:5], 
                 by = c("year", "us_state", "clearance_port")) %>%
  left_join(us_states, by = c("us_state" = "NAME")) %>%
  left_join(ports_filt[c(1, 4, 7:9)], by = c("clearance_port" = "port_code"))

names(net)[c(2, 8:9, 12:13)] <- c("state", "state_lon", "state_lat", "city_lat", "city_lon")
net %<>% mutate_at(c(1:7), ~replace(., is.na(.), 0))
# write.csv(net, "data/processed/net_salmon.csv")

# aggregating by city 
net_us_city <- net %>%
  group_by(year, state, city) %>%
  summarize(imp_quant = sum(imp_quant), imp_value = sum(imp_value), exp_quant = sum(exp_quant), exp_value = sum(exp_value), state_lon = first(state_lon), state_lat = first(state_lat), city_lon = first(city_lon), city_lat = first(city_lat))

# write.csv(net_us_city, "data/processed/netus_bycity.csv")

# WORLD DATA #
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
  left_join(ports_filtworld[c(1, 4, 7:9)], by = c("clearance_port" = "port_code"))

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
net_world <- net_world %>%
  left_join(countries_cent, by = c("country" = "name_long")) %>%
  left_join(ports_filtworld[c(1, 4, 7:9)], by = c("clearance_port" = "port_code"))

# nwNA <- net_world %>%
#   filter(is.na(X))
# uniqueNA <- unique(nwNA$country)

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

net_world %<>% mutate_at(c(1:7), ~replace(., is.na(.), 0))
names(net_world)[c(2, 8:9, 12:13)] <- c("state","state_lon", "state_lat", "city_lat", "city_lon")

# simplify by city to reduce clutter on map
net_world_city <- net_world %>%
  group_by(year, state, city) %>%
  summarize(imp_quant = sum(imp_quant), imp_value = sum(imp_value), exp_quant = sum(exp_quant), exp_value = sum(exp_value), state_lon = first(state_lon), state_lat = first(state_lat), city_lon = first(city_lon), city_lat = first(city_lat))

net_all <- rbind(net_us_city, net_world_city)
unique(net_all$year)
# 
# write.csv(net_world, "data/processed/net_world.csv")
# write.csv(net_all, "data/processed/net_all.csv")
