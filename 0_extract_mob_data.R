# Code to extract the human mobility data from 
#https://www.transportes.gob.es/ministerio/proyectos-singulares/estudios-de-movilidad-con-big-data/opendata-movilidad
# install packages
packages <- c("spanishoddata", "data.table", "sf", "dbplyr", "tidyverse", "fs")
missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(missing_packages)) install.packages(missing_packages)
rm(packages, missing_packages)

# load packages
library(spanishoddata)
library(data.table)
library(sf)
library(tidyverse)
library(fs)

# set data directory for `spanishoddata`
spod_set_data_dir("data/input/mitms-data")


# get origin-destination flows data --------------------------------------

# set desired data and dates range
data_type <- "origin-destination"
zones <- "municipalities"
dates <- c(start = "2023-04-01", end = "2023-11-30")

# pre-download data
downloaded_files <- spod_download(
  type = data_type,
  zones = zones,
  dates = dates,
  max_download_size_gb = 32,
  quiet = FALSE,
  return_local_file_paths = TRUE,
  ignore_missing_dates = TRUE
)

# check total size of all downloaded files, shoul be about 29 Gb
round(sum(file.size(downloaded_files)) / 1024^3, 0) == 29
# check the number of csv gz files, should be 237 files
length(file.exists(downloaded_files)) == 237

# convert data to DuckDB for faster aggregation
duckdb_file <- "data/proc/od_municipalities_apr_2023_nov_2023.duckdb"
duckdb_file <- spod_convert(
  type = data_type,
  zones = zones,
  dates = "cached_v2", # use previously downloaded data
  save_format = "duckdb",
  save_path = "data/proc/od_municipalities_apr_2023_nov_2023.duckdb",
  ignore_missing_dates = TRUE,
  quiet = FALSE,
  max_mem_gb = 24,
  max_n_cpu = 16
)

# connect to the database file
od_data <- spod_connect(duckdb_file, max_n_cpu = 16, max_mem_gb = 24)

# aggregate data to mean trips per day from April 2023 to November 2023
mean_daily_trips_apr_2023_nov_2023 <- od_data |>
  filter(date >= "2023-04-01" & date <= "2023-11-30") |>
  group_by(id_origin, id_destination, date) |>
  summarise(n_trips = sum(n_trips, na.rm = TRUE), .groups = "drop") |>
  group_by(id_origin, id_destination) |>
  summarise(mean_n_trips = mean(n_trips, na.rm = TRUE), .groups = "drop") |>
  arrange(desc(mean_n_trips), id_origin, id_destination) |>
  collect() |>
  setDT()

# save data
fs::dir_create("data/od_flows/mitma_municip", recurse = TRUE)
fwrite(
  mean_daily_trips_apr_2023_nov_2023,
  "data/od_flows/mitma_municip/mean_daily_trips_apr_2023_nov_2023.csv.gz"
)

# disconnect from the database
spod_disconnect(od_data)

# get MITMS regions ------------------------------------------------------

# get MITMS data regions
mitma_municip <- spod_get_zones(zones = "districts", ver = 2) |>
  rename(id_mitma = id)

# save data
fs::dir_create("data/boundaries/mitma_municip", recurse = TRUE)
st_write(mitma_municip, "data/boundaries/mitma_municip.gpkg", delete_dsn = TRUE)

# create INE-MITMS correspondence table ----------------------------------

# load data
ine_municip <- st_read("data/boundaries/ine_municip.gpkg") |>
  st_transform(st_crs(mitma_municip))
# check for invalid geometries should be 0
sum(!st_is_valid(ine_municip)) == 0

## establish the correspondence between mitma and ine
# nrow(ine_municip)
# length(unique(ine_municip$id_ine))
ine_municip_centroids <- st_point_on_surface(ine_municip)
id_correspondence_table <- st_join(
  x = ine_municip_centroids[, "id_ine"],
  y = mitma_municip[, "id_mitma"],
  join = st_within
) |>
  st_drop_geometry() |>
  setDT()

# check for non-overlapping geometries should be empty
id_correspondence_table[is.na(id_mitma), id_ine]

fwrite(
  id_correspondence_table,
  "data/boundaries/ine_to_mitma_id_correspondence.csv.gz",
  quote = T
)
