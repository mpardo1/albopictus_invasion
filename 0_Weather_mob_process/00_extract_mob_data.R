# Code to extract the human mobility data from
#https://www.transportes.gob.es/ministerio/proyectos-singulares/estudios-de-movilidad-con-big-data/opendata-movilidad with spanishoddata package
# install packages
packages <- c(
  "spanishoddata",
  "data.table",
  "sf",
  "dbplyr",
  "tidyverse",
  "fs",
  "mapSpain"
)
missing_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(missing_packages)) {
  install.packages(missing_packages)
}
rm(packages, missing_packages)

# set according to your available resources
max_mem_gb <- 22 # available memory in GB
max_n_cpu <- 14 # available number of processor cores


# load packages
library(spanishoddata)
library(data.table)
library(sf)
library(tidyverse)
library(fs)
library(mapSpain)

# set cache data directory for `spanishoddata`, folder is created automatically if it does not exist
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

# filter file list to only requested dates
files_dt <- data.table(filepath = downloaded_files)
files_dt <- files_dt[path_file(filepath) == "Viajes_municipios.csv.gz"]
files_dt <- files_dt[
  grepl("/por-municipios/", filepath) & grepl("/ficheros-diarios/", filepath)
]
files_dt[, `:=`(
  year_val = as.numeric(str_extract(filepath, "(?<=year=)\\d+")),
  month_val = as.numeric(str_extract(filepath, "(?<=month=)\\d+")),
  day_val = as.numeric(str_extract(filepath, "(?<=day=)\\d+"))
)]
start_d <- as.Date(dates["start"])
end_d <- as.Date(dates["end"])
files_dt[, file_date := make_date(year_val, month_val, day_val)]
relevant_files <- unique(files_dt[
  file_date >= start_d & file_date <= end_d,
  filepath
])


# check total size of all downloaded files, shoul be about 29 Gb
stopifnot(round(sum(file.size(relevant_files)) / 1024^3, 0) == 29)
# check the number of csv gz files, should be 237 files
stopifnot(length(file.exists(relevant_files)) == 237)

# convert data to DuckDB for faster aggregation
duckdb_file <- "data/proc/od_municipalities_apr_2023_nov_2023.duckdb"
duckdb_file <- spod_convert(
  type = data_type,
  zones = zones,
  dates = dates,
  save_format = "duckdb",
  save_path = duckdb_file,
  ignore_missing_dates = TRUE,
  overwrite = FALSE,
  quiet = FALSE,
  max_mem_gb = max_mem_gb,
  max_n_cpu = max_n_cpu
)

# connect to the database file
od_data <- spod_connect(
  duckdb_file,
  max_n_cpu = max_n_cpu,
  max_mem_gb = max_mem_gb
)

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
dir_create("data/od_flows/mitma_municip", recurse = TRUE)
fwrite(
  mean_daily_trips_apr_2023_nov_2023,
  "data/od_flows/mitma_municip/mean_daily_trips_apr_2023_nov_2023.csv"
)

# disconnect from the database
spod_disconnect(od_data)

# get MITMS regions ------------------------------------------------------

# get MITMS data regions
mitma_municip <- spod_get_zones(zones = "municip", ver = 2) |>
  rename(id_mitma = id)

# save data
dir_create("data/boundaries/mitma_municip", recurse = TRUE)
st_write(mitma_municip, "data/boundaries/mitma_municip.gpkg", delete_dsn = TRUE)

# create INE-MITMS correspondence table ----------------------------------

# load data
ine_municip_centroids <- esp_get_munic_siane(
  year = "2022",
  moveCAN = FALSE,
  epsg = "3035"
) |>
  st_transform(st_crs(mitma_municip)) |>
  st_make_valid() |>
  st_point_on_surface() |>
  select(id_ine = LAU_CODE)

## establish the correspondence between mitma and ine
id_correspondence_table <- st_join(
  x = ine_municip_centroids[, "id_ine"],
  y = mitma_municip[, "id_mitma"],
  join = st_within
) |>
  st_drop_geometry() |>
  setDT()

# check for non-overlapping geometries should be empty
stopifnot(id_correspondence_table[is.na(id_mitma), .N] == 0)

fwrite(
  id_correspondence_table,
  "data/boundaries/ine_to_mitma_id_correspondence.csv",
  quote = T
)
