# Code to read raster data ERA5 land:
#https://cds.climate.copernicus.eu/cdsapp#!/dataset/eu.copernicus.climate.reanalysis-era5-land?tab=overview
# To run the code be on the albopictus_invasion directory if not change path adding the path to albopictus_invasion directory in lines 14,15

# Load pkgs and remove data -----------------------------------------------------
rm(list = ls())
library(data.table)
library(ggplot2)
library(terra)
library(sf)
library(tidyverse)

# Path to input and output data
path_in <- "data/"
path_out <- "data/output"

# Rainfall CERRA -----------------------------------------------------------
# Load list files download from CERRA
Path_dir <- paste0(path_in,"rain/")
# Path_dir <- paste0("U:/Colonization/data/CERRA/temp/")
list_files <- list.files(Path_dir, pattern = "grib")
print(list_files)

# Load mitma mobility regions
path <- paste0(path_in, "mitma_municip.gpkg") # Download from 0_extract_mob_data.R
muni_mitma <- st_read(path)

# Loop through all the data sets from CERRA
process_CERRA <- function(Path){
  clim_df <- rast(Path)
  clim_df <- clim_df[[time(clim_df) %like% "00:00:00"]]#tapp(clim_df, index = "days", fun = "mean") # Aggregate daily
  time_vec <- time(clim_df) # Extract time for after
  # plot(clim_df[[23]]) # plot to see the extension of the raster
  print(time_vec[1])
  # Extract by mask
  clim_df <- terra::project(clim_df,muni_mitma)
  clim_df <- terra::crop(clim_df, muni_mitma) %>%
    terra::mask(., muni_mitma)
  # plot(clim_df[[23]])
  
  # Filter muni mitma ID to extract weather for mitma regions
  id_mitma_df <- muni_mitma[,"id_mitma"]
  id_mitma_df$geometry <- NULL
  id_mitma_df$ID <- c(1:nrow(id_mitma_df))
  
  # extract data to a data frame
  clim_df <- terra::extract(clim_df,muni_mitma)
  colnames(clim_df)[2:ncol(clim_df)] <- time_vec
  clim_df1 <- reshape2::melt(clim_df, id.vars = c("ID"))
  clim_df1$value <- clim_df1$value*1000
  clim_df1 <- clim_df1 %>% group_by(ID,variable) %>%
    summarise(prec = mean(value, na.rm = TRUE),
              max_prec = max(value, na.rm = TRUE))
  df_time <- data.frame(variable = unique(clim_df1$variable), time = time_vec)
  clim_df1 <- clim_df1 %>% left_join(df_time) %>% left_join(id_mitma_df)
  clim_df1 <- clim_df1[,c("id_mitma","time","prec", "max_prec")]
  clim_df1$date <- as.Date(clim_df1$time)
  # clim_df1 <- clim_df1 %>% group_by(id_mitma,date) %>%
  #   summarise(prec = sum(prec, na.rm = TRUE))
  saveRDS(clim_df1,paste0(Path_dir,"era5_land_prec_mitma_",unique(month(clim_df1$date)),
                          "_",unique(year(clim_df1$date)),".Rds"))
  return(clim_df1)
}

Path <- paste0(Path_dir,list_files[[2]])
for(i in c(1:length(list_files))){
  print(list_files[[i]])
  process_CERRA(paste0(Path_dir,list_files[[i]]))
}

# Check if working properly
clim_df1 <- readRDS(paste0(Path_dir,"era5_land_prec_mitma_",1,
                        "_",2014,".Rds"))
clim_df1 <- muni_mitma %>% left_join(clim_df1[clim_df1$date == clim_df1$date[30], ])
ggplot(clim_df1) +
  geom_sf(aes(fill= tmean), color = NA) +scale_fill_viridis_c()
