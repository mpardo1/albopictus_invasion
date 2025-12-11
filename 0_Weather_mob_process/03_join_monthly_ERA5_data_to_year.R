# Code to join monthly processed data from ERA5 Land from the two files in the 0_Weather_mob_process directory with name starting with 02_process_ERa5_monthly...
rm(list = ls())
library("data.table")
library("ggplot2")
library("terra")
library("sf")
library("tidyverse")

# Join monthly datasets into one year
path_in <- "data/"

# For rain 
for(i in c(2005:2023)){
# for(i in c(2024)){
  print(paste0("i:",i))
  Path_file <- paste0("rain/era5_land_prec_mitma_",1,"_",i,".Rds")
  year_df <- readRDS(paste0(path_in,Path_file))
  print(year_df$date[1])
  for(j in c(2:12)){
    print(paste0("j:",j))
    Path_file <- paste0("rain/era5_land_prec_mitma_",j,"_",i,".Rds")
    aux_df <- readRDS(paste0(path_in,Path_file))
    print(aux_df$date[1])
    year_df <- rbind(year_df, aux_df)
  }
  saveRDS(year_df, paste0(path_in, "rain/yearly/clim_",i,".Rds"))
}

# For temperature 
for(i in c(2005:2023)){
# for(i in c(2024)){
  print(paste0("i:",i))
  Path_file <- paste0("temp/era5_land_temp_mitma_",1,"_",i,".Rds")
  year_df <- readRDS(paste0(path_in,Path_file))
  print(year_df$date[1])
  for(j in c(2:12)){
    print(paste0("j:",j))
    Path_file <- paste0("temp/era5_land_temp_mitma_",j,"_",i,".Rds")
    aux_df <- readRDS(paste0(path_in,Path_file))
    print(aux_df$date[1])
    year_df <- rbind(year_df, aux_df)
  }
  saveRDS(year_df, paste0(path_in, "temp/yearly/temp_",i,".Rds"))
}

# Join temp and rain yearly
for(i in c(2005:2023)){
  temp <- readRDS( paste0(path_in, "temp/yearly/temp_",i,".Rds"))
  rain <- readRDS( paste0(path_in, "rain/yearly/clim_",i,".Rds"))
  clim_df <- temp %>%  left_join(rain)
  saveRDS(clim_df, paste0(path_out,"3_clim_df_",i,".Rds"))
}
