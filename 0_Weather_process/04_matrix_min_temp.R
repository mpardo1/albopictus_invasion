# Code to read raster data ERA5 land
# Load pkgs and remove data -----------------------------------------------------
rm(list = ls())
library("data.table")
library("ggplot2")
library("terra")
library("sf")
library("tidyverse")
library("exactextractr")

# Path to input and output data
path_data <- "data/"
path_output <- "data/output"

# Load comarcas shapefile
# https://www.mapa.gob.es/es/cartografia-y-sig/ide/descargas/agricultura/default.aspx
comarcas <- read_sf(paste0(path_data,"ComarcasAgrarias.shp"))
comarcas <- comarcas[comarcas$DS_CCAA != "Ceuta" &
                       comarcas$DS_CCAA != "Melilla" &
                       comarcas$DS_CCAA != "Islas Baleares" &
                       comarcas$DS_CCAA != "Canarias", ]
comarcas$ID <- c(1:nrow(comarcas))
comarcas_df <- comarcas[,c("ID", "CO_COMARCA")]
comarcas_df$geometry <- NULL

# Load list files era5 with min daily temperature 
# Download data from ERA5 Land ~/albo_mobility/code/weather_process/download_ERA5LAnd.py
# https://cds.climate.copernicus.eu/datasets/reanalysis-era5-land?tab=overview
Path <- paste0(path_data,"Min_temp/")
list_files <- list.files(Path)

# Function to process ERA5Land data to a matrix with columns each comarca and
# rows each year 
transform_minTemp <- function(i){
  time_init <- Sys.time()
  # Load raster
  rast_clim <- rast(paste0(Path,list_files[[i]]))
  rast_clim <- crop(rast_clim, ext(-10,5,35,45))
  rast_clim <- tapp(rast_clim, index= "days", fun = "min") # Agg by day
  rast_clim <- rast_clim - 273.15
  # Crop Spain
  # plot(rast_clim[[1]])
  
  # Extract values for each comarca
  rast_clim <- terra::project(rast_clim,comarcas)
  rast_clim <- terra::crop(rast_clim, comarcas) %>% 
    terra::mask(., comarcas)
  rast_clim_e <- exact_extract(rast_clim,comarcas,
                                fun = "mean")
  
  # add comarca ID
  rast_clim_e$CO_COMARCA <- comarcas_df$CO_COMARCA
  
  # Aggregate by comarca
  rast_clim <- reshape2::melt(rast_clim_e,id.vars = "CO_COMARCA")
  rast_clim$date <- as.Date(gsub("mean.d_", "",rast_clim$variable ), format= "%Y.%m.%d")
  rast_clim <- rast_clim %>% group_by(CO_COMARCA, date) %>%
    summarise(min_temp = mean(value, na.rm = TRUE))

  # Transform to wide format
  rast_clim <- pivot_wider(data = rast_clim,
                           names_from =CO_COMARCA, values_from= min_temp )
  print(paste0("Execution time:",Sys.time()- time_init))
  return(rast_clim)
}

init_ind = 1
mat_mintemp <- transform_minTemp(init_ind)
# Run through all the months and years
end_ind = 50
for(i in c((init_ind+1):end_ind)){#length(list_files) 233
  print(paste0("i:",i))
  aux_mat <- transform_minTemp(i)
  mat_mintemp <- rbind(aux_mat,mat_mintemp)
}

saveRDS(mat_mintemp,
        paste0(path_output, "min_temp_ESP",init_ind,"_",end_ind,".Rds"))
# mat_mintemp <- readRDS("~/albo_mobility/data/InputHanski/ESP/min_temp_ESP.Rds")

 # Join all files from clc
 list_files <- list.files(path_output, pattern = "min_temp")
 mat_mintemp <- data.frame()
 for(i in c(1:length(list_files))){
   aux <- readRDS(paste0(Path, list_files[[i]]))
   mat_mintemp <- rbind(aux, mat_mintemp)
 }
mat_mintemp <- mat_mintemp[year(mat_mintemp$date)>2004,]
mat_mintemp$ID
mat_mintemp <- mat_mintemp[sort(mat_mintemp$date, decreasing=FALSE),]
write.csv(mat_mintemp, paste0(Path, "min_temp_ESP.csv"))


# Parallel code -------------------------------------
# library(parallel)
# transform_minTemp(1)
# # Parallel processing using mclapply
# results <- mclapply(1:length(list_files), function(i) {
#   print(paste0("i:", i))
#   transform_minTemp(i)
# }, mc.cores = 10)
# 
# # Combine the results
# mat_mintemp <- do.call(rbind,  results)

# Save as csv
# mat_mintemp <- readRDS(path_output,"min_temp_ESP.Rds")
# # Test
# mat_mintemp_p <- data.frame(CO_COMARCA = rownames(t(mat_mintemp[1,c(2:ncol(mat_mintemp))])),
#                             temp_min = t(mat_mintemp[,c(2:ncol(mat_mintemp))]))
# mat_mintemp_p$CO_COMARCA <- as.integer(mat_mintemp_p$CO_COMARCA)
# mat_mintemp_p <- comarcas %>% left_join(mat_mintemp_p)
# ggplot(mat_mintemp_p) +
#   geom_sf(aes(fill = temp_min)) +
#   scale_fill_viridis_c()
# mat_mintemp_p[is.na(mat_mintemp_p$temp_min),]

# Future min temp ------------------------------------------------------------
min_temp <- paste0(path_output,"min_temp_Comarcas_2023_2033.Rds")
min_temp <- readRDS(min_temp)
min_temp$date <- as.Date(paste0( min_temp$year,"-",
                                ifelse(min_temp$month<10,
                                       paste0("0",min_temp$month),
                                       min_temp$month), "-15")) 

# Transform to wide format
min_temp <- min_temp[,c("CO_COMARCA", "date", "meanvar")] %>%
  group_by(CO_COMARCA,date) %>% 
  summarise(meanvar = mean(meanvar)) %>% 
  pivot_wider(names_from = CO_COMARCA, values_from = meanvar)

# Save as csv
write.csv(min_temp[,-ncol(min_temp)], paste0(path_output, "min_temp_ESP_future.csv"))

rM_fut <- read.csv(paste0(Path, "rm_alb_ESP_com_future.csv"))
rM_fut <- rM_fut[,-ncol(rM_fut)]
write.csv(rM_fut,paste0(Path, "rm_alb_ESP_com_future.csv"))

# Check which comarca is missing
colnames(min_temp)

# Load shapefile comarcas
# https://www.mapa.gob.es/es/cartografia-y-sig/ide/descargas/agricultura/default.aspx
comarcas <- read_sf(path_data, "ComarcasAgrarias.shp")
comarcas <- comarcas[comarcas$DS_CCAA != "Ceuta" &  # Remove non iberian peninsula
                       comarcas$DS_CCAA != "Melilla" &
                       comarcas$DS_CCAA != "Islas Baleares" &
                       comarcas$DS_CCAA != "Canarias", ]
comarcas <- st_make_valid(comarcas)
comarcas$area <- st_area(comarcas)
comarca_unique <- comarcas %>%
  group_by(CO_COMARCA) %>%          # Group by ID
  slice_max(order_by = area, n = 1) %>%  # Keep the row with the highest area
  ungroup()

# Difference in colnames and comarcas CO_COMARCA
setdiff(colnames(min_temp)[2:338],comarca_unique$CO_COMARCA)
setdiff(comarca_unique$CO_COMARCA,colnames(min_temp)[2:339])
setdiff(comarca_unique$CO_COMARCA,gsub("^X([0-9]+)$", "\\1",colnames(rM_fut)[2:340]))
comarca_unique[comarca_unique$CO_COMARCA == 2510,]
comarca_unique[comarca_unique$CO_COMARCA == 5007,]

# Add missing column with same data 
ind = which(colnames(min_temp) == "X2505")
min_temp <- min_temp %>% 
  add_column("X2510" = min_temp[, ind], .after = "X2505")

ind = which(colnames(rM_fut) == "X2505")
rM_fut <- rM_fut %>% 
  add_column("2510" = rM_fut[, ind], .after = "X2505")

# Save as csv
write.csv(rM_fut[-1,],paste0(path_output, "rm_alb_ESP_com_future.csv"))
write.csv(min_temp,paste0(path_output, "min_temp_ESP_future.csv"))
