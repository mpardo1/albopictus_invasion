# scripts to process the data from aemet that its the cmip6 data for spain at a 
# 5 km grid. data avaliable at: https://archivo-proyecciones-climaticas.aemet.es/

# remove everything in the working directory
rm(list = ls())

# Load Libraries --------------------------------------------------
library(data.table)
library(vroom)
library(ggplot2)
library(tidyverse)
library(mapSpain)
library(sf)

# function to process the aemet-cmip6 data
climate_proc <- function(file_path){
  # Read the File Using data.table
  large_data <- fread(file_path, header = FALSE, sep = ";", skip = 4)
  
  # separate rows with ids and lot lat
  lon_lat_id <- as.data.frame(t(large_data[1:3,-1]))
  colnames(lon_lat_id) <- c("id", "lat", "lon")
  
  # test
  # ggplot(lon_lat_id) + geom_point(aes(lon, lat, color = id))
  
  # daily data
  prec_df <- as.data.frame(large_data[4:nrow(large_data),])
  
  # process the data --------------------------------------------------
  # change column names and transform date column
  colnames(prec_df) <- c("date",lon_lat_id$id)
  prec_df$date <- as.Date(as.character(prec_df$date), format = "%Y%m%d")
  
  # transform it into long format
  prec_df <- reshape2::melt(prec_df, id.vars = "date")
  colnames(prec_df) <- c("date", "id", "var_clim")
  prec_df$id <- as.character(prec_df$id)
  lon_lat_id$id <- as.character(lon_lat_id$id)
  prec_df <- prec_df %>% left_join(lon_lat_id)
  
  # remove -999 = na
  prec_df$var_clim <- ifelse(prec_df$var_clim == -999 , NA, prec_df$var_clim)
  # test
  # ggplot(prec_df[prec_df$date == prec_df$date[100],]) +
  #   geom_point(aes(lon, lat, color = prec)) +
  #   scale_color_viridis_c()
  
  # agregate monthly
  prec_df <- prec_df %>% 
    mutate(month = month(date),
           year = year(date)) %>% 
    group_by(month,year, lon, lat) %>% 
    summarise(meanvar = mean(var_clim, na.rm = TRUE),
              sumvar = sum(var_clim, na.rm = TRUE),
              minvar = min(var_clim, na.rm = TRUE),
              maxvar = max(var_clim, na.rm = TRUE))
  
  return(prec_df)
}


# load data --------------------------------------------------------
# dir path 
path_dir <- "~/aemet-cmip6/PENINSULA_Y_BALEARES"
list_dir <- list.dirs(path_dir)

# Function to aggregate datasets and join years
avg_datasets_fun <- function(var,ind_init,ind_end){
  list_dir <- list_dir[list_dir %like% var]
  
  # Create an empty data frame
  df_clim <- data.frame()
  
  # aggregate models and years 
  for (j in c(ind_init:ind_end)) {
    print(paste0("j:",j))
    for (i in c(1:11)) {
      print(paste0("i:",i))
      list_files <- list.files(list_dir[i]) # remove years before 2025
      file_path <- paste0(list_dir[i],"/", list_files[[j]])
      df_aux <- climate_proc(file_path)
      df_clim <- rbind(df_aux, df_clim) # join data sets and aggregate by year, month and lon lat
      df_clim <- df_clim %>% 
        group_by(month,year, lon, lat) %>% 
        summarise(meanvar = mean(meanvar, na.rm = TRUE),
                  sumvar = mean(sumvar, na.rm = TRUE),
                  minvar = mean(minvar, na.rm = TRUE),
                  maxvar = mean(maxvar, na.rm = TRUE))
      
    }
  }
  
  # Save the resulting data frame
  saveRDS(df_clim, paste0("~/aemet-cmip6/",var,"_agg_2025-2044.Rds"))
  
}

# filter dirs for specific var
var <- "min"
avg_datasets_fun(var,11,30)

var <- "max"
avg_datasets_fun(var,11,30)

var <- "prec"
avg_datasets_fun(var,11,30)

## Check processed data and downscale to comarcas -----------------------------
library(sf)
library(dplyr)
library(ggplot2)

# Load data ------------------------------------------------------------------
Path <- "~/Documentos/PHD/2025/Colonization/data/aemet-cmip6/"
list_files <- list.files(Path)
ind = 1
proc_data <- readRDS(paste0(Path, list_files[[ind]]))

# Plot to see if it looks ok
ggplot(proc_data[proc_data$month == 8 & proc_data$year == "2064",]) +
  geom_raster(aes(x= lon, y = lat, fill = minvar)) +
  scale_fill_viridis_c()

# Transform df into shapefile
lon_lat_df <- unique(proc_data[,c("lon", "lat")])
lon_lat_df$ind <- c(1:nrow(lon_lat_df))
lon_lat_sf <- st_as_sf(lon_lat_df, coords = c("lon", "lat"), crs = 4326) # CRS: WGS84

# Load comarcas ---------------------------------------------------------------
comarcas <- read_sf("/home/marta/Documentos/PHD/2024/Colonization/data/ComarcasAgrarias.shp")
comarcas <- comarcas[comarcas$DS_CCAA != "Ceuta" &
                       comarcas$DS_CCAA != "Melilla" &
                       comarcas$DS_CCAA != "Islas Baleares" &
                       comarcas$DS_CCAA != "Canarias", ]

# Transformar el shapefile al CRS de los puntos (WGS84)
comarcas <- st_make_valid(st_transform(comarcas, crs = st_crs(lon_lat_sf)))

# Realizar la unión espacial para obtener CO_COMARCA
lonlat_comarca <- st_join(lon_lat_sf, comarcas)

# Convertir a data frame para trabajar más fácilmente
lonlat_comarca <- lonlat_comarca %>%
  st_drop_geometry() %>%
  select(ind, CO_COMARCA)

# add lon lat
lonlat_comarca <- lon_lat_df %>% left_join(lonlat_comarca)

# Save as a template
saveRDS(lonlat_comarca,"/home/marta/Documentos/PHD/2025/Colonization/data/lonlat_ref_aemet_C0_COMARCA.Rds")

# Process data to comarca level ---------------------------------------------
# Load data ------------------------------------------------------------------
Path <- "~/Documentos/PHD/2025/Colonization/data/aemet-cmip6/"
list_files <- list.files(Path)

# Function to process rast to comarca level
upscale_comarca <- function(ind){
  # Read file
  clim_df <- readRDS(paste0(Path,list_files[[ind]]))
  
  # Join with CO_COMARCA
  lonlat_comarca <- readRDS("/home/marta/Documentos/PHD/2025/Colonization/data/lonlat_ref_aemet_C0_COMARCA.Rds")
  clim_df <- clim_df %>% left_join(lonlat_comarca)
  clim_df <- clim_df %>% group_by(CO_COMARCA, month, year) %>% 
    summarise(meanvar = mean(meanvar),
              maxvar = max(maxvar),
              minvar = min(minvar))
  
  # Save file with the same name to free memmory
  saveRDS(clim_df,paste0(Path,list_files[[ind]]))
}

for(i in c(1:length(list_files))){
  upscale_comarca(i)
}

# Load data ------------------------------------------------------------------
Path <- "~/Documentos/PHD/2025/Colonization/data/aemet-cmip6/"
var = "pr"
list_files <- list.files(Path, pattern = var)

join_df <- data.frame()
for(i in c(1:length(list_files))){
  clim_df <- readRDS(paste0(Path,list_files[[i]]))
  join_df <- rbind(clim_df, join_df)
}
saveRDS(join_df,paste0(Path, var, "_Comarcas_2023_2033.Rds"))

# Load data and average min max and test ----------------------------------
Path_max <- "~/Documentos/PHD/2025/Colonization/data/aemet-cmip6/max_temp_Comarcas_2023_2033.Rds"
Path_min <- "~/Documentos/PHD/2025/Colonization/data/aemet-cmip6/min_temp_Comarcas_2023_2033.Rds"
Path_prec <- "~/Documentos/PHD/2025/Colonization/data/aemet-cmip6/pr_Comarcas_2023_2033.Rds"

tmax <- readRDS(Path_max)
tmax$tmax <- tmax$meanvar
tmin <- readRDS(Path_min)
tmin$tmin <- tmin$meanvar
prec <- readRDS(Path_prec)
prec$prec <- prec$meanvar

# Join all variables
clim_df <- tmax[,c("CO_COMARCA", "month", "year", "tmax")] %>% 
  left_join(tmin[,c("CO_COMARCA", "month", "year", "tmin")]) %>%
  left_join(prec[,c("CO_COMARCA", "month", "year", "prec")])
clim_df$tmean <- (clim_df$tmax + clim_df$tmin)/2
clim_df$date <- as.Date(paste0(clim_df$year,"-",
                        ifelse(clim_df$month<10,
                               paste0("0",as.character(clim_df$month)),
                               as.character(clim_df$month)), "-15"))

# Test
clim_df_agg <- clim_df %>%  group_by(CO_COMARCA) %>% 
  summarise(tmean = mean(tmean))
comarcas <- read_sf("/home/marta/Documentos/PHD/2024/Colonization/data/ComarcasAgrarias.shp")
comarcas <- comarcas[comarcas$DS_CCAA != "Ceuta" &
                       comarcas$DS_CCAA != "Melilla" &
                       comarcas$DS_CCAA != "Islas Baleares" &
                       comarcas$DS_CCAA != "Canarias", ]
clim_df_agg <- comarcas %>%  left_join(clim_df_agg)
ggplot(clim_df_agg) +
  geom_sf(aes(fill = tmean), color =NA)  +
  scale_fill_distiller(palette = "Spectral") +
  theme_void() +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) 

# Save file  
saveRDS(clim_df,paste0(Path, "clim_Comarcas_2023_2033.Rds"))

# Test
df_plot <- clim_df[clim_df$CO_COMARCA == 813 &
                     clim_df$year < 2060,]
ggplot(df_plot) +
  geom_line(aes(date, tmean)) +
  geom_smooth(aes(date, tmean)) 

ggplot(df_plot) +
  geom_line(aes(date, prec)) +
  geom_smooth(aes(date, prec)) 

# Compute RM for each comarca for future climates ---------------------------
library(data.table)
source("~/INVASIBILITY_THRESHOLD/code/funcR0.R") # Load code with function to compute RM
Path <- "~/Documentos/PHD/2025/Colonization/data/aemet-cmip6/"
clim_df <- readRDS(paste0(Path, "clim_Comarcas_2023_2033.Rds"))

# Create date column as the 15 of each month
clim_df$date <- as.Date(paste0(clim_df$year, "-", clim_df$month,"-15"))

# Load comarcas ---------------------------------------------------------------
comarcas <- read_sf("/home/marta/Documentos/PHD/2024/Colonization/data/ComarcasAgrarias.shp")
comarcas <- comarcas[comarcas$DS_CCAA != "Ceuta" &
                       comarcas$DS_CCAA != "Melilla" &
                       comarcas$DS_CCAA != "Islas Baleares" &
                       comarcas$DS_CCAA != "Canarias", ]

# test
month_n <- 4
clim_df_plot <- comarcas %>%  left_join(clim_df[clim_df$month == month_n & clim_df$year == 2035, ])
ggplot(clim_df_plot) + geom_sf(aes(fill = tmean)) + scale_fill_viridis_c()
ggplot(clim_df_plot) + geom_sf(aes(fill = prec)) + scale_fill_viridis_c()

# Transform to data table# Transform to clim_df_plotdata table
class(clim_df)
clim_df <- setDT(clim_df)

# Add human density to the data table -----------------------------------
# https://www.mapa.gob.es/es/cartografia-y-sig/ide/descargas/agricultura/default.aspx
comarcas <- read_sf("/home/marta/Documentos/PHD/2024/Colonization/data/ComarcasAgrarias.shp")
comarcas <- comarcas[comarcas$DS_CCAA != "Ceuta" &
                       comarcas$DS_CCAA != "Melilla" &
                       comarcas$DS_CCAA != "Islas Baleares" &
                       comarcas$DS_CCAA != "Canarias", ]

# Load spanish map
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
esp_can <- esp_can[esp_can$ine.ccaa.name != "Ceuta" &
                     esp_can$ine.ccaa.name != "Melilla" &
                     esp_can$ine.ccaa.name != "Balears, Illes" &
                     esp_can$ine.ccaa.name != "Canarias", ]
esp_can <- esp_can[,c("cmun", "cpro", "NATCODE")]
esp_can <- st_make_valid(esp_can)
esp_can$cpro <- as.integer(esp_can$cpro); esp_can$cmun <- as.integer(esp_can$cmun)

# Load population density
year = 2023
pop_df <- read.csv(paste0("~/albo_mobility/data/pop/pobmun",substr(as.character(year),3,4),".csv"))
pop_df <- pop_df[,c(1,3,5)]
colnames(pop_df) <- c("cpro", "cmun", "pob")
pop_df$pob <- as.numeric(gsub("\\.","", pop_df$pob))

# Join pop_density with shape file spain
comarca_NATCODE <- readRDS( "~/albo_mobility/data/InputHanski/ESP/comarca_NATCODE.Rds")
pop_df <- pop_df %>% left_join(esp_can)
pop_df$geometry <- NULL
pop_df <- comarcas[,"CO_COMARCA"]  %>%  left_join(comarca_NATCODE) %>% left_join(pop_df)
pop_df <- st_make_valid(pop_df)
pop_df$area <- as.numeric(st_area(pop_df))*1.e-6
pop_df$geometry <- NULL
pop_df <- pop_df %>% group_by(CO_COMARCA) %>% summarise(pob = sum(pob, na.rm = TRUE),
                                                        area = min(area))
pop_df$dens <- pop_df$pob/pop_df$area
pop_df <- pop_df[!is.na(pop_df$area),]
saveRDS(pop_df, "~/albo_mobility/data/InputHanski/ESP/comarca_denspop_2023.Rds")

# Read population density
pop_df <- readRDS("~/albo_mobility/data/InputHanski/ESP/comarca_denspop_2023.Rds")

# Compute RM 
clim_df <- clim_df %>% left_join(pop_df)
clim_df[, R0_alb := mapply(R0_func_alb_2, tmean, prec, dens)]
clim_df <- unique(clim_df[,c("CO_COMARCA", "date", "R0_alb")])

# Test
unique(comarcas[comarcas$DS_CCAA == "Cataluña",c("CO_COMARCA", "DS_COMARCA")])
ggplot(clim_df[clim_df$CO_COMARCA %in% c(813, 803) &
                 year(clim_df$date)<2028,]) +
  geom_line(aes(date, R0_alb, color = as.factor(CO_COMARCA)))

# Transform into a matrix with time
RM_mat <- clim_df %>%
  pivot_wider(names_from = CO_COMARCA, values_from = R0_alb)

# Save file
write.csv(RM_mat,"~/albo_mobility/data/InputHanski/ESP/rm_alb_ESP_com_future_4.csv")
RM_mat <-read.csv("~/albo_mobility/data/InputHanski/ESP/rm_alb_ESP_com_future.csv")
