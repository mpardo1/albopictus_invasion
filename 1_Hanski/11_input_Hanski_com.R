# Input data sets for julia parameter estimation code
rm(list=ls())
library(spdep)
library(mapSpain)
library(sf)
library(dplyr)
library(tidyverse)
library(data.table)
library(raster)
library(ggplot2)
library(ggpubr)

# Set up path
path_data <- "data/"
path_output <- "data/output/"

# Load comarcas shapefile ---------------------------------------
# https://www.mapa.gob.es/es/cartografia-y-sig/ide/descargas/agricultura/default.aspx
comarcas <- read_sf( paste0(path_data,"ComarcasAgrarias.shp"))

# Remove non peninsula
comarcas <- comarcas[comarcas$DS_CCAA != "Ceuta" &
                       comarcas$DS_CCAA != "Melilla" &
                       comarcas$DS_CCAA != "Islas Baleares" &
                       comarcas$DS_CCAA != "Canarias", ]

# Check which comarca has more than one geometry
duplicates <- comarcas %>%
  group_by(CO_COMARCA) %>%
  filter(n() > 1)  # Identifies IDs that are repeated

# Remove duplicates, there are 5 comarcas with duplicates, ie. two geometries
# keeping the biggest area
comarcas <- st_make_valid(comarcas)
comarcas$area <- st_area(comarcas)
comarca_unique <- comarcas %>%
  group_by(CO_COMARCA) %>%          # Group by ID
  slice_max(order_by = area, n = 1) %>%  # Keep the row with the highest area
  ungroup()  # Ungroup to return a normal dataframe

# Plot to test
ggplot(comarca_unique) + geom_sf(aes(fill=as.factor(CO_COMARCA)), color = NA)

# Compute distance matrix ------------------------------------------
# Compute centroid and after distance between comarcas centroid
comarcas <- comarca_unique
comarcas$centroid <- st_centroid(comarcas)
comarcas <- comarcas[order(comarcas$CO_COMARCA),]
dist_mat <- st_distance(comarcas$centroid)

# Convert to numeric (remove units)
dist_mat <- as.matrix(dist_mat)  # Convert to matrix
dist_mat <- apply(dist_mat, c(1,2), as.numeric) 

# Test
ind_t <- 203 #sample(1:300,1)
dist_df <- data.frame(CO_COMARCA = comarcas$CO_COMARCA, dist = as.numeric(dist_mat[,ind_t]))
dist_df <- comarcas %>%  left_join(dist_df)
dist_df_filt <- dist_df[dist_df$dist <= 35000,]
ggplot(dist_df) +
  geom_sf(aes(fill=as.numeric(dist)), color = NA) +
  scale_fill_viridis_c()

# Write csv
write.csv(dist_mat, paste0(path_output, "dist_mat_com_ESP.csv"))

# Compute relationship comarcas NATCODE --------------------------------------
# Load Spanish muni
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))

# Remove non peninsula
esp_can <- esp_can[esp_can$ine.ccaa.name != "Ceuta" &
                     esp_can$ine.ccaa.name != "Melilla" &
                     esp_can$ine.ccaa.name != "Balears, Illes" &
                     esp_can$ine.ccaa.name != "Canarias", ]

# Compute relationship comarcas and municipalities
esp <- st_make_valid(esp_can)
esp$centroid <- st_centroid(esp$geometry)
comarcas <- comarcas[,c("CO_COMARCA")]

# Intersect geometries comarcas and centroid munis
st_crs(comarcas) == st_crs(esp) # Check if they are in the same coordinate system
esp$geometry <- NULL
esp <- st_as_sf(esp[, c("centroid", "NATCODE")])
comarcas <- st_make_valid(comarcas)
inter_cent_comar <- st_join(esp, comarcas)

# Test
comarcas[comarcas$CO_COMARCA==801,]
Comarca_test <- comarcas$CO_COMARCA[sample(1:nrow(comarcas),1)]
ou_comar <- inter_cent_comar[inter_cent_comar$CO_COMARCA == Comarca_test ,]
ggplot(comarcas[comarcas$CO_COMARCA == Comarca_test,]) +
  geom_sf(aes(fill = CO_COMARCA), alpha= 0.4, color = NA) +
  geom_sf(data = ou_comar, aes(color = "red"), size = 1)

# Select columns and save
comarca_NATCODE <- inter_cent_comar[,c("CO_COMARCA", "NATCODE")]
comarca_NATCODE$centroid <- NULL

# Load data pa with mitma boundaries
NATCODE_mitma_pa <-readRDS( paste0(path_data,"pa_mitma.Rds")) # File taken from analyze_stas_mitma.R

# Join comarca mitma and natcode
comarca_NATCODE_mitma <- comarca_NATCODE %>% left_join(NATCODE_mitma_pa[,c("id_mitma", "NATCODE")])

# Filter only peninsula
comarca_NATCODE_mitma <- comarca_NATCODE_mitma[comarca_NATCODE_mitma$NATCODE %in% esp_can$NATCODE,]

# Load MITMA shapefile
# https://www.transportes.gob.es/ministerio/proyectos-singulares/estudios-de-movilidad-con-big-data/opendata-movilidad
path <- paste0(path_data,"/mitma_municip.gpkg")
muni_mitma <- st_read(path)
# Filter Peninsula
muni_mitma <- muni_mitma[muni_mitma$id_mitma %in% unique(NATCODE_mitma_pa$id_mitma),]
muni_mitma <- muni_mitma[!is.na(muni_mitma$name),]

# Save csv
write.csv(comarca_NATCODE_mitma, paste0(path_output,"comarca_mitma_NATCODE_ESP.csv"))

# Compute year observation
comarca_NATCODE_mitma_pa <- comarca_NATCODE_mitma %>%  left_join(NATCODE_mitma_pa[,c("NATCODE", "year_detec")])
comarca_pa <- comarca_NATCODE_mitma_pa %>%  group_by(CO_COMARCA) %>% 
  summarise(year_detec=min(year_detec, na.rm=TRUE))
comarca_pa$year_detec <- ifelse(comarca_pa$year_detec ==Inf, NA, comarca_pa$year_detec )

# Reload comarcas shapefile and plot
comarcas_d <- read_sf(paste0(path_data,"ComarcasAgrarias.shp"))
comarcas_d <- comarcas_d[comarcas_d$DS_CCAA != "Ceuta" &   # Remove non peninsula
                         comarcas_d$DS_CCAA != "Melilla" &
                         comarcas_d$DS_CCAA != "Islas Baleares" &
                         comarcas_d$DS_CCAA != "Canarias", ]
comarca_pa_p <- comarcas_d[,"CO_COMARCA"] %>% left_join(comarca_pa) # Join presence absence + shapefile
ggplot(comarca_pa_p) + geom_sf(aes(fill = as.factor(year_detec)), color = NA) +
  scale_fill_viridis_d(name = "Year Detection", na.value = "#F1F1F1",
                       labels = c(as.character(c(2004:2023)),"ND")) + theme_minimal()

# Remove NAs
comarca_pa <- comarca_pa[!is.na(comarca_pa$CO_COMARCA),]
comarca_pa$year_detec <- ifelse(is.na(comarca_pa$year_detec ),0,comarca_pa$year_detec )

# Save PA comarcas file
write.csv(comarca_pa,paste0(path_output,"pa_com.csv"))

# Compute flows between comarcas ---------------------------------------------
library(data.table)
# Data from extract_mob_data.R
path <- paste0(path_data,"mean_daily_trips_apr_2023_nov_2023.csv")
flows_df <- setDT(read.csv(path))
flows_df <- flows_df[flows_df$id_origin != flows_df$id_destination,]

# Unir flows_df con NATCODE_mitma_prov para obtener el CO_COMARCA del origen
flows_df <- flows_df %>%
  left_join(comarca_NATCODE_mitma[,c("id_mitma", "CO_COMARCA")], by = c("id_origin" = "id_mitma")) %>%
  rename(CO_COMARCA_origin = CO_COMARCA)  # Renombrar la nueva columna

# Uni nuevamente para obtener el CO_COMARCA del destino
flows_df <- flows_df %>%
  left_join(comarca_NATCODE_mitma[,c("id_mitma", "CO_COMARCA")], by = c("id_destination" = "id_mitma")) %>%
  rename(CO_COMARCA_destination = CO_COMARCA)  # Renombrar la nueva columna

# Remove duplicates
flows_df <- unique(flows_df[!is.na(flows_df$CO_COMARCA_origin),])
flows_df <- unique(flows_df[!is.na(flows_df$CO_COMARCA_destination),])

# Agrupar por CO_COMARCA de origen y destino y sumar los viajes
flows_comarca_df <- flows_df %>%
  group_by(CO_COMARCA_origin, CO_COMARCA_destination) %>%
  summarise(mean_n_trips = sum(mean_n_trips, na.rm = TRUE), .groups = "drop")

# Save data frame with flows between comarcas
write.csv(flows_comarca_df, paste0(path_output,"flows_com_df.csv"))

# Create the flows matrix
flows_comarca_df$id_origin <- factor(flows_comarca_df$CO_COMARCA_origin,
                                     levels = comarcas$CO_COMARCA)
flows_comarca_df$id_destination <- factor(flows_comarca_df$CO_COMARCA_destination,
                                          levels = comarcas$CO_COMARCA)
# Create the matrix using xtabs()
flows_mat <- xtabs(mean_n_trips ~ id_origin + id_destination, data = flows_comarca_df)
flows_mat <- as.matrix(flows_mat)

# Save the flows matrix
write.csv(flows_mat,
          paste0(path_output,"flows_apr_2023_nov_2023_mitma_ESP_com_v2.csv"))

# Plot corr
dist_mob <- data.frame(flows=c(flows_mat),dist= as.numeric(c(dist_mat)))
ggplot(dist_mob) +
  geom_point(aes(log10(dist), log10(flows)))

# Filter relationship comarca mitma
mitma_prov <- comarca_NATCODE_mitma[,c("CO_COMARCA", "id_mitma")]

# Compute R_M by comarcas with low human density weight ----------------------- 
library(data.table)
source("funcR0.R") # Load code with function to compute RM
year = 2005
# Processed data from ERA5LAnd extracted from Weather_process/join_monthly_ERA5_data_to_year.R
Path <- paste0(path_data,"3_clim_df_",year,".Rds")
df_clim <- readRDS(Path)
df_clim_ESP <- df_clim %>%  left_join(mitma_prov)
df_clim_ESP <- df_clim_ESP %>%  group_by(CO_COMARCA,date) %>% 
  summarise(tmean = mean(tmean, na.rm=TRUE),
            prec = mean(prec, na.rm=TRUE))

# Load population density
pop_df <- read.csv(paste0(path_data,"pobmun",substr(as.character(year),3,4),".csv"))
pop_df <- pop_df[,c(1,3,5)]
colnames(pop_df) <- c("cpro", "cmun", "pob")
pop_df$pob <- as.numeric(gsub("\\.","", pop_df$pob))
# pop_df <- pop_df %>% group_by(cpro) %>% summarise(pob = sum(pob, na.rm=TRUE))

# Compute pop density
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
# ggplot( esp_can %>% left_join(pop_df))+ geom_sf(aes(fill = dens)) + scale_fill_viridis_c(option="D")

# Compute RM albopicus
df_clim_ESP$CO_COMARCA <- as.numeric(df_clim_ESP$CO_COMARCA)
df_clim_ESP <- setDT(df_clim_ESP %>%  left_join(pop_df[,c("CO_COMARCA", "dens")]))
df_clim_ESP[, R0_alb := mapply(R0_func_alb_2, tmean, prec, dens)]

for(i in c(2006:2023)){
  year = i
  print(paste0("i:",i))
  # Processed data from ERA5LAnd extracted from Weather_process/join_monthly_ERA5_data_to_year.R
  Path <- paste0(path_data,"/clim_df_",year,".Rds")
  df_clim <- readRDS(Path)
  df_clim <- df_clim %>%  left_join(mitma_prov)
  df_clim <- df_clim %>%  group_by(CO_COMARCA,date) %>% 
    summarise(tmean = mean(tmean, na.rm=TRUE),
              prec = mean(prec, na.rm=TRUE))
  
  # Load population density
  pop_df <- read.csv(paste0(path_data,"pobmun",substr(as.character(year),3,4),".csv"))
  pop_df <- pop_df[,c(1,3,5)]
  colnames(pop_df) <- c("cpro", "cmun", "pob")
  pop_df$pob <- as.numeric(gsub("\\.","", pop_df$pob))
  # pop_df <- pop_df %>% group_by(cpro) %>% summarise(pob = sum(pob, na.rm=TRUE))
  
  # Compute pop density
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
  # ggplot( esp_can %>% left_join(pop_df))+ geom_sf(aes(fill = dens)) + scale_fill_viridis_c(option="D")
  # Compute RM albopicus
  df_clim$CO_COMARCA <- as.numeric(df_clim$CO_COMARCA)
  df_clim <- setDT(df_clim %>%  left_join(pop_df[,c("CO_COMARCA", "dens")]))
  df_clim[, R0_alb := mapply(R0_func_alb_2, tmean, prec, dens)]
  
  df_clim_ESP <- rbind(df_clim_ESP, df_clim)
}

# Remove rows with na comarca
df_clim_ESP <- df_clim_ESP[!is.na(df_clim_ESP$CO_COMARCA),]
# df_clim_ESP[is.na(df_clim_ESP$R0_alb),]
df_clim_ESP <- reshape(df_clim_ESP[,c(1,2,6)], idvar = "date", timevar = "CO_COMARCA", direction = "wide" )
# Save the data frame to a csv file
write.csv(df_clim_ESP,paste0(path_output,"3_rm_alb_ESP_com_0_2_v2.csv"))


# Average RM
RM_ESP <- read.csv(paste0(path_output,"3_rm_alb_ESP_com_0_2.csv"))
RM_RM <- colSums(RM_ESP[, c(3:ncol(RM_ESP))]/nrow(RM_ESP))
tmin_ESP <- read.csv("~/albo_mobility/data/InputHanski/ESP/min_temp_ESP.csv")
tmin_ESP_avg <- colSums(tmin_ESP[, c(3:ncol(tmin_ESP))]/nrow(tmin_ESP))
tmin_ESP <- apply(tmin_ESP[, c(3:ncol(tmin_ESP))], 2, min)

write.csv(RM_RM, paste0(path_output,"3_rm_alb_ESP_com_0_2.csv"))
