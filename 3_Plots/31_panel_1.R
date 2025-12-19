# Code to compute plots for panel 1 in the main manuscript
# To run the code be on the albopictus_invasion directory if not change path adding the path to albopictus_invasion directory in lines 18 and 19

# Remove everything before starting
rm(list=ls())

# Load libraries
library(mapSpain)
library(sf)
library(dplyr)
library(tidyverse)
library(data.table)
library(ggplot2)
library(ggpubr)
library(tidyr)

# Paths
path_out <- "data/output/" # Path to processed files
path_plots <- "plots/" # Select a path to save the plots

# Load data --------------------------------------------------------------
# Load data pa with mitma boundaries
NATCODE_mitma_pa <- read.csv(paste0(path_out,"comarca_mitma_NATCODE_ESP.csv")) 

# Load Spanish muni
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
esp_can <- esp_can[esp_can$ine.ccaa.name != "Ceuta" &
                     esp_can$ine.ccaa.name != "Melilla" &
                     esp_can$ine.ccaa.name != "Balears, Illes" &
                     esp_can$ine.ccaa.name != "Canarias", ]

# Add detection data
pa <- read.csv(paste0(path_out,"detection_albopictus.csv")) 

# Join  data sets to have geometries
pa$X <- NULL
NATCODE_mitma_pa <- NATCODE_mitma_pa %>% left_join(pa)
NATCODE_mitma_pa <- esp_can %>% left_join(NATCODE_mitma_pa)

# compute the perimeter of Spain to draw
ESP_per <- esp_get_ccaa_siane()
ESP_per <- ESP_per[!(ESP_per$ine.ccaa.name %in% c("Balears, Illes" ,"Ceuta" ,"Melilla","Canarias"   )),]
ESP_per <- st_make_valid(ESP_per)
ESP_per <- st_union(ESP_per)
saveRDS(ESP_per, paste0(path_out,"/Spain_perimeter.Rds"))

# Aggregate by year_detec to have number of municipalities detected each year
NATCODE_mitma_pa_plot <- NATCODE_mitma_pa
NATCODE_mitma_pa$geom <- NULL
NATCODE_mitma_pa <- unique(NATCODE_mitma_pa[,c("year_detec", "id_mitma")])
NATCODE_mitma_pa <- NATCODE_mitma_pa %>% group_by(year_detec) %>% 
  summarise(sum_munis=n())

# Compute cumulative num municipalities
NATCODE_mitma_pa$cum <- cumsum(NATCODE_mitma_pa$sum_munis)

# Compute inflows and map  -------------------------------------------------------------
# Load data comarcas shapefile
comarcas <- read_sf(paste0(path_out,"ComarcasAgrarias.shp")) # Download files (see details in readme Data Raw section)
comarcas <- comarcas[comarcas$DS_CCAA != "Ceuta" &
                       comarcas$DS_CCAA != "Melilla" &
                       comarcas$DS_CCAA != "Islas Baleares" &
                       comarcas$DS_CCAA != "Canarias", ]

# Load flows data
path <- paste0(path_out,"flows_apr_2023_nov_2023_mitma_ESP_com_v2.csv")
flows_df <-read.csv(path)
flows_df <- flows_df[,-1]
diag(flows_df) <- 0
pa_obs <- read.csv(paste0(path_out,"pa_com.csv"))
flows_df <- data.frame(CO_COMARCA = pa_obs$CO_COMARCA, inflows = rowSums(flows_df))
flows_df <- comarcas %>% left_join(flows_df)

# Map increase suitable days ESP 2004-2023 ------------------------------------
df_clim_ESP <-read.csv(paste0(path_out,"3_rm_alb_ESP_com_0_2_v2.csv"))

# Convert from wide to long format
df_clim_ESP <- df_clim_ESP %>%
  pivot_longer(cols = starts_with("R0_alb."),  # Select columns to reshape
               names_to = "R0_alb",           # Name for the new categorical column
               values_to = "Value")         # Name for the values column

head(df_clim_ESP)

# Extract CO_COMARCA
df_clim_ESP <- df_clim_ESP %>%
  mutate(CO_COMARCA = str_extract(R0_alb, "\\d+$"))
df_clim_ESP <- df_clim_ESP[, c(2,4,5)]

# Aggregate by year
df_clim_ESP$year <- year(df_clim_ESP$date)
df_clim_ESP$bool <- ifelse(df_clim_ESP$Value>1,1,0 )
df_clim_ESP_agg <- df_clim_ESP %>% group_by(CO_COMARCA, year) %>% 
  summarise(suit_days = sum(bool),
            mean_RM = mean(Value))

# Plot average number of suitable days  -----------------------------------
df_clim_ESP_agg_y <- df_clim_ESP_agg %>% group_by(CO_COMARCA) %>% 
  summarise(suit_days = mean(suit_days),
            mean_RM = mean(mean_RM))
df_clim_ESP_agg_y$CO_COMARCA <- as.integer(df_clim_ESP_agg_y$CO_COMARCA)
df_clim_ESP_agg_y <- comarcas %>%  left_join(df_clim_ESP_agg_y)
# df_suit_days_avg <- ggplot(df_clim_ESP_agg_y) +
#   geom_sf(aes(fill = suit_days), color =NA)  +
#   geom_sf(data = ESP_per, color = "black", fill = NA, size = 0.8) +
#   scale_fill_distiller(palette = "Spectral", name = "Average number \n suitable days 2005-2023  ") +
#   theme_void() +
#   theme(legend.position = c(0.9,0.2),
#         legend.title = element_text(size = 12),
#         legend.text = element_text(size = 12)) 
# 
# df_suit_days_avg

plot_num_munis <- ggplot(NATCODE_mitma_pa[!is.na(NATCODE_mitma_pa$year_detec),]) +
  geom_line(aes(year_detec,cum)) +
  xlab("Year") + ylab("Number of regions") +
  geom_vline(xintercept = 2014, linetype = "dashed",size = 1.5,  color = "#fddd1d") +
  
  theme_bw() +
  theme( plot.margin = margin(40, 10, 40, 10, "pt"),
         plot.title = element_text(size = 20), # Change title size
         axis.title.x = element_text(size = 15), # Change x-axis title size
         axis.title.y = element_text(size = 15), # Change y-axis title size
         axis.text.x = element_text(size = 15), # Change x-axis text size
         axis.text.y = element_text(size = 15),
         legend.title = element_text(size = 12),
         legend.text = element_text(size = 12)) # Change y-axis text size
plot_num_munis


# Plot map inflows
col_bor <- "#383838"
inflows_map <- ggplot(flows_df) +
  geom_sf(aes(fill = log10(inflows)), color = col_bor,linewidth = 0.3) +
  geom_sf(data = ESP_per, color = "black", fill = NA, size = 0.8) +
  scale_fill_distiller(palette = "Spectral",
  # scale_fill_gradientn(colors = pal(10),
  #                      name = "Inflows (log10) \n ",
                       na.value = "#FCFCFC") +
  theme_void() + theme( plot.margin = margin(40, 10, 40, 10, "pt"),
                        plot.title = element_text(size = 20), # Change title size 
                        legend.title = element_text(size = 12),
                        legend.text = element_text(size = 12)) 
inflows_map

# Mean rm
library(latex2exp)
df_suit_days_avg <- ggplot(df_clim_ESP_agg_y) +
  geom_sf(aes(fill = mean_RM), color = col_bor,linewidth = 0.3)  +
  geom_sf(data = ESP_per, color = "black", fill = NA, size = 0.8) +
  scale_fill_distiller(palette = "Spectral",
                       name = TeX("Average $R_M$"),
                       na.value = "#FCFCFC") +
  theme_void() +
  theme(legend.position = c(0.9,0.2),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) 

df_suit_days_avg

# Plot obs by year comarcas -----------------------------------------
pa_obs <- comarcas %>%  left_join(pa_obs)
pa_obs[pa_obs$year_detec == 0, ]$year_detec <- NA
year_detec <- ggplot(pa_obs) +
  geom_sf(aes(fill = year_detec), color = col_bor,linewidth = 0.3) +
  geom_sf(data = ESP_per, color = "black", fill = NA, size = 0.8) +
  scale_fill_distiller(palette = "Spectral", direction = 1,
                      name = "Year detected \n  ",
                      breaks = c(2004,2014,2023),
                      labels = c("2004","2014","2023"),
                      na.value = "#FCFCFC") +
  theme_void() + theme(legend.position = "top",
                       legend.text = element_text(size = 12),
                       legend.title = element_text(size = 12))
year_detec

# Arrange 4 panels
leg_pos <- c(0.9,0.2)
gg1 <- ggarrange(plot_num_munis + theme(plot.margin = margin(20, 20, 20, 20, "pt")), 
                 year_detec + theme(legend.position= leg_pos,
                                    legend.direction = "vertical",
                                    plot.margin = margin(10, 10, 10, 10, "pt")),
                 df_suit_days_avg +
                   theme(legend.position = leg_pos,
                         legend.direction = "vertical",
                         plot.margin = margin(10, 10, 10, 10, "pt")),
                 inflows_map+
                   theme(legend.direction = "vertical",
                         legend.position = leg_pos,
                         plot.margin = margin(10, 10, 10, 10, "pt")),
                 ncol = 2,nrow = 2, labels = c("a", "b", "c", "d"))
gg1

# Figure 1 main text of the manuscript
ggsave(paste0(path_plots,"panel1_v1.png"),
       gg1, width =9.5, height = 7, dpi = 300)

ggsave(paste0(path_plots,"panel1_v1.pdf"),
       gg1, width =10, height = 8.5)

