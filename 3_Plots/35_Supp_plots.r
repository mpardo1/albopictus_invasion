# Code to compute plots for the Supplementary
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

# Data path
path_out <- "data/output/"
path_plots <- "plots/" # Select a path to save the plots

# Supplementary plot ------------------------------------------------
# Plot correlation distance mobility -------------------------------------------
flows_mat <- read.csv(paste0(path_out,"flows_apr_2023_nov_2023_mitma_ESP_com_v2.csv"))
diag(flows_mat) <- 0
dist_mat <- read.csv(paste0(path_out,"dist_mat_com_ESP.csv"))
diag(dist_mat) <- 0
max(flows_mat)
flows_mat <- as.matrix(flows_mat)
flows_mat <- flows_mat[,2:ncol(flows_mat)]
dist_mat <- as.matrix(dist_mat)
dist_mat <- dist_mat[,2:ncol(dist_mat)]
dist_flows_df <- data.frame(dist = c(as.numeric(dist_mat)),
                            flows =  c(flows_mat))
dist_flows_df <- dist_flows_df[dist_flows_df$flows != 0 & dist_flows_df$dist != 0, ]
size_let = 14

corr_flows_dist <- ggplot(dist_flows_df) + 
  geom_point(aes(log10(dist), log10(flows)), size = 0.1) +
  ylab("Number of people moving, log10") + 
  xlab("Distance, log10") +
  theme_bw() + 
  theme(plot.margin = margin(40, 10, 40, 10, "pt"),
        axis.title = element_text(size = size_let),   # Increase axis title font size
        axis.text = element_text(size = size_let),    # Increase axis text font size
        plot.title = element_text(size = size_let),   # Increase plot title font size
        legend.title = element_text(size = size_let), # Increase legend title font size
        legend.text = element_text(size = size_let)   # Increase legend text font size
  ) +
  geom_smooth(aes(log10(as.numeric(dist)), log10(flows)), method="lm") +
  scale_x_reverse()  # Reverse the x-axis

corr_flows_dist

cor(log10(dist_flows_df$dist),log10(dist_flows_df$flows))

# Save plots
ggsave(paste0(path_plots,"corr_flows_dist_corr_0_49.png"),
       corr_flows_dist, width =8, height = 6, dpi = 300)

ggsave(paste0(path_plots,"corr_flows_dist_corr_0_49.pdf"),
       corr_flows_dist, width = 8, height = 6, dpi = 300)

# Plot number of suitable days -----------------------------------------------
list_n <- c(3201,1502,3105,3307) # Atlantico/cantabrico
list_n <- c(4107,1105,304,1720,1207) # Mediterraneo
list_n <- c(3201,1502,3105,4107,1105,304)
list_names <- comarcas[comarcas$CO_COMARCA %in% list_n,c("DS_PROVINC", "CO_COMARCA")]
df_clim_ESP_agg$CO_COMARCA_fact <- factor(df_clim_ESP_agg$CO_COMARCA,
                                          levels = sort(unique(df_clim_ESP_agg$CO_COMARCA)))

trend_lines <- ggplot(df_clim_ESP_agg[df_clim_ESP_agg$CO_COMARCA_fact %in% list_n,]) +
  geom_point(aes(year, suit_days, color = CO_COMARCA_fact), size = 1.3) +
  geom_smooth(aes(year, suit_days, color = CO_COMARCA_fact), size = 1.3, method= "lm") +
  theme_bw() +
  ylab("Number of suitable days") +
  scale_colour_brewer(palette = "Dark2",
                      name = "Comarca",
                      labels = c("Cádiz", "A Coruña", "Alicante", "Navarra", "Ourense", "Sevilla")) +
  theme(legend.position = "top",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))


# Compute difference in suitable days 2023 to 2004
df_clim_ESP_agg_04_23 <- df_clim_ESP_agg[df_clim_ESP_agg$year %in% c(2005,2023),]

# Convert from long to wide format
df_clim_ESP_agg_04_23 <- df_clim_ESP_agg_04_23 %>%
  pivot_wider(names_from = year, values_from = suit_days, names_prefix = "Year_")
df_clim_ESP_agg_04_23$diff_suit_days <- df_clim_ESP_agg_04_23$Year_2023 - df_clim_ESP_agg_04_23$Year_2005

# Plot in a  map
df_clim_ESP_agg_04_23$CO_COMARCA <- as.integer(df_clim_ESP_agg_04_23$CO_COMARCA)
df_clim_ESP_agg_04_23 <- comarcas %>%  left_join(df_clim_ESP_agg_04_23)
ggplot(df_clim_ESP_agg_04_23) +
  geom_sf(aes(fill = diff_suit_days), color =NA)  +
  scale_fill_distiller(palette = "Spectral", name = "Diff\n suitable days") +
  theme_void() +
  theme(legend.position = c(0.9,0.2),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) 

# Compute slope linear regression
trend_data <- df_clim_ESP_agg %>%
  group_by(CO_COMARCA) %>%
  summarize(
    slope = coef(lm(suit_days ~ year))[2],  # Extrae la pendiente (tendencia)
    p_value = summary(lm(suit_days ~ year))$coefficients[2,4] # Extrae p-value
  ) %>%
  ungroup()

# Reload comarcas shapefile 
comarcas <- read_sf(paste0(path_out,"ComarcasAgrarias.shp"))
comarcas <- comarcas[comarcas$DS_CCAA != "Ceuta" &
                       comarcas$DS_CCAA != "Melilla" &
                       comarcas$DS_CCAA != "Islas Baleares" &
                       comarcas$DS_CCAA != "Canarias", ]

# Join with slope data frame
trend_data$CO_COMARCA <- as.integer(trend_data$CO_COMARCA)
trend_data <- comarcas %>%  left_join(trend_data)
trend_plot <- ggplot(trend_data) +
  geom_sf(aes(fill = slope), color =NA)  +
  geom_sf(data = ESP_per, color = "black", fill = NA, size = 0.6) +
  scale_fill_distiller(palette = "Spectral", name = "Trend suitable\n days 2005-2023") +
  theme_void() +
  theme(legend.position = c(0.9,0.2),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) 
trend_plot

# Create panel
gg1 <- ggarrange(trend_lines,trend_plot )

# Save trend plot
ggsave(paste0(path_plots,"trend_plot.png"),
       gg1, width =13, height = 6, dpi = 300)

ggsave(paste0(path_plots,"trend_plot.pdf"),
       gg1, width = 13, height = 6, dpi = 300)

# Map ECDC vs MA ESP -------------------------------------------------
PA_ESP <- read_sf(paste0(path_out,"status.shp")) # Data request to the ECDC https://www.ecdc.europa.eu/en/publications-data/request-eueaa-surveillance-data
PA_ESP <- PA_ESP[PA_ESP$cntryCode == "ES" & PA_ESP$ECDC == 1 &
                   !(PA_ESP$locName %in% c("Ceuta", "Melilla",
                                           "La Palma", "Lanzarote", "El Hierro",
                                           "Eivissa y Formentera", "Fuerteventura",
                                           "Tenerife" ,"Mallorca" , "Gran Canaria" , 
                                           "Menorca", "La Gomera")),]

# Update with May 2024
PA_ESP[PA_ESP$locName == "Lugo",]$albopictus <- "introduced_ECDC"
PA_ESP[PA_ESP$locName == "Cantabria",]$albopictus <- "absent"

# Read ESP perimeter
ESP_per <- readRDS(paste0(path_out,"Spain_perimeter.Rds"))

# Plot ECDC
ECDC_map <- ggplot() +
  geom_sf(data=PA_ESP, color =NA, aes(fill=albopictus)) +
  geom_sf(data=ESP_per, color ="black", fill=NA) +
  scale_fill_manual(name = "",values= c("#018E42", "#ED1C24", "#E6F14A", "#E7E7E7"),
                    labels = c("Absent", "Established", "Introduced", "No data")) +
  theme_void() + theme(legend.position = "top",
                       legend.title = element_text(size = 12),
                       legend.text = element_text(size = 12))

NATCODE_mitma_pa$PA <- ifelse(is.na(NATCODE_mitma_pa$year_detec), 0,1)
MA_map <- ggplot() +
  geom_sf(data=NATCODE_mitma_pa, color =NA, alpha = 0.6,
          size = 0.01, aes(fill=as.factor(PA))) +
  geom_sf(data=ESP_per, color ="black", fill=NA) +
  scale_fill_manual(name = "",values= c( "#E7E7E7","#018E42"),
                    labels = c( "No detected/No data","Detected")) +
  theme_void() + theme(legend.position = "top",
                       legend.title = element_text(size = 12),
                       legend.text = element_text(size = 12))

panel_ecdc_MA <- ggarrange(ECDC_map, MA_map) 
ggsave(paste0(path_plots,"panel_ecdc_MA.png"),
       panel_ecdc_MA, width =11, height = 6, dpi = 300)

ggsave(paste0(path_plots,"panel_ecdc_MA.pdf"),
       panel_ecdc_MA, width =11, height = 6, dpi = 300)
