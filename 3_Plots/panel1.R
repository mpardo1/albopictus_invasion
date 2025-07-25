# Code to compute plots for panel 1, data panel
rm(list=ls())
library(mapSpain)
library(sf)
library(dplyr)
library(tidyverse)
library(data.table)
library(ggplot2)
library(ggpubr)

# Data path
path_in <- "data/output/"
path_plots <- "plots/"

# Load data --------------------------------------------------------------
# Load data pa with mitma boundaries
NATCODE_mitma_pa <- readRDS(paste0(path_in,"pa_mitma.Rds")) # File taken from analyze_stas_mitma.R

# Load Spanish muni
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
esp_can <- esp_can[esp_can$ine.ccaa.name != "Ceuta" &
                     esp_can$ine.ccaa.name != "Melilla" &
                     esp_can$ine.ccaa.name != "Balears, Illes" &
                     esp_can$ine.ccaa.name != "Canarias", ]

# Join two data sets to have geomtries
NATCODE_mitma_pa <- esp_can %>% left_join(NATCODE_mitma_pa)

# compute the perimeter of Spain to draw
ESP_per <- esp_get_ccaa_siane()
ESP_per <- ESP_per[!(ESP_per$ine.ccaa.name %in% c("Balears, Illes" ,"Ceuta" ,"Melilla","Canarias"   )),]
ESP_per <- st_make_valid(ESP_per)
ESP_per <- st_union(ESP_per)
saveRDS(ESP_per, paste0(path_in,"/Spain_perimeter.Rds"))

# Aggregate by year_detec to have number of municipalities detected each year
NATCODE_mitma_pa_plot <- NATCODE_mitma_pa
NATCODE_mitma_pa$geometry <- NULL
NATCODE_mitma_pa <- unique(NATCODE_mitma_pa[,c("year_detec", "id_mitma")])
NATCODE_mitma_pa <- NATCODE_mitma_pa %>% group_by(year_detec) %>% 
  summarise(sum_munis=n())

# Compute cumulative num municipalities
NATCODE_mitma_pa$cum <- cumsum(NATCODE_mitma_pa$sum_munis)

# Compute inflows and map  -------------------------------------------------------------
# Load data comarcas shapefile
comarcas <- read_sf(paste0(path_in,"ComarcasAgrarias.shp"))
comarcas <- comarcas[comarcas$DS_CCAA != "Ceuta" &
                       comarcas$DS_CCAA != "Melilla" &
                       comarcas$DS_CCAA != "Islas Baleares" &
                       comarcas$DS_CCAA != "Canarias", ]

# Load flows data
path <- paste0(path_in,"flows_apr_2023_nov_2023_mitma_ESP_com_v2.csv")
flows_df <-read.csv(path)
flows_df <- flows_df[,-1]
diag(flows_df) <- 0
pa_obs <- read.csv(paste0(path_in,"pa_com.csv"))
flows_df <- data.frame(CO_COMARCA = pa_obs$CO_COMARCA, inflows = rowSums(flows_df))
flows_df <- comarcas %>% left_join(flows_df)

# Map increase suitable days ESP 2004-2023 ------------------------------------
df_clim_ESP <-read.csv(paste0(path_in,"rm_alb_ESP_com.csv"))

# Load data comarcas shapefile
comarcas <- read_sf(paste0(path_in,"ComarcasAgrarias.shp"))
comarcas <- comarcas[comarcas$DS_CCAA != "Ceuta" &
                       comarcas$DS_CCAA != "Melilla" &
                       comarcas$DS_CCAA != "Islas Baleares" &
                       comarcas$DS_CCAA != "Canarias", ]
# Load necessary library
library(tidyr)

# Convert from wide to long format
df_clim_ESP <- df_clim_ESP %>%
  pivot_longer(cols = starts_with("R0_alb."),  # Select columns to reshape
               names_to = "R0_alb",           # Name for the new categorical column
               values_to = "Value")         # Name for the values column

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

# Add mosquito Alert picture
library(ggimage)
df <- data.frame(
  x = 2010.5,
  y = 700,
  image = paste0(path_in,"MAlogo.png")  # Replace with the path to your image
)
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
plot_num_munis <- plot_num_munis +
  geom_image(data=df, aes(x,y,image = image), size = 0.3)
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

ggsave(paste0(path_plots,"panel1_v1.png"),
       gg1, width =9.5, height = 7, dpi = 300)

ggsave(paste0(path_plots,"panel1_v1.pdf"),
       gg1, width =10, height = 8.5)

# Supplementary plot ------------------------------------------------
# Plot correlation distance mobility -------------------------------------------
flows_mat <- read.csv(paste0(path_in,"flows_apr_2023_nov_2023_mitma_ESP_com_v2.csv"))
diag(flows_mat) <- 0
dist_mat <- read.csv(paste0(path_in,"dist_mat_com_ESP.csv"))
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
comarcas <- read_sf(path_in,"ComarcasAgrarias.shp"))
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
ggsave(path_plots,"trend_plot.png"),
       gg1, width =13, height = 6, dpi = 300)

ggsave(path_plots,"trend_plot.pdf"),
       gg1, width = 13, height = 6, dpi = 300)
# Map ECDC vs MA ESP -------------------------------------------------
PA_ESP <- read_sf(path_in,"status.shp"))
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
ESP_per <- readRDS(path_plots,"Spain_perimeter.Rds"))

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
