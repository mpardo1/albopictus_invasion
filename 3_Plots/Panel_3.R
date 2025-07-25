# Code to reproduce panel 2 results paper
rm(list=ls())
library(data.table)
library(ggplot2)
library(tidyverse)
library("RColorBrewer")
library(ggpubr)
library(sf)
library(ggsci)
library(mapSpain)

# Data path
path_in <- "data/output/"
path_plots <- "plots/"

# Run before anything -----------------------------------------
# Load shapefile
comarcas <- read_sf(paste0(path_in,"ComarcasAgrarias.shp"))
comarcas <- comarcas[comarcas$DS_CCAA != "Ceuta" &
                       comarcas$DS_CCAA != "Melilla" &
                       comarcas$DS_CCAA != "Islas Baleares" &
                       comarcas$DS_CCAA != "Canarias", ]

# Spain perimeter
ESP_per <- esp_get_ccaa_siane()
ESP_per <- ESP_per[!(ESP_per$ine.ccaa.name %in% c("Balears, Illes" ,"Ceuta" ,"Melilla","Canarias"   )),]
ESP_per <- st_make_valid(ESP_per)
ESP_per <- st_union(ESP_per)

# --------------------------------------------------------------
# Plots RM 2023 -------------------------------------------------
df_clim_ESP <-read.csv(paste0(path_in,"rm_alb_ESP_com.csv"))
df_clim_ESP <- df_clim_ESP[year(df_clim_ESP$date) == 2023,]

# Convert to long format
df_clim_ESP <- df_clim_ESP[,c(2:ncol(df_clim_ESP))] %>%
  pivot_longer(cols = starts_with("R0_alb."), # Select columns to pivot
               names_to = "CO_COMARCA",          # Name for new column
               values_to = "R0_alb")        # Name for values column
df_clim_ESP$CO_COMARCA <- as.numeric(gsub("R0_alb\\.", "", df_clim_ESP$CO_COMARCA))

# Compute number of suitable days
df_clim_ESP$bool <- ifelse(df_clim_ESP$R0_alb>1,1,0)
df_clim_ESP <- df_clim_ESP %>%  group_by(CO_COMARCA) %>% 
  summarise(sum_days = sum(bool))

# Join with shapefile
df_clim_ESP <- comarcas %>% left_join(df_clim_ESP)
suit_days <- ggplot(df_clim_ESP) + 
  geom_sf(aes(fill = sum_days), alpha= 0.8, color = NA) +
  geom_sf(data = ESP_per, color = "black", fill = NA) +
  scale_fill_distiller(palette = "Spectral", name = "Suitable days\n ",
                       limits = c(0,258),
                       breaks = c(0,125,258)) +
  theme_void() +
  theme(legend.position = "top",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

# Plot inflows prensece comarca 2023 --------------------------------------
flows_df <- read.csv(paste0(path_in,"flows_com_df.csv"))
flows_df$X <- NULL
flows_df$CO_COMARCA <- flows_df$CO_COMARCA_origin
pa_com <- read.csv(paste0(path_in,"/pa_com.csv"))
pa_com$X <- NULL

# Join presence data set with the origin
flows_df <- flows_df %>%  left_join(pa_com)
flows_df <- flows_df[flows_df$year_detec != 0,]
flows_df <- flows_df %>%  group_by(CO_COMARCA_destination) %>% 
  summarise(sum_trips = sum(mean_n_trips))

# Join with shapefile
flows_df <- comarcas %>% left_join(flows_df, by = join_by(CO_COMARCA == CO_COMARCA_destination))
inflows_map <- ggplot(flows_df) + 
  geom_sf(aes(fill = log10(sum_trips)), alpha= 0.8, color = NA) +
  geom_sf(data = ESP_per, color = "black", fill = NA) +
  scale_fill_distiller(palette = "Spectral", name = "Inflows (log10)\n ",
                       limits = c(3.5,6.6),
                       breaks = c(3.5,5,6.5)) +
  theme_void() +
  theme(legend.position = "top",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

inflows_map
max(log10(flows_df$sum_trips))
min(log10(flows_df$sum_trips))

# Sigmoidal no log10 ----------------------------------------------------
param <- c(0.0005706731568571639, 97.78894801162276, 5424.950376421077,
           6.977623970027249e-5, 51314.77501452205, -6.874687389443809, -80.94731156667282)
m_c = 0.0051

# Sigmoid function
sigmoid <- function(x){param[1]/(1+exp(-param[2]*m_c*x+ param[3]))}
flows_mat <- read.csv(paste0(path_in,"flows_apr_2023_nov_2023_mitma_ESP_com_v2.csv"))[,-1]
vec <- seq(0, max(flows_mat,na.rm =TRUE), 100)
out <- sapply(vec, sigmoid)
df_out_exp <- data.frame(vec,out_med = out)
sig_plot <- ggplot(df_out_exp) +
  geom_line(aes(vec, out_med)) +
  ylab("Colonization rate") +
  xlab("Number of people") +
  theme_bw() +
  theme(legend.position = c(0.7,0.8),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key.height = unit(0.4,"cm"),
        legend.key.width = unit(0.4,"cm"),
        plot.title = element_text(size = 20), # Change title size 
        axis.title.x = element_text(size = 15), # Change x-axis title size 
        axis.title.y = element_text(size = 15), # Change title size 
        axis.text.x = element_text(size = 12), # Change x-axis title size 
        axis.text.y = element_text(size = 12))

min(df_out_exp[df_out_exp$out_med > 1.e-12,]$vec)
min(df_out_exp[df_out_exp$out_med == max(df_out_exp$out_med),]$vec)

# Plot the flows that are with colonimax()# Plot the flows that are with colonization rate big ----------------------
library(maptools)
flows_df <- read.csv(paste0(path_in,"flows_com_df.csv"))

# Remove flows from the same origin and destination
flows_df$X <- NULL
flows_df <- flows_df[flows_df$CO_COMARCA_destination !=flows_df$CO_COMARCA_origin,]
flows_thres <- min(df_out_exp[df_out_exp$out>=1.e-308, "vec"])  #round(param[3]/(param[2]*m_c),2)
flows_df_filt <- flows_df[flows_df$mean_n_trips>=flows_thres,] # Remove flows with low colonization rate

# Add presence information in origin
flows_df_filt <- flows_df_filt %>%  left_join(pa_com, by = join_by(CO_COMARCA_origin == CO_COMARCA))
flows_df_filt <- flows_df_filt[flows_df_filt$year_detec !=0,]

# Compute centroids
comarcas <- st_make_valid(comarcas)
comarcas_cent <- st_centroid(comarcas[,"CO_COMARCA"])

# Join centroids with flows
flows_df_filt_or <- merge(flows_df_filt, comarcas_cent, by.x = "CO_COMARCA_origin",by.y = "CO_COMARCA" )
names(flows_df_filt_or)[5] <- "centroid_origin"
flows_df_filt_or_dest <- merge(flows_df_filt_or, comarcas_cent, by.x = "CO_COMARCA_destination",by.y = "CO_COMARCA" )
names(flows_df_filt_or_dest)[6] <- "centroid_destination"

# Extract lon lat
flows_df_filt_or_dest$or.x=st_coordinates(st_as_sf(flows_df_filt_or_dest$centroid_origin))[,1]
flows_df_filt_or_dest$or.y=st_coordinates(st_as_sf(flows_df_filt_or_dest$centroid_origin))[,2]
flows_df_filt_or_dest$dest.x=st_coordinates(st_as_sf(flows_df_filt_or_dest$centroid_destination))[,1]
flows_df_filt_or_dest$dest.y=st_coordinates(st_as_sf(flows_df_filt_or_dest$centroid_destination))[,2]

# Plot aesthetics
xquiet<- scale_x_continuous("", breaks=NULL)
yquiet<-scale_y_continuous("", breaks=NULL)
quiet<-list(xquiet, yquiet)

# Palette
library(paletteer)
pal <- rev(paletteer_c("viridis::cividis", 14))
pal <- paletteer_c("ggthemes::Blue-Green Sequential", 30)
pal <- paletteer_c("grDevices::Blue-Yellow", 30)
pal <- paletteer_c("grDevices::Viridis", 30)
#pal <- paletteer_c("ggthemes::Temperature Diverging", 13)
pal <- colorRampPalette(pal)

# Plot flows
flows_thres_plot <- ggplot()+
  geom_sf(data = comarcas, color = "grey", fill = NA, size = 0.1, alpha = 0.7) +
  geom_sf(data = ESP_per, color = "black", fill = NA) +
  geom_segment(data = flows_df_filt_or_dest,
               aes(x=or.x, y=or.y,xend=dest.x,
                   yend=dest.y, color=log10(mean_n_trips)),alpha = 0.7,size = 0.8)+
  # scale_color_distiller(palette = "Spectral", name = "Inflows \n",
  #                       breaks=c(4.1,4.8,5.6)) +
  # scale_color_viridis_c(name = "Inflows (log10)\n",option = "magma") +
  scale_color_gradientn(colors = pal(10),
                       name = "Inflows (# people)   \n",
                       breaks = c(4,4.5,5,5.5),
                       labels = c(
                         expression(10^4),
                         expression(10^4.5),
                         expression(10^5),
                         expression(10^5.5)
                       )) +
  theme_void() +
  theme(legend.position = "top",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

flows_thres_plot

# # Save plots
# ggsave(paste0(path_plots,"flows_thres_julia_prec.png"),
#        flows_thres, width =8, height = 6, dpi = 300)
# 
# ggsave(paste0(path_plots,"flows_thres_julia_prec.pdf"),
#        flows_thres, width = 8, height = 6, dpi = 300)

# Check the distance threshold given by the model-----------------------------
dist_mat <- read.csv(paste0(path_in,"dist_mat_com_ESP.csv"))
comarcas <- st_make_valid(comarcas)
comarcas$area <- st_area(comarcas)
comarca_unique <- comarcas %>%
  group_by(CO_COMARCA) %>%          # Group by ID
  slice_max(order_by = area, n = 1) %>%  # Keep the row with the highest area
  ungroup()  # Ungroup to return a normal dataframe

dist_mat <- dist_mat[,-1]
ind = sample(1:338,1)
dist_df <- data.frame(CO_COMARCA = comarca_unique$CO_COMARCA, dist = as.numeric(dist_mat[ind,]))
dist_df <- comarca_unique %>%  left_join(dist_df)
dist_df_filt <- dist_df[dist_df$dist <= 35000,]
ggplot(dist_df_filt) +
  geom_sf(data = ESP_per, color = "black", fill = NA) +
  geom_sf(data = comarcas, color = "black", fill = NA) +
  geom_sf(aes(fill=log10(as.numeric(dist)+0.001)), color = NA, alpha = 0.6) +
  scale_fill_viridis_c(name = "dist  \n") +
  theme_void() +
  theme(legend.position = "top" ,
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))

# Plot distances histogram of neightbour geometries---------------------------
dist_mat <- read.csv(paste0(path_in,"dist_mat_com_ESP.csv"))
dist_mat <- dist_mat[,-1]

# Compute neightbout distances
dist_neight <- readRDS(paste0(path_in,"dist_neight.Rds"))

# Join two distances
dist_neight_cent <- data.frame(dist_neig = as.numeric(c(dist_neight)),
                               dist_cent = c(as.matrix(dist_mat)))
dist_neight_cent <- dist_neight_cent[dist_neight_cent$dist_cent != 0,] # Remove distance to itself
ggplot(dist_neight_cent[dist_neight_cent$dist_neig==0,]) +
  geom_density(aes(dist_cent), fill = "#69b3a2") +
  theme_minimal() + xlab("Distance (m)")

# Distance between "important" flows 
flows_mat <- read.csv(paste0(path_in,"flows_apr_2023_nov_2023_mitma_ESP_com_v2.csv"))
dist_mat <- read.csv(paste0(path_in,"dist_mat_com_ESP.csv"))
flows_mat <- as.matrix(flows_mat)
flows_mat <- flows_mat[,2:ncol(flows_mat)]
dist_mat <- as.matrix(dist_mat)
dist_mat <- dist_mat[,2:ncol(dist_mat)]
dist_flows_df <- data.frame(dist = c(as.numeric(dist_mat)),
                            flows =  c(flows_mat))

flows_thres <- min(df_out_exp[df_out_exp$out > 1.e-308,]$vec)
dist_flows_df_1 <- dist_flows_df[dist_flows_df$flows != 0 & dist_flows_df$dist != 0
                               & dist_flows_df$flows > flows_thres, ]
flows_thres_max <- min(df_out_exp[df_out_exp$vec >= 11000,]$vec)
dist_flows_df_2 <- dist_flows_df[dist_flows_df$flows != 0 & dist_flows_df$dist != 0
                                 & dist_flows_df$flows > flows_thres_max, ]

# Fake data set for kafe legend
df_fake <-data.frame(x=0, y=0, category = c("Neighbour","Colonization rate >0","Colonization rate max"))

# Choose color
col_flow <- "#46307E"
library(scales)

# Plot both distribtuion distance neightbours and distance flows
distri_plot <- ggplot() +
  geom_density(data= dist_flows_df_1,
               aes(dist), fill = col_flow, alpha = 0.6) +
  geom_vline(xintercept = mean(dist_flows_df_1$dist),
             color =col_flow, linetype = "dashed") +
  theme_minimal() + xlab("Distance (m)") + ylab("") +
  theme(legend.position = "top",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key.height = unit(0.4,"cm"),
        legend.key.width = unit(0.4,"cm"),
        plot.title = element_text(size = 20), # Change title size 
        axis.title.x = element_text(size = 15), # Change x-axis title size 
        axis.title.y = element_text(size = 15), # Change title size 
        axis.text.x = element_text(size = 12), # Change x-axis title size 
        axis.text.y = element_text(size = 12))

distri_plot

# Save the plot
ggsave(paste0(path_plots,"distibution_dist_mob.png"),
       dpi = 350, height = 4, width = 7)
ggsave(paste0(path_plots,"distibution_dist_mob.pdf"),
       height = 4, width = 7)

# Add points to sig plot
sig_plot <- ggplot() +
  geom_line(data = df_out_exp[df_out_exp$vec < flows_thres,],
            aes(log10(vec), out_med), size = 1, color = "grey") +
  geom_line(data = df_out_exp[df_out_exp$vec > flows_thres,],
            aes(log10(vec), out_med), color = col_flow, size = 1) +
  geom_vline(aes(xintercept =log10(flows_thres)),
             color = col_flow, linetype = "dashed") +
  ylab("Colonization rate") +
  xlab("Human flows (# people)") +
  theme_bw() +
  scale_y_continuous(breaks = c(0, 2.e-4,4.e-4),
                     labels = c("0", "2e-4","4e-4")) +
  scale_x_continuous(breaks = c(2, 3,4,5,6),
                     labels = c(
                       expression(10^2),
                       expression(10^3),
                       expression(10^4),
                       expression(10^5),
                       expression(10^6)
                     )) +
  theme(legend.position = c(0.7,0.8),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key.height = unit(0.4,"cm"),
        legend.key.width = unit(0.4,"cm"),
        plot.title = element_text(size = 20), # Change title size 
        axis.title.x = element_text(size = 15), # Change x-axis title size 
        axis.title.y = element_text(size = 15), # Change title size 
        axis.text.x = element_text(size = 12), # Change x-axis title size 
        axis.text.y = element_text(size = 12))

sig_plot

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
dist_flows_df <- dist_flows_df[dist_flows_df$flows != 0 &
                                 dist_flows_df$dist != 0, ]
size_let = 14

corr_flows_dist <- ggplot() + 
  geom_point(data = dist_flows_df[dist_flows_df$flows>= flows_thres,],
             aes(log10(dist), log10(flows)), size = 0.4, color = col_flow) +
  geom_point(data = dist_flows_df[dist_flows_df$flows<= flows_thres,], 
             aes(log10(dist), log10(flows)), size = 0.4, color = "grey") +
  geom_hline(aes(yintercept = log10(flows_thres)), color = col_flow) +
  ylab("Human flows (# people)") + 
  xlab("Distance (m)") +
  scale_x_continuous(breaks = c(4,
                                4.5,
                                5,
                                5.5,
                                6),
                     labels = c(
                       expression(10^4),
                       expression(10^4.5),
                       expression(10^5),
                       expression(10^5.5),
                       expression(10^6)
                     )) +
  scale_y_continuous(breaks = c(0,2,4),
                     labels = c(
                       expression(10^0),
                       expression(10^2),
                       expression(10^4)
                     )) +
  ggtitle("c") +
  theme_bw() + 
  theme(axis.title = element_text(size = size_let),   # Increase axis title font size
        axis.text = element_text(size = size_let),    # Increase axis text font size
        plot.title = element_text(size = 16,
                                  face = "bold"),   # Increase plot title font size
        legend.title = element_text(size = size_let), # Increase legend title font size
        legend.text = element_text(size = size_let)   # Increase legend text font size
  ) # Reverse the x-axis

corr_flows_dist

# Arrange panel supplementary ------------------------------------------------
# Convert the inset plot to a grob (graphical object)
inset_grob <- ggplotGrob(corr_flows_dist )

# Add the inset plot to the main plot
final_plot <- distri_plot +
  annotation_custom(
    grob = inset_grob,
    xmin = 100000, xmax = 300000,   # x-coordinates of the inset box
    ymin = 0.000001, ymax = 0.00003 # y-coordinates of the inset box
  )
final_plot

ggarrange(ggarrange(sig_plot, 
                    final_plot,
                    heights = c(1,1.5),
                    ncol = 1, nrow = 2, labels = c("a", "b")),
          flows_thres_plot, widths = c(1,1.2), labels = c("","d"))

# Save the plot
ggsave(paste0(path_plots,"panel_sup_hum_mob.png"),
       dpi = 350, height = 7, width = 13.5)
ggsave(paste0(path_plots,"panel_sup_hum_mob.pdf"),
       height = 7, width = 13.5)

# Media km distancias
mean(as.numeric(c(dist_mat)))

# Plot suitability 2023 ------------------------------------------------
pa_com <- read.csv(paste0(path_in,"pa_com.csv"))[,-1]
rm2 <- read.csv(paste0(path_in,"3_rm_alb_ESP_com.csv"))
rm2 <- rm2[year(rm2$date) == 2023,-1]

# Transform to long format
rm2 <- rm2 %>%  
  pivot_longer(
    cols = starts_with("R0_alb."),
    names_to = "CO_COMARCA",
    names_prefix = "R0_alb.",
    values_to = "RM"
  )

# Compute number of suitable days RM>1
rm2$bool <- ifelse(rm2$RM >1, 1, 0)
rm2_agg <- rm2 %>%  group_by(CO_COMARCA) %>% 
  summarise(suit_days = sum(bool),
            avg_rm = mean(RM))
rm2_agg$CO_COMARCA <- as.integer(rm2_agg$CO_COMARCA)

# Join with shapefile
rm2_agg <- comarcas %>%  left_join(rm2_agg)
suit_days <- ggplot(rm2_agg) + 
  geom_sf(aes(fill = suit_days), alpha= 0.8, color = NA) +
  geom_sf(data = ESP_per, color = "black", fill = NA) +
  scale_fill_distiller(palette = "Spectral", name = "Suitable days\n ",
                       limits = c(0,258),
                       breaks = c(0,125,258)) +
  theme_void() +
  theme(legend.position = "top",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))
suit_days

# Arrange plots
ggarrange(plot_2023, suit_days)

# Save the plot
ggsave(paste0(path_plots,"suit_days_vs_full_model.png"),
       dpi = 350, height =7 , width = 12)
ggsave(paste0(path_plots,"suit_days_vs_full_model.pdf"),
       height = 7, width = 12)

# Maximum natural dispersal ------------------------------------------------------
# Load data
df_out_nat <- read.csv(paste0(path_in,"natural_dispersal_one_year_2025-05-28.csv"))
dist_mat <- read.csv(paste0(path_in,"dist_mat_com_ESP.csv"))[,-1]

# Extract first detection
comarca_init <- df_out_nat[df_out_nat$out == 1,]$CO_COMARCA
comarcas[comarcas$CO_COMARCA == comarca_init, ] # Test

# Extract distance first detection
which(df_out_nat$CO_COMARCA == comarca_init)
dist_df_init <- data.frame(CO_COMARCA = df_out_nat$CO_COMARCA, dist = as.numeric(dist_mat[56,]))

# Test
dist_df_plot <- comarcas %>%  left_join(dist_df_init)
df_out_nat_plot <- comarcas %>%  left_join(df_out_nat)
ggplot(dist_df_plot) + geom_sf(aes(fill = dist))
ggplot(df_out_nat_plot) + geom_sf(aes(fill = out))

# Join probabilities and distance
dist_out_df_plot <- df_out_nat %>% left_join(dist_df_init)

# Quantiles
q1 <- 4.392931e-1
q2 <- 8.647432e-1
q3 <- 9.96609e-1
mean_prob <- 0.70

# Quantiles
quan <- quantile(df_out_nat$out, probs = c(0.75, 0.9,0.95,1))
nrow(df_out_nat[df_out_nat$out>=quan[1],])
hist(df_out_nat$out)

# Compute distance
days_disp <- 365
max(dist_out_df_plot[dist_out_df_plot$out>=quan[2],]$dist)/(days_disp)
max(dist_out_df_plot[dist_out_df_plot$out>=quan[3],]$dist)/(days_disp)
max(dist_out_df_plot[dist_out_df_plot$out>=quan[4],]$dist)/(days_disp)
max(dist_out_df_plot[dist_out_df_plot$out>=quan[4],]$dist)/(days_disp)
max(dist_out_df_plot[dist_out_df_plot$out>=mean_prob,]$dist)/(days_disp)
max(dist_out_df_plot[dist_out_df_plot$out>=q2,]$dist)/(days_disp)
max(dist_out_df_plot[dist_out_df_plot$out>=q1,]$dist)/(days_disp)
max(dist_out_df_plot[dist_out_df_plot$out>=0.5,]$dist)/(days_disp)
