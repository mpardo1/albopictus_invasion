# Code to compute plots for panel 2 in the main manuscript
# Remove everything before starting
rm(list=ls())

# Load packages
library(data.table)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(ggpubr)
library(sf)
library(ggsci)
library(mapSpain)
library(tidyr)
library(dplyr)

# Data path
path_out <- "data/output/" # Path to processed files
path_plots <- "plots/" # Select a path to save the plots

# Matrices simulation and observations -----------------------------
Path <- paste0(path_out,"obs_2005-2023.csv")
obs <- read.csv(Path)
Path <- paste0(path_out,"output_mean_tminRM_H_0_2_IC_2004_2025-05-23.csv")
sim_mat <- read.csv(Path)
sim_mat <- sim_mat[,-1] # Remove ids comarcas
Path <- paste0(path_out,"pa_com.csv")
pa_com <- read.csv(Path)

# Convert to a data frame
colnames(sim_mat) <- 2005:2023 
df_sim <- as.data.frame(sim_mat)
df_sim$CO_COMARCA <- pa_com$CO_COMARCA # Add Comarca names as a column

# Reshape from wide to long format
df_sim <- df_sim %>%
  pivot_longer(cols = -CO_COMARCA, names_to = "Year", values_to = "sim") %>%
  mutate(Year = as.integer(Year))  # Ensure Year is numeric

# Convert to a data frame
colnames(obs) <- c("CO_COMARCA", 2005:2023)
df_obs <- as.data.frame(obs)

# Reshape from wide to long format
library(tidyr)
library(dplyr)
df_obs <- df_obs %>%
  pivot_longer(cols = -CO_COMARCA, names_to = "Year", values_to = "obs") %>%
  mutate(Year = as.integer(Year))  # Ensure Year is numeric

# Join both data sets
df_obs_sim <- df_obs %>%  left_join(df_sim)
df_obs_sim$diff <- (df_obs_sim$obs-df_obs_sim$sim)^2

# Plot box plot
# bp <- ggplot(out_df,aes(as.factor(PA), sim, fill = as.factor(PA))) +
bp <- ggplot(df_obs_sim,aes(as.factor(obs), sim, fill=as.factor(obs))) +
  geom_jitter(color = "black", alpha = 0.2, size = 0.4)+
  geom_boxplot(alpha = 0.7) +
  # scale_fill_brewer(name = "" , palette = Palette, direction =-1) +
  # scale_fill_viridis_d(name = "" , option ="B") +
  scale_fill_bmj(name = "" ) +
  ylab("Occupancy probability, p(t)") +
  xlab("Observations") +
  theme_bw() + 
  theme( plot.margin = margin(10, 40, 10, 40, "pt"),
         legend.position = "none",
         plot.title = element_text(size = 20), # Change title size 
         axis.title.x = element_text(size = 15), # Change x-axis title size 
         axis.title.y = element_text(size = 14), # Change y-axis title size 
         axis.text.x = element_text(size = 15), # Change x-axis text size 
         axis.text.y = element_text(size = 15))
bp

# Map with LS of all years ---------------------------------------------
# Load data comarcas shapefile
comarcas <- read_sf(paste0(path_out,"ComarcasAgrarias.shp"))
comarcas <- comarcas[comarcas$DS_CCAA != "Ceuta" &
                       comarcas$DS_CCAA != "Melilla" &
                       comarcas$DS_CCAA != "Islas Baleares" &
                       comarcas$DS_CCAA != "Canarias", ]

# Aggregate by comarca
df_obs_sim_agg <- df_obs_sim %>%  group_by(CO_COMARCA) %>% 
  summarise(diff =sum(diff))
df_obs_sim_agg <- comarcas %>%  left_join(df_obs_sim_agg)

# Spain perimeter
ESP_per <- esp_get_ccaa_siane()
ESP_per <- ESP_per[!(ESP_per$ine.ccaa.name %in% c("Balears, Illes" ,"Ceuta" ,"Melilla","Canarias"   )),]
ESP_per <- st_make_valid(ESP_per)
ESP_per <- st_union(ESP_per)

hist(df_obs_sim_agg$diff)
# Plot the map
map_ls <- ggplot(df_obs_sim_agg) +
  geom_sf(aes(fill = diff)) +
  geom_sf(data = ESP_per, color = "black", fill = NA) +
  # scale_fill_viridis_c(name = "BS",
  #                      breaks = c(min(out_df_com$diff,na.rm = TRUE),
  #                                 max(out_df_com$diff,na.rm = TRUE)),
  #                      labels = c("0", "450")) +
  scale_fill_gradient(low = "#2A6EBBFF",
                      high = "#F0AB00FF",
                      name = "LS \n",
                      limits = c(0,10),
                      breaks = c(0,5,10)) +
  theme_void() +
  theme( legend.position = c(0.8,0.25),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))
map_ls

# Save plots
ggsave(paste0(path_plots,"map_LS_full_model.pdf"),
       map_ls, width =8, height = 8)

ggsave(paste0(path_plots,"map_LS_full_model.png"),
       map_ls, width = 8, height = 8, dpi = 300)

# Plot PA map with Probabilities model ------------------------------
# out_df <- out_df[!is.na(out_df$DS_COMARCA),]
out_df <- comarcas %>% left_join(df_obs_sim)
out_df_centroid <- out_df
out_df_centroid <- st_make_valid(out_df_centroid)
out_df_centroid <- st_centroid(out_df_centroid)
out_df_centroid <- out_df_centroid[out_df_centroid$obs==1,]
year_ref <- 2023 #2out_df_centroidyear_ref <- 2022 #2012  #2023  # 2023
plot_2023 <- ggplot(out_df[out_df$Year == year_ref &  !is.na(out_df$Year ),]) + 
  geom_sf(aes(fill = sim), alpha=1) +
  geom_sf(data = ESP_per, color = "black", fill = NA, size = 0.4) +
  geom_sf(data = out_df_centroid, alpha= 0.8, size = 0.5) +
  scale_fill_distiller(palette = "Spectral", name = "Probability \n",
                       breaks = c(0,0.5,1),
                       limits = c(0,1.001)) +
  theme_void() +
  theme(legend.position = "top",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))
plot_2023

# # Save plots for GIFT
# for(i in c(2005:2023)){
#   print(paste0("i:",i))
#   year_ref <- 2023 #2out_df_centroidyear_ref <- 2022 #2012  #2023  # 2023
#   plot_aux <- ggplot(out_df[out_df$Year == i &  !is.na(out_df$Year ),]) + 
#     geom_sf(aes(fill = sim), alpha= 0.8) +
#     geom_sf(data = ESP_per, color = "black", fill = NA) +
#     # geom_sf(data = out_df_centroid, alpha= 0.8, size = 0.5) +
#     scale_fill_distiller(palette = "Spectral", name = "Probability \n",
#                          breaks = c(0,0.5,1),
#                          limits = c(0,1.001)) +
#     theme_void() +
#     ggtitle(paste0("Year:", i)) +
#     theme(legend.position = "top",
#           legend.title = element_text(size = 12),
#           legend.text = element_text(size = 12))
#   plot_aux
#   
#   # Save plots
#   ggsave(paste0(path_plots,"map_",i,".png"),
#          plot_aux, width = 8, height = 8, dpi = 250)
# }

# Plot the sigmoidal function -------------------------------------
param <- c(0.0005706731568571639, 97.78894801162276, 5424.950376421077,
           6.977623970027249e-5, 51314.77501452205, -6.874687389443809, -80.94731156667282)
m_c = 0.0051

# Sigmoid function
sigmoid <- function(x){param[1]/(1+exp(-param[2]*m_c*x+ param[3]))}
vec <- seq(1000,100000,1)
out <- sapply(vec, sigmoid)
df_out_sig <- data.frame(vec, out)

round(param[3]/(param[2]*m_c),2)
ggplot(df_out_sig) + geom_line(aes(vec,out)) 

# Plot
sig_plot <- ggplot(df_out_sig) + geom_line(aes(log10(vec),out)) +
  # geom_vline(xintercept = log10(param[4]/(param[3]*m_c)),
  #            linetype = "dashed", color = "#AE0000") +
  # annotate("text", label = paste0("Mob = ",
  #                                 round(param[4]/(param[3]*m_c),2),
  #                                 " people"),
  #          x = (log10(param[4]/(param[3]*m_c)) - 0.6), y = 0.1,
  #          color = "#AE0000", size = 5) +
  ylim(c(0,0.001)) +
  theme_bw() + ylab("Human mediated dispersal rate") + xlab("Number of people moving (log10)") +
  theme(legend.position = c(0.85,0.3),
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

# Exponential function -------------------------------------------------
b = param[5]
a = param[4]
texp <- function(x){a*exp(-(1/b)*x)}
vec <- seq(0,500000,10)
out_exp <- sapply(vec, texp)
out <- sapply(vec, sigmoid)
df_out_exp <- data.frame(vec, out_nat = out_exp, out_med = out)
df_out_exp <- df_out_exp %>%  pivot_longer(
  cols = starts_with("out_"),
  names_to = "disp",
  values_to = "rate"
)

dist_exp_plot <- ggplot(df_out_exp) + 
  geom_line(aes(log10(vec),rate, color = disp), size = 1) +
  scale_color_viridis_d(name ="", labels = c("Human-mediated dispersal",
                                             "Natural dispersal")) +
  theme_bw() + ylab("Dispersal rate") +
  xlab("Distance (m) or Human flows (log10)") +
  theme(legend.position = c(0.2,0.9),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key.height = unit(0.4,"cm"),
        legend.key.width = unit(0.4,"cm"),
        plot.title = element_text(size = 20), # Change title size 
        axis.title.x = element_text(size = 15), # Change x-axis title size 
        axis.title.y = element_text(size = 15), # Change title size 
        axis.text.x = element_text(size = 12), # Change x-axis title size 
        axis.text.y = element_text(size = 12))

dist_exp_plot

# Exponential function -------------------------------------------------
a = param[6]
b = param[7]
texp <- function(x){exp(a*x+b)}
vec <- seq(-12,0,0.1)
out <- sapply(vec, texp)
df_out_exp_ext <- data.frame(vec, out)

tmin_exp_plot <- ggplot(df_out_exp_ext) + geom_line(aes(vec,out)) +
  theme_bw() + ylab("Extinction rate") + xlab("Temperature (°C)") +
  theme(legend.position = c(0.85,0.3),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key.height = unit(0.4,"cm"),
        legend.key.width = unit(0.4,"cm"),
        plot.title = element_text(size = 20), # Change title size 
        axis.title.x = element_text(size = 15), # Change x-axis title size 
        axis.title.y = element_text(size = 15), # Change title size 
        axis.text.x = element_text(size = 12), # Change x-axis title size 
        axis.text.y = element_text(size = 12))

tmin_exp_plot

# Check metrics
max(df_out_exp[df_out_exp$rate>0.0001 & 
                 df_out_exp$disp == "out_nat","vec"])
min(df_out_exp[df_out_exp$rate>0.0001 & 
                 df_out_exp$disp == "out_med","vec"])
max(df_out_exp_ext[df_out_exp_ext$out>1,"vec"])

# Arrange both plots
ggarrange_tm <- ggarrange(dist_exp_plot ,
          tmin_exp_plot ,
          widths = c(1,0.6),
          labels = c("A","B"),
          ncol = 2)
ggarrange_tm

# Test different models --------------------------------------------------
plot_bp_map <- function(Path,year_n, pa_bool){
  # Matrices simulation and observations -----------------------------
  Path_obs <- paste0(path_out,"obs_2005-2023.csv")
  obs <- read.csv(Path_obs)
  sim_mat <- read.csv(Path)
  sim_mat <- sim_mat[,-1]
  Path_pa <- paste0(path_out,"pa_com.csv")
  pa_com <- read.csv(Path_pa)
  
  # Convert to a data frame
  colnames(sim_mat) <- 2005:2023 
  df_sim <- as.data.frame(sim_mat)
  df_sim$CO_COMARCA <- pa_com$CO_COMARCA # Add Comarca names as a column
  
  # Reshape from wide to long format
  library(tidyr)
  library(dplyr)
  df_sim <- df_sim %>%
    pivot_longer(cols = -CO_COMARCA, names_to = "Year", values_to = "sim") %>%
    mutate(Year = as.integer(Year))  # Ensure Year is numeric
  
  # Convert to a data frame
  obs <- read.csv(paste0(path_out,"obs_2005-2023.csv"))
  obs <- obs[,-1]
  colnames(obs) <- 2005:2023 
  df_obs <- as.data.frame(obs)
  df_obs$CO_COMARCA <- pa_com$CO_COMARCA # Add Comarca names as a column
  
  # Reshape from wide to long format
  library(tidyr)
  library(dplyr)
  df_obs <- df_obs %>%
    pivot_longer(cols = -CO_COMARCA, names_to = "Year", values_to = "obs") %>%
    mutate(Year = as.integer(Year))  # Ensure Year is numeric
  
  # Join both data sets
  df_obs_sim <- df_obs %>%  left_join(df_sim)
  df_obs_sim$diff <- (df_obs_sim$obs-df_obs_sim$sim)^2
  sum(df_obs_sim$diff)  
  
  # Plot box plot
  # bp <- ggplot(out_df,aes(as.factor(PA), sim, fill = as.factor(PA))) +
  bp <- ggplot(df_obs_sim,aes(as.factor(obs), sim, fill=as.factor(obs))) +
    geom_jitter(color = "black", alpha = 0.2, size = 0.4)+
    geom_boxplot(alpha = 0.7) +
    # scale_fill_brewer(name = "" , palette = Palette, direction =-1) +
    # scale_fill_viridis_d(name = "" , option ="B") +
    scale_fill_bmj(name = "" ) +
    ylab("Occupancy probability, p(t)") +
    xlab("Observations") +
    theme_bw() + 
    theme( plot.margin = margin(10, 40, 10, 40, "pt"),
           legend.position = "none",
           plot.title = element_text(size = 15), # Change title size 
           axis.title.x = element_text(size = 15), # Change x-axis title size 
           axis.title.y = element_text(size = 15), # Change y-axis title size 
           axis.text.x = element_text(size = 15), # Change x-axis text size 
           axis.text.y = element_text(size = 15))
  
  # Plot PA map with Probabilities model ------------------------------
  # out_df <- out_df[!is.na(out_df$DS_COMARCA),]
  out_df <- comarcas %>% left_join(df_obs_sim)
  out_df_centroid <- out_df
  out_df_centroid <- st_make_valid(out_df_centroid)
  out_df_centroid <- st_centroid(out_df_centroid)
  out_df_centroid <- out_df_centroid[out_df_centroid$obs==1,]
  year_ref <- year_n #2out_df_centroidyear_ref <- 2022 #2012  #2023  # 2023
  
  plot_2023 <- ggplot(out_df[out_df$Year == year_ref &  !is.na(out_df$Year ),]) + 
    geom_sf(aes(fill = sim), alpha= 1) +
    geom_sf(data = ESP_per, color = "black", fill = NA, size = 0.3) +
    # geom_sf(data = out_df_centroid, alpha= 0.8, size = 0.3) +
    scale_fill_distiller(palette = "Spectral", name = "Probability \n",
                         breaks = c(0,0.5,1)) +
    theme_void() +
    theme(legend.position = "top",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12))
  
  if(pa_bool == 1){
    plot_2023 <- plot_2023 +  geom_sf(data = out_df_centroid, alpha= 0.8, size = 0.3) 
  }
  
  return(list(bp, plot_2023))
  
}

# Load shapefile
comarcas <- read_sf(paste0(path_out,"ComarcasAgrarias.shp"))
comarcas <- comarcas[comarcas$DS_CCAA != "Ceuta" &
                       comarcas$DS_CCAA != "Melilla" &
                       comarcas$DS_CCAA != "Islas Baleares" &
                       comarcas$DS_CCAA != "Canarias", ]

# Spain perimeter
ESP_per <- esp_get_ccaa_siane()
ESP_per <- ESP_per[!(ESP_per$ine.ccaa.name %in% c("Balears, Illes" ,"Ceuta" ,"Melilla","Canarias"   )),]
ESP_per <- st_make_valid(ESP_per)
ESP_per <- st_union(ESP_per)

# H=0.2 different fits ----------------------------------------------------
# Try different models data from julia code: albo_mobility/code/Hanski/ESP/...jl
year_n = 2023
Path <- paste0(path_out,"output_mean_tminRM_H_0_2_IC_2004_2025-05-23.csv")
full_mod <- plot_bp_map(Path,year_n,1)
full_mod[[1]]
full_mod[[2]]
Path <- paste0(path_out,"output_estimation_hummob_meanrm__IC_2004_tmin_H_0_2_2025-05-23.csv")
hum_mob <- plot_bp_map(Path,year_n,1)
hum_mob[[1]]
hum_mob[[2]]
Path <- paste0(path_out,"output_estimation_pop_growth_meanRMtmin_H_0_2_2025-05-23.csv")
pop_growth <- plot_bp_map(Path,year_n,1)
pop_growth[[1]]
pop_growth[[2]]
Path <- paste0(path_out,"output_estimation_dist_meanRM_tmin_H_0_2_2025-05-23.csv")
dist_mod_df <- read.csv(Path)
dist_mod <- plot_bp_map(Path,2023,1)
dist_mod[[1]]
dist_mod[[2]]

library(ggpubr)
ggarrange(full_mod[[1]] + ggtitle("Full Model"),
          hum_mob[[1]] + ggtitle("Human-mediated Dispersal"),
          dist_mod[[1]]  + ggtitle("Natural Dispersal"),
          pop_growth[[1]] + ggtitle("Climate-driven Growth"))

# Save the plot
ggsave(paste0(path_out,"bp_all_models_meanRM_IC_2004.png"),
       dpi = 350, height =9 , width = 12)
ggsave(paste0(path_out,"bp_all_models_meanRM_IC_2004.pdf"),
       height = 9, width = 12)


library(ggpubr)
maps_panel <- ggarrange(full_mod[[2]] + ggtitle("Full Model"),
          hum_mob[[2]] + ggtitle("Human-mediated Dispersal"),
          dist_mod[[2]]  + ggtitle("Natural Dispersal"),
          pop_growth[[2]] + ggtitle("Climate-driven Growth"), common.legend = TRUE)
maps_panel

# Save the plot
ggsave(paste0(path_out,"val_maps_all_models_meanRM_IC_2004.png"),
       dpi = 350, height =9 , width = 9)
ggsave(paste0(path_out,"val_maps_all_models_meanRM_IC_2004.pdf"),
       height = 9, width = 9)

# # Knock-out scenarios ---------------------------------------------------
# Path <- "/home/marta/Documentos/PHD/2024/Colonization/output/knockout_full_model_dispersal_2025-05-06.csv"
# pop_growth <- plot_bp_map(Path,2023)
# pop_growth[[2]]
Path <- paste0(path_out,"output_mean_tminRM_H_0_2_nodist_IC_2004_2025-05-23.csv")
nodist_mod <- plot_bp_map(Path,2023,0)
nodist_mod[[2]]
Path <- paste0(path_out,"output_mean_tminRM_H_0_2_nohum_IC_2004_2025-05-23.csv")
nohum_mob <- plot_bp_map(Path,2023,0)
nohum_mob[[2]]

# Arrange plots
ggarrange(nodist_mod[[2]] +
            ggtitle("Human-mediated Dispersal"),
          nohum_mob[[2]] + ggtitle("Natural Dispersal"),
          legend = "bottom", common.legend = TRUE)

# Save the plot
ggsave(paste0(path_plots,"knock_out_esc_IC_2004.png"),
       dpi = 350, height =5 , width = 10)
ggsave(paste0(path_plots,"knock_out_esc_IC_2004.pdf"),
       height = 5, width = 10)


# Arrangement of plots for panel ------------------------------
ggarrange(ggarrange(bp, dist_exp_plot + theme(legend.position = c(0.3,0.8)) ,
                 tmin_exp_plot ,
                 labels = c("a", "b", "c"),
                 widths = c(0.8,1,0.8),
                 ncol = 3, nrow = 1),
          ggarrange(plot_2023 ,
                    nohum_mob[[2]] + theme(legend.position = "bottom") ,
                    nodist_mod[[2]],
                    labels = c("d", "e", "f"), common.legend = TRUE,
                    ncol = 3,nrow=1), nrow = 2, ncol = 1, heights = c(1,1.5))


ggsave(paste0(path_plots,"panel2_knock_out_IC2004.png"),
       dpi = 350, height = 8.5, width = 13.2)
ggsave(paste0(path_plots,"panel2_knock_out_IC2004.pdf"),
       height = 8.5, width = 13.2)


# Arrange results with other models ------------------
Path <- paste0(path_out,"output_estimation_hummob_meanrm__IC_2004_tmin_H_0_2_2025-05-20.csv")
hum_mob_0 <- plot_bp_map(Path,year_n,0)
hum_mob_0[[2]]
Path <- paste0(path_plots,"output_estimation_dist_meanRM_tmin_H_0_2_IC_2004_2025-05-20.csv")
dist_mod_0 <- plot_bp_map(Path,2023,0)
dist_mod_0[[2]]

# Arrange and save
ggarrange(ggarrange(bp, dist_exp_plot + theme(legend.position = c(0.3,0.8)) ,
                    tmin_exp_plot + xlim(c(-15,0)),
                    labels = c("a", "b", "c"),
                    widths = c(0.8,1,0.8),
                    ncol = 3, nrow = 1),
          ggarrange(plot_2023 ,
                    hum_mob_0[[2]] + theme(legend.position = "bottom") ,
                    dist_mod_0[[2]],
                    labels = c("d", "e", "f"), common.legend = TRUE,
                    ncol = 3,nrow=1), nrow = 2, ncol = 1, heights = c(1,1.5))


ggsave(paste0(path_plots,"panel2_other_models_IC2004.png"),
       dpi = 350, height = 8, width = 13.2)
ggsave(paste0(path_plots,"panel2_other_models_IC2004.pdf"),
       height = 8, width = 13.2)

# Plots supplementary ---------------------------------------------------
# Plot the sigmoidal function -------------------------------------
param <- c(0.0005706731568571639, 97.78894801162276, 5424.950376421077,
           6.977623970027249e-5, 51314.77501452205, -6.874687389443809, -80.94731156667282)
param_nat <- c(0.0005639646833044143, 32553.774005521933, -0.18821772990171218, -8.597835989480085)
param_hum <- c(0.00019500768571673895, 0.1707553296090941, 6.355391841965978, -0.7581545344650137, -44.14554324330141)
m_c = 0.0051

# Sigmoid function
sigmoid <- function(x){param[1]/(1+exp(-param[2]*m_c*x+ param[3]))}
sigmoid_hum <- function(x){param_hum[1]/(1+exp(-param_hum[2]*m_c*x+ param_hum[3]))}
vec <- seq(10,100000,10)

# Exponential function -------------------------------------------------
texp <- function(x){param[4]*exp(-(1/param[5])*x)}
texp_disp <- function(x){param_nat[1]*exp(-(1/param_nat[2])*x)}

# Compute output
out_exp <- sapply(vec, texp)
out <- sapply(vec, sigmoid)
out_exp_nat <- sapply(vec, texp_disp)
out_hum <- sapply(vec, sigmoid_hum)

# Save in a data frame
df_out_exp <- data.frame(vec, out_nat = out_exp, out_med = out,
                         out_nat2 = out_exp_nat, out_med2 = out_hum)

# Transform to long format
df_out_exp <- df_out_exp %>%  pivot_longer(
  cols = starts_with("out_"),
  names_to = "disp",
  values_to = "rate"
)

dist_exp_plot <- ggplot(df_out_exp) + 
  geom_line(aes(log10(vec),rate, color = disp), size = 1) +
  scale_color_viridis_d(name ="", labels = c("a human-mediated dispersal",
                                             "b human-mediated dispersal",
                                             "a natural dispersal",
                                             "c natural dispersal")) +
  theme_bw() + ylab("Dispersal rate") +
  xlab("Distance or Human flows (log10)") +
  # geom_vline(xintercept = ((-log(0.01))*a),
  #            linetype = "dashed", color = "#AE0000") +
  # annotate("text", label = paste0("Dist:",round(((-log(0.01))*a)/1000,2),"km"),
  #          x = 300000, y = 0.1,
  #          color = "#AE0000", size = 5) +
  theme(legend.position = c(0.35,0.7),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.key.height = unit(0.4,"cm"),
        legend.key.width = unit(0.4,"cm"),
        plot.title = element_text(size = 20), # Change title size 
        axis.title.x = element_text(size = 15), # Change x-axis title size 
        axis.title.y = element_text(size = 15), # Change title size 
        axis.text.x = element_text(size = 12), # Change x-axis title size 
        axis.text.y = element_text(size = 12))

dist_exp_plot

# Exponential function -------------------------------------------------
texp <- function(x){exp(param[6]*x+ param[7])}
texp_nat <- function(x){exp(param_nat[3]*x+ param[4])}
texp_hum <- function(x){exp(param_hum[4]*x+ param_hum[5])}
vec_nat <- seq(-12.5,0,0.1)
out_norm <- sapply(vec_nat, texp)

# compute and transform to data frame
out_nat <- sapply(vec_nat, texp_nat)
out_hum <- sapply(vec_nat, texp_hum)
df_out_exp_ext <- data.frame(vec_nat, out_norm, out_nat, out_hum)

# Transform to long format
df_out_exp_ext <- df_out_exp_ext %>%  pivot_longer(
  cols = starts_with("out_"),
  names_to = "disp",
  values_to = "rate"
)

tmin_exp_plot <- ggplot(df_out_exp_ext) +
  geom_line(aes(vec_nat,rate, color = disp), size = 1) +
  scale_color_viridis_d(name ="", labels = c("b extinction",
                                             "c exponential",
                                             "a sigmoid")) +
  theme_bw() + ylab("Extinction rate") + xlab("Temperature") +
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

tmin_exp_plot

## Arrange plots
library(ggpubr)
maps_panel <- ggarrange(ggarrange(dist_exp_plot,
                        tmin_exp_plot, ncol = 2, nrow = 1),
              ggarrange(full_mod[[2]] + ggtitle("a Full Model"),
                        hum_mob[[2]] + ggtitle("b Human-mediated Dispersal"),
                        dist_mod[[2]]  + ggtitle("c Natural Dispersal"),
                        pop_growth[[2]] + ggtitle("d Climate-driven Growth"),
                        nrow = 2, ncol = 2, common.legend = TRUE),
              ncol = 1, nrow=2, heights = c(0.4,1))
maps_panel

# Save panel
ggsave(paste0(path_plots,"panelsup_val_models.png"),
       dpi = 350, height = 15, width = 12)
ggsave(paste0(path_plots,"panelsup_val_models.pdf"),
       height = 15, width = 12)

