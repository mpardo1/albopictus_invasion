# Code to reproduce figure 4 main and figure S9 Supplementary material
# Remove everything before starting
rm(list=ls())
library(data.table)
library(ggplot2)
library(dplyr)
library(ggplot2)
library(sf)
library(ggbreak)
library(patchwork)

# Path depending on location
path_out <- "data/output/" # Path to processed files
path_plots <- "plots/" # Select a path to save the plots

# Estimated parameters from the Parameter estimation from the Full model 12_Param_estimation_full_model.jl
param <- c(0.0005706731568571638, 97.78894801162286, 5424.950376421082,
           6.977623970027257e-5, 51314.77501452204, -6.874687389443816, -80.947311566672)
m_c = 0.0051

# c2 vs e2 -------------------------------------------------------------------
# Read output Present
path_pc <- paste0(path_out,"output_phase_space/")
list_f <- list.files(path_pc, pattern = "ext_2")
grid1_pres <- readRDS(paste0(path_pc, list_f[[1]]))
grid2_pres <- readRDS(paste0(path_pc, list_f[[2]]))
list_f <- list.files(path_pc, pattern = "ext_v")
list_f <- list_f[list_f %like% "c1"]
grid12_pres <- readRDS(paste0(path_pc, list_f[[1]]))
grid1_pres <- rbind(grid1_pres,grid12_pres )
list_f <- list.files(path_pc, pattern = "ext_v")
list_f <- list_f[list_f %like% "cd"]
grid22_pres <- readRDS(paste0(path_pc, list_f[[1]]))
grid2_pres <- rbind(grid2_pres,grid22_pres )

# Read output future
list_f <- list.files(path_pc, pattern = "fut")
grid1_fut <- readRDS(paste0(path_pc, list_f[list_f%like% "c1"]))
grid2_fut <- readRDS(paste0(path_pc, list_f[list_f%like% "cd"]))

# Check numbers for MS
(1-min(grid1_pres[grid1_pres$cte1 == 0 & grid1_pres$lambda_M >0, ]$cte2))*100
(1-min(grid1_fut[grid1_fut$cte1 == 0 & grid1_fut$lambda_M >0, ]$cte2))*100
  
(1-min(grid2_pres[grid2_pres$cte1 == 0 & grid2_pres$lambda_M >0, ]$cte2))*100
(1-min(grid2_fut[grid2_fut$cte1 == 0 & grid2_fut$lambda_M >0, ]$cte2))*100
# Select colors
blue = "#3F4C6B"#"#584B9F"#"#78A269"# "#585858"#"#4461A8"#"#26828E" #"#529985"
low_blue =  "#E6D159"#"#00B4B5" #"#EABD4C" #"#C7C7C7"#"#E4F4FF"# "#D4CC49"
low_yellow = "#FFF8E3"
red = low_blue #"#FDE725" #"#C26B51"

# Color points change to red for supplementary figure
col_points <- "white"
size_points = 5
# Plot output
metapop1 <- ggplot(grid1_pres) +
  # geom_point(aes(cte1,cte2,color=lambda_M)) +
  geom_point(aes(cte1,cte2,color=as.factor(sign(lambda_M)))) +
  # scale_color_gradientn(
  #         colors = c(blue, "white",red),
  #         values = scales::rescale(c(min_1, 0, max_1))) +
  scale_color_manual(values= c(blue, red), name = "", 
                     labels = c("Extinction", "Persistance")) +
  theme_classic() + scale_y_break(c(0.2,0.9), scales = 0.15) +
  geom_point(aes(1,1), color = col_points) +
  annotate("text", x = 1,y =1, label = "a",
           hjust = 1.5 , vjust = 1, color = col_points, size = size_points) +
  geom_point(aes(1,0.05), color = col_points) +
  annotate("text", x = 1,y =0.05, label = "b",
           hjust = 1.5 , vjust = 1, color = col_points, size = size_points) +
  geom_point(aes(0.01,1), color = col_points) +
  annotate("text", x = 0.01,y =1, label = "c",
           hjust = -1.5 , vjust = 1, color = col_points, size = size_points) +
  geom_point(aes(0.01,0.05), color = col_points) +
  annotate("text", x = 0.01,y =0.05, label = "d",
           hjust = -1.5 , vjust = 1, color = col_points, size = size_points) +
  xlab("Human-mediated dispersal") +
  scale_y_continuous(breaks = c(0,0.05,0.10,0.15,0.2,0.9,1)) +
  ylab("Extinction rate") +
  theme(legend.position = "top" ,
        color = guide_legend(override.aes = list(size=6)),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20), # Change title size
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.y.right = element_blank(),
        axis.title.x = element_text(size = 15), # Change x-axis title size
        axis.title.y = element_text(size = 15), # Change title size
        axis.text.x = element_text(size = 12), # Change x-axis title size
        axis.text.y = element_text(size = 12),
        plot.margin= unit(c(0,0,0,0), "cm"))

metapop1

# Extract nice legend
library(ggpubr)
leg <- get_legend(ggplot(grid1_pres) +
                    geom_point(aes(cte1,cte2,color=as.factor(sign(lambda_M))), size = 3) +
                    scale_color_manual(values= c(blue, red), name = "", 
                                       labels = c("Extinction", "Persistance")) +
                    theme_classic() + 
                    theme(legend.position = "top" ,
                          color = guide_legend(override.aes = list(size=6)),
                          legend.title = element_text(size = 12),
                          legend.text = element_text(size = 12)))

# e2vsc2 future -----------------------------------------------------

# Plot phase space
# Plot output
col_points <- "red"
metapop1_fut <- ggplot(grid1_fut) +
  # geom_point(aes(cte1,cte2,color=lambda_M)) +
  geom_point(aes(cte1,cte2,color=as.factor(sign(lambda_M)))) +
  # scale_color_gradientn(
  #         colors = c(blue, "white",red),
  #         values = scales::rescale(c(min_1, 0, max_1))) +
  scale_color_manual(values= c(blue, red)) +
  theme_classic() + scale_y_break(c(0.2,0.9), scales = 0.15) +
  geom_point(aes(1,1), color = col_points) +
  annotate("text", x = 1,y =1, label = "a",
           hjust = 1.5 , vjust = 1, color = col_points, size = size_points) +
  geom_point(aes(1,0.05), color = col_points) +
  annotate("text", x = 1,y =0.05, label = "b",
           hjust = 1.5 , vjust = 1, color = col_points, 
           size = size_points) +
  geom_point(aes(0.01,1), color = col_points) +
  annotate("text", x = 0.01,y =1, label = "c",
           hjust = -1.5 , vjust = 1, color = col_points, size = size_points) +
  geom_point(aes(0.01,0.05), color = col_points) +
  annotate("text", x = 0.01,y =0.05, label = "d",
           hjust = -1.5 , vjust = 1, color = col_points, size = size_points) +
  xlab("Human-mediated dispersal") +
  scale_y_continuous(breaks = c(0,0.05,0.10,0.15,0.2,0.9,1)) +
  ylab("Extinction rate") +
  theme(legend.position = "none" ,
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12),
        plot.title = element_text(size = 20), # Change title size
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.line.y.right = element_blank(),
        axis.title.x = element_text(size = 15), # Change x-axis title size
        axis.title.y = element_text(size = 15), # Change title size
        axis.text.x = element_text(size = 12), # Change x-axis title size
        axis.text.y = element_text(size = 12),
        plot.margin= unit(c(0,0,0,0), "cm"))

metapop1_fut

# Arrange two panels
metapop1 + metapop1_fut


library(patchwork)
lambda_m_pan <- (((metapop1 + ylab("Extinction rate factor") +
                     xlab("Human-mediated factor")+
                     theme(legend.position = "none",
                                   plot.title = element_text(face = "bold",size = 14,hjust = 0.5)) +
                     ggtitle("Past climate scenario")) + 
                    (metapop1_fut + ylab("Extinction rate factor") +
                      xlab("Human-mediated factor")+
                       theme(legend.position = "none",
                             plot.title = element_text(face = "bold",size = 14,hjust = 0.5))+                       
                       ggtitle("Future climate scenario")))) 

lambda_m <- as_ggplot(leg) / lambda_m_pan + plot_layout(heights = c(0.5,7))
lambda_m

# Save Panels
ggsave(paste0(path_plots, "phasespace_pres_fut.png"),
       dpi = 300,
       height = 8,
       width = 9)
ggsave(paste0(path_plots, "phasespace_pres_fut.pdf"),
       height = 8,
       width = 9)


# Future scenarios ----------------------------------------------------
# Load shapefile comarcas
comarcas <- read_sf(paste0(path_out, "ComarcasAgrarias.shp"))
comarcas <- comarcas[comarcas$DS_CCAA != "Ceuta" &
                       comarcas$DS_CCAA != "Melilla" &
                       comarcas$DS_CCAA != "Islas Baleares" &
                       comarcas$DS_CCAA != "Canarias", ]

# Load observations
Path <- paste0(path_out, "pa_com.csv")
pa_com <- read.csv(Path)


# Spain perimeter
library(mapSpain)
ESP_per <- esp_get_ccaa_siane()
ESP_per <- ESP_per[!(ESP_per$ine.ccaa.name %in% c("Balears, Illes" ,"Ceuta" ,"Melilla","Canarias"   )),]
ESP_per <- st_make_valid(ESP_per)
ESP_per <- st_union(ESP_per)

# Process to long format
proc_mat <- function(df_aux){
  # Convert to a data frame
  colnames(df_aux) <- 2025:2049
  df_aux <- as.data.frame(df_aux)
  df_aux$CO_COMARCA <- pa_com$CO_COMARCA

  # Transform data to long format
  library(tidyr)
  df_aux <- df_aux %>%
    pivot_longer(cols = -CO_COMARCA, names_to = "Year", values_to = "sim") %>%
    mutate(Year = as.integer(Year))
  return(df_aux)
}

# Function to plot future scenarios
plot_sce <- function(df_aux, year_n){
  library(paletteer)
  pal <- rev(paletteer_c("grDevices::Spectral", 14))
  pal <- paletteer_c("grDevices::Geyser", 14)
  # pal <- rev(paletteer_c("grDevices::RdYlBu", 30))
  pal <- paletteer_c("viridis::cividis", 14)
  #pal <- paletteer_c("ggthemes::Temperature Diverging", 13)
  pal <- colorRampPalette(pal)
  #colors <- paletteer::paletteer_c("ggthemes::Temperature Diverging", n=30)
  df_aux <- proc_mat(df_aux)
  # Join with shapefile
  plot_df <- comarcas %>% left_join(df_aux[df_aux$Year == year_n,])
  ggplot(plot_df) +
    geom_sf(aes(fill = sim), alpha= 0.8, color = NA) +
    geom_sf(data = ESP_per, color = "black", fill = NA, size = 0.4) +
    scale_fill_gradientn(colors = pal(10),
                         name = "Probability \n",
                         breaks = c(0,0.5,1),
                         limits = c(-0.01,1.01)) +
    theme_void() +
    theme(legend.position = "bottom",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12))
}

# Future scenarios
Path <- paste0(path_out, "com_opt_simulation_dits_sig_mob_tmin_RM_fut_2025-05-27.csv")
normal_sce <- read.csv(Path)
Path <-   paste0(path_out, "low_sig_factor_com_opt_simulation_dits_sig_mob_tmin_RM_fut_2025-05-27.csv")
low_sig_sce <- read.csv(Path)
Path <- paste0(path_out, "low_sig_high_e_factor_com_opt_simulation_dits_sig_mob_tmin_RM_fut_2025-05-27.csv")
low_sig_high_e_sce <- read.csv(Path)
Path <- paste0(path_out, "high_e_factor_com_opt_simulation_dits_sig_mob_tmin_RM_fut_2025-05-27.csv")
high_e_sce <- read.csv(Path)

# Arrange plots
library(ggpubr)
year_n <- 2049

maps_arr <- ggarrange(plot_sce(normal_sce,year_n) +
                        ggtitle("a") + theme(plot.title = element_text(color = "red",
                                                                       face = "bold", size = 14)),
                      plot_sce(high_e_sce,year_n) +
                        ggtitle("b") + theme(plot.title = element_text(color = "red",
                                                                       face = "bold", size = 14)),
          plot_sce(low_sig_sce,year_n)  +
            ggtitle("c") + theme(plot.title = element_text(color = "red",
                                                           face = "bold", size = 14)),
          plot_sce(low_sig_high_e_sce,year_n)  +
            ggtitle("d") + theme(plot.title = element_text(color = "red",
                                                           face = "bold", size = 14)),
          common.legend=TRUE, legend = "bottom")

maps_arr <- annotate_figure(maps_arr,
                            top = text_grob("2050 projections", 
                                            color = "black", 
                                            face = "bold", 
                                            size = 14
                                            ))

# Panel 3
ggarrange(NULL,
          ggarrange(lambda_m,NULL,nrow=2, heights = c(1,0.1)),
          maps_arr ,
          ncol = 3, 
          widths = c(0.1,1.2,1))

ggsave(paste0(path_plots, "panel3_cividis.png"),
       dpi = 350,
       height = 6,
       width = 14)
ggsave(paste0(path_plots, "panel3_cividis.pdf"),
       height = 6,
       width = 14)

# Other arrange
ggarrange(lambda_m,
          maps_arr ,
          nrow = 2,
          heights = c(1,2))

ggsave(paste0(path_plots, "panel3_cividis_ver.png"),
       dpi = 350,
       height = 11,
       width = 8)
ggsave(paste0(path_plots, "panel3_cividis_ver.pdf"),
       height = 11,
       width = 8)

# Panel supplementary present future climate ----------------------------
# Load data
mean_RM_fut <- readRDS(paste0(path_out, "mean_RM_fut_ESP.Rds"))
mean_fut <- data.frame(CO_COMARCA = sub("X","", names(mean_RM_fut)) ,
                          mean_RM_fut =as.numeric(mean_RM_fut))
min_tmin_fut <- readRDS(paste0(path_out, "min_temp_yearly_mean_fut_ESP.Rds"))
mean_fut$tmin_fut <- min_tmin_fut
mean_RM <- readRDS(paste0(path_out, "mean_RM_ESP.Rds"))
mean_pres <- data.frame(CO_COMARCA = sub("R0_alb.","", names(mean_RM)) ,
                      mean_RM =as.numeric(mean_RM))
min_tmin <- readRDS(paste0(path_out, "min_tmin_ESP.Rds"))
mean_pres$tmin <- min_tmin
min(min_tmin)
min(min_tmin_fut)
# Load shapefile comarcas
comarcas <- read_sf(paste0(path_out, "ComarcasAgrarias.shp"))
comarcas <- comarcas[comarcas$DS_CCAA != "Ceuta" &
                       comarcas$DS_CCAA != "Melilla" &
                       comarcas$DS_CCAA != "Islas Baleares" &
                       comarcas$DS_CCAA != "Canarias", ]

# Spain perimeter
library(mapSpain)
ESP_per <- esp_get_ccaa_siane()
ESP_per <- ESP_per[!(ESP_per$ine.ccaa.name %in% c("Balears, Illes" ,"Ceuta" ,"Melilla","Canarias"   )),]
ESP_per <- st_make_valid(ESP_per)
ESP_per <- st_union(ESP_per)

# Join all datasets
comarcas$CO_COMARCA <- as.character(comarcas$CO_COMARCA)
pres_fut <- comarcas %>% left_join(mean_pres) %>% left_join(mean_fut)

# Save boundaries rm and tmin
min_rm <- min(pres_fut$mean_RM,pres_fut$mean_RM_fut , na.rm = TRUE)
max_rm <- max(pres_fut$mean_RM,pres_fut$mean_RM_fut , na.rm = TRUE)
min_t <- min(pres_fut$tmin,pres_fut$tmin_fut , na.rm = TRUE)
max_t <- max(pres_fut$tmin,pres_fut$tmin_fut , na.rm = TRUE)

# Plots  population growth 
col_bor <- "#383838"
library("latex2exp")
rm_pres <- ggplot(pres_fut) +
  geom_sf(aes(fill = mean_RM), color = col_bor,linewidth = 0.3)  +
  geom_sf(data = ESP_per, color = "black", fill = NA, size = 0.8) +
  scale_fill_distiller(palette = "Spectral",
                       name = TeX("Average $R_M$"),
                       na.value = "#FCFCFC",
                       limits = c(min_rm, max_rm)) +
  theme_void() +
  theme(legend.position = c(0.9,0.2),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) 

rm_fut <- ggplot(pres_fut) +
  geom_sf(aes(fill = mean_RM_fut), color = col_bor,linewidth = 0.3)  +
  geom_sf(data = ESP_per, color = "black", fill = NA, size = 0.8) +
  scale_fill_distiller(palette = "Spectral",
                       name = TeX("Average $R_M$"),
                       na.value = "#FCFCFC",
                       limits = c(min_rm, max_rm)) +
  theme_void() +
  theme(legend.position = c(0.9,0.2),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) 

# Difference between fut and pres
pres_fut$rm_diff <- pres_fut$mean_RM_fut - pres_fut$mean_RM

rm_diff <- ggplot(pres_fut) +
  geom_sf(aes(fill = rm_diff), color = col_bor,linewidth = 0.3)  +
  geom_sf(data = ESP_per, color = "black", fill = NA, size = 0.8) +
  scale_fill_distiller(palette = "Spectral",
                       name = "Diff\n",
                       na.value = "#FCFCFC") +
  theme_void() +
  theme(legend.position = "top",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) 

# Arrrange pres and future
rm_pres_fut <- ggarrange(rm_pres, rm_fut, common.legend = TRUE)
rm_pres_fut_diff <- ggarrange(rm_pres_fut, rm_diff, nrow = 1, widths = c(2,1))

# Plot tmin pres and future
tmin_pres <- ggplot(pres_fut) +
  geom_sf(aes(fill = tmin), color = col_bor,linewidth = 0.3)  +
  geom_sf(data = ESP_per, color = "black", fill = NA, size = 0.8) +
  scale_fill_distiller(palette = "Spectral",
                       name = "Average minimum \ntemperature (Cº)",
                       na.value = "#FCFCFC",
                       limits = c(min_t, max_t)) +
  theme_void() +
  theme(legend.position = c(0.9,0.2),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) 

tmin_fut <- ggplot(pres_fut) +
  geom_sf(aes(fill = tmin_fut), color = col_bor,linewidth = 0.3)  +
  geom_sf(data = ESP_per, color = "black", fill = NA, size = 0.8) +
  scale_fill_distiller(palette = "Spectral",
                       name = "Average minimum \ntemperature (Cº)",
                       na.value = "#FCFCFC",
                       limits = c(min_t, max_t)) +
  theme_void() +
  theme(legend.position = c(0.9,0.2),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) 

# Difference between fut and pres
pres_fut$tmin_diff <- pres_fut$tmin_fut - pres_fut$tmin

tmin_diff <- ggplot(pres_fut) +
  geom_sf(aes(fill = tmin_diff), color = col_bor,linewidth = 0.3)  +
  geom_sf(data = ESP_per, color = "black", fill = NA, size = 0.8) +
  scale_fill_distiller(palette = "Spectral",
                       name = "Diff (Cº)",
                       na.value = "#FCFCFC") +
  theme_void() +
  theme(legend.position = "top",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12)) 

# Arrrange pres and future
tmin_pres_fut <- ggarrange(tmin_pres, tmin_fut, common.legend = TRUE)
tmin_pres_fut_diff <- ggarrange(tmin_pres_fut, tmin_diff, nrow = 1, widths = c(2,1))

# Arrange tmin and rm
ggarrange(rm_pres_fut_diff, tmin_pres_fut_diff, nrow = 2)
ggsave(paste0(path_plots, "panel_tmin_rm_pres_fut.png"),
       dpi = 350,
       height = 8,
       width = 12)
ggsave(paste0(path_plots, "panel_tmin_rm_pres_fut.pdf"),
       height = 8,
       width = 12)

# Supplementary material future scenarios past climate------------------
# Load shapefile comarcas
comarcas <- read_sf(paste0(path_out, "ComarcasAgrarias.shp"))
comarcas <- comarcas[comarcas$DS_CCAA != "Ceuta" &
                       comarcas$DS_CCAA != "Melilla" &
                       comarcas$DS_CCAA != "Islas Baleares" &
                       comarcas$DS_CCAA != "Canarias", ]

# Load observations
Path <- paste0(path_out, "pa_com.csv")
pa_com <- read.csv(Path)


# Spain perimeter
library(mapSpain)
ESP_per <- esp_get_ccaa_siane()
ESP_per <- ESP_per[!(ESP_per$ine.ccaa.name %in% c("Balears, Illes" ,"Ceuta" ,"Melilla","Canarias"   )),]
ESP_per <- st_make_valid(ESP_per)
ESP_per <- st_union(ESP_per)

# Process to long format
proc_mat <- function(df_aux){
  # Convert to a data frame
  colnames(df_aux) <- 2025:2049
  df_aux <- as.data.frame(df_aux)
  df_aux$CO_COMARCA <- pa_com$CO_COMARCA
  
  # Transform data to long format
  library(tidyr)
  df_aux <- df_aux %>%
    pivot_longer(cols = -CO_COMARCA, names_to = "Year", values_to = "sim") %>%
    mutate(Year = as.integer(Year))
  return(df_aux)
}

# Function to plot future scenarios
plot_sce <- function(df_aux, year_n){
  library(paletteer)
  pal <- rev(paletteer_c("grDevices::Spectral", 14))
  pal <- paletteer_c("grDevices::Geyser", 14)
  # pal <- rev(paletteer_c("grDevices::RdYlBu", 30))
  pal <- paletteer_c("viridis::cividis", 14)
  #pal <- paletteer_c("ggthemes::Temperature Diverging", 13)
  pal <- colorRampPalette(pal)
  #colors <- paletteer::paletteer_c("ggthemes::Temperature Diverging", n=30)
  df_aux <- proc_mat(df_aux)
  # Join with shapefile
  plot_df <- comarcas %>% left_join(df_aux[df_aux$Year == year_n,])
  ggplot(plot_df) +
    geom_sf(aes(fill = sim), alpha= 0.8, color = NA) +
    geom_sf(data = ESP_per, color = "black", fill = NA, size = 0.4) +
    scale_fill_gradientn(colors = pal(10),
                         name = "Probability \n",
                         breaks = c(0,0.5,1),
                         limits = c(-0.01,1.01)) +
    theme_void() +
    theme(legend.position = "top",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 12))
}

# Future scenarios
Path <- paste0(path_out, "com_opt_simulation_dits_sig_mob_tmin_RM_fut_past_clim_2025-05-28.csv")
normal_sce <- read.csv(Path)
Path <-  paste0(path_out, "low_sig_factor_com_opt_simulation_dits_sig_mob_tmin_RM_fut__past_clim_2025-05-28.csv")
low_sig_sce <- read.csv(Path)
Path <-  paste0(path_out, "high_e_factor_com_opt_simulation_dits_sig_mob_tmin_RM_fut__past_clim_2025-05-28.csv")
high_e_sce <- read.csv(Path)
Path <- paste0(path_out, "low_sig_high_e_factor_com_opt_simulation_dits_sig_mob_tmin_RM_fut__past_clim_2025-05-28.csv")
low_sig_high_e_sce <- read.csv(Path)

# Arrange plots
library(ggpubr)
year_n <- 2049

maps_arr <- ggarrange(plot_sce(normal_sce,year_n) +
                        ggtitle("a") + theme(plot.title = element_text(color = "red",
                                                                       face = "bold", size = 14)),
                      plot_sce(high_e_sce,year_n) +
                        ggtitle("b") + theme(plot.title = element_text(color = "red",
                                                                       face = "bold", size = 14)),
                      plot_sce(low_sig_sce,year_n)  +
                        ggtitle("c") + theme(plot.title = element_text(color = "red",
                                                                       face = "bold", size = 14)),
                      plot_sce(low_sig_high_e_sce,year_n)  +
                        ggtitle("d") + theme(plot.title = element_text(color = "red",
                                                                       face = "bold", size = 14)),
                      common.legend=TRUE, legend = "bottom")

maps_arr <- annotate_figure(maps_arr,
                            top = text_grob("2050 projections", 
                                            color = "black", 
                                            face = "bold", 
                                            size = 14
                            ))

# Panel 3
ggarrange(lambda_m,
          maps_arr ,
          widths = c(1.2,1))

ggsave(paste0(path_plots, "panel3Sup_cividis.png"),
       dpi = 350,
       height = 6,
       width = 14)
ggsave(paste0(path_plots, "panel3Sup_cividis.pdf"),
       height = 6,
       width = 14)

