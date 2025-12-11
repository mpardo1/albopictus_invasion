# Code to answer reviews
rm(list=ls())
library(data.table)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
library(ggpubr)
library(sf)
library(ggsci)
library(mapSpain)
library(pROC)
library(circlize)
library(dplyr)

# Path data
path <- "data/output_plots/"
list.files(path)

# Compute AUC, sensitivity and specificity ----------------------------------------
# Load observations
obs <- read.csv(paste0(path, "obs_2005-2023.csv"))
colnames(obs) <- 2005:2023 
df_obs <- as.data.frame(obs)
pa_com <- read.csv(paste0(path, "pa_com.csv"))
df_obs$CO_COMARCA <- pa_com$CO_COMARCA 
df_obs <- df_obs %>%
  pivot_longer(cols = -CO_COMARCA, names_to = "Year", values_to = "obs") %>%
  mutate(Year = as.integer(Year))  

# Aggregate to have static view
df_obs_det <- df_obs %>% group_by(CO_COMARCA) %>% 
  summarise(obs = max(obs, na.rm = TRUE))
saveRDS(df_obs_det, paste0(path, "detection_2023_albo.Rds"))

#  Function to plot ROC curve and AUC value
plot_roc <- function(sim_mat, tit){
  colnames(sim_mat) <- 2005:2023 
  df_sim <- as.data.frame(sim_mat)
  df_sim$CO_COMARCA <- pa_com$CO_COMARCA # Add Comarca names as a column
  df_sim <- df_sim %>%
    pivot_longer(cols = -CO_COMARCA, names_to = "Year", values_to = "sim") %>%
    mutate(Year = as.integer(Year)) 
  
  # Join both data sets
  df_obs_sim <- df_obs %>%  left_join(df_sim)
  
  # Compute the AUC
  roc_obj <- roc(df_obs_sim$obs, df_obs_sim$sim)
  print(roc_obj$auc)
  
  # Extract ROC curve data
  roc_df <- data.frame(
    fpr = 1 - roc_obj$specificities,
    tpr = roc_obj$sensitivities
  )
  
  # Compute AUC
  auc_value <- auc(roc_obj)
  
  # Plot with ggplot2
  gg_roc <- ggplot(roc_df, aes(x = fpr, y = tpr)) +
    geom_line(color = "#1c61b6", size = 1.2) +
    geom_abline(linetype = "dashed", color = "gray") +
    annotate("text", x = 0.7, y = 0.1, 
             label = paste("AUC =", round(auc_value, 3)),
             size = 5, color = "black") +
    labs(
      title = tit,
      x = "False Positive Rate ",
      y = "True Positive Rate "
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5)
    )
  
  return(gg_roc)
}

# Load different models
full_mod <- read.csv(paste0(path, "output_mean_tminRM_H_0_2_IC_2004_2025-05-20.csv"))
roc_full <- plot_roc(full_mod, "Full model")
hum_mob <- read.csv(paste0(path, "output_estimation_hummob_meanrm__IC_2004_tmin_H_0_2_2025-05-23.csv"))
roc_hum <- plot_roc(hum_mob, "Human-mediated dispersal")
pop_growth <- read.csv(paste0(path, "outputoutput_estimation_pop_growth_meanRMtmin_H_0_2_2025-05-23.csv"))
roc_pop <- plot_roc(pop_growth,  "Climate-driven growth")
dist_mob <- read.csv(paste0(path, "output_estimation_dist_meanRM_tmin_H_0_2_2025-05-23.csv"))
roc_dist <- plot_roc(dist_mob,"Natural dispersal")

# Arrange in a panel
ggarrange(roc_full, roc_hum, roc_dist, roc_pop)

# Specificity and Sensitivity
fun_sens_spec <- function(sim_mat){
  colnames(sim_mat) <- 2005:2023 
  df_sim <- as.data.frame(sim_mat)
  df_sim$CO_COMARCA <- pa_com$CO_COMARCA # Add Comarca names as a column
  df_sim <- df_sim %>%
    pivot_longer(cols = -CO_COMARCA, names_to = "Year", values_to = "sim") %>%
    mutate(Year = as.integer(Year)) 
  
  # Join both data sets
  df_obs_sim <- df_obs %>%  left_join(df_sim)
  
  # Set a threshold
  threshold <- 0.5

  # Predicted class
  df_obs_sim$y_hat <- ifelse(df_obs_sim$sim >= threshold, 1, 0)
  
  # Compute components
  TP <- sum(df_obs_sim$obs == 1 & df_obs_sim$y_hat == 1)
  TN <- sum(df_obs_sim$obs == 0 & df_obs_sim$y_hat == 0)
  FP <- sum(df_obs_sim$obs == 0 & df_obs_sim$y_hat == 1)
  FN <- sum(df_obs_sim$obs == 1 & df_obs_sim$y_hat == 0)
  
  # Sensitivity (True Positive Rate)
  sensitivity <- TP / (TP + FN)
  
  # Specificity (True Negative Rate)
  specificity <- TN / (TN + FP)
  
  print(paste0("sensitivity:",sensitivity))
  print(paste0("specificity:",specificity))
}

# Load different models
print( "Full model:\n")
fun_sens_spec(full_mod)
print( "Human-mediated dispersal:\n")
fun_sens_spec(hum_mob)
print( "Climate-driven growth:\n")
fun_sens_spec(pop_growth )
print( "Natural dispersal:\n")
fun_sens_spec(dist_mob)

# High risk comarcas and high risk links--------------------------------------------------
full_model <- read.csv(paste0(path, "full_model_2024.csv"))

# Join both data sets
df_obs_sim <- df_obs %>%  left_join(full_mod)

# Filter zeros and list highst probability
df_obs_sim[df_obs_sim$obs == 0,]

# Join with comarcas
df_obs_sim <- comarcas[,c("CO_COMARCA", "DS_COMARCA")] %>% 
  left_join(df_obs_sim[df_obs_sim$obs == 0 &
                         df_obs_sim$Year == 2023,])

# Sort by hightest prob
df_obs_sim <- df_obs_sim %>% arrange(desc(sim))

# Plot
map_risk <- ggplot(df_obs_sim) +
  geom_sf(aes(fill = sim)) +
  scale_fill_distiller(name = "Occupancy \nprobability",
                       palette = "Spectral") +
  theme_minimal()

# Save plot
ggsave("/home/marta/Documentos/PHD/2025/Colonization/Plots/Panels/RiskMap.png",
                 plot = map_risk,
                 width = 6, height = 6, dpi = 250)

# Save csv with highest prob
df_obs_sim$geometry <- NULL
write.csv(df_obs_sim[1:30,], "/home/marta/Documentos/PHD/2025/Colonization/high_risk_comarcas.csv")

# Select flows connecting those comarcas -----------------------------------------------
flows_df <- read.csv(paste0(path,"flows_com_df.csv"))

# Remove flow same destination and origin
flows_df <- flows_df[flows_df$CO_COMARCA_origin!=flows_df$CO_COMARCA_destination,]

# Read high risk 
df_obs_sim <- read.csv("/home/marta/Documentos/PHD/2025/Colonization/high_risk_comarcas.csv")

# Filter flows with destination high risk
flows_df <- flows_df[flows_df$CO_COMARCA_destination %in% df_obs_sim$CO_COMARCA,]
  
# Remove flows from the same origin and destination
flows_df$X <- NULL
flows_df <- flows_df[flows_df$CO_COMARCA_destination !=flows_df$CO_COMARCA_origin,]
flows_thres <- 9800  #round(param[3]/(param[2]*m_c),2)
flows_df_filt <- flows_df[flows_df$mean_n_trips>=flows_thres,] # Remove flows with low colonization rate

# high risk flows
flows_df_filt_s <- flows_df_filt %>% 
  left_join(df_obs_sim, by = join_by("CO_COMARCA_destination" == "CO_COMARCA"))
flows_df_filt_s$obs <- NULL
flows_df_filt_s$Year <- NULL
flows_df_filt_s$X <- NULL

# Add name comarca origin and rename
colnames(flows_df_filt_s)[4] <- "DS_COMARCA_destination"
flows_df_filt_s <- flows_df_filt_s %>%
  left_join(comarcas[,c(5,6)],
            by = join_by("CO_COMARCA_origin" == "CO_COMARCA"))
colnames(flows_df_filt_s)[6] <- "DS_COMARCA_origin"

# Select flows with origin Aedes albopictus detected
df_obs_det <- readRDS(paste0(path, "detection_2023_albo.Rds"))
flows_df_filt_s <- flows_df_filt_s[flows_df_filt_s$CO_COMARCA_origin %in% df_obs_det[df_obs_det$obs==1,]$CO_COMARCA,]

# Save csv
flows_df_filt_s$geometry <- NULL
write.csv(flows_df_filt_s %>% arrange(desc(sim)), "/home/marta/Documentos/PHD/2025/Colonization/high_risk_30_flows.csv")

# Aggregate by prov
comarcas$geometry <- NULL

# Add name comarcas
flows_df_filt <- flows_df_filt %>%
  left_join(comarcas[,c(5,6)], by = join_by("CO_COMARCA_origin" == "CO_COMARCA"))
colnames(flows_df_filt)[4] <- "DS_COMARCA_origin"
flows_df_filt <- flows_df_filt %>%
  left_join(comarcas[,c(5,6)], by = join_by("CO_COMARCA_destination" == "CO_COMARCA"))
colnames(flows_df_filt)[5] <- "DS_COMARCA_destination"

# Convert to matrix
nodes <- unique(c(flows_df_filt$DS_COMARCA_destination,
                  flows_df_filt$DS_COMARCA_origin))
mat <- matrix(0, nrow = length(nodes), ncol = length(nodes),
              dimnames = list(nodes, nodes))

# Create a unique id 
df_unique <- data.frame(id_origin = c(1:length(nodes)),
                        id_dest = c(1:length(nodes)),
                        DS_COMARCA_origin = nodes,
                        DS_COMARCA_destination = nodes)
flows_df_filt <- flows_df_filt %>% left_join(df_unique[,c(1,3)]) 
flows_df_filt <- flows_df_filt %>% left_join(df_unique[,c(2,4)]) 
for(i in 1:nrow(flows_df_filt)) {
  mat[flows_df_filt$id_dest[i],
      flows_df_filt$id_origin[i]] <- flows_df_filt$mean_n_trips[i]
}

diag(mat) <- 0

# raw chord diagram without default labels
cols <- hcl.colors(nrow(mat), "Spectral")
chordDiagram(mat,
             grid.col = cols,
             annotationTrack = "grid",
             transparency = 0.3, preAllocateTracks = 1)

# Add custom, perpendicular labels
circos.trackPlotRegion(
  track.index = 1,
  panel.fun = function(x, y) {
    par(mar = c(1,1,1,1))
    sector.name <- get.cell.meta.data("sector.index")
    xlim <- get.cell.meta.data("xlim")
    ylim <- get.cell.meta.data("ylim")
    circos.text(
      x = mean(xlim),
      y = ylim[1] + mm_y(5),      # move label outside the circle
      labels = sector.name,
      facing = "clockwise",       # make text perpendicular
      niceFacing = TRUE,          # keep upright orientation
      adj = c(0, 0.5),
      cex = 0.8
    )
  },
  bg.border = NA
)

# Save plot
ggsave("/home/marta/Documentos/PHD/2025/Colonization/Plots/Panels/chord_plot.png",
       plot = last_plot(),
       width = 6, height = 6, dpi = 250)

# Test different shapes logistic -----------------------------------------------------
vec <- seq(0,10000,100)
sigmoid <- function(x){param[1]/(1+exp(-param[2]*0.0051*x+ param[3]))}
param <- c(0.25,0.3,18)
out1 <- sapply(vec, sigmoid)
param <- c(0.46,0.1,1)
out2 <- sapply(vec, sigmoid)
param <- c(0.5,6.5,34)
out3 <- sapply(vec, sigmoid)
df_out <- data.frame(vec, out1,out2,out3)
df_out <- reshape2::melt(df_out, id.vars = "vec")
ggplot(df_out) + 
  geom_line(aes(vec, value, color = variable), size = 1) +
  scale_color_viridis_d(labels = c("(0.25,0.3,18)",
                                   "(0.46,0.1,1)",
                                   "(0.5,6.5,34)")) +
  xlab("Number of people") + ylab("Human-mediated colonization rate") +
  theme_bw(base_size = 12)


# Kernel natural dispersal ------------------------------------------------------------
# Load data comarcas shapefile
comarcas <- read_sf(paste0(path, "comarcas/ComarcasAgrarias.shp"))
comarcas <- comarcas[comarcas$DS_CCAA != "Ceuta" &
                       comarcas$DS_CCAA != "Melilla" &
                       comarcas$DS_CCAA != "Islas Baleares" &
                       comarcas$DS_CCAA != "Canarias", ]


# Load data
df_out_nat <- read.csv(paste0(path, "natural_dispersal_one_year_2025-05-28.csv"))
dist_mat <- read.csv(paste0("data/InputHanski/ESP/","dist_mat_com_ESP.csv"))[,-1]

# Extract first detection
comarca_init <- df_out_nat[df_out_nat$out == 1,]$CO_COMARCA
comarcas[comarcas$CO_COMARCA == comarca_init, ] # Test

# Extract distance first detection
which(df_out_nat$CO_COMARCA == comarca_init)
dist_df_init <- data.frame(CO_COMARCA = df_out_nat$CO_COMARCA, dist = as.numeric(dist_mat[56,]))

# Test
dist_df_plot <- comarcas %>%  left_join(dist_df_init)
df_out_nat_plot <- comarcas %>%  left_join(df_out_nat)
ggplot(dist_df_plot) + geom_sf(aes(fill = dist)) + theme_minimal()
disp_one <- ggplot(df_out_nat_plot) +
  geom_sf(aes(fill = out)) +
  scale_fill_distiller(palette = "Spectral", name = "Occupancy \nprobability") +
  theme_minimal()

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
days_disp <- 30*6
days_disp <- 365
max(dist_out_df_plot[dist_out_df_plot$out>=quan[3],]$dist)/(days_disp)

# Days distance
dist_out_df_plot$dist_daily <- dist_out_df_plot$dist/days_disp
prob <- ggplot(dist_out_df_plot[dist_out_df_plot$out<1,]) +
  geom_point(aes(dist_daily, out ), color = "darkblue") +
  geom_line(aes(dist_daily, out ), size = 0.6) +
  geom_vline(xintercept = 250, linetype = "dashed", color = "red") +
  xlab("Natural dispersal (meters/day)") +
  ylab("Occupancy probability") +
  theme(base_size = 14) +
  theme_bw()

mean(dist_out_df_plot[dist_out_df_plot$out>1.e-6,]$dist_daily)
quan(dist_out_df_plot[dist_out_df_plot$out>1.e-6,]$dist_daily)

# Arrange plots
ggarrange(disp_one, prob, widths = c(1, 0.8), labels= c("a", "b"))

# Compute mean occupancy proability occupied patches
sim_mat <- read.csv(paste0(path, "output_estimation_dist_meanRM_tmin_H_0_2_2025-05-23.csv"))
colnames(sim_mat) <- 2005:2023 
df_sim <- as.data.frame(sim_mat)
df_sim$CO_COMARCA <- pa_com$CO_COMARCA # Add Comarca names as a column
df_sim <- df_sim %>%
  pivot_longer(cols = -CO_COMARCA, names_to = "Year", values_to = "sim") %>%
  mutate(Year = as.integer(Year)) 

# Join both data sets
df_obs_sim <- df_obs %>%  left_join(df_sim)
mean(df_obs_sim[df_obs_sim$obs==1,]$sim)


# Validation 2024 and 2024 ----------------------------------------------------------------
obs <- read.csv2(paste0(path, "InvaMoSP_2004_2024_v3.csv"), stringsAsFactors = FALSE)
obs_2024 <- obs[obs$ALB_YEAR_CTS == 2024 & !is.na(obs$ALB_YEAR_CTS), ]
pa_mitma <- read.csv2(paste0(path, "comarca_mitma_NATCODE_ESP.csv"), sep = ",")

# Join detection 2024 with comarca 
pa_com <- read.csv(paste0(path, "pa_com.csv"))
CO_COMARCA_2024 <- setdiff(unique(pa_mitma[pa_mitma$NATCODE %in% obs_2024[,c(5)],]$CO_COMARCA),pa_com[pa_com$year_detec > 0,]$CO_COMARCA)

# Load data comarcas shapefile
comarcas <- read_sf(paste0(path, "comarcas/ComarcasAgrarias.shp"))
comarcas <- comarcas[comarcas$DS_CCAA != "Ceuta" &
                       comarcas$DS_CCAA != "Melilla" &
                       comarcas$DS_CCAA != "Islas Baleares" &
                       comarcas$DS_CCAA != "Canarias", ]

# Detected 2024
comarcas$detec_2024 <- 0
comarcas[comarcas$CO_COMARCA %in% CO_COMARCA_2024,]$detec_2024 <- 1
comarcas[comarcas$CO_COMARCA %in% pa_com[pa_com$year_detec>0,]$CO_COMARCA,]$detec_2024 <- -1

# plot
ggplot(comarcas) +
  geom_sf(aes(fill = as.factor(detec_2024)))

# Load probability
prob_2024 <- read.csv(paste0(path, "full_model_2024.csv"))
comarcas_prob <- comarcas %>% left_join(prob_2024)

# Compute centroid for plot
comarcas_prob_cent <- st_make_valid(comarcas_prob)
comarcas_prob_cent <- st_centroid(comarcas_prob_cent)

# plot
comarcas_prob[comarcas_prob$detec_2024 == -1,]$out <- NA
map_val <- ggplot(comarcas_prob) +
  geom_sf(data= comarcas_prob, aes( fill = out)) +
  geom_sf(data =comarcas_prob_cent[comarcas_prob_cent$detec_2024 == 1,]  ,
          aes(color = as.factor(detec_2024)), alpha = 0.9) +
  scale_fill_distiller(palette = "Spectral", name = "Occupancy prob") +
  scale_color_manual(values = c("black"),
                     labels = c("Detected in 2024"),
                     name = "") +
  theme_minimal()

mean(comarcas_prob[comarcas_prob$detec_2024 == 1,]$out)
var(comarcas_prob[comarcas_prob$detec_2024 == 1,]$out)
mean(comarcas_prob[comarcas_prob$detec_2024 == 0,]$out)
var(comarcas_prob[comarcas_prob$detec_2024 == 0,]$out)

# Box plot
bp_val <- ggplot(comarcas_prob[comarcas_prob$detec_2024 != -1,]) +
  geom_boxplot(aes(as.factor(detec_2024), out, fill = as.factor(detec_2024))) +
  scale_fill_viridis_d() +
  theme_bw(base_size = 15) + xlab("") + 
  ylab("Occupancy probability 2024") +
  scale_x_discrete(labels = c("0" = "Not detected", "1" = "Detected in 2024")) +
  theme(legend.position = "none")

# Arrange plots
gg1 <- ggarrange(map_val + 
                   theme(legend.position = c(0.85,0.2)),
                 bp_val,
          ncol = 2,
          widths = c(1,0.8), labels = c("a","b"))

ggsave("/home/marta/Documentos/PHD/2025/Colonization/Plots/Panels/panel_validation.png",
       plot = gg1,
       width = 13,
       height = 6,
       dpi = 250)

# Check population density vs detection method ------------------------------------------------------
# Load pa data
Path <- "/home/marta/albo_mobility/data/pa_data/MUNS_ANYS.csv"
pa_data <- read.csv(Path)

# Create column with first detection method
pa_data[pa_data$A_PRIM_DET_OFICIAL == 0,]$A_PRIM_DET_OFICIAL <- 9999999
pa_data[pa_data$A_PRIM_DET_CITSCI == 0,]$A_PRIM_DET_CITSCI <- 9999999
pa_data$src <- ifelse(pa_data$A_PRIM_DET_OFICIAL <= pa_data$A_PRIM_DET_CITSCI &
                        pa_data$A_PRIM_DET_OFICIAL != 9999999, "trad",
                      ifelse(pa_data$A_PRIM_DET_OFICIAL > pa_data$A_PRIM_DET_CITSCI, "citsci",
                                           "nodetec"))
pa_data$year_detec <- ifelse(
  pa_data$A_PRIM_DET_OFICIAL <= pa_data$A_PRIM_DET_CITSCI, pa_data$A_PRIM_DET_OFICIAL,
  ifelse(pa_data$A_PRIM_DET_CITSCI  == 9999999 & pa_data$A_PRIM_DET_OFICIAL  == 9999999, 0,
         pa_data$A_PRIM_DET_CITSCI))


# Add population density to municipality
pa_data$pop_dens <- 0
for(i in c(1:nrow(pa_data))){
  print(paste0("i:",i))
  year <- pa_data[i,]$year_detec
  if( year >0 & year < 9999){
    # Load pop density for that year  
    pop_df <- read.csv(paste0("~/albo_mobility/data/pop/pobmun",substr(as.character(year),3,4),".csv"))
    pop_df <- pop_df[,c(1,3,5)]
    colnames(pop_df) <- c("cpro", "cmun", "pob")
    pop_df$pob <- as.numeric(gsub("\\.","", pop_df$pob))
    
    # Load spanish map
    esp_can <- esp_get_munic_siane(moveCAN = TRUE)
    esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                         esp_can$cpro,
                                         esp_can$LAU_CODE))
    esp_can <- esp_can[esp_can$ine.ccaa.name != "Ceuta" &
                         esp_can$ine.ccaa.name != "Melilla" &
                         esp_can$ine.ccaa.name != "Balears, Illes" &
                         esp_can$ine.ccaa.name != "Canarias", ]
    
    # Transform to match pop density 
    esp_can <- esp_can[,c("cmun", "cpro", "NATCODE")]
    esp_can$cpro <- as.integer(esp_can$cpro); esp_can$cmun <- as.integer(esp_can$cmun)
    
    # Compute area
    esp_can <- st_make_valid(esp_can)
    esp_can$area <- as.numeric(st_area(esp_can))*10^-6
    
    # Join pop dens with area
    pop_df <- pop_df %>% left_join(esp_can)
    pop_df$dens <- pop_df$pob/pop_df$area
    
    # Add to detection file
    dens_NATCODE <- pop_df[pop_df$NATCODE == pa_data[i,]$NATCODE & !is.na(pop_df$NATCODE),]$dens
    pa_data[i,]$pop_dens <- ifelse(length(dens_NATCODE) > 0, dens_NATCODE, 0)
  }
  
}

# Add human pop density to the non detected using 2023 data
# Load pop density for that year  
pop_df <- read.csv(paste0("~/albo_mobility/data/pop/pobmun23.csv"))
pop_df <- pop_df[,c(1,3,5)]
colnames(pop_df) <- c("cpro", "cmun", "pob")
pop_df$pob <- as.numeric(gsub("\\.","", pop_df$pob))

# Load spanish map
esp_can <- esp_get_munic_siane(moveCAN = TRUE)
esp_can$NATCODE <- as.numeric(paste0("34",esp_can$codauto,
                                     esp_can$cpro,
                                     esp_can$LAU_CODE))
esp_can <- esp_can[esp_can$ine.ccaa.name != "Ceuta" &
                     esp_can$ine.ccaa.name != "Melilla" &
                     esp_can$ine.ccaa.name != "Balears, Illes" &
                     esp_can$ine.ccaa.name != "Canarias", ]

# Transform to match pop density 
esp_can <- esp_can[,c("cmun", "cpro", "NATCODE")]
esp_can$cpro <- as.integer(esp_can$cpro); esp_can$cmun <- as.integer(esp_can$cmun)

# Compute area
esp_can <- st_make_valid(esp_can)
esp_can$area <- as.numeric(st_area(esp_can))*10^-6

# Join pop dens with area
pop_df <- pop_df %>% left_join(esp_can)
pop_df$dens <- pop_df$pob/pop_df$area

# Add pop dens to non-detected NATCODEs
pa_data <- pa_data %>% left_join(pop_df[,c("NATCODE", "dens")])
pa_data$pop_dens_c <- ifelse(pa_data$src == "nodetec", pa_data$dens,pa_data$pop_dens)

# Save data set
saveRDS(pa_data, "/home/marta/Documentos/PHD/2025/Colonization/output/human_density_detection_method.Rds")
pa_data <- readRDS("/home/marta/Documentos/PHD/2025/Colonization/output/human_density_detection_method.Rds")

# Plot distribution cit sci and trad
ggplot(pa_data) +
  geom_density(aes(log10(pop_dens_c) ,
                   group = src, fill = src), alpha = 0.4) +
  scale_fill_viridis_d(name= "Detection method",
                       labels = c("Citizen Science",
                                  "Not detected","Traps")) +
  theme_bw(base_size = 14)  +
  xlab("Population density (log10)") +
  ylab("Density")

# Check if the three distributions are significantly different
library(dgof)
ks.test(pa_data[pa_data$src == "citsci", ]$pop_dens, 
        pa_data[pa_data$src == "trad", ]$pop_dens)

ks.test(pa_data[pa_data$src == "trad", ]$pop_dens, 
        pa_data[pa_data$src == "nodetec", ]$pop_dens)

ks.test(pa_data[pa_data$src == "citsci", ]$pop_dens, 
        pa_data[pa_data$src == "nodetec", ]$pop_dens)


