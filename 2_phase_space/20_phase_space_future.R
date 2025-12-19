# Code to run the computation of the eigenvalues for the future climate change
# conditions needed to create Figure 4 in the main text
# Remove everything before starting
rm(list=ls())

# Load packages
library(data.table)
library(ggplot2)
library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)

# Path of the processed files
path_out <- "data/output/"

# Load data
flows_mat <- read.csv(paste0(path_out,"flows_apr_2023_nov_2023_mitma_ESP_com_v2.csv"))
eta <- as.matrix(flows_mat[,-1])
diag(eta) <- 0
dist_mat <- read.csv(paste0(path_out,"dist_mat_com_ESP.csv"))
dist <- as.matrix(dist_mat[,-1])
diag(dist) <- 0

# Filter year 
year_f <- 2050

# Compute an average RM
df_clim_ESP <-read.csv(paste0(path_out,"rm_alb_ESP_com_future.csv"))
df_clim_ESP <- df_clim_ESP[year(df_clim_ESP$date) < year_f,]
mean_RM <- colSums(df_clim_ESP[,c(2:ncol(df_clim_ESP))])/nrow(df_clim_ESP)
max_RM <- apply(df_clim_ESP[,c(2:ncol(df_clim_ESP))], 2,max)
N = nrow(eta)
vec_RM <- matrix(0,N,N)
diag(vec_RM) <- mean_RM

# Save as a Rds to plot
saveRDS(mean_RM, paste0(path_out,"mean_RM_fut_ESP.Rds"))

# Compute an average yearly minimum temp --------------------------------------------
df_tmin_ESP <-read.csv(paste0(path_out,"min_temp_ESP_future.csv"))
df_tmin_ESP$date <- as.Date(df_tmin_ESP$date)

# compute minimum year
df_tmin_ESP <- df_tmin_ESP[,c(1:ncol(df_tmin_ESP))] %>%  
  pivot_longer(cols = starts_with("X"),
               names_to = "CO_COMARCA",
               values_to = "value") %>% 
  mutate(CO_COMARCA = as.integer(sub("^X", "", CO_COMARCA))) %>%
  group_by(year(date), CO_COMARCA) %>% 
  summarise(tmin = min(value))

# Test
colnames(df_tmin_ESP)[1] <- "year"
ggplot(df_tmin_ESP[df_tmin_ESP$CO_COMARCA == df_tmin_ESP$CO_COMARCA[1],] ) +
  geom_point(aes(year, tmin))+
  geom_line(aes(year, tmin))

# filter years
df_tmin_ESP <- df_tmin_ESP[df_tmin_ESP$year < year_f,] 
min_fut <- df_tmin_ESP %>%  group_by(CO_COMARCA) %>% 
  summarise(tmin= mean(tmin))
min_tmin <- min_fut$tmin

# Save as a Rds to plot
saveRDS(min_tmin, paste0(path_out, "min_temp_yearly_mean_fut_ESP.Rds"))

## save
# write.csv(min_tmin, "data/InputHanski/ESP/min_temp_yearly_mean_fut_ESP.csv")

# Estimated parameters
param <- c(0.0005706731568571638, 97.78894801162286, 5424.950376421082,
           6.977623970027257e-5, 51314.77501452204, -6.874687389443816, -80.947311566672)
m_c = 0.0051

# Sigmoidal mobility and tmin -------------------------------------------------------
# Create grid and evaluate maximum eigenvalue
id_mat <- matrix(0,N,N)
diag(id_mat) <- 1
# multp_mat <-vec_RM%*%(1/(1+exp(-a*m_c*eta+b)))
# eigen_mat <- max(Re(eigen(vec_RM%*%(a/(1+exp(-b*m_c*eta+c))))$values))
lambda_M_func <- function(c1,c2,c3,cd,alp,e1,e2){
  mat_aux <- ((c1/(1+exp((-c2*m_c)*eta+c3)))+cd*exp(-(1/alp)*dist))
  mat1 <- vec_RM%*%mat_aux
  diag(mat1) <- 0
  mat2 <- matrix(0,N,N)
  diag(mat2) <- exp(e1*min_tmin+e2)
  mat <- mat1-mat2
  return(max(Re(eigen(mat)$values)))
}

# Test
# plot(eigen(mat)$values)
c1 <- param[1]
cd <- param[4]
c2 <- param[2]
c3 <- param[3]
e1 <- param[6]
e2 <- param[7]
alp <- param[5]

# Estimated parameters
p=param
size = 150

# Better to run in cluster to avoid memmory problems
#  c1 vs ext -----------------------------------------------------------------
x1 <- seq(0, 1, length.out = size)
x2 <- seq(0, 0.2, length.out = size)
grid_df <- setDT(expand.grid(cte1 = x1, cte2 = x2))
grid_df$c1 <- p[1]*grid_df$cte1
grid_df$c2 <- p[2]
grid_df$c3 <- p[3]
grid_df$cd <- p[4]
grid_df$alp <- p[5]
grid_df$e1 <- p[6]*grid_df$cte2
grid_df$e2 <- p[7]*grid_df$cte2

# Compute metapop capacity
grid_df[,lambda_M := mapply(lambda_M_func, c1,c2,c3,cd,alp,e1,e2)]
grid_df1 <- grid_df # Save for later

#  c1 vs ext -----------------------------------------------------------------
x1 <- seq(0, 1, length.out = 100)
x2 <- seq(0.9, 1, length.out = 20)
grid_df <- setDT(expand.grid(cte1 = x1, cte2 = x2))
grid_df$c1 <- p[1]*grid_df$cte1
grid_df$c2 <- p[2]
grid_df$c3 <- p[3]
grid_df$cd <- p[4]
grid_df$alp <- p[5]
grid_df$e1 <- p[6]*grid_df$cte2
grid_df$e2 <- p[7]*grid_df$cte2

# Compute metapop capacity
grid_df[,lambda_M := mapply(lambda_M_func, c1,c2,c3,cd,alp,e1,e2)]

# ggplot(grid_df) +
#   # geom_point(aes(cte1,cte2,color = as.factor(sign(lambda_M))))
#   geom_point(aes(cte1,cte2,color = as.factor(sign(lambda_M))))

# Save data frame# Save data framelambda_M_func()
saveRDS(rbind(grid_df, grid_df1), paste0(path_out,"output_phase_space/phase_space_dist_c1_vs_ext_fut_",Sys.Date(),".Rds"))
