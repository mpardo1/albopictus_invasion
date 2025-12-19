# Code to run the computation of the eigenvalues for the constant climate 
# conditions needed to create Figure S9 in the Supplementary material
# To run the code be on the albopictus_invasion directory if not change path adding the path to albopictus_invasion directory in line 16

# Remove everything before starting
rm(list=ls())

# Load packages
library(data.table)
library(ggplot2)
library(dplyr)
library(sf)
library(tidyr)

# Path of the processed files
path_out <- "data/output/"

# Load data ------------------------------------------------------------
# Data from input_Hanski_com.R
flows_mat <- read.csv(paste0(path_out,"flows_apr_2023_nov_2023_mitma_ESP_com_v2.csv"))
eta <- as.matrix(flows_mat[,-1])
diag(eta) <- 0
dist_mat <- read.csv(paste0(path_out,"dist_mat_com_ESP.csv"))
dist <- as.matrix(dist_mat[,-1])
diag(dist) <- 0

# Compute an average RM
df_clim_ESP <-read.csv(paste0(path_out,"3_rm_alb_ESP_com_0_2_v2.csv"))
mean_RM <- colSums(df_clim_ESP[,c(2:ncol(df_clim_ESP))])/nrow(df_clim_ESP)
max_RM <- apply(df_clim_ESP[,c(2:ncol(df_clim_ESP))], 2,max)
N = nrow(eta)
vec_RM <- matrix(0,N,N)
diag(vec_RM) <- mean_RM

# Save as Rds to plot
saveRDS(mean_RM, paste0(path_out, "mean_RM_ESP.Rds"))

# Load the average minimum temperature for the period 2005-2023
min_tmin <- read.csv(paste0(path_out, "min_temp_yearly_mean_ESP.csv"))
min_tmin <- min_tmin[,-1]

# Estimated parameters
param <- c(0.0005706731568571638, 97.78894801162286, 5424.950376421082,
           6.977623970027257e-5, 51314.77501452204, -6.874687389443816, -80.947311566672)
m_c = 0.0051

# Metapopulation capacity -------------------------------------------------------

# Function to savely compute exponential without Float digit limit
safe_exp <- function(x,  lower = -700,upper= 700){
  x_clipped <- pmin(pmax(x,lower), upper)
  exp(x_clipped)
}

# Function metapopulation
lambda_M_func <- function(c1,c2,c3,cd,alp,e1,e2){
  mat_aux <- ((c1/(1+safe_exp((-c2*m_c)*eta+c3)))+cd*safe_exp(-(1/alp)*dist))
  mat1 <- vec_RM%*%mat_aux
  diag(mat1) <- 0
  mat2 <- matrix(0,N,N)
  diag(mat2) <- safe_exp(e1*min_tmin+e2)
  mat <- mat1-mat2
  mat[1:4,1:4]
  return(max(Re(eigen(mat)$values)))
}

# # Test
# # plot(eigen(mat)$values)
c1 <- param[1]
cd <- param[4]
c2 <- param[2]
c3 <- param[3]
e1 <- param[6]
e2 <- param[7]
alp <- param[5]
cte <- runif(1,0,0.1)
lambda_M_func(c1,c2,c3,cd,alp, cte*e1, cte*e2)

# Estimated parameters
p=param
size = 50

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

ggplot(grid_df) +
  # geom_point(aes(cte1,cte2,color = as.factor(sign(lambda_M))))
  geom_point(aes(cte1,cte2,color = as.factor(sign(lambda_M))))

grid1_df <- grid_df # rename to use later
#  c1 vs ext -----------------------------------------------------------------
x1 <- seq(0, 1, length.out = 100)
x2 <- seq(0, 0.2, length.out = 100)
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

ggplot(grid_df) +
  # geom_point(aes(cte1,cte2,color = as.factor(sign(lambda_M))))
  geom_point(aes(cte1,cte2,color = as.factor(sign(lambda_M))))

# Save data frame# Save data framelambda_M_func()
saveRDS(rbind(grid_df,grid1_df), paste0(path_out,"output_phase_space/phase_space_dist_c1_vs_pres_",Sys.Date(),".Rds"))
