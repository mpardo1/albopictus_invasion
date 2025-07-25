#------------------------FUNCTIONS---------------------------#
# Main functions 
Briere_func <- function(cte, tmin, tmax, temp){
  outp <- temp*cte*(temp - tmin)*(tmax - temp)^(1/2)
  if(outp < 0 | is.na(outp)){
    outp <- 0
  }
  return(outp)
}

Quad_func <- function(cte, tmin, tmax, temp){
  outp <- -cte*(temp - tmin)*(temp - tmax)
  if(outp < 0 | is.na(outp)){
    outp <- 0
  }
  return(outp)
}

Lin_func <- function(cte, cte1, temp){
  outp <- temp*cte + cte1
  if(outp < 0 | is.na(outp)){
    outp <- 0.00001
  }
  return(outp)
}


Quad <- function(cte, cte1,cte2, temp){
  outp <- cte*temp^2 + cte1*temp + cte2
  if(outp < 0 | is.na(outp)){
    outp <- 0
  }
  return(outp)
}

QuadN_func <- function(cte, c1, c2, temp){
  outp <- cte*temp^2 + c1*temp + c2
  if(outp < 0 | is.na(outp)){
    outp <- 0
  }
  return(outp)
}
### Incorporating rain and human density:
h_f <- function(hum, rain){
  # Constants: 
  erat = 0.5
  e0 = 1.5
  evar = 0.05
  evar = 0.1
  eopt = 8
  efac = 0.01
  edens = 0.01
  
  hatch <- (1-erat)*(((1+e0)*exp(-evar*(rain-eopt)^2))/(exp(-evar*(rain - eopt)^2) + e0)) +
    erat*(edens/(edens + exp(-efac*hum)))
  return(hatch)
}

### Incorporating rain and human density:
h_f_jap <- function(land, rain){
  # Constants:
  e0 = 1.5
  evar = 0.05
  # evar = 0.1
  eopt = 8
  
  hatch <- land*(((1+e0)*exp(-evar*(rain-eopt)^2))/(exp(-evar*(rain - eopt)^2) + e0))
  return(hatch)
}

### Incorporating rain and human density:
h_f_jap_2 <- function(land1, rain, land2){
  # Constants:
  e0 = 0.5
  evar = 0.05
  # evar = 0.1
  eopt = 8
  
  hatch <- land1*(((1+e0)*exp(-evar*(rain-eopt)^2))/(exp(-evar*(rain - eopt)^2) + e0)) + land2
  
  return(hatch)
}

### Incorporating rain and human density:
h_f_jap_3 <- function(land, rain){
  # Constants:
  e0 = 0.5
  evar = 0.05
  #evar = 0.1
  eopt = 8
  
  hatch <- land*(((1+e0)*exp(-evar*(rain-eopt)^2))/(exp(-evar*(rain - eopt)^2) + e0))
  return(hatch)
}
#### -------------------------- Albopictus ------------------------- ####
## Thermal responses Aedes Albopictus from Mordecai 2017:
a_f_alb <- function(temp){Briere_func(0.000193,10.25,38.32,temp)} # Biting rate
TFD_f_alb <- function(temp){Briere_func(0.0488,8.02,35.65,temp)} # Fecundity
pLA_f_alb <- function(temp){Quad_func(0.002663,6.668,38.92,temp)} # Survival probability Egg-Adult
MDR_f_alb <- function(temp){Briere_func(0.0000638,8.6,39.66,temp)} # Mosquito Development Rate
lf_f_alb <- function(temp){Quad_func(1.43,13.41,31.51,temp)} # Adult life span
dE_f_alb <- function(temp){Quad_func(0.00071,1.73,40.51,temp)} # Adult life span

# R0 function by temperature:
R0_func_alb <- function(Te, rain, hum){
  if(is.na(Te) | is.na(rain) | is.na(hum)){
    R0 <- NA
  }else{
    a <- a_f_alb(Te)
    f <- (1/2)*TFD_f_alb(Te)
    deltaa <- lf_f_alb(Te)
    dE <- dE_f_alb(Te)
    probla <- pLA_f_alb(Te)
    h <- h_f(hum,rain)
    deltaE = 0.1
    R0 <- ((f*a*deltaa)*probla*(h*dE/(h*dE+deltaE)))^(1/3)
  }
  return(R0)
}

####------------------------------Aegypti------------------------####
a_f_aeg <- function(temp){Briere_func(0.000202,13.35,40.08,temp)} # Biting rate
EFD_f_aeg <- function(temp){Briere_func(0.00856,14.58,34.61,temp)} # Fecundity
pLA_f_aeg <- function(temp){Quad_func(0.004186,9.373,40.26,temp)} # Survival probability Egg-Adult
MDR_f_aeg <- function(temp){Briere_func(0.0000786,11.36,39.17,temp)} # Mosquito Development Rate
lf_f_aeg <- function(temp){Quad_func(0.148,9.16,37.73,temp)} # Adult life span
dE_f_aeg <- function(temp){Briere_func(0.0003775 ,14.88,37.42,temp)} # Adult life span

# R0 function by temperature:
R0_func_aeg <- function(Te, rain,hum){
  if(is.na(Te) | is.na(rain) | is.na(hum)){
    R0 <- NA
  }else{
    a <- a_f_aeg(Te)
    f <- 40
    deltaa <- lf_f_aeg(Te)
    dE <- dE_f_aeg(Te)
    probla <- pLA_f_aeg(Te)
    h <- h_f(hum,rain)
    deltE = 0.1
    R0 <- ((f*a*deltaa)*probla*(h*dE/(h*dE+deltE)))^(1/3)
  }
  return(R0)
}


#####----------------Japonicus-----------------####
dE_f_jap <- function(temp){Briere_func(0.0002859,6.360,35.53 ,temp)} # Mosquito Development Rate
dL_f_jap <- function(temp){Briere_func(7.000e-05,9.705e+00,3.410e+01,temp)} # Survival probability Egg-Adult
lf_f_jap <- function(temp){Lin_func(-2.5045,82.6525,temp)} # Adult life span
deltaL_f_jap <- function(temp){QuadN_func(0.0021476,-0.0806067 ,1.0332455,temp)} # Adult life span

# R0 function by temperature:
R0_func_jap <- function(Te, rain,hum){
  if(is.na(Te) | is.na(rain) | is.na(hum)){
    R0 <- NA
  }else{
    a <- 0.35
    f <- 40 #183/2
    lf <- lf_f_jap(Te)
    deltaL <- deltaL_f_jap(Te)
    deltE = 0.1
    dE <- dE_f_jap(Te)
    dL <- dL_f_jap(Te)
    h <- h_f_jap(hum,rain)
    if(dL == 0 | f == 0 | a == 0 | dE == 0 |  Te<0){
      R0 <- 0
    }else{
      R0 <- ((f*a*lf)*(dL/(dL+deltaL))*(h*dE/(h*dE+deltE)))^(1/3)
    }
  }
  return(R0)
}

# Second function R_M with independent landcover
R0_func_jap_2 <- function(Te, rain,land1, land2){
  if(is.na(Te) | is.na(rain) | is.na(land1)){
    R0 <- NA
  }else{
    a <- 0.35
    f <- 40 #183/2
    lf <- lf_f_jap(Te)
    deltaL <- deltaL_f_jap(Te)
    deltE = 0.1
    dE <- dE_f_jap(Te)
    dL <- dL_f_jap(Te)
    if(rain == 0){
      print("0 rain")
    }
    h <- h_f_jap_2(land1,rain,land2)
    if(dL == 0 | f == 0 | a == 0 | dE == 0 |  Te<0){
      R0 <- 0
    }else{
      R0 <- ((f*a*lf)*(dL/(dL+deltaL))*(h*dE/(h*dE+deltE)))^(1/3)
    }
  }
  return(R0)
}

# R0 function by temperature with evar = 0.05:
R0_func_jap_3 <- function(Te, rain,hum){
  if(is.na(Te) | is.na(rain) | is.na(hum)){
    R0 <- NA
  }else{
    a <- 0.35
    f <- 40 #183/2
    lf <- lf_f_jap(Te)
    deltaL <- deltaL_f_jap(Te)
    deltE = 0.1
    dE <- dE_f_jap(Te)
    dL <- dL_f_jap(Te)
    h <- h_f_jap_3(hum,rain)
    if(dL == 0 | f == 0 | a == 0 | dE == 0 |  Te<0){
      R0 <- 0
    }else{
      R0 <- ((f*a*lf)*(dL/(dL+deltaL))*(h*dE/(h*dE+deltE)))^(1/3)
    }
  }
  return(R0)
}