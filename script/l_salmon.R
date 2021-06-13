#Esmelda analysis

# load the require packages
if (!require(pacman)){
  install.packages("pacman")
}
pacman::p_load(char = c("tidyverse", "drc"))
 
                       
#read csv
salmon <- read_csv(here::here("data", "salmon.csv"))


#calculate mean OD
salmon <- salmon %>% rowwise() %>% mutate(std_odM = mean(c(std_od1, std_od2)), std_concM = mean(c(std_conc1, std_conc2)))

#conmpute concentration
#' Calculate the concentration of a substrate from a known OPTICAL DENSITY
#' 
#' This function will give a calculated concentration of a subsance in 25 well 
#' plate given the optical density from a plate reader.
#' 
#' @param OD Optical density of the STANDARD DILUTION [numeric vector]
#' @param conc KNOWN concentrations of the standard dilution [numeric vector]
#' @param OD_sample Optical densities of samples [named vector]
#' @return A data frame with OD, log fitted values and calculated concentrations
#'  of the samples and a plot with a standard curve standard and blue samples
#' @details This function is useful when working with old plate readers. It uses 
#' the FOUR PARAMETER CURVE to fit the model to your standards. 

Esmelda <- function(OD, conc, OD_sample){
  
  logconc <- log10(conc)# log10 from conc
  stdcrvdata <- data.frame(OD,conc,logconc)
  fit<-drm(formula = OD ~ conc , data = stdcrvdata, fct = LL.4())
  samples <- data.frame(OD = OD_sample)# data from mesurments
  samples$loganswer <- fit$coefficients[4] * (((-1* fit$coefficients[3] + samples$OD) / (fit$coefficients[2] - samples$OD))^(1 / fit$coefficients[1]))
  samples$conc <- 10^samples$loganswer
  plot(fit)
  lines(samples$loganswer, samples$OD, type = "points", col = "blue") 
  return(samples)
  
}

Esmelda(salmon$std_odM, salmon$std_concM, salmon$std_odM)




