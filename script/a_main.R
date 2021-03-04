#Last edited by - Deus Thindwa
#Date - 25/02/2021

# load packages
somipa.packages <- c("tidyverse", "data.table", "grid","ggrepel", "pwr", "splitstackshape", "socialmixr","reshape2", "patchwork", "ggpubr", "here")
lapply(somipa.packages, library, character.only=TRUE)

# work out power analysis for social mixing patterns
dev.off()
source(here::here("script", "b_power_analysis.R"))

# work out stratified sampling based on SCALE study 
dev.off()
source(here::here("script", "c_somipa_sampling.R"))

# identify determinants of social mixing frequency and mixing types
dev.off()
source(here::here("script", "d_somipa_determinants.R"))

# estimate social mixing matrices by contact type
dev.off()
source(here::here("script", "e_somipa_matrices1.R"))

# estimate social mixing matrices by hiv status and places of contact
dev.off()
source(here::here("script", "e_somipa_matrices2.R"))

# description plots of social mixing events
dev.off()
source(here::here("script", "f_somipa_descriptive.R"))

# estimate spatial distance and number of social mixing events 
dev.off()
source(here::here("script", "g_somipa_spatial.R"))

# simulate a theoretic epidemic based on estimated social mixing rates 
dev.off()
source(here::here("script", "h_somipa_epidemic.R"))

#END SCRIPT

