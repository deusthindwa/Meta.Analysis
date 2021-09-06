#Last edited by - Deus Thindwa
#Date - 28/10/2019

#=================================================================================

# load the require packages
if (!require(pacman)){
  install.packages("pacman")
}
pacman::p_load(char = c("tidyverse", "lubridate","data.table", "grid","ggrepel", "pwr", "splitstackshape","PropCIs", "table1",
                        "socialmixr","reshape2", "patchwork", "ggpubr", "foreign","forcats", "geosphere", "boot", "broman", "here"))

options(stringsAsFactors = FALSE)
setwd(here::here())

# work out stratified sampling based on SCALE study 
dev.off()
source(here::here("script", "b_get_datasets.R"))


# identify determinants of social mixing frequency and mixing types
dev.off()
source(here::here("script", "c_somipa_determinants.R"))


# description plots of social mixing events
dev.off()
source(here::here("script", "d_contacts_description.R"))


# estimate spatial distance and number of social mixing events 
dev.off()
source(here::here("script", "e_contacts_spatial.R"))


# estimate social mixing matrices by contact type
dev.off()
source(here::here("script", "f_contacts_matrix_crude.R"))


# estimate social mixing matrices by hiv status and places of contact
dev.off()
source(here::here("script", "g_contacts_matrix_strat.R"))


# simulate a theoretic epidemic based on estimated social mixing rates 
dev.off()
source(here::here("script", "h_epidemic_simulation.R"))


#END SCRIPT

