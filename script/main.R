#Last edited by - Deus Thindwa
#Date - 28/10/2019

# load packages
power.packages <- c("plyr", "tidyverse", "data.table", "grid","ggrepel", "pwr", "splitstackshape", "socialmixr", "here")
lapply(power.packages, library, character.only=TRUE)

# work out power analysis for social mixing patterns
dev.off()
source(here::here("Social.Mixing", "script", "power_calculation.R"))

# work out stratified sampling based on SCALE study 
dev.off()
source(here::here("Social.Mixing", "script", "somipa_sampling.R"))

# work out determinants of social contact frequency and types in bayesian framework 
dev.off()
source(here::here("Social.Mixing", "script", "somipa_bayes.R"))

# work out social contacts matrices
dev.off()
source(here::here("Social.Mixing", "script", "somipa_matrices.R"))

# work out spatial distance and number of social contacts 
dev.off()
source(here::here("Social.Mixing", "script", "somipa_spatial.R"))

# simulate an epidemic based on estimated social contact rates 
dev.off()
source(here::here("Social.Mixing", "script", "somipa_epidemic.R"))

