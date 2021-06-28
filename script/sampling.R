
#Last edited by - Deus Thindwa
#Date - 28/10/2019

#=================================================================================

# load the require packages
if (!require(pacman)){
  install.packages("pacman")
}
pacman::p_load(char = c("tidyverse", "lubridate","data.table", "grid","ggrepel", "pwr", "splitstackshape","PropCIs",
                        "socialmixr","reshape2","readstata13", "patchwork", "ggpubr", "foreign","forcats", "geosphere", "broman", "here"))

options(stringsAsFactors = FALSE)
setwd(here::here())

#sample from SCALE study second list of participants
scale <- filter(read_csv(here::here("data", "scale.csv")), s02cl_id >=1 & s02cl_id <=14)
scalehiv<- read_csv(here::here("data", "scalehiv.csv"))
address<- select(read.dta13(here::here("other", "address", "address.dta")), hh_id, h06adres)

scalefinal0 <- left_join(scale, scalehiv)
scalefinal1 <- scalefinal0 %>% filter(is.na(hiv) & (d12sumres ==2 | d12sumres ==3))

scalefinal2 <- left_join(scalefinal1, address)

#output csv
write.csv(scalefinal2, "data/scalehiv2.csv")

