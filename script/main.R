#Last edited by - Deus Thindwa
#Date - 28/10/2019

#=================================================================================

#install the following packages only once
#install.packages("R.rsp")
#install_github("nyiuab/NBZIMM", force=T, build_vignettes=T)

# load the require packages
if (!require(pacman)){
  install.packages("pacman")
}
pacman::p_load(char = c("tidyverse", "remotes", "NBZIMM", "dplyr", "lubridate","data.table", "grid","ggrepel", "pwr", "splitstackshape","PropCIs", "table1",
                        "socialmixr","reshape2", "patchwork", "naniar", "scales", "ggpubr", "deSolve", "foreign","forcats", "geosphere", "boot", "broman",
                        "sf", "MASS", "epiR", "here"))

options(stringsAsFactors = FALSE)
setwd(here::here())


# load all required datasets
hh.unlabel <- read_csv(here::here("data", "hh.unlabel.csv"))
hh.labeled <- read_csv(here::here("data", "hh.labeled.csv"))
pp.unlabel <- read_csv(here::here("data", "pp.unlabel.csv"))
pp.labeled <- read_csv(here::here("data", "pp.labeled.csv"))
cn.unlabel <- read_csv(here::here("data", "cn.unlabel.csv"))
cn.labeled <- read_csv(here::here("data", "cn.labeled.csv"))

#update number of contacts in pp datasets
pp.unlabel <- pp.unlabel %>% left_join(cn.labeled %>% group_by(somipa_pid) %>% tally()) %>% dplyr::select(scale_pid:transdk, n, hiv:cvdelderly) %>% rename("cntno" = n)
pp.labeled <- pp.labeled %>% left_join(cn.labeled %>% group_by(somipa_pid) %>% tally()) %>% dplyr::select(scale_pid:transdk, n, hiv:cvdelderly) %>% rename("cntno" = n)

# create survey object by combining participants and contacts datasets
part.m <- rename(dplyr::select(pp.labeled, date, somipa_pid, scale_pid, agey, sex, cvdcnt, hiv), "part_age" = agey, "part_sex" = sex, "part_cvd" = cvdcnt, "part_hiv" = hiv)
part.m <- part.m %>% mutate(country = "Malawi", year = 2021, dayofweek = wday(dmy_hm(date)))

cnt.m <- dplyr::select(cn.labeled, somipa_pid, cnt_age, cnt_type, cnt_loc, cnt_plc, cnt_rel)
cnt.m <- cnt.m %>% arrange(somipa_pid) %>% mutate(cnt_id = 1:n())
cnt.m$cnt_pid <- c(0, cumsum(as.numeric(with(cnt.m, somipa_pid[1:(length(somipa_pid)-1)] != somipa_pid[2:length(somipa_pid)])))) + 1
cnt.m <- cnt.m %>% dplyr::select(cnt_id, cnt_pid, somipa_pid, everything())

#rename contact matrix merging variable from somipa_pid to part_id
part.m <- part.m %>% rename("part_id" = somipa_pid)
cnt.m <- cnt.m %>% rename("part_id" = somipa_pid)

#=================================================================================

# participant and contactees description 
dev.off()
source(here::here("script", "Fig1_participant_contact_desc.R"))

# mixing events and spatial distance
dev.off()
source(here::here("script", "Fig2a_spatially_contacts.R"))

# mixing events description
dev.off()
source(here::here("script", "Fig2b_contact_event_desc.R"))

# matrices of social mixing rates by overall and mixing type 
dev.off()
source(here::here("script", "Fig3_crude_contact_matrix.R"))

# contact matrices and HIV status
dev.off()
source(here::here("script", "Fig4a_hiv_contac_matrix.R"))

# matrices of stratified social mixing rates
dev.off()
source(here::here("script", "Fig4b_strat_contacts_matrix.R"))

#=================================================================================

# community and household characteristics
dev.off()
source(here::here("script", "FigS1_household_char.R"))

# travel history of participants and COVID-19 risk
dev.off()
source(here::here("script", "FigS2_travel_history_covid.R"))

# box plots on the number of contacts and COVID-19 risk behavior 
dev.off()
source(here::here("script", "FigS3_covid_number_contact.R"))

# contact matrices and COVID-19 risk behavior
dev.off()
source(here::here("script", "FigS4_covid_contact_matrix.R"))

#END SCRIPT
