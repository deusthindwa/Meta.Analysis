#Last edited by - Deus Thindwa
#Date - 28/10/2019

#=================================================================================

#load household dataset
householdQ <- read_csv(here::here("data", "household.csv"))

householdQ <- select(rename(householdQ,
"istatus" = index_status, "date" = data_date, "scale_hhid" = s1_1, "scale_pid" = s1_2, "community" = s1_3c, "cluster" = s1_4c, 
"hh_lon" = s1_5c, "hh_lat" = s1_6c, "age" = s1_7c, "sex" = s1_8c, "hhid" = s2_1, "iid" = s2_2, "room" = s2_3, "sroom" = s2_4, 
"gas" = s2_5_01, "electric" = s2_5_02, "firewood" = s2_5_03, "charcoal" = s2_5_04, "kerosene" = s2_5_05, "diesel" = s2_5_06, 
"indoor" = s2_6, "vent" = s2_7, "settle" = s2_8, "nlive" = s3_1, "nleft" = s3_2, "ndied" = s3_3, "smoke" = s3_4),
istatus, date, scale_hhid, scale_pid, community, cluster, hh_lon, hh_lat, age, sex, hhid, iid, room, sroom,
gas, electric, firewood, charcoal, kerosene, diesel, indoor, vent, settle, nlive, nleft, ndied, smoke)

#enrolled households without labels (hh1.enrol)
hh.unlabel <- filter(householdQ, istatus == 1) %>% mutate(sex = if_else(sex == "Male", 1, 2))
householdQ <- filter(householdQ, istatus != 1)

#enrolled households with labels (hh2.enrol)
hh.labeled <- hh.unlabel
hh.labeled <- hh.unlabel %>% 
  mutate (sex = if_else(sex == 1, "Male", "Female"), 
          gas = if_else(gas == 1, "Yes", "No"),
          electric = if_else(electric == 1, "Yes", "No"),
          firewood = if_else(firewood == 1, "Yes", "No"),
          charcoal = if_else(charcoal == 1, "Yes", "No"),
          kerosene = if_else(kerosene == 1, "Yes", "No"),
          diesel = if_else(diesel == 1, "Yes", "No"),
          indoor = if_else(indoor == 1, "Yes", "No"),
          vent = if_else(vent == 1, "Yes", "No"),
          smoke = if_else(smoke == 1, "Yes", if_else(smoke == 2, "No", "Dont know")))


#=================================================================================

#load participant dataset without labels (pp1.enrol)
pp.unlabel <- read_csv(here::here("data", "participant.csv"))

pp.unlabel <- select(rename(pp.unlabel,
"sdate" = data_date, "iid" = s4_0, "hhid" = s4_1, "pno" = s4_2, "pid" = pid, "pstatus" = s4_3, 
"dob" = dob, "age" = s4_6, "agescale" = s4_7, "agedeter" = s4_8, "sex" = s4_10, "occup" = s4_11, "educ" = s4_12,
"dtravel" = s5_1, "pvisit" = s5_2, "vtime" = s5_3, "transfot" = s5_4_01, "transbic" = s5_4_02, "transcar" = s5_4_03, "transpub" = s5_4_04, "transbac" = s5_4_05, "transdk" = s5_4_06,
"cvdcnt" = s5_5, "cvdno" = s5_6, "cvdhome" = s5_7_01, "cvdwork" = s5_7_02, "cvdxool" = s5_7_03, "cvdchurch" = s5_7_04, "cvdmrkt" = s5_7_05, "cvdothr" = s5_7_06,
"cvdinfant" = s5_8_01, "cvdprexool" = s5_8_02, "cvdprixool" = s5_8_03, "cvdsecxool" = s5_8_04, "cvdadult" = s5_8_05, "cvdelderly" = s5_8_06, "cntno" = s6_1_0),
sdate, iid, hhid, pno, pid, pstatus, dob, age, agescale, agedeter, sex, occup, educ, dtravel, pvisit, vtime, transfot, transbic, transcar, transpub, transbac, transdk, cvdcnt, cvdno, cvdhome, cvdwork, cvdxool, cvdchurch, cvdmrkt, cvdothr, cvdinfant, cvdprexool, cvdprixool, cvdsecxool, cvdadult, cvdelderly, cntno)

pp.unlabel <- pp.unlabel %>% mutate(age = if_else(agescale == 1, age/12, age), agey = if_else(is.na(dob), age, dob))

pp.unlabel <- select(pp.unlabel %>% mutate(agexact = if_else(agedeter == 1 | is.na(agedeter), 1, 2)),
sdate, iid, hhid, pno, pid, pstatus, agey, agexact, sex, occup, educ, dtravel, pvisit, vtime, transfot, transbic, transcar, transpub, transbac, transdk, cvdcnt, cvdno, cvdhome, cvdwork, cvdxool, cvdchurch, cvdmrkt, cvdothr, cvdinfant, cvdprexool, cvdprixool, cvdsecxool, cvdadult, cvdelderly, cntno)

pp.unlabel <- distinct(pp.unlabel, pid, .keep_all = TRUE)

#load participant dataset with labels (pp2.enrol)
pp.labeled <- pp.unlabel %>% 
  
#sex
mutate(sex = if_else(sex == 1, "Male", "Female"),

#occupation
occup = if_else(occup == 1, "Preschool", if_else(occup == 2, "School", if_else(occup == 3, "Office",if_else(occup == 4, "Shop",
        if_else(occup == 5, "Manual", if_else(occup == 6, "Agriculture", if_else(occup == 7, "Business", if_else(occup == 8, "Domestic",
        if_else(occup == 9, "Unemployed", if_else(occup == 10, "Retired", if_else(occup == 11, "Other", if_else(occup == 88, "Refuse", "Dont know")))))))))))),

#education
educ = if_else(educ == 1, "No education", if_else(educ == 2, "Primary", if_else(educ == 3, "Secondary", if_else(educ == 4, "College", NA_character_)))),
     
#distance travelled    
dtravel = if_else(dtravel == 1, "<5km", if_else(dtravel == 2, "5-10km", if_else(dtravel == 3, ">10km", "Never"))),
   
#frequency of visit      
pvisit = if_else(pvisit == 1, "Often per week", if_else(pvisit == 2, ">=1 per weeks", if_else(pvisit == 3, ">1 per month", if_else(pvisit == 4, "<1 per month","Never")))),
         
#time length of visit
vtime = if_else(vtime == 1, "<1hr", if_else(vtime == 2, "1-2hr", if_else(vtime == 3, "Half day", if_else(vtime == 4, "Whole day", if_else(vtime == 5, "1wk",
if_else(vtime == 6, ">1wk",if_else(vtime == 88, "Refuse", "Dont know"))))))),
       
#transport on foot  
transfot = if_else(transfot == 1, "Yes", "No"),

#transport on bicycle
transbic = if_else(transbic == 1, "Yes", "No"),

#transport on car
transcar = if_else(transcar == 1, "Yes", "No"),

#transport on public transport
transpub = if_else(transpub == 1, "Yes", "No"),

#transport on the back
transbac = if_else(transbac == 1, "Yes", "No"),

#dont know means of transport
transdk = if_else(transdk == 1, "Yes", "No"),

#effect of COVID-19
cvdcnt = if_else(cvdcnt == 1, "Yes", "No"))

#=================================================================================

#create contact dataset without labels
contacts.Q <- read_csv(here::here("data", "participant.csv"))

contacts.Q <- select(rename(contacts.Q, 
"sdate" = start, "edate" = end, "iid" = s4_0, "hhid" = s4_1, "pno" = s4_2, "pid" = pid, "part_sex" = s4_10, "cnt_no" = s6_1_0, 
"cnt_age" = s6_1_2a, "cnt_scale" = s6_1_2b, "cnt_sex" = s6_1_3, "cnt_type" = s6_1_4, "cnt_rel" = s6_1_5, "cnt_loc" = s6_1_6, "cnt_plc" = s6_1_7, "cnt_freq" = s6_1_9, "cnt_dur" = s6_1_10), 
sdate, iid, hhid, pno, pid, part_sex, cnt_no, cnt_age, cnt_scale, cnt_sex, cnt_type, cnt_rel, cnt_loc, cnt_plc, cnt_freq, cnt_dur)

contacts.Q <- select(contacts.Q %>% mutate(cnt_age = if_else(cnt_scale == 1, cnt_age/12, cnt_age)),
                   sdate, iid, hhid, pno, pid, part_sex, cnt_no, cnt_age, cnt_sex, cnt_type, cnt_rel, cnt_loc, cnt_plc, cnt_freq, cnt_dur)

cn.unlabel <- contacts.Q <- contacts.Q %>% filter(!is.na(pid))

#create contact dataset without labels
cn.labeled <- cn.unlabel %>% mutate(
  
  part_sex = if_else(part_sex == 1L, "Male", "Female"),
  
  cnt_sex = if_else(cnt_sex == 1L, "Male", "Female"),

  cnt_type = if_else(cnt_type == 1L, "Physical", "Non-physical"),

  cnt_rel = if_else(cnt_rel == 1L, "House\nmember",
                            if_else(cnt_rel == 2L, "Other\nrelative",
                                    if_else(cnt_rel == 3L, "Coworker\nSchoolmate",
                                            if_else(cnt_rel == 4L, "Friend",
                                                    if_else(cnt_rel == 88L, "Refuse", "Random\npeople"))))),

  cnt_loc = if_else(cnt_loc == 1L | cnt_loc == 2L, "Home",
                            if_else(cnt_loc == 3L, "Work",
                                    if_else(cnt_loc == 4L, "School",
                                            if_else(cnt_loc == 5L, "Mosque\nChurch",
                                                    if_else(cnt_loc == 6L, "Transport",
                                                            if_else(cnt_loc == 7L, "Leisure",
                                                                    if_else(cnt_loc == 8L | cnt_loc == 9L, "Market\nShop", 
                                                                            if_else(cnt_loc == 10L, "Water\nsource",
                                                                                    if_else(cnt_loc == 11L, "Garden", "Other"))))))))),

  cnt_plc = if_else(cnt_plc == 1L, "Within\ncommunity", "Outside\ncommunity"),

  cnt_freq = if_else(cnt_freq == 1L, "Daily",
                             if_else(cnt_freq == 2L, "≥1 Weekly",
                                     if_else(cnt_freq == 3L, "≥1 Monthly",
                                             if_else(cnt_freq == 4L, "≥1 Yearly", 
                                                     if_else(cnt_freq == 5L, "First time", "Refuse"))))),

  cnt_dur = if_else(cnt_dur == 1L, "5-15 mins",
                            if_else(cnt_dur == 2L, ">15-60 mins",
                                    if_else(cnt_dur == 3L, ">1-2 hrs",
                                            if_else(cnt_dur == 4L, ">2-4 hrs", ">4 hrs")))))

#===========================================================================

# load participants dataset 
spatial1 <- read_csv(here::here("data", "participant.csv"))
spatial1 <- select(rename(spatial1, 
                          "part_id" = pid, "part_sex" = s4_10, "dob" = dob, "age" = s4_6, "agescale" = s4_7, "agedeter" = s4_8, "cnt_type" = s6_1_4, 
                          "eplon" = epal_location_longitude, "eplat" = epal_location_latitude), 
                   part_id, part_sex, dob, age, agescale, agedeter, cnt_type, eplon, eplat)

spatial1 <- select(spatial1 %>% mutate(age = if_else(agescale == 1, age/12, age), part_age = if_else(is.na(dob), age, dob), hhid = str_sub(part_id, 1, 6)),
                   part_id, hhid, part_sex, part_age, cnt_type, eplon, eplat)

spatial1 <- spatial1 %>% filter(!is.na(part_id))

# load household dataset 
spatial2 <- read_csv(here::here("data", "household.csv"))
spatial2 <- filter(select(rename(spatial2, "hhid" = s2_1, "hh_lon" = s1_5c, "hh_lat" = s1_6c), hhid, hh_lon, hh_lat), !is.na(hhid))
spatial2 <- distinct(spatial2, hhid, .keep_all = TRUE)

# merge household dataset and participant data
spatial <- filter(left_join(spatial1, spatial2, by = "hhid"), !is.na(eplon), !is.na(eplat), !is.na(hh_lon), !is.na(hh_lat))
rm(spatial1, spatial2)

# calculate spatial distance between two coordinates
get_geo_distance = function(long1, lat1, long2, lat2) {
  
  longlat1 = map2(long1, lat1, function(x,y) c(x,y))
  longlat2 = map2(long2, lat2, function(x,y) c(x,y))
  
  distance_list = map2(longlat1, longlat2, function(x,y) distHaversine(x, y))
  distance_m = sapply(distance_list, function(col) { col[1] })
  distance = distance_m
  distance
}

spatial$cnt_dist <- get_geo_distance(spatial$hh_lon, spatial$hh_lat, spatial$eplon, spatial$eplat)
spatial <- spatial %>% select(part_id, hhid, part_sex, part_age, cnt_type, cnt_dist)

# load scale dataset to extract HIV status of participant
scale.hivQ <- select(read_csv(here::here("data", "scaleNdix.csv")), hh_id, ind_id, hiv)
connecta.Q <- rename(select(hh.unlabel, scale_hhid,  scale_pid,  hhid), "hh_id" = scale_hhid, "ind_id" = scale_pid)

#merge datasets
X <- left_join(connecta.Q, scale.hivQ)
spatialhiv <- spatial %>% filter(str_sub(part_id, -1,-1) == 1) %>% left_join(scale.hivQ %>% select(hhid, hiv))

