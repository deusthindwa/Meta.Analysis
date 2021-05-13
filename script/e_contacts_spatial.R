#Last edited by - Deus Thindwa
#Date - 28/10/2019

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

#===========================================================================

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

#===========================================================================

# load scale dataset to extract HIV status of participant
scalehiv <- select(scale, hh_id, ind_id, hiv)
connecta <- rename(select(hh1.enrol, scale_hhid,  scale_pid,  hhid), "hh_id" = scale_hhid, "ind_id" = scale_pid)

#merge datasets
scalehiv <- left_join(connecta, scalehiv)
spatialhiv <- spatial %>% filter(str_sub(part_id, -1,-1) == 1) %>% left_join(scalehiv %>% select(hhid, hiv))

#===========================================================================

# mixing events
A <- spatial %>%
  filter(!is.na(part_age), !is.na(cnt_dist)) %>% 
  mutate(part_agegp = if_else(part_age < 1, "<1y",
                              if_else(part_age >= 1 & part_age < 5, "1-4y",
                                      if_else(part_age >= 5 & part_age < 15, "5-14y",
                                              if_else(part_age >= 15 & part_age < 20, "15-19y",
                                                      if_else(part_age >= 20 & part_age < 50, "20-49y", "50+y")))))) %>%
  ggplot(aes(x = cnt_dist, color = factor(part_agegp, levels(factor(part_agegp))[c(1,2,5,3,4,6)]))) + 
  geom_step(aes(y = 1 - ..y..), stat='ecdf', size = 0.8) + 
  theme_bw() +
  labs(title = "A, all mixing types") +
  xlab(bquote('Distance in meters ('*delta~')')) +
  coord_cartesian(xlim = c(0,3000)) +
  ylab(bquote("")) +
  theme(plot.title = element_text(size = 16), axis.text.x = element_text(face = "bold", size = 11), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = c(0.6, 0.65)) + 
  guides(color=guide_legend(title="Age group"))

#===========================================================================

# physical contacts by age group
B <- spatial %>%
  filter(cnt_type == 1 & !is.na(part_age), !is.na(cnt_dist)) %>% 
  mutate(part_agegp = if_else(part_age < 1, "<1y",
                              if_else(part_age >= 1 & part_age < 5, "1-4y",
                                      if_else(part_age >= 5 & part_age < 15, "5-14y",
                                              if_else(part_age >= 15 & part_age < 20, "15-19y",
                                                      if_else(part_age >= 20 & part_age < 50, "20-49y", "50+y")))))) %>%
  ggplot(aes(x = cnt_dist, color = factor(part_agegp, levels(factor(part_agegp))[c(1,2,5,3,4,6)]))) + 
  geom_step(aes(y = 1 - ..y..), stat='ecdf', size = 0.8) + 
  theme_bw() +
  labs(title = "B, physical mixing") +
  xlab(bquote('Distance in meters ('*delta~')')) +
  coord_cartesian(xlim = c(0,3000)) +
  ylab(bquote("")) +
  theme(plot.title = element_text(size = 16), axis.text.x = element_text(face = "bold", size = 11), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = c(0.6, 0.65)) + 
  guides(color=guide_legend(title="Age group"))

#===========================================================================

# non-physical contacts by age group
C <- spatial %>% 
  filter(cnt_type == 2 & !is.na(part_age), !is.na(cnt_dist)) %>% 
  mutate(part_agegp = if_else(part_age < 1, "<1y",
                              if_else(part_age >= 1 & part_age < 5, "1-4y",
                                      if_else(part_age >= 5 & part_age < 15, "5-14y",
                                              if_else(part_age >= 15 & part_age < 20, "15-19y",
                                                      if_else(part_age >= 20 & part_age < 50, "20-49y", "50+y")))))) %>%
  ggplot(aes(x = cnt_dist, color = factor(part_agegp, levels(factor(part_agegp))[c(1,2,5,3,4,6)]))) + 
  geom_step(aes(y = 1 - ..y..), stat='ecdf', size = 0.8) + 
  theme_bw() +
  labs(title = "C, non-physical mixing") +
  xlab(bquote('Distance in meters ('*delta~')')) +
  coord_cartesian(xlim = c(0,3000)) +
  ylab("") +
  theme(plot.title = element_text(size = 16), axis.text.x = element_text(face = "bold", size = 11), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = c(0.6, 0.65)) + 
  guides(color=guide_legend(title="Age group"))

#===========================================================================

# contacts
D <- filter(spatial, !is.na(cnt_type), !is.na(cnt_dist)) %>% 
  mutate(cnt_type = if_else(cnt_type == 1, "Physical mixing", "Non-physical mixing")) %>%
  ggplot(aes(x = cnt_dist, color = cnt_type)) + 
  geom_step(aes(y = 1 - ..y..), stat='ecdf', size = 0.8) + 
  theme_bw() +
  labs(title = "D, Mixing type") +
  xlab(bquote('Distance in meters ('*delta~')')) +
  coord_cartesian(xlim = c(0,3000)) +
  ylab(bquote('Proportion '*delta~' or further away from home')) +
  theme(plot.title = element_text(size = 16), axis.text.x = element_text(face = "bold", size = 11), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = c(0.6, 0.8)) + 
  guides(color=guide_legend(title="mixing type"))

#===========================================================================

# non-physical contacts by age group
E <- spatial %>% 
  filter(!is.na(part_sex), !is.na(cnt_dist)) %>% 
  mutate(part_sexgp = if_else(part_sex == 1, "Male", "Female")) %>%
  ggplot(aes(x = cnt_dist, color = part_sexgp)) + 
  geom_step(aes(y = 1 - ..y..), stat='ecdf', size = 0.8) + 
  theme_bw() +
  labs(title = "E, Sex") +
  xlab(bquote('Distance in meters ('*delta~')')) +
  coord_cartesian(xlim = c(0,3000)) +
  ylab("") +
  theme(plot.title = element_text(size = 16), axis.text.x = element_text(face = "bold", size = 11), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = c(0.6, 0.65)) + 
  guides(color=guide_legend(title="Sex"))

#===========================================================================

# non-physical contacts by age group
F <- spatialhiv %>% 
  filter(!is.na(hiv), !is.na(cnt_dist)) %>% 
  ggplot(aes(x = cnt_dist, color = hiv)) + 
  geom_step(aes(y = 1 - ..y..), stat='ecdf', size = 0.8) + 
  theme_bw() +
  labs(title = "F, HIV status") +
  xlab(bquote('Distance in meters ('*delta~')')) +
  coord_cartesian(xlim = c(0,3000)) +
  ylab("") +
  theme(plot.title = element_text(size = 16), axis.text.x = element_text(face = "bold", size = 11), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = c(0.6, 0.65)) + 
  guides(color=guide_legend(title="HIV status"))

#===========================================================================

(A | B | C) /(D | E | F)

ggsave(here("output", "Fig4_spatial.tiff"),
       plot = (A | B | C) /(D | E | F),
       width = 16, height = 8, unit="in", dpi = 200)
