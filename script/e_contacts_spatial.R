#Last edited by - Deus Thindwa
#Date - 28/10/2019

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
  ylab(bquote('Proportion '*delta~' or further away from home')) +
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
