#Last edited by - Deus Thindwa
#Date - 28/10/2019

#===========================================================================

# mixing types
A <- filter(cn.unlabel, !is.na(cnt_type), !is.na(cnt_dist)) %>% 
  group_by(cnt_type) %>%
  mutate(cnt_type = if_else(cnt_type == 1, "Physical mixing", "Non-physical mixing"),
         dist_sum = paste0(format(round(median(cnt_dist),1), nsmall=1),"m", ", ", 
                          format(round(quantile(cnt_dist,prob=0.25),1), nsmall=1),"-", 
                          format(round(quantile(cnt_dist, prob=0.75),1), nsmall=1)),
         category = paste0(cnt_type, " (", dist_sum, ")")) %>%
  
  ggplot(aes(x = cnt_dist, linetype = category)) + 
  geom_step(aes(y = 1 - ..y..), stat='ecdf', size = 0.5) + 
  theme_bw() +
  labs(title = "A, Mixing type") +
  xlab(bquote('Distance in meters ('*delta~')')) +
  coord_cartesian(xlim = c(0,5000)) +
  ylab(bquote('Proportion '*delta~' or further away from home')) +
  theme(plot.title = element_text(size = 16), axis.text.x = element_text(face = "bold", size = 11), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = c(0.6, 0.8)) + 
  guides(linetype=guide_legend(title="mixing type (median distance, IQR)"))

#===========================================================================
# physical contacts by age group
B <- left_join(cn.unlabel, dplyr::select(pp.unlabel, somipa_pid, agey)) %>%
  filter(!is.na(agey), !is.na(cnt_dist)) %>% 
  mutate(part_agegp = if_else(agey < 1, "<1y",
                              if_else(agey >= 1 & agey < 5, "1-4y",
                                      if_else(agey >= 5 & agey < 15, "5-14y",
                                              if_else(agey >= 15 & agey < 20, "15-19y",
                                                      if_else(agey >= 20 & agey < 50, "20-49y", "50+y")))))) %>%
  group_by(part_agegp) %>%
  mutate(dist_sum = paste0(format(round(median(cnt_dist),1), nsmall=1),"m", ", ", 
                           format(round(quantile(cnt_dist,prob=0.25),1), nsmall=1),"-", 
                           format(round(quantile(cnt_dist, prob=0.75),1), nsmall=1)),
         category = paste0(part_agegp, " (", dist_sum, ")")) %>%
  
  ggplot(aes(x = cnt_dist, color = factor(category, levels(factor(category))[c(1,2,5,3,4,6)]))) + 
  geom_step(aes(y = 1 - ..y..), stat='ecdf', size = 0.5) + 
  theme_bw() +
  labs(title = "B, All mixing events") +
  xlab(bquote('Distance in meters ('*delta~')')) +
  coord_cartesian(xlim = c(0,5000)) +
  ylab(bquote("")) +
  theme(plot.title = element_text(size = 16), axis.text.x = element_text(face = "bold", size = 11), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = c(0.6, 0.65)) + 
  guides(color=guide_legend(title="Age group (median distance, IQR)"))

#===========================================================================

# non-physical contacts by age group
C <- left_join(cn.unlabel, dplyr::select(pp.unlabel, somipa_pid, agey)) %>%
  filter(cnt_type == 1 & !is.na(agey), !is.na(cnt_dist)) %>% 
  mutate(part_agegp = if_else(agey < 1, "<1y",
                              if_else(agey >= 1 & agey < 5, "1-4y",
                                      if_else(agey >= 5 & agey < 15, "5-14y",
                                              if_else(agey >= 15 & agey < 20, "15-19y",
                                                      if_else(agey >= 20 & agey < 50, "20-49y", "50+y")))))) %>%
  group_by(part_agegp) %>%
  mutate(dist_sum = paste0(format(round(median(cnt_dist),1), nsmall=1),"m", ", ", 
                           format(round(quantile(cnt_dist,prob=0.25),1), nsmall=1),"-", 
                           format(round(quantile(cnt_dist, prob=0.75),1), nsmall=1)),
         category = paste0(part_agegp, " (", dist_sum, ")")) %>%
  
  ggplot(aes(x = cnt_dist, color = factor(category, levels(factor(category))[c(1,2,5,3,4,6)]))) + 
  geom_step(aes(y = 1 - ..y..), stat='ecdf', size = 0.5) + 
  theme_bw() +
  labs(title = "C, physical mixing events") +
  xlab(bquote('Distance in meters ('*delta~')')) +
  coord_cartesian(xlim = c(0,5000)) +
  ylab(bquote("")) +
  theme(plot.title = element_text(size = 16), axis.text.x = element_text(face = "bold", size = 11), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = c(0.6, 0.65)) + 
  guides(color=guide_legend(title="Age group (median distance, IQR)"))

#===========================================================================

(A | B | C)

ggsave(here("output", "Fig3_spatially_contacts.png"),
       plot = (A | B | C),
       width = 14, height = 4, unit="in", dpi = 300)
