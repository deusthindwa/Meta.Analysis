#Last edited by - Deus Thindwa
#Date - 28/10/2019

#===========================================================================

# mixing types
A <- filter(cn.unlabel, !is.na(cnt_type) & !is.na(cnt_dist)) %>% 
  group_by(cnt_type) %>%
  mutate(cnt_type = if_else(cnt_type == 1, "Physical", "Non-physical"),
         dist_sum = paste0(format(round(median(cnt_dist),1), nsmall=1),"m", ", ", 
                          format(round(quantile(cnt_dist,prob=0.25),1), nsmall=1),"-", 
                          format(round(quantile(cnt_dist, prob=0.75),1), nsmall=1)),
         category = paste0(cnt_type, " (", dist_sum, ")")) %>%
  
  ggplot(aes(x = cnt_dist, linetype = category)) + 
  scale_linetype_manual(values = c(1,3)) +
  geom_step(aes(y = 1 - ..y..), stat='ecdf', size = 0.8, color = "gray30") + 
  theme_bw() +
  labs(title = "A, Mixing type") +
  xlab(bquote('Distance in meters ('*delta~')')) +
  scale_x_continuous(trans = log10_trans()) +
  ylab(bquote('Proportion '*delta~' or further away from household')) +
  theme(plot.title = element_text(size = 16), axis.text.x = element_text(face = "bold", size = 11), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = c(0.32, 0.2), legend.text=element_text(size = 9), legend.title = element_text(size = 9)) + 
  guides(linetype=guide_legend(title="Mixing type (Median, IQR)"))

#===========================================================================

# physical contacts by age group 
B <- left_join(cn.unlabel, dplyr::select(pp.unlabel, somipa_pid, agey)) %>%
  filter(cnt_type == 1 & !is.na(agey) & !is.na(cnt_dist)) %>% 
  mutate(part_agegp = if_else(agey < 5, "<5y",
                              if_else(agey >= 5 & agey < 15, "5-14y",
                                      if_else(agey >= 15 & agey < 20, "15-19y", "20+y")))) %>%
  group_by(part_agegp) %>%
  mutate(dist_sum = paste0(format(round(median(cnt_dist),1), nsmall=1),"m", ", ", 
                           format(round(quantile(cnt_dist,prob=0.25),1), nsmall=1),"-", 
                           format(round(quantile(cnt_dist, prob=0.75),1), nsmall=1)),
         category = paste0(part_agegp, " (", dist_sum, ")")) %>%
  
  ggplot(aes(x = cnt_dist, color = factor(category, levels(factor(category))[c(1,4,2,3)]), linetype = category)) + 
  geom_step(aes(y = 1 - ..y..), stat='ecdf', size = 0.8) + 
  scale_linetype_manual(values = c(4,4,4,4)) +
  theme_bw() +
  labs(title = "B, Physical mixing") +
  xlab(bquote('Distance in meters ('*delta~')')) +
  scale_x_continuous(trans = log10_trans()) +
  ylab(bquote("")) +
  theme(plot.title = element_text(size = 16), axis.text.x = element_text(face = "bold", size = 11), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = c(0.27, 0.2), legend.text=element_text(size=9), legend.title = element_text(size = 9)) + 
  guides(color=guide_legend(title="Age group (Median, IQR)"), linetype=FALSE)

#===========================================================================

# non-physical contacts by age group
C <- left_join(cn.unlabel, dplyr::select(pp.unlabel, somipa_pid, agey)) %>%
  filter(cnt_type == 2 & !is.na(agey) & !is.na(cnt_dist)) %>% 
  mutate(part_agegp = if_else(agey < 5, "<5y",
                              if_else(agey >= 5 & agey < 15, "5-14y",
                                      if_else(agey >= 15 & agey < 20, "15-19y", "20+y")))) %>%
  group_by(part_agegp) %>%
  mutate(dist_sum = paste0(format(round(median(cnt_dist),1), nsmall=1),"m", ", ", 
                           format(round(quantile(cnt_dist,prob=0.25),1), nsmall=1),"-", 
                           format(round(quantile(cnt_dist, prob=0.75),1), nsmall=1)),
         category = paste0(part_agegp, " (", dist_sum, ")")) %>%
  
  ggplot(aes(x = cnt_dist, color = factor(category, levels(factor(category))[c(1,4,2,3)]))) + 
  geom_step(aes(y = 1 - ..y..), stat='ecdf', size = 0.8) + 
  theme_bw() +
  labs(title = "C, Non-physical mixing") +
  xlab(bquote('Distance in meters ('*delta~')')) +
  scale_x_continuous(trans = log10_trans()) +
  ylab(bquote("")) +
  theme(plot.title = element_text(size = 16), axis.text.x = element_text(face = "bold", size = 11), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = c(0.26, 0.2), legend.text=element_text(size=9), legend.title = element_text(size = 9)) + 
  guides(color=guide_legend(title="Age group (Median, IQR)"))

#===========================================================================

ggsave(here("output", "Fig3_spatially_contacts.png"),
       plot = (A | B | C),
       width = 15, height = 5, unit="in", dpi = 300)
