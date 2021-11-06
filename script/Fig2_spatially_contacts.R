#Last edited by - Deus Thindwa
#Date - 28/10/2019

#===========================================================================

# mixing types
G <- filter(cn.unlabel, !is.na(cnt_type) & !is.na(cnt_dist)) %>% 
    group_by(cnt_type) %>%
    mutate(cnt_type = if_else(cnt_type == 1, "Physical", "Non-physical"),
           cnt_distx = if_else(cnt_dist >= 30, cnt_dist, NA_real_),
           dist_sum = paste0(format(round(median(cnt_distx, na.rm = TRUE),1), nsmall = 1), "m", ", ", 
                             format(round(quantile(cnt_distx,prob = 0.25, na.rm = TRUE), 1), nsmall = 1),"-", 
                             format(round(quantile(cnt_distx, prob = 0.75, na.rm = TRUE), 1), nsmall = 1)),
           category = paste0(cnt_type, " (", dist_sum, ")")) %>%
  
  ggplot(aes(x = cnt_dist, linetype = category)) + 
  scale_linetype_manual(values = c(1,3)) +
  geom_step(aes(y = 1 - ..y..), stat='ecdf', size = 0.8, color = "gray30") + 
  theme_bw() +
  labs(title = "G, Mixing type") +
  xlab(bquote('Distance in meters ('*delta~')')) +
  scale_x_continuous(trans = log10_trans()) +
  coord_cartesian(xlim=c(30,12360)) +
  ylab(bquote('Proportion of contacts further away from home')) +
  theme(plot.title = element_text(size = 22), axis.text.x = element_text(face = "bold", size = 11), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = c(0.6, 0.6), legend.text=element_text(size = 9), legend.title = element_text(size = 9)) + 
  guides(linetype=guide_legend(title="Mixing type (Median, IQR)\n \n        Overall (121.6m, 57.2-369.5)"))

#===========================================================================

# physical contacts by age group 
H <- left_join(cn.unlabel, dplyr::select(pp.unlabel, somipa_pid, agey)) %>%
  filter(cnt_type == 1 & !is.na(agey) & !is.na(cnt_dist)) %>% 
  mutate(part_agegp = if_else(agey < 5, "<5y",
                              if_else(agey >= 5 & agey < 15, "5-14y",
                                      if_else(agey >= 15 & agey < 20, "15-19y", "20+y")))) %>%
  group_by(part_agegp) %>%
  mutate(cnt_distx = if_else(cnt_dist>=30, cnt_dist, NA_real_),
         dist_sum = paste0(format(round(median(cnt_distx, na.rm = TRUE),1), nsmall=1),"m", ", ", 
                           format(round(quantile(cnt_distx,prob=0.25, na.rm = TRUE),1), nsmall=1),"-", 
                           format(round(quantile(cnt_distx, prob=0.75, na.rm = TRUE),1), nsmall=1)),
         category = paste0(part_agegp, " (", dist_sum, ")")) %>%
  
  ggplot(aes(x = cnt_dist, color = factor(category, levels(factor(category))[c(1,4,2,3)]), linetype = category)) + 
  geom_step(aes(y = 1 - ..y..), stat='ecdf', size = 0.8) + 
  scale_linetype_manual(values = c(4,4,4,4)) +
  theme_bw() +
  labs(title = "H, Physical mixing") +
  xlab(bquote('Distance in meters ('*delta~')')) +
  scale_x_continuous(trans = log10_trans()) +
  coord_cartesian(xlim=c(30,12360)) +
  ylab(bquote("")) +
  theme(plot.title = element_text(size = 22), axis.text.x = element_text(face = "bold", size = 11), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = c(0.6, 0.6), legend.text=element_text(size=9), legend.title = element_text(size = 9)) + 
  guides(color=guide_legend(title="Age group (Median, IQR)"), linetype=FALSE)

#===========================================================================

# non-physical contacts by age group
I <- left_join(cn.unlabel, dplyr::select(pp.unlabel, somipa_pid, agey)) %>%
  filter(cnt_type == 2 & !is.na(agey) & !is.na(cnt_dist)) %>% 
  mutate(part_agegp = if_else(agey < 5, "<5y",
                              if_else(agey >= 5 & agey < 15, "5-14y",
                                      if_else(agey >= 15 & agey < 20, "15-19y", "20+y")))) %>%
  group_by(part_agegp) %>%
  mutate(cnt_distx = if_else(cnt_dist >= 30, cnt_dist, NA_real_),
         dist_sum = paste0(format(round(median(cnt_distx, na.rm = TRUE),1), nsmall = 1), "m", ", ", 
                           format(round(quantile(cnt_distx,prob=0.25, na.rm =TRUE), 1), nsmall = 1),"-", 
                           format(round(quantile(cnt_distx, prob=0.75, na.rm = TRUE), 1), nsmall = 1)),
         category = paste0(part_agegp, " (", dist_sum, ")")) %>%
  
  ggplot(aes(x = cnt_dist, color = factor(category, levels(factor(category))[c(1,4,2,3)]))) + 
  geom_step(aes(y = 1 - ..y..), stat='ecdf', size = 0.8) + 
  theme_bw() +
  labs(title = "I, Non-physical mixing") +
  xlab(bquote('Distance in meters ('*delta~')')) +
  scale_x_continuous(trans = log10_trans()) +
  coord_cartesian(xlim=c(30,12360)) +
  ylab(bquote("")) +
  theme(plot.title = element_text(size = 22), axis.text.x = element_text(face = "bold", size = 11), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = c(0.6, 0.6), legend.text=element_text(size=9), legend.title = element_text(size = 9)) + 
  guides(color=guide_legend(title="Age group (Median, IQR)"))

#===========================================================================

ggsave(here("output", "Fig3_spatially_contacts.png"),
       plot = (A | B | C),
       width = 15, height = 5, unit="in", dpi = 300)
