#Last edited by - Deus Thindwa
#Date - 28/10/2019

#===========================================================================

# contacts
A <- filter(contacts, !is.na(phys_contact)) %>% 
  mutate(cnt_phys = if_else(phys_contact == 1, "Physical mixing", "Non-physical mixing")) %>%
  ggplot(aes(x = cnt_age_exact, color = cnt_phys)) + 
  geom_step(aes(y = 1 - ..y..), stat='ecdf', size = 0.8) + 
  theme_bw() +
  labs(title = "A, contacts") +
  xlab(bquote('Distance in kilometers ('*delta~')')) +
  ylab(bquote('Proportion '*delta~' away from household')) +
  theme(plot.title = element_text(size = 16), axis.text.x = element_text(face = "bold", size = 11), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = c(0.7, 0.8)) + 
  guides(color=guide_legend(title="Contact type"))

#===========================================================================

# mixing events
B <- filter(contacts, !is.na(frequency_multi) & frequency_multi >= 1 & !is.na(phys_contact)) %>% 
  slice(rep(1:n(), frequency_multi)) %>% 
  mutate(cnt_phys = if_else(phys_contact == 1, "Physical mixing", "Non-physical mixing")) %>%
  ggplot(aes(x = cnt_age_exact, color = cnt_phys)) + 
  geom_step(aes(y = 1 - ..y..), stat='ecdf', size = 0.8) + 
  theme_bw() +
  labs(title = "B, mixing events") +
  xlab(bquote('Distance in kilometers ('*delta~')')) + 
  ylab("") + 
  theme(plot.title = element_text(size = 16), axis.text.x = element_text(face = "bold", size = 11), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = c(0.7, 0.8)) + 
  guides(color=guide_legend(title="Contact type"))

#===========================================================================

# physical contacts by age group
C <- right_join(participants, contacts) %>% 
  filter(phys_contact == 1 & !is.na(part_age)) %>% 
  mutate(part_agegp = if_else(part_age < 1, "<1y",
                              if_else(part_age >= 1 & part_age < 5, "1-4y",
                                      if_else(part_age >= 5 & part_age < 15, "5-14y",
                                              if_else(part_age >= 15 & part_age < 20, "15-19y",
                                                      if_else(part_age >= 20 & part_age < 50, "20-49y", "50+y")))))) %>%
  ggplot(aes(x = cnt_age_exact, color = factor(part_agegp, levels(factor(part_agegp))[c(1,2,5,3,4,6)]))) + 
  geom_step(aes(y = 1 - ..y..), stat='ecdf', size = 0.8) + 
  theme_bw() +
  labs(title = "C, physical mixing") +
  xlab(bquote('Distance in kilometers ('*delta~')')) +
  ylab(bquote("")) +
  theme(plot.title = element_text(size = 16), axis.text.x = element_text(face = "bold", size = 11), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = c(0.8, 0.65)) + 
  guides(color=guide_legend(title="Age group"))

#===========================================================================

# non-physical contacts by age group
D <- right_join(participants, contacts) %>% 
  filter(phys_contact == 2 & !is.na(part_age)) %>% 
  mutate(part_agegp = if_else(part_age < 1, "<1y",
                              if_else(part_age >= 1 & part_age < 5, "1-4y",
                                      if_else(part_age >= 5 & part_age < 15, "5-14y",
                                              if_else(part_age >= 15 & part_age < 20, "15-19y",
                                                      if_else(part_age >= 20 & part_age < 50, "20-49y", "50+y")))))) %>%
  ggplot(aes(x = cnt_age_exact, color = factor(part_agegp, levels(factor(part_agegp))[c(1,2,5,3,4,6)]))) + 
  geom_step(aes(y = 1 - ..y..), stat='ecdf', size = 0.8) + 
  theme_bw() +
  labs(title = "D, non-physical mixing") +
  xlab(bquote('Distance in kilometers ('*delta~')')) +
  ylab("") +
  theme(plot.title = element_text(size = 16), axis.text.x = element_text(face = "bold", size = 11), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = c(0.8, 0.65)) + 
  guides(color=guide_legend(title="Age group"))

#===========================================================================

ggarrange((A | B | C | D), nrow=1, common.legend = FALSE)

ggsave(here("output", "Fig4_spatial.tiff"),
       plot = (A | B | C | D),
       width = 14, height = 4, unit="in", dpi = 200)
