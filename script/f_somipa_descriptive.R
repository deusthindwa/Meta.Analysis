# Last edited by - Deus Thindwa
# Date - 28/10/2019

#===========================================================================

# data manipulation
contacts$cnt_type <- if_else(contacts$phys_contact == 1L, "Physical", "Non-physical")

contacts$cnt_dur <- if_else(contacts$duration_multi >= 0L & contacts$duration_multi <= 1L, "5-15 mins",
                            if_else(contacts$duration_multi == 2L, "15-59 mins",
                                    if_else(contacts$duration_multi == 3L, "1-2 hrs",
                                            if_else(contacts$duration_multi == 4L, ">2-4 hrs", ">4 hrs"))))


contacts$cnt_freq <- if_else(contacts$frequency_multi >= 0L & contacts$frequency_multi <= 1L, "Daily",
                            if_else(contacts$frequency_multi == 2L, ">=1/week",
                                    if_else(contacts$frequency_multi == 3L, ">=1/month",
                                            if_else(contacts$frequency_multi == 4L, "<1/month", "Never"))))

contacts$cnt_rel <- if_else(contacts$frequency_multi >= 0L & contacts$frequency_multi <= 1L, "House\nmember",
                             if_else(contacts$frequency_multi == 2L, "Other\nrelative",
                                     if_else(contacts$frequency_multi == 3L, "Coworker",
                                             if_else(contacts$frequency_multi == 4L, "Friend", "Other"))))

#===========================================================================

# duration of contacts
A <- filter(contacts, !is.na(cnt_dur) & !is.na(cnt_type)) %>% group_by(cnt_dur, cnt_type) %>% tally() %>% mutate(cnt_dur_p = n/sum(n)) %>%
ggplot(mapping = aes(x = factor(cnt_dur,levels(factor(cnt_dur))[c(5,4,3,1,2)]), y = cnt_dur_p, color = cnt_type, fill = cnt_type)) + 
  geom_bar(stat = "identity", color = "black", size = 0.7) +
  theme_bw() +
  labs(title = "A", x = "Mixing duration", y = "Proportion of mixing events") +
  scale_y_continuous(breaks = seq(0, 1, 0.2), labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = "none")

#===========================================================================

# frequency of contacts
B <- filter(contacts, !is.na(cnt_freq) & !is.na(cnt_type)) %>% group_by(cnt_freq, cnt_type) %>% tally() %>% mutate(cnt_freq_p = n/sum(n)) %>%
  ggplot(mapping = aes(x = factor(cnt_freq, levels(factor(cnt_freq))[c(4,3,2,1,5)]), y = cnt_freq_p, color = cnt_type, fill = cnt_type)) + 
  geom_bar(stat = "identity", color = "black", size = 0.7) +
  theme_bw() +
  labs(title = "B", x = "Mixing frequency", y = "") +
  scale_y_continuous(breaks = seq(0, 1, 0.2), labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = "none")

#===========================================================================

# relationship to contacts
C <- filter(contacts, !is.na(cnt_rel) & !is.na(cnt_type)) %>% group_by(cnt_rel, cnt_type) %>% tally() %>% mutate(cnt_rel_p = n/sum(n)) %>%
  ggplot(mapping = aes(x = factor(cnt_rel, levels(factor(cnt_rel))[c(3,2,5,1,4)]), y = cnt_rel_p, color = cnt_type, fill = cnt_type)) + 
  geom_bar(stat = "identity", color = "black", size = 0.7) +
  theme_bw() +
  labs(title = "C", x = "Relationship to contact", y = "") +
  scale_y_continuous(breaks = seq(0, 1, 0.2), labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = "none")

#===========================================================================

# location of contacts
D <- contacts %>% 
  rename("Home" = "cnt_home", "Work" = "cnt_work", "School" = "cnt_school", "Transport" = "cnt_transport", "Leisure" = "cnt_leisure", "Other" = "cnt_otherplace") %>%
  gather("Home", "Work", "School", "Transport", "Leisure", "Other", key = cnt_loc, value = cnt_n) %>% 
  filter(cnt_n ==1 & !is.na(cnt_loc) & !is.na(cnt_type)) %>% 
  group_by(cnt_loc, cnt_type) %>% 
  tally() %>% 
  mutate(cnt_loc_p = n/sum(n)) %>% 
  ggplot(mapping = aes(x = factor(cnt_loc, levels(factor(cnt_loc))[c(1,4,6,5,2,3)]), y = cnt_loc_p, color = cnt_type, fill = cnt_type)) + 
  geom_bar(stat = "identity", color = "black", size = 0.7) + 
  theme_bw() + 
  labs(title = "D", x = "Location", y = "Proportion of contact events") + 
  scale_y_continuous(breaks = seq(0, 1, 0.2), labels = scales::percent_format(accuracy = 1)) + 
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  guides(fill=guide_legend(title="Mixing type")) +
  theme(legend.position = "right")

#===========================================================================

(A | B | C) / D

ggsave(here("output", "Fig3_descriptive.tiff"),
       plot = (A | B | C) / D,
       width = 12, height = 7, unit="in", dpi = 200)
