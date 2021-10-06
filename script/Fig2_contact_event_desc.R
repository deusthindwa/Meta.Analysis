# Last edited by - Deus Thindwa
# Date - 28/10/2019

#===========================================================================

# duration of contacts
A <- filter(cn.labeled, !is.na(cnt_dur) & !is.na(cnt_type)) %>% group_by(cnt_dur, cnt_type) %>% tally() %>% mutate(cnt_dur_p = n/sum(n)) %>%
ggplot(mapping = aes(x = factor(cnt_dur,levels(factor(cnt_dur))[c(5,2,1,3,4)]), y = cnt_dur_p, color = cnt_type, fill = cnt_type)) + 
  geom_bar(stat = "identity", color = "black", size = 0.7) +
  geom_text(aes(label = n), color = "black", position = position_stack(vjust = 0.5)) +
  scale_fill_brewer() +
  theme_bw() +
  labs(title = "A", x = "Mixing duration", y = "Proportion of mixing events") +
  scale_y_continuous(breaks = seq(0, 1, 0.2), labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = "none")

#===========================================================================

# frequency of contacts
B <- filter(cn.labeled, !is.na(cnt_freq) & !is.na(cnt_type)) %>% group_by(cnt_freq, cnt_type) %>% tally() %>% mutate(cnt_freq_p = n/sum(n)) %>%
  ggplot(mapping = aes(x = factor(cnt_freq, levels(factor(cnt_freq))[c(4,2,1,3,5)]), y = cnt_freq_p, color = cnt_type, fill = cnt_type)) + 
  geom_bar(stat = "identity", color = "black", size = 0.7) +
  geom_text(aes(label = n), color = "black", position = position_stack(vjust = 0.5)) +
  scale_fill_brewer() +
  theme_bw() +
  labs(title = "B", x = "Mixing frequency", y = "") +
  scale_y_continuous(breaks = seq(0, 1, 0.2), labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = "none")

#===========================================================================

# relationship to contacts
C <- filter(cn.labeled, !is.na(cnt_rel) & !is.na(cnt_type)) %>% group_by(cnt_rel, cnt_type) %>% tally() %>% mutate(cnt_rel_p = n/sum(n)) %>%
  ggplot(mapping = aes(x = factor(cnt_rel, levels(factor(cnt_rel))[c(3,2,4,1,5)]), y = cnt_rel_p, color = cnt_type, fill = cnt_type)) + 
  geom_bar(stat = "identity", color = "black", size = 0.7) +
  geom_text(aes(label = n), color = "black", position = position_stack(vjust = 0.5)) +
  scale_fill_brewer() +
  theme_bw() +
  labs(title = "C", x = "Relationship to contactee", y = "") +
  scale_y_continuous(breaks = seq(0, 1, 0.2), labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  guides(fill=guide_legend(title="Mixing type")) +
  theme(legend.position = "right", legend.text=element_text(size=10, face = "bold"))

#===========================================================================

# location of contact events
D <- cn.labeled %>% 
  filter(!is.na(cnt_loc) & !is.na(cnt_type)) %>% 
  group_by(cnt_loc, cnt_type) %>% 
  tally() %>% 
  mutate(cnt_loc_p = n/sum(n)) %>% 
  ggplot(mapping = aes(x = factor(cnt_loc, levels(factor(cnt_loc))[c(1,7,8,2,3,6,5,10,4,9)]), y = cnt_loc_p, color = cnt_type, fill = cnt_type)) + 
  geom_bar(stat = "identity", color = "black", size = 0.7) + 
  geom_text(aes(label = n), color = "black", position = position_stack(vjust = 0.5)) +
  scale_fill_brewer() +
  theme_bw() + 
  labs(title = "D", x = "Location of mixing events", y = "Proportion of mixing events") + 
  scale_y_continuous(breaks = seq(0, 1, 0.2), labels = scales::percent_format(accuracy = 1)) + 
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  guides(fill=guide_legend(title="Mixing type")) +
  theme(legend.position = "none")

#===========================================================================

# place of contact events
E <- cn.labeled %>% 
  filter(!is.na(cnt_plc) & !is.na(cnt_type)) %>% 
  group_by(cnt_plc, cnt_type) %>% 
  tally() %>% 
  mutate(cnt_plc_p = n/sum(n)) %>% 
  ggplot(mapping = aes(x = factor(cnt_plc, levels(factor(cnt_plc))[c(2,1)]), y = cnt_plc_p, color = cnt_type, fill = cnt_type)) + 
  geom_bar(stat = "identity", color = "black", size = 0.7) + 
  geom_text(aes(label = n), color = "black", position = position_stack(vjust = 0.5)) +
  scale_fill_brewer() +
  theme_bw() + 
  labs(title = "E", x = "Community boundary", y = "") + 
  scale_y_continuous(breaks = seq(0, 1, 0.2), labels = scales::percent_format(accuracy = 1)) + 
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 0, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  guides(fill=guide_legend(title="Mixing type")) +
  theme(legend.position = "none")

#===========================================================================

# participant sex
F <- cn.labeled %>% 
  filter(!is.na(cnt_sex) & !is.na(cnt_type)) %>% 
  group_by(cnt_sex, cnt_type) %>% 
  tally() %>% 
  mutate(cnt_sex_p = n/sum(n)) %>% 
  ggplot(mapping = aes(x = cnt_sex, y = cnt_sex_p, color = cnt_type, fill = cnt_type)) + 
  geom_bar(stat = "identity", color = "black", size = 0.7) + 
  geom_text(aes(label = n), color = "black", position = position_stack(vjust = 0.5)) +
  scale_fill_brewer() +
  theme_bw() + 
  labs(title = "F", x = "Contactee sex", y = "") + 
  scale_y_continuous(breaks = seq(0, 1, 0.2), labels = scales::percent_format(accuracy = 1)) + 
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 0, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = "none")

#===========================================================================

ggsave(here("output", "Fig2_contact_event_desc.png"),
       plot = (A | B | C) / (D + E + F + plot_layout(ncol=3, widths=c(3,1,1))),
       width = 15, height = 9, unit="in", dpi = 300)
