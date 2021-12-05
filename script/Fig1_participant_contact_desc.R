# Last edited by - Deus Thindwa
# Date - 28/10/2019

#suppress warnings
defaultW <- getOption("warn") 
options(warn = -1)

#===========================================================================
# distribution of participants by age
A <- pp.labeled %>%  
  ggplot() + 
  geom_histogram(aes(x = agey), bins = 92, color = "black", fill =  brocolors("crayons")["Goldenrod"]) +
  geom_density(aes(x = agey, y = ..density../0.001), alpha = 0.3, size = 1) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 87, 10)) +
  scale_y_continuous("Number of participants", sec.axis = sec_axis(~. * 0.001, name = "Probability density"), limits = c(0, 70)) + 
  labs(title = "A", x = "Participant age (years)") +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 0, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 12), axis.title.y = element_text(face = "bold", size = 12)) +
  theme(legend.position = "none")

#===========================================================================

# distribution of participant age
X <- pp.labeled %>% 
  mutate(agegp = if_else(agey < 1, "<1y", if_else(agey >= 1 & agey < 5, "1-4y", 
                                                  if_else(agey >= 5 & agey < 15, "5-14y", 
                                                          if_else(agey >= 15 & agey < 20, "15-19y", 
                                                                  if_else(agey >= 20 & agey < 50, "20-49y", "50+y"))))))  %>% 
  group_by(agegp) %>% tally() %>% 
  mutate(perc = n/sum(n), lci = exactci(n, sum(n), 0.95)$conf.int[1:6], uci = exactci(n, sum(n), 0.95)$conf.int[7:12]) %>%
  
ggplot(aes(x = factor(agegp, levels(factor(agegp))[c(1, 2, 5, 3, 4, 6)]), y = perc)) + 
  geom_bar(stat = "identity", color = "black", size = 1, fill =  brocolors("crayons")["Goldenrod"]) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.3, position = position_dodge(0.9), size = 1.3) +
  geom_text(aes(label = n), color = "black", position = position_stack(vjust = 0.5), size = 4) +
  scale_y_continuous(breaks = seq(0, 0.4, 0.05), labels = scales::percent_format(accuracy = 1)) + 
  theme_bw() +
  labs(title = "", x = "Participant age (years)", y = "Proportion of participant") +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 12), axis.title.y = element_text(face = "bold", size = 12)) +
  theme(legend.position = "none")

#===========================================================================

B <- cn.labeled %>%  
  ggplot() + 
  geom_histogram(aes(x = cnt_age), bins = 92, color = "black", fill =  brocolors("crayons")["Violet (Purple)"]) +
  geom_density(aes(x = cnt_age, y = ..density../0.0001), alpha = 0.3, size = 1) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 87, 10)) +
  scale_y_continuous("Number of contacts", sec.axis = sec_axis(~. * 0.0001, name = "Probability density"), limits = c(0, 1050)) + 
  labs(title = "B", x = "Contact age (years)") +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 0, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 12), axis.title.y = element_text(face = "bold", size = 12)) +
  theme(legend.position = "none")

#===========================================================================

# distribution of participant age
Y <- cn.labeled %>% 
  mutate(agegp = if_else(cnt_age < 1, "<1y", if_else(cnt_age >= 1 & cnt_age < 5, "1-4y", 
                                                  if_else(cnt_age >= 5 & cnt_age < 15, "5-14y", 
                                                          if_else(cnt_age >= 15 & cnt_age < 20, "15-19y", 
                                                                  if_else(cnt_age >= 20 & cnt_age < 50, "20-49y", "50+y"))))))  %>% 
  group_by(agegp) %>% tally() %>% 
  mutate(perc = n/sum(n), lci = exactci(n, sum(n), 0.95)$conf.int[1:6], uci = exactci(n, sum(n), 0.95)$conf.int[7:12]) %>%
  
  ggplot(aes(x = factor(agegp, levels(factor(agegp))[c(1, 2, 5, 3, 4, 6)]), y = perc)) + 
  geom_bar(stat = "identity", color = "black", size = 1, fill = brocolors("crayons")["Violet (Purple)"]) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.3, position = position_dodge(0.9), size = 1.3) +
  geom_text(aes(label = n), color = "black", position = position_stack(vjust = 0.5), size = 4) +
  scale_y_continuous(breaks = seq(0, 0.4, 0.05), labels = scales::percent_format(accuracy = 1)) + 
  theme_bw() +
  labs(title = "", x = "Contact age (years)", y = "Proportion of contacts") +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 12), axis.title.y = element_text(face = "bold", size = 12)) +
  theme(legend.position = "none")


#===========================================================================

#proportion of contacts by weekday

C <- left_join(pp.labeled %>% mutate(datex = dmy(str_sub(date, 1, 10)), dow = weekdays(datex-1)) %>%  group_by(dow) %>% tally() %>% 
  mutate(perc = n/sum(n), lci = exactci(n, sum(n), 0.95)$conf.int[1:7], uci = exactci(n, sum(n), 0.95)$conf.int[8:14]),
cn.labeled %>% mutate(datex = dmy(str_sub(date, 1, 10)), dow = weekdays(datex-1)) %>%  group_by(dow) %>% tally() %>% rename("n2"="n")) %>%
  
  ggplot(aes(x = factor(dow, levels(factor(dow))[c(2, 6, 7, 5, 1, 3, 4)]), y = perc, group = 1)) + 
  geom_bar(stat = "identity", color = "black", size = 1, fill =  brocolors("crayons")["Goldenrod"]) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.3, position = position_dodge(0.9), size = 1.3) +
  geom_text(aes(label = n), color = "black", position = position_stack(vjust = 0.5), size = 4) +
  geom_text(aes(label = n2), color = "black", position = position_stack(vjust = 0.4), size = 5, fontface="bold") +
  scale_y_continuous(breaks = seq(0, 0.4, 0.05), labels = scales::percent_format(accuracy = 1)) + 
  theme_bw() +
  labs(title = "C", x = "Day of contact events", y = "Proportion of participants") +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 0, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 12), axis.title.y = element_text(face = "bold", size = 12)) +
  theme(legend.position = "none")

#===========================================================================

# number of contactees per person

D <- cn.labeled %>% 
  ggplot() + 
  geom_histogram(aes(x = cnt_no), bins = 90, color = "black", fill =  brocolors("crayons")["Violet (Purple)"]) +
  geom_density(aes(x = cnt_no, y = ..density../0.00018), alpha = 0.3, size = 1) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 25, 2)) +
  scale_y_continuous("Number of contact events", sec.axis = sec_axis(~. * 0.00018, name = "Probability density"), limits = c(0, 1700)) + 
  labs(title = "D", x = "Number of contacts per person") +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 0, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 12), axis.title.y = element_text(face = "bold", size = 12)) +
  theme(legend.position = "none")

#===========================================================================

#turn on warnings
options(warn = defaultW)

#combined plots
ggsave(here("output", "Fig1_participant_contact_desc.png"),
       plot = (A | inset_element(X, right = 0.9, left = 0.4, bottom = 0.3, top = 0.95) | B | inset_element(Y, right = 0.9, left = 0.4, bottom = 0.3, top = 0.95) | plot_layout(ncol = 2, width = c(3,3))) / (C | D | plot_layout(ncol = 2, width = c(3,3))),
       width = 19, height = 14, unit="in", dpi = 300)
