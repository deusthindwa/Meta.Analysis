# Last edited by - Deus Thindwa
# Date - 28/10/2019

#===========================================================================

# distribution of participant age
A <- pp.labeled %>% 
  mutate(agegp = if_else(agey < 1, "<1y", if_else(agey >= 1 & agey < 5, "1-4y", 
                                                  if_else(agey >= 5 & agey < 15, "5-14y", 
                                                          if_else(agey >= 15 & agey < 20, "15-19y", 
                                                                  if_else(agey >= 20 & agey < 50, "20-49y", "50+y"))))))  %>% 
  group_by(agegp) %>% tally() %>% 
  mutate(perc = n/sum(n), lci = exactci(n, sum(n), 0.95)$conf.int[1:6], uci = exactci(n, sum(n), 0.95)$conf.int[7:12]) %>%
  
ggplot(aes(x = factor(agegp, levels(factor(agegp))[c(1, 2, 5, 3, 4, 6)]), y = perc)) + 
  geom_bar(stat = "identity", color = "black", size = 0.7, fill = brocolors("crayons")["Sea Green"]) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(label = n), color = "black", position = position_stack(vjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 0.4, 0.1), labels = scales::percent_format(accuracy = 1)) + 
  theme_bw() +
  labs(title = "A", x = "Age,y", y = "Proportion") +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = "none")

#===========================================================================

# distribution of participant education
B <- pp.labeled %>% 
  filter(!is.na(educ)) %>% group_by(educ) %>% tally() %>% 
  mutate(perc = n/sum(n), lci = exactci(n, sum(n), 0.95)$conf.int[1:4], uci = exactci(n, sum(n), 0.95)$conf.int[5:8]) %>%
  
  ggplot(aes(x = factor(educ, levels(factor(educ))[c(2, 3, 4, 1)]), y = perc)) + 
  geom_bar(stat = "identity", color = "black", size = 0.7, fill = brocolors("crayons")["Green Yellow"]) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(label = n), color = "black", position = position_stack(vjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 0.6, 0.1), labels = scales::percent_format(accuracy = 1)) + 
  theme_bw() +
  labs(title = "B", x = "Education status", y = "") +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = "none")

#===========================================================================

# distribution of participant employment
C <- pp.labeled %>% 
  filter(!is.na(occup)) %>% 
  mutate(occupgp = if_else(occup == "Agriculture", "Manual", 
                           if_else(occup == "Business", "Business", 
                                   if_else(occup == "Domestic", "Other", 
                                           if_else(occup == "Manual", "Manual",
                                                   if_else(occup == "Office", "Office",
                                                           if_else(occup == "Other", "Other",
                                                                   if_else(occup == "Preschool", "Preschool",
                                                                           if_else(occup == "Retired", "Retired", 
                                                                                   if_else(occup == "School", "School",
                                                                                           if_else(occup == "Shop", "Business", "Unemployed")))))))))))  %>%

  group_by(occupgp) %>% tally() %>% 
  mutate(perc = n/sum(n), lci = exactci(n, sum(n), 0.95)$conf.int[1:8], uci = exactci(n, sum(n), 0.95)$conf.int[9:16]) %>%
  
  ggplot(aes(x = occupgp, y = perc)) + 
  geom_bar(stat = "identity", color = "black", size = 0.7, fill = brocolors("crayons")["Red Violet"]) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(label = n), color = "black", position = position_stack(vjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 0.6, 0.1), labels = scales::percent_format(accuracy = 1)) + 
  theme_bw() +
  labs(title = "C", x = "Occupation status", y = "") +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = "none")

#===========================================================================

# distribution of participant sex
D <- pp.labeled %>% 
  filter(!is.na(sex)) %>% group_by(sex) %>% tally() %>% 
  mutate(perc = n/sum(n), lci = exactci(n, sum(n), 0.95)$conf.int[1:2], uci = exactci(n, sum(n), 0.95)$conf.int[3:4]) %>%
  
  ggplot(aes(x = sex, y = perc)) + 
  geom_bar(stat = "identity", color = "black", size = 0.7, fill = brocolors("crayons")["Sky Blue"]) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.2, position = position_dodge(0.9)) +
  geom_text(aes(label = n), color = "black", position = position_stack(vjust = 0.5)) +
  scale_y_continuous(breaks = seq(0, 0.6, 0.1), labels = scales::percent_format(accuracy = 1)) + 
  theme_bw() +
  labs(title = "D", x = "Sex", y = "") +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = "none")

#===========================================================================

cn.labeled %>% mutate(datex = dmy(str_sub(sdate, 1, 9)), dow = weekdays(datex)) %>%  group_by(dow) %>% tally() %>% 
  mutate(perc = n/sum(n), lci = exactci(n, sum(n), 0.95)$conf.int[1:7], uci = exactci(n, sum(n), 0.95)$conf.int[8:14]) %>%
  
  ggplot(aes(x = factor(dow, levels(factor(dow))[c(2, 6, 7, 5, 1, 3, 4)]), y = perc)) + 
  geom_point(aes(size = n), color = brocolors("crayons")["Sepia"]) +
  geom_errorbar(aes(ymin = lci, ymax = uci), color = brocolors("crayons")["Sepia"], size = 0.9) + 
  geom_text(aes(label = n), color = "black", position = position_stack(vjust = 1.2)) +
  scale_y_continuous(breaks = seq(0, 0.3, 0.05), labels = scales::percent_format(accuracy = 1)) + 
  theme_bw() +
  labs(title = "E", x = "Day of the week", y = "Proportion") +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = "none")


#===========================================================================

(A | B | C | D | plot_layout(ncol = 4, width = c(2,2,3,1)))

ggsave(here("output", "Fig2_descriptive.tiff"),
       plot = (A | B | C) / (D + E + F + plot_layout(ncol=3, widths=c(3,1,1))),
       width = 12, height = 7, unit="in", dpi = 200)
