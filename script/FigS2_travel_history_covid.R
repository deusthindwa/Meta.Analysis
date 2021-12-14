# Last edited by - Deus Thindwa
# Date - 28/10/2019

#suppress warnings
defaultW <- getOption("warn") 
options(warn = -1)

#===========================================================================

# distance travelled in the last 24 hours
A <- pp.labeled %>% 
  group_by(dtravel) %>% tally() %>% 
  mutate(perc = n/sum(n), lci = exactci(n, sum(n), 0.95)$conf.int[1:4], uci = exactci(n, sum(n), 0.95)$conf.int[5:8]) %>%
  
  ggplot(aes(x = dtravel, y = perc)) + 
  geom_bar(stat = "identity", color = "black", size = 1, fill = brocolors("crayons")["Sea Green"]) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.3, position = position_dodge(0.9), size = 1.3) +
  geom_text(aes(label = n), color = "black", position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(breaks = seq(0, 0.7, 0.1), labels = scales::percent_format(accuracy = 1)) + 
  theme_bw() +
  labs(title = "A", x = "Travel outside community", y = "Proportion of participants") +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 12), axis.title.y = element_text(face = "bold", size = 12)) +
  theme(legend.position = "none")

#===========================================================================

# frequency of visit outside community
B <- pp.labeled %>% 
  mutate(pvisitc = if_else(pvisit == "Often per week", "~Daily per week", 
                           if_else(pvisit == ">=1 per weeks", "≥1 per week", 
                                   if_else(pvisit == ">1 per month", "≥1 per month", 
                                           if_else(pvisit == "<1 per month", "<1 per month", "Never"))))) %>%
  group_by(pvisitc) %>% tally() %>% 
  mutate(perc = n/sum(n), lci = exactci(n, sum(n), 0.95)$conf.int[1:5], uci = exactci(n, sum(n), 0.95)$conf.int[6:10]) %>%
  
  ggplot(aes(x = factor(pvisitc, levels(factor(pvisitc))[c(2, 4, 3, 1, 5)]), y = perc)) + 
  geom_bar(stat = "identity", color = "black", size = 1, fill = brocolors("crayons")["Green Yellow"]) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.3, position = position_dodge(0.9), size = 1.3) +
  geom_text(aes(label = n), color = "black", position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(breaks = seq(0, 0.6, 0.1), labels = scales::percent_format(accuracy = 1)) + 
  theme_bw() +
  labs(title = "B", x = "Visit frequency outside community", y = "") +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 25, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 12), axis.title.y = element_text(face = "bold", size = 12)) +
  theme(legend.position = "none")

#===========================================================================

# Duration of stay outside community
C <- pp.labeled %>% 
  mutate(vtimec = if_else(vtime == "<1hr", "<1 hour", 
                           if_else(vtime == "1-2hr", "within 2 hours", 
                                   if_else(vtime == "Half day", "within 12 hours",
                                           if_else(vtime == "Whole day", "within 24 hours",
                                                   if_else(vtime == "1wk", "within a week",
                                                           if_else(vtime == ">1wk", ">1 week", NA_character_))))))) %>%
                                                           
  filter(!is.na(vtimec)) %>%
  group_by(vtimec) %>% tally() %>% 
  mutate(perc = n/sum(n), lci = exactci(n, sum(n), 0.95)$conf.int[1:6], uci = exactci(n, sum(n), 0.95)$conf.int[7:12]) %>%
  
  ggplot(aes(x = factor(vtimec, levels(factor(vtimec))[c(1, 4, 3, 5, 6, 2)]), y = perc)) + 
  geom_bar(stat = "identity", color = "black", size = 1, fill = brocolors("crayons")["Brick Red"]) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.3, position = position_dodge(0.9), size = 1.3) +
  geom_text(aes(label = n), color = "black", position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(breaks = seq(0, 0.6, 0.1), labels = scales::percent_format(accuracy = 1)) + 
  theme_bw() +
  labs(title = "C", x = "Duration of stay outside community", y = "") +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 25, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 12), axis.title.y = element_text(face = "bold", size = 12)) +
  theme(legend.position = "none")

#===========================================================================

# distribution of participant sex
D <- X %>% 
  dplyr::select(transfot, transbic, transcar, transpub, transbac) %>%
  pivot_longer(transfot:transbac, names_to = "question", values_to = "response") %>% 
  group_by(question, response) %>% 
  tally() %>% 
  mutate(perc = n/sum(n), 
         lci = exactci(n, sum(n), 0.95)$conf.int[1:2], 
         uci = exactci(n, sum(n), 0.95)$conf.int[3:4],
         questionc = if_else(question == "transbac", "Back",
                             if_else(question == "transbic", "Bicycle",
                                     if_else(question == "transcar", "Private car",
                                             if_else(question == "transfot", "Foot", "Public transport"))))) %>%
  naniar::replace_with_na_all(condition = ~.x == "No") %>%
  filter(!is.na(response)) %>%
  
  ggplot(aes(x = factor(questionc, levels(factor(questionc))[c(5, 1, 3, 4, 2)]), y = perc, size = n)) + 
  geom_bar(stat = "identity", color = "black", size = 1, fill = brocolors("crayons")["Sky Blue"]) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.3, position = position_dodge(0.9), size = 1.3) +
  geom_text(aes(label = n), color = "black", position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(breaks = seq(0, 0.9, 0.1), labels = scales::percent_format(accuracy = 1)) + 
  theme_bw() +
  labs(title = "D", x = "Main transportation means", y = "") +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 025, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 12), axis.title.y = element_text(face = "bold", size = 12)) +
  theme(legend.position = "none")

#===========================================================================

E <- filter(pp.labeled, !is.na(cvdcnt)) %>% 
  group_by(cvdcnt) %>% tally() %>% 
  mutate(perc = n/sum(n), lci = exactci(n, sum(n), 0.95)$conf.int[1:2], uci = exactci(n, sum(n), 0.95)$conf.int[3:4]) %>%
  
  ggplot(aes(x = cvdcnt, y = perc)) + 
  geom_bar(stat = "identity", color = "black", size = 1, fill = brocolors("crayons")["Sunglow"]) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.3, position = position_dodge(0.9), size = 1.3) +
  geom_text(aes(label = n), color = "black", position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), labels = scales::percent_format(accuracy = 1)) + 
  theme_bw() +
  labs(title = "E", x = "Low mixing due to COVID-19", y = "Proportion of participants") +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 0, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 12), axis.title.y = element_text(face = "bold", size = 12)) +
  theme(legend.position = "none")

#===========================================================================

# distribution of potential encounters missed due to COVID-19
F <- pp.labeled %>%  
  ggplot() + 
  geom_density(aes(x = cvdno), alpha = 0.3, size = 1, fill = brocolors("crayons")["Sky Blue"]) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 20, 2)) +
  labs(title = "F", x = "Missed # of contacts due to COVID-19", y = "Probability density") +
  theme(axis.text.x = element_text(face = "bold", size = 12), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 12), axis.title.y = element_text(face = "bold", size = 12)) +
  theme(legend.position = "none")

# distribution of potential encounters missed due to COVID-19 by age groups
#cols <- c("<1y" = "red", "1-4y" = "black", "5-14y" = "green", "15-19y" = "blue", "20-49y" = "orange", "50+y" = "yellow")
G <- pp.labeled %>%  
  ggplot() + 
  geom_density(aes(x = cvdinfant, color = "<1y"), alpha = 0.3, size = 1) +
  geom_density(aes(x = cvdprexool, color = "1-4y"), alpha = 0.3, size = 1) +
  geom_density(aes(x = cvdprixool, color = "5-14y"), alpha = 0.3, size = 1) +
  geom_density(aes(x = cvdsecxool, color = "15-19y"), alpha = 0.3, size = 1) +
  geom_density(aes(x = cvdadult, color = "20-49y"), alpha = 0.3, size = 1) +
  geom_density(aes(x = cvdelderly, color = "50+y"), alpha = 0.3, size = 1) +
  theme_bw() +
  labs(title = "G", x = "Missed # of contacts due to COVID-19", y = "Probability density") +
  theme(axis.text.x = element_text(face = "bold", size = 12), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 12), axis.title.y = element_text(face = "bold", size = 12)) +
  theme(legend.position = c(0.6, 0.5)) +
  guides(color=guide_legend(title="")) 

#===========================================================================

# potential place of missed encouters
H <- filter(pp.labeled, cvdcnt == "Yes") %>% 
  dplyr::select(cvdhome:cvdothr) %>%
  pivot_longer(cvdhome:cvdothr, names_to = "question", values_to = "response") %>% 
  group_by(question, response) %>% 
  tally() %>% 
  mutate(perc = n/sum(n), 
         questionc = if_else(question == "cvdchurch", "Church",
                             if_else(question == "cvdhome", "Home",
                                     if_else(question == "cvdmrkt", "Market", 
                                             if_else(question == "cvdothr", "Other",
                                                     if_else(question == "cvdwork", "Work", "School"))))),
         response = if_else(response == 1, "Yes", "No")) %>%
  
  ggplot(mapping = aes(x = factor(questionc,levels(factor(questionc))[c(2,3,4,1,5,6)]), y = perc, color = factor(response), fill = factor(response))) + 
  geom_bar(stat = "identity", color = "black", size = 0.7) +
  scale_fill_brewer() +
  geom_text(aes(label = n), color = "black", position = position_stack(vjust = 0.5)) +
  theme_bw() +
<<<<<<< HEAD
  labs(title = "H", x = "Potential place of missed mixing", y = "Proportion of contactees") +
=======
  labs(title = "H", x = "Potential place of missed contacts", y = "Proportion of participants") +
>>>>>>> e89deea4ed4a577937af675763b03d95327089e9
  scale_y_continuous(breaks = seq(0, 1, 0.2), labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 0, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 12), axis.title.y = element_text(face = "bold", size = 12)) +
  guides(fill = guide_legend(title="")) +
  theme(legend.position = c(0.9, 0.75))

#===========================================================================

#turn on warnings
options(warn = defaultW)

ggsave(here("output", "FigS2_travel_history_covid.png"),
       plot = (A | B | C | D | plot_layout(ncol = 4, width = c(1,2,3,2))) / (E | F | G | H | plot_layout(ncol = 4, width = c(1,2,2,3))),
       width = 21, height = 10, unit="in", dpi = 300)
