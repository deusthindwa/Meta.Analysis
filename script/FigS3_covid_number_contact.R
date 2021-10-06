# Last edited by - Deus Thindwa
# Date - 28/10/2019

#suppress warnings
defaultW <- getOption("warn") 
options(warn = -1)

#===========================================================================

# distribution of all contacts by age group with or without COVID-19 restrictions
A <- rbind((pp.labeled %>% 
  mutate(agegp = if_else(agey < 1, "<1y", 
                        if_else(agey >= 1 & agey < 5, "1-4y", 
                                if_else(agey >= 5 & agey < 15, "5-14y", 
                                        if_else(agey >= 15 & agey < 20, "15-19y", 
                                                if_else(agey >= 20 & agey < 50, "20-49y", "50+y"))))),
         cat = "COVID19 restrictions") %>% 
  dplyr::select(agegp, cntno, cat)),

(pp.labeled %>% 
  mutate(cvdnor = if_else(is.na(cvdno), 0, cvdno),
         totcnt = cntno + cvdnor,
         agegp = if_else(agey < 1, "<1y", 
                         if_else(agey >= 1 & agey < 5, "1-4y", 
                                 if_else(agey >= 5 & agey < 15, "5-14y", 
                                         if_else(agey >= 15 & agey < 20, "15-19y", 
                                                 if_else(agey >= 20 & agey < 50, "20-49y", "50+y"))))),
         cat = "No COVID19 restrictions") %>% 
   dplyr::select(agegp, totcnt, cat) %>%
   rename("cntno" = totcnt))) %>%
  
  ggplot() + 
  geom_boxplot(aes(x = factor(agegp, levels(factor(agegp))[c(1, 2, 5, 3, 4, 6)]), y = cntno, 
                   fill = cat), notch = FALSE, color = "gray50", size = 0.6, position=position_dodge(width=0.8), width = 0.6) + 
  theme_bw() +
  labs(title = "A, All mixing events", x = "", y = "Number of reported mixing events") +
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 0, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  guides(fill=guide_legend(title="")) +
  theme(legend.position = c(0.2, 0.85))
  
#===========================================================================
# distribution of female contacts by age group with or without COVID-19 restrictions
B <- rbind((filter(pp.labeled, sex == "Female") %>% 
         mutate(cvdnor = if_else(is.na(cvdno), 0, cvdno),
                totcnt = cntno + cvdnor,
                agegp = if_else(agey < 1, "<1y", 
                                if_else(agey >= 1 & agey < 5, "1-4y", 
                                        if_else(agey >= 5 & agey < 15, "5-14y", 
                                                if_else(agey >= 15 & agey < 20, "15-19y", 
                                                        if_else(agey >= 20 & agey < 50, "20-49y", "50+y"))))),
                cat = "With COVID19 restrictions") %>% 
         dplyr::select(agegp, cntno, cat)),
      
      (filter(pp.labeled, sex == "Female") %>% 
         mutate(cvdno2 = if_else(is.na(cvdno), 0, cvdno),
                totcnt = cntno + cvdno2,
                agegp = if_else(agey < 1, "<1y", 
                                if_else(agey >= 1 & agey < 5, "1-4y", 
                                        if_else(agey >= 5 & agey < 15, "5-14y", 
                                                if_else(agey >= 15 & agey < 20, "15-19y", 
                                                        if_else(agey >= 20 & agey < 50, "20-49y", "50+y"))))),
                cat = "Without COVID19 restrictions") %>% 
         dplyr::select(agegp, totcnt, cat) %>%
         rename("cntno" = totcnt))) %>%
  
  ggplot() + 
  geom_boxplot(aes(x = factor(agegp, levels(factor(agegp))[c(1, 2, 5, 3, 4, 6)]), y = cntno, 
                   fill = cat), notch = FALSE, color = "gray50", size = 0.6, position=position_dodge(width=0.8), width = 0.6) + 
  theme_bw() +
  labs(title = "B, Female", x = "", y = "") +
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 0, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  guides(fill=guide_legend(title="")) +
  theme(legend.position = "none")

#===========================================================================

# distribution of school contacts by age group with or without COVID-19 restrictions
C <- rbind((filter(pp.labeled, occup == "School") %>% 
         mutate(cvdnor = if_else(is.na(cvdno), 0, cvdno),
                totcnt = cntno + cvdnor,
                agegp = if_else(agey < 1, "<1y", 
                                if_else(agey >= 1 & agey < 5, "1-4y", 
                                        if_else(agey >= 5 & agey < 15, "5-14y", 
                                                if_else(agey >= 15 & agey < 20, "15-19y", 
                                                        if_else(agey >= 20 & agey < 50, "20-49y", "50+y"))))),
                cat = "With COVID19 restrictions") %>% 
         dplyr::select(agegp, cntno, cat)),
      
      (filter(pp.labeled, occup == "School") %>% 
         mutate(cvdno2 = if_else(is.na(cvdno), 0, cvdno),
                totcnt = cntno + cvdno2,
                agegp = if_else(agey < 1, "<1y", 
                                if_else(agey >= 1 & agey < 5, "1-4y", 
                                        if_else(agey >= 5 & agey < 15, "5-14y", 
                                                if_else(agey >= 15 & agey < 20, "15-19y", 
                                                        if_else(agey >= 20 & agey < 50, "20-49y", "50+y"))))),
                cat = "Without COVID19 restrictions") %>% 
         dplyr::select(agegp, totcnt, cat) %>%
         rename("cntno" = totcnt))) %>%
  
  ggplot() + 
  geom_boxplot(aes(x = factor(agegp, levels(factor(agegp))[c(1, 2, 5, 3, 4, 6)]), y = cntno, 
                   fill = cat), notch = FALSE, color = "gray50", size = 0.6, position=position_dodge(width=0.8), width = 0.5) + 
  theme_bw() +
  labs(title = "C, School", x = "", y = "") +
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 0, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  guides(fill=guide_legend(title="")) +
  theme(legend.position = "none")

#===========================================================================
# distribution of physical contacts by age group with or without COVID-19 restrictions
D <- rbind(
  (left_join(
  pp.labeled, 
  cnt.m  %>% 
  filter(cnt_type == "Physical") %>% 
  group_by(part_id, cnt_type) %>% 
  tally() %>% rename("somipa_pid" = part_id) %>%
  dplyr::select(somipa_pid, cnt_type)) %>%
  
  mutate(cvdnor = if_else(is.na(cvdno), 0, cvdno),
         totcnt = cntno + cvdnor,
         agegp = if_else(agey < 1, "<1y", 
                         if_else(agey >= 1 & agey < 5, "1-4y", 
                                 if_else(agey >= 5 & agey < 15, "5-14y",
                                         if_else(agey >= 15 & agey < 20, "15-19y", 
                                                 if_else(agey >= 20 & agey < 50, "20-49y", "50+y"))))),
         cat = "With COVID19 restrictions") %>% 
  dplyr::select(agegp, cntno, cat, cnt_type)),
           
           (left_join(
             pp.labeled, 
             cnt.m  %>% 
               filter(cnt_type == "Physical") %>% 
               group_by(part_id, cnt_type) %>% 
               tally() %>% rename("somipa_pid" = part_id) %>%
               dplyr::select(somipa_pid, cnt_type)) %>%
              
              mutate(cvdno2 = if_else(is.na(cvdno), 0, cvdno),
                     totcnt = cntno + cvdno2,
                     agegp = if_else(agey < 1, "<1y", 
                                     if_else(agey >= 1 & agey < 5, "1-4y", 
                                             if_else(agey >= 5 & agey < 15, "5-14y", 
                                                     if_else(agey >= 15 & agey < 20, "15-19y", 
                                                             if_else(agey >= 20 & agey < 50, "20-49y", "50+y"))))),
                     cat = "Without COVID19 restrictions") %>% 
              dplyr::select(agegp, totcnt, cat, cnt_type) %>%
              rename("cntno" = totcnt))) %>%
  filter(cnt_type == "Physical") %>%
  
  ggplot() + 
  geom_boxplot(aes(x = factor(agegp, levels(factor(agegp))[c(1, 2, 5, 3, 4, 6)]), y = cntno, 
                   fill = cat), notch = FALSE, color = "gray50", size = 0.6, position=position_dodge(width=0.8), width = 0.6) + 
  theme_bw() +
  labs(title = "D, Physical mixing", x = "Participant age in years (y)", y = "Number of reported mixing events") +
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 0, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  guides(fill=guide_legend(title="")) +
  theme(legend.position = "none")

#===========================================================================

# distribution of female contacts by age group with or without COVID-19 restrictions
E <- rbind((filter(pp.labeled, sex == "Male") %>% 
         mutate(cvdnor = if_else(is.na(cvdno), 0, cvdno),
                totcnt = cntno + cvdnor,
                agegp = if_else(agey < 1, "<1y", 
                                if_else(agey >= 1 & agey < 5, "1-4y", 
                                        if_else(agey >= 5 & agey < 15, "5-14y", 
                                                if_else(agey >= 15 & agey < 20, "15-19y", 
                                                        if_else(agey >= 20 & agey < 50, "20-49y", "50+y"))))),
                cat = "With COVID19 restrictions") %>% 
         dplyr::select(agegp, cntno, cat)),
      
      (filter(pp.labeled, sex == "Male") %>% 
         mutate(cvdno2 = if_else(is.na(cvdno), 0, cvdno),
                totcnt = cntno + cvdno2,
                agegp = if_else(agey < 1, "<1y", 
                                if_else(agey >= 1 & agey < 5, "1-4y", 
                                        if_else(agey >= 5 & agey < 15, "5-14y", 
                                                if_else(agey >= 15 & agey < 20, "15-19y", 
                                                        if_else(agey >= 20 & agey < 50, "20-49y", "50+y"))))),
                cat = "Without COVID19 restrictions") %>% 
         dplyr::select(agegp, totcnt, cat) %>%
         rename("cntno" = totcnt))) %>%
  
  ggplot() + 
  geom_boxplot(aes(x = factor(agegp, levels(factor(agegp))[c(1, 2, 5, 3, 4, 6)]), y = cntno, 
                   fill = cat), notch = FALSE, color = "gray50", size = 0.6, position=position_dodge(width=0.8), width = 0.6) + 
  theme_bw() +
  labs(title = "E, Male", x = "Participant age in years (y)", y = "") +
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 0, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  guides(fill=guide_legend(title="")) +
  theme(legend.position = "none")

#===========================================================================

# distribution of school contacts by age group with or without COVID-19 restrictions
F <- rbind((filter(pp.labeled, occup != "School") %>% 
         mutate(cvdnor = if_else(is.na(cvdno), 0, cvdno),
                totcnt = cntno + cvdnor,
                agegp = if_else(agey < 1, "<1y", 
                                if_else(agey >= 1 & agey < 5, "1-4y", 
                                        if_else(agey >= 5 & agey < 15, "5-14y", 
                                                if_else(agey >= 15 & agey < 20, "15-19y", 
                                                        if_else(agey >= 20 & agey < 50, "20-49y", "50+y"))))),
                cat = "With COVID19 restrictions") %>% 
         dplyr::select(agegp, cntno, cat)),
      
      (filter(pp.labeled, occup != "School") %>% 
         mutate(cvdno2 = if_else(is.na(cvdno), 0, cvdno),
                totcnt = cntno + cvdno2,
                agegp = if_else(agey < 1, "<1y", 
                                if_else(agey >= 1 & agey < 5, "1-4y", 
                                        if_else(agey >= 5 & agey < 15, "5-14y", 
                                                if_else(agey >= 15 & agey < 20, "15-19y", 
                                                        if_else(agey >= 20 & agey < 50, "20-49y", "50+y"))))),
                cat = "Without COVID19 restrictions") %>% 
         dplyr::select(agegp, totcnt, cat) %>%
         rename("cntno" = totcnt))) %>%
  
  ggplot() + 
  geom_boxplot(aes(x = factor(agegp, levels(factor(agegp))[c(1, 2, 5, 3, 4, 6)]), y = cntno, 
                   fill = cat), notch = FALSE, color = "gray50", size = 0.6, position=position_dodge(width=0.8), width = 0.5) + 
  theme_bw() +
  labs(title = "F, Other occupation", x = "Participant age in years (y)", y = "") +
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 0, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  guides(fill=guide_legend(title="")) +
  theme(legend.position = "none")

#===========================================================================

#turn on warnings
options(warn = defaultW)

#combined plots
ggsave(here("output", "FigS3_covid_number_contact.png"),
       plot = (A | B | C) / (D | E | F),
       width = 19, height = 10, unit="in", dpi = 300)
