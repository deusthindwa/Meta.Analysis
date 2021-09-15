# Last edited by - Deus Thindwa
# Date - 28/10/2019

#suppress warnings
defaultW <- getOption("warn") 
options(warn = -1)

#===========================================================================
# distribution of contacts by age group
pp.labeled %>% 
  mutate(cvdno2 = if_else(is.na(cvdno), 0, cvdno),
         totcnt = cntno + cvdno2,
         agegp = if_else(agey < 1, "<1y", 
                        if_else(agey >= 1 & agey < 5, "1-4y", 
                                if_else(agey >= 5 & agey < 15, "5-14y", 
                                        if_else(agey >= 15 & agey < 20, "15-19y", 
                                                if_else(agey >= 20 & agey < 50, "20-49y", "50+y")))))) %>%
  dplyr::select(agegp, cntno, totcnt) %>%
  
  ggplot() + 
  geom_boxplot(aes(x = factor(agegp, levels(factor(agegp))[c(1, 2, 5, 3, 4, 6)]), y = cntno, fill = agegp), notch = TRUE, color = "black", size = 1, position=position_dodge(width=0.8), width = 0.5, na.rm = TRUE) + 
  geom_boxplot(aes(x = factor(agegp, levels(factor(agegp))[c(1, 2, 5, 3, 4, 6)]), y = totcnt, fill = agegp), notch = TRUE, color = "black", size = 1,  position=position_dodge(width=0.5), width = 0.5, na.rm = TRUE) +
  theme_bw() +
  labs(title = "All contacts", x = "Age (years)", "Age (years)") +
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = "none")
  

#===========================================================================

# distribution of contacts by age group
pp.labeled %>% 
  
  mutate(totcnt = cntno + cvdno,
         agegp = if_else(agey < 1, "<1y", 
                         if_else(agey >= 1 & agey < 5, "1-4y", 
                                 if_else(agey >= 5 & agey < 15, "5-14y", 
                                         if_else(agey >= 15 & agey < 20, "15-19y", 
                                                 if_else(agey >= 20 & agey < 50, "20-49y", "50+y")))))) %>%
  
  ggplot(aes(x = factor(agegp, levels(factor(agegp))[c(1, 5, 2, 3, 4, 6)]), y = cntno, fill = agegp)) + 
  geom_boxplot(notch = TRUE, color = "black", size = 1) + 
  geom_boxplot(notch = TRUE, color = "black", size = 1) +
  theme_bw() +
  labs(title = "All contacts", x = "Age (years)", "Age (years)") +
  #scale_y_continuous(breaks = seq(0, 40, 4)) +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = "none")

#===========================================================================

#turn on warnings
options(warn = defaultW)

#combined plots
ggsave(here("output", "Fig1_participant_contact_desc.png"),
       plot = (A | B | C | plot_layout(ncol = 3, width = c(3,2,3))) / (D | E | F | plot_layout(ncol = 3, width = c(3,2,3))),
       width = 19, height = 11, unit="in", dpi = 300)
