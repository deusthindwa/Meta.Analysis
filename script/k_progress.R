#progress report

#households
hh.labeled %>% group_by(cluster) %>% tally()

#participants
pp.labeled %>% 
  mutate(part_agegp = if_else(agey < 1, "<1y",
                              if_else(agey >= 1 & agey < 5, "1-4y",
                                      if_else(agey >= 5 & agey < 15, "5-14y",
                                              if_else(agey >= 15 & agey < 20, "15-19y",
                                                      if_else(agey >= 20 & agey < 50, "20-49y", "50+y"))))))  %>% 
  group_by(part_agegp) %>% tally()
  
#contacts
cn.labeled %>% 
  mutate(cnt_agegp = if_else(cnt_age < 1, "<1y",
                              if_else(cnt_age >= 1 & cnt_age < 5, "1-4y",
                                      if_else(cnt_age >= 5 & cnt_age < 15, "5-14y",
                                              if_else(cnt_age >= 15 & cnt_age < 20, "15-19y",
                                                      if_else(cnt_age >= 20 & cnt_age < 50, "20-49y", "50+y"))))))  %>% 
  group_by(cnt_agegp, cnt_type) %>% tally() %>%
  
  ggplot(aes(x = cnt_agegp, y = n, color = cnt_type)) + 
  geom_bar(stat = "identity") +
  theme_bw() + 
  guides(color = guides(title = "Contact type"))




