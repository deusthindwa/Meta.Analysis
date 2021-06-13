#load dataset
protect <- select(read_csv(here::here("data", "protect.csv")), pid, od_baseline, od_followup)
protect <- protect %>% filter(od_baseline > 1.1) %>% mutate(igg_status = if_else(od_baseline > od_followup, "decreasing", "increasing"))
protect_decr <- protect %>% filter(igg_status == "decreasing")
protect_decr <- protect_decr %>% mutate(status = if_else(od_followup <1.1, "Negative at 6m", "Positive at 6m"))
protect_incr <- protect %>% filter(igg_status == "increasing")

lm(protect_decr$od_followup ~ protect_decr$od_baseline)$coeff[[2]]
lm(protect_incr$od_followup ~ protect_incr$od_baseline)$coeff[[2]]

protect1 <- protect_decr %>% select(pid, od_baseline, igg_status, status) %>% mutate(od_status = "Baseline") %>% rename("od_value" = od_baseline)
protect2 <- protect_decr %>% select(pid, od_followup, igg_status, status) %>% mutate(od_status = "Follow up") %>% rename("od_value" = od_followup)

protect3 <- protect_incr %>% select(pid, od_baseline, igg_status) %>% mutate(od_status = "Baseline") %>% rename("od_value" = od_baseline)
protect4 <- protect_incr %>% select(pid, od_followup, igg_status) %>% mutate(od_status = "Follow up") %>% rename("od_value" = od_followup)

t.test(protect_decr$od_followup, protect_decr$od_baseline, paired=TRUE)
A <- rbind(protect1, protect2) %>%
  ggplot(aes(x = od_status, y = od_value, color = status)) + 
  geom_line(aes(group = pid)) +
  theme_bw() +
  labs(title = "Decreasing", x = "Time", y = "OD value") +
  theme(legend.position = "right") +
  ylim(0, 4.5) +
  annotate(geom = "text", x = 1.5, 4.4, y = , label = "Average slope = -0.2931") + 
  geom_hline(yintercept = 1.1, linetype="dashed", color = "black")
  #annotate(geom = "text", x = 1.5, 4.0, y = , label = "Student's t-test for paired samples") + 
  #annotate(geom = "text", x = 1.5, 3.5, y = , label = "t = -6.0056, df = 27, p<0.001\nMean of the differences -0.85, 95%CI: -1.14 - -0.56")

t.test(protect_incr$od_followup, protect_incr$od_baseline, paired=TRUE)
B <- rbind(protect3, protect4) %>%
  ggplot(aes(x = od_status, y = od_value, color = od_status)) + 
  geom_line(aes(group = pid), color = "gray") +
  geom_boxplot(outlier.size=2, notch=FALSE, size=1) +
  theme_bw() +
  labs(title = "Increasing", x = "Time", y = "OD value") +
  theme(legend.position = "none") + 
  ylim(0, 4.5) +
  annotate(geom = "text", x = 1.5, 4.4, y = , label = "Average slope = 1.2770") + 
  annotate(geom = "text", x = 1.5, 4.0, y = , label = "Student's t-test for paired samples") + 
  annotate(geom = "text", x = 1.5, 3.5, y = , label = "t = 10.01, df = 108, p<0.001\nMean of the differences 0.71, 95%CI: 0.57 - 0.85")

A | B
