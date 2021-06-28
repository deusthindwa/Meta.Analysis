#load protect dataset
protect <- select(read_csv(here::here("data", "protect.csv")), pid, od_baseline, od_followup)
protect <- protect %>% filter(od_baseline >= 1.1) %>% mutate(igg_status = if_else(od_baseline > od_followup, "Decreasing", "Increasing"))

#create dataset for decreasing IgG
protect_decr <- protect %>% filter(igg_status == "Decreasing")
protect_decr <- protect_decr %>% mutate(status = if_else(od_followup <=1.1, "Negative at 6m", "Positive at 6m"))

#create dataset for increasing IgG
protect_incr <- protect %>% filter(igg_status == "Increasing")
protect_incr <- protect_incr %>% mutate(status = if_else(od_followup >1.1, "Positive at 6m", "Negative at 6m"))

#reshape datasets to long format for plotting
protect_decr1 <- protect_decr %>% select(pid, od_baseline, igg_status, status) %>% mutate(od_status = "Baseline") %>% rename("od_value" = od_baseline)
protect_decr2 <- protect_decr %>% select(pid, od_followup, igg_status, status) %>% mutate(od_status = "Follow up (6m)") %>% rename("od_value" = od_followup)

protect_incr3 <- protect_incr %>% select(pid, od_baseline, igg_status, status) %>% mutate(od_status = "Baseline") %>% rename("od_value" = od_baseline)
protect_incr4 <- protect_incr %>% select(pid, od_followup, igg_status, status) %>% mutate(od_status = "Follow up (6m)") %>% rename("od_value" = od_followup)

rbind(protect_decr1, protect_decr2, protect_incr3, protect_incr4) %>%
  ggplot(aes(x = od_status, y = od_value, color = status, lty = igg_status)) + 
  geom_line(aes(group = pid)) +
  theme_bw() +
  labs(title = "", x = "", y = "IgG OD value") +
  theme(legend.position = "right") +
  ylim(0, 4.5) +
  geom_hline(yintercept = 1.1, linetype="dashed", color = "black") +
  guides(lty = guide_legend(title = "Change in IgG Ab"), color = guide_legend(title = "Serostatus"))

#expand the decreasing IgG dataset to compute month integer
protect_decr_all <- protect_decr %>% 
  rename("t0" = od_baseline, "t6" = od_followup) %>% 
  mutate(m = (t0-t6)/6, t1 = t0-m, t2 = t1-m, t3 = t2-m, t4 = t3-m, t5 = t4-m) %>% select(pid, t0, t1, t2, t3, t4, t5, t6, everything())

d0 <- protect_decr_all %>% select(pid, t0, igg_status, status) %>% mutate(time = 0) %>% rename("od_value" = t0)
d1 <- protect_decr_all %>% select(pid, t1, igg_status, status) %>% mutate(time = 1) %>% rename("od_value" = t1)
d2 <- protect_decr_all %>% select(pid, t2, igg_status, status) %>% mutate(time = 2) %>% rename("od_value" = t2)
d3 <- protect_decr_all %>% select(pid, t3, igg_status, status) %>% mutate(time = 3) %>% rename("od_value" = t3)
d4 <- protect_decr_all %>% select(pid, t4, igg_status, status) %>% mutate(time = 4) %>% rename("od_value" = t4)
d5 <- protect_decr_all %>% select(pid, t5, igg_status, status) %>% mutate(time = 5) %>% rename("od_value" = t5)
d6 <- protect_decr_all %>% select(pid, t6, igg_status, status) %>% mutate(time = 6) %>% rename("od_value" = t6)

#expand the Increasing IgG dataset to compute month integer
protect_incr_all <- protect_incr %>% 
  rename("t0" = od_baseline, "t6" = od_followup) %>% 
  mutate(m = (t0-t6)/6, t1 = t0-m, t2 = t1-m, t3 = t2-m, t4 = t3-m, t5 = t4-m) %>% select(pid, t0, t1, t2, t3, t4, t5, t6, everything())

i0 <- protect_incr_all %>% select(pid, t0, igg_status, status) %>% mutate(time = 0) %>% rename("od_value" = t0)
i1 <- protect_incr_all %>% select(pid, t1, igg_status, status) %>% mutate(time = 1) %>% rename("od_value" = t1)
i2 <- protect_incr_all %>% select(pid, t2, igg_status, status) %>% mutate(time = 2) %>% rename("od_value" = t2)
i3 <- protect_incr_all %>% select(pid, t3, igg_status, status) %>% mutate(time = 3) %>% rename("od_value" = t3)
i4 <- protect_incr_all %>% select(pid, t4, igg_status, status) %>% mutate(time = 4) %>% rename("od_value" = t4)
i5 <- protect_incr_all %>% select(pid, t5, igg_status, status) %>% mutate(time = 5) %>% rename("od_value" = t5)
i6 <- protect_incr_all %>% select(pid, t6, igg_status, status) %>% mutate(time = 6) %>% rename("od_value" = t6)

#combine all IgG datasets for half-life and rate of decay plotting
protect_all <- rbind(d0, d1, d2, d3, d4, d5, d6, i0, i1, i2, i3, i4, i5, i6)

protect_avg <- rbind(d0, d1, d2, d3, d4, d5, d6) %>%
  group_by(time) %>% 
  mutate(avg_od_value = mean(od_value), cat = "Standard (mean)")

ggplot(data = protect_all, aes(x = time, y = od_value, color = status, lty = igg_status)) + 
  geom_line(aes(group = pid)) +
  geom_line(data = protect_avg, aes(x = time, y = avg_od_value, color = cat), size =1) +
  theme_bw() +
  labs(title = "", x = "Time (month)", y = "IgG OD value") +
  theme(legend.position = "right") +
  ylim(0, 4.5) +
  scale_x_continuous(breaks = seq(0, 6, 0.2)) +
  geom_hline(yintercept = 1.1, linetype="dashed", color = "black") +
  guides(lty = guide_legend(title = "Change in IgG Ab"), color = guide_legend(title = "Serostatus")) +
  annotate(geom = "text", x = 2.5, y = 4.4, label = "Average half-life = 4.4 months") +
  annotate(geom = "text", x = 2.5, y = 4, label = "Average decay rate = 0.143/month")

#end script