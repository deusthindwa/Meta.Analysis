#
X <- part.m %>% filter(part_age <5) %>% group_by(part_id, part_age) %>% tally()
