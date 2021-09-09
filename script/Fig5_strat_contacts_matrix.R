#Last edited by - Deus Thindwa
#Date - 28/10/2019

#==========================contact matrix for males participants

# prepare a participant population (for null model of probability of contact under random mixing)
survey.pops <- read.csv(here::here("data", "survey_pop.csv"))
survey.pops <- survey.pops %>% 
  mutate(lower.age.limit = if_else(age >= 0 & age < 1, 0,
                                   if_else(age >= 1 & age <= 4, 1,
                                           if_else(age > 4 & age <= 9, 5,
                                                   if_else(age > 9 & age <= 14, 10,
                                                           if_else(age > 14 & age <= 19, 15,
                                                                   if_else(age > 19 & age <= 24, 20,
                                                                           if_else(age > 24 & age <= 34, 25,
                                                                                   if_else(age > 34 & age <= 44, 35,
                                                                                           if_else(age > 44 & age <= 54, 45, 55)))))))))) %>% 
  group_by(lower.age.limit) %>% tally() %>% rename("population" = n)


#create survey object by combining separate male and female part and cnt datasets
somipa.sexm <- survey(part.m %>% filter(part_sex == "Male"), cnt.m)

#build a contact matrix via sampling contact survey using bootstrapping
somipa.sexm <- contact_matrix(
  somipa.sexm,
  countries = c("Malawi"),
  survey.pop = survey.pops,
  age.limits = c(0, 1, 5, 10, 15, 20, 25, 35, 45, 55),
  filter = FALSE,
  n = 1000,
  bootstrap = TRUE,
  counts = FALSE,
  symmetric = FALSE,
  split = FALSE,
  weigh.dayofweek = FALSE,
  sample.all.age.groups = FALSE,
  quiet = FALSE
)

#calculate the mean of matrices generated through bopostrapping for uncertainty
somipa.sexm <- melt(Reduce("+", lapply(somipa.sexm$matrices, function(x) {x$matrix})) / length(somipa.sexm$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")


#==========================contact matrix for female participants

# create survey object by combining separate male and female part and cnt datasets
somipa.sexf <- survey(part.m %>% filter(part_sex == "Female"), cnt.m)

# build a contact matrix via sampling contact survey using bootstrapping
somipa.sexf <- contact_matrix(
  somipa.sexf,
  countries = c("Malawi"),
  survey.pop = survey.pops,
  age.limits = c(0, 1, 5, 10, 15, 20, 25, 35, 45, 55),
  filter = FALSE,
  n = 1000,
  bootstrap = TRUE,
  counts = FALSE,
  symmetric = FALSE,
  split = FALSE,
  weigh.dayofweek = FALSE,
  sample.all.age.groups = FALSE,
  quiet = FALSE
)

# calculate the mean of matrices generated through bopostrapping for uncertainty
somipa.sexf <- melt(Reduce("+", lapply(somipa.sexf$matrices, function(x) {x$matrix})) / length(somipa.sexf$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")

#==========================contact matrix for participants within household

# create survey object for household location
somipa.whh <- survey(part.m, cnt.m %>% filter(cnt_loc == "Home"))

# build a contact matrix via sampling contact survey using bootstrapping
somipa.whh <- contact_matrix(
  somipa.whh,
  countries = c("Malawi"),
  survey.pop = survey.pop,
  age.limits = c(0, 1, 5, 10, 15, 20, 25, 35, 45, 55),
  filter = FALSE,
  n = 1000,
  bootstrap = TRUE,
  counts = FALSE,
  symmetric = FALSE,
  split = FALSE,
  weigh.dayofweek = FALSE,
  sample.all.age.groups = FALSE,
  quiet = FALSE
)

# calculate the mean of matrices generated through bopostrapping for uncertainty
somipa.whh <- melt(Reduce("+", lapply(somipa.whh$matrices, function(x) {x$matrix})) / length(somipa.whh$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")


#==========================contact matrix for participants outside household

# create survey object for household location
somipa.ohh <- survey(part.m, cnt.m %>% filter(cnt_loc != "Home"))

# build a contact matrix via sampling contact survey using bootstrapping
somipa.ohh <- contact_matrix(
  somipa.ohh,
  countries = c("Malawi"),
  survey.pop = survey.pop,
  age.limits = c(0, 1, 5, 10, 15, 20, 25, 35, 45, 55),
  filter = FALSE,
  n = 1000,
  bootstrap = TRUE,
  counts = FALSE,
  symmetric = FALSE,
  split = FALSE,
  weigh.dayofweek = FALSE,
  sample.all.age.groups = FALSE,
  quiet = FALSE
)

# calculate the mean of matrices generated through bopostrapping for uncertainty
somipa.ohh <- melt(Reduce("+", lapply(somipa.ohh$matrices, function(x) {x$matrix})) / length(somipa.ohh$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")


#==========================contact matrix for participants within community

# create survey object community location
somipa.wcom <- survey(part.m, cnt.m %>% filter(cnt_plc != "Within community"))

# build a contact matrix via sampling contact survey using bootstrapping
somipa.wcom <- contact_matrix(
  somipa.wcom,
  countries = c("Malawi"),
  survey.pop = survey.pop,
  age.limits = c(0, 1, 5, 10, 15, 20, 25, 35, 45, 55),
  filter = FALSE,
  n = 1000,
  bootstrap = TRUE,
  counts = FALSE,
  symmetric = FALSE,
  split = FALSE,
  weigh.dayofweek = FALSE,
  sample.all.age.groups = FALSE,
  quiet = FALSE
)

# calculate the mean of matrices generated through bopostrapping for uncertainty
somipa.wcom <- melt(Reduce("+", lapply(somipa.wcom$matrices, function(x) {x$matrix})) / length(somipa.wcom$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")


#==========================contact matrix for participants outside community

# create survey object for community location
somipa.ocom <- survey(part.m, cnt.m %>% filter(cnt_plc != "Outside community"))

# build a contact matrix via sampling contact survey using bootstrapping
somipa.ocom <- contact_matrix(
  somipa.ocom,
  countries = c("Malawi"),
  survey.pop = survey.pop,
  age.limits = c(0, 1, 5, 10, 15, 20, 25, 35, 45, 55),
  filter = FALSE,
  n = 1000,
  bootstrap = TRUE,
  counts = FALSE,
  symmetric = FALSE,
  split = FALSE,
  weigh.dayofweek = FALSE,
  sample.all.age.groups = FALSE,
  quiet = FALSE
)

# calculate the mean of matrices generated through bopostrapping for uncertainty
somipa.ocom <- melt(Reduce("+", lapply(somipa.ocom$matrices, function(x) {x$matrix})) / length(somipa.ocom$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")

# ggplotting the matrices
somipa.sexm <- somipa.sexm %>% mutate(Category = "A, male")
somipa.sexf <- somipa.sexf %>% mutate(Category = "B, female")
somipa.whh <- somipa.whh %>% mutate(Category = "C, within household")
somipa.ohh <- somipa.ohh %>% mutate(Category = "D, outside household")
somipa.wcom <- somipa.wcom %>% mutate(Category = "E, within community")
somipa.ocom <- somipa.ocom %>% mutate(Category = "F, outside community")

A <- rbind(somipa.sexm, somipa.whh, somipa.wcom) %>%
  ggplot(aes(x = Participant.age, y = Contact.age, fill = Mixing.rate)) + 
  geom_tile(color = "white") + 
  geom_text(aes(label = sprintf("%1.2f", Mixing.rate)), color = "white", size = 2) +
  theme_bw() +
  scale_fill_gradient(low="lightgreen", high="red") +
  facet_grid(.~ Category) +
  labs(title = "", x = "", y = "Contacts age") +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  theme(strip.text.x = element_text(size = 14)) +
  guides(fill=guide_legend(title="Average number\nof daily contacts")) +
  theme(legend.position = "right") +
  geom_vline(xintercept = c(2.5, 5.5, 12), linetype="dashed", color = "black", size = 0.2) +
  geom_hline(yintercept = c(2.5, 5.5, 12), linetype="dashed", color = "black", size = 0.2)

B <- rbind(somipa.sexf, somipa.ohh, somipa.ocom) %>%
  ggplot(aes(x = Participant.age, y = Contact.age, fill = Mixing.rate)) + 
  geom_tile(color = "white") + 
  geom_text(aes(label = sprintf("%1.2f", Mixing.rate)), color = "white", size = 2) +
  theme_bw() +
  scale_fill_gradient(low="lightgreen", high="red") +
  facet_grid(.~ Category) +
  labs(title = "", x = "Participants age", y = "Contacts age") +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  theme(strip.text.x = element_text(size = 14)) +
  guides(fill=guide_legend(title="Average number\nof daily contacts")) +
  theme(legend.position = "right") + 
  geom_vline(xintercept = c(2.5, 5.5, 12), linetype="dashed", color = "black", size = 0.2) +
  geom_hline(yintercept = c(2.5, 5.5, 12), linetype="dashed", color = "black", size = 0.2)

#===========================================================================

ggsave(here::here("output", "Fig5_strat_contacts_matrix.png"),
       plot = (A / B),
       width = 18, height = 9, unit="in", dpi = 300)
