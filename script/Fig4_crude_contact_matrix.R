# Last edited by - Deus Thindwa
# Date - 28/10/2019

# overall contact matrix
somipa.all <- survey(part.m, cnt.m)

# check if dataset is formatted for working with contact matrices
check(
  somipa.all,
  columns = FALSE,
  quiet = FALSE,
  error = TRUE,
  id.column = "part_id",
  participant.age.column = "part_age",
  contact.age.column = "cnt_age",
  country.column = "country",
  year.column = "year"
)

# clean the survey object
clean(somipa.all, country.column = "Malawi", participant.age.column = "part_age", contact.age.column = "cnt_age")

# prepare a participant population (for null model of probability of contact under random mixing)
survey.pop <- read.csv(here::here("data", "survey_pop.csv"))
survey.pop <- survey.pop %>% 
  mutate(lower.age.limit = if_else(age >= 0 & age < 1, 0,
                                   if_else(age >= 1 & age <= 4, 1,
                                           if_else(age > 4 & age <= 9, 5,
                                                   if_else(age > 9 & age <= 14, 10,
                                                           if_else(age > 14 & age <= 19, 15,
                                                                   if_else(age > 19 & age <= 24, 20,
                                                                           if_else(age > 24 & age <= 29, 25,
                                                                                   if_else(age > 29 & age <= 34, 30,
                                                                                           if_else(age > 34 & age <= 39, 35,
                                                                                                   if_else(age > 39 & age <= 44, 40,
                                                                                                           if_else(age > 44 & age <= 49, 45, 50)))))))))))) %>% 
  group_by(lower.age.limit) %>% tally() %>% rename("population" = n)

# build a contact matrix via sampling contact survey using bootstrapping
somipa.all <- contact_matrix(
  somipa.all,
  countries = c("Malawi"),
  survey.pop = survey.pop,
  age.limits = c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50),
  filter = FALSE,
  n = 1000,
  bootstrap = TRUE,
  counts = FALSE,
  symmetric = TRUE,
  split = FALSE,
  weigh.dayofweek = TRUE,
  sample.all.age.groups = FALSE,
  quiet = FALSE
)

# calculate the mean mixing rate of matrices generated through bootstrapping for uncertainty
somipa.all <- melt(Reduce("+", lapply(somipa.all$matrices, function(x) {x$matrix})) / length(somipa.all$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")

#=============================================================================================

# physical contact matrix

# create survey object by combining separate male and female part and cnt datasets
somipa.phys <- survey(part.m, cnt.m %>% filter(cnt_type == "Physical"))

# build a contact matrix via sampling contact survey using bootstrapping
somipa.phys <- contact_matrix(
  somipa.phys,
  countries = c("Malawi"),
  survey.pop = survey.pop,
  age.limits = c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50),
  filter = FALSE,
  n = 1000,
  bootstrap = TRUE,
  counts = FALSE,
  symmetric = TRUE,
  split = FALSE,
  weigh.dayofweek = TRUE,
  sample.all.age.groups = FALSE,
  quiet = FALSE
)

# calculate the mean of matrices generated through bopostrapping for uncertainty
somipa.phys <- melt(Reduce("+", lapply(somipa.phys$matrices, function(x) {x$matrix})) / length(somipa.phys$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")

#=============================================================================================

# non-physical contact matrix

# create survey object by combining separate male and female part and cnt datasets
somipa.verb <- survey(part.m, cnt.m %>% filter(cnt_type == "Non-physical"))

# build a contact matrix via sampling contact survey using bootstrapping
somipa.verb <- contact_matrix(
  somipa.verb,
  countries = c("Malawi"),
  survey.pop = survey.pop,
  age.limits = c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50),
  filter = FALSE,
  n = 1000,
  bootstrap = TRUE,
  counts = FALSE,
  symmetric = TRUE,
  split = FALSE,
  weigh.dayofweek = TRUE,
  sample.all.age.groups = FALSE,
  quiet = FALSE
)

# calculate the mean of matrices generated through bopostrapping for uncertainty
somipa.verb <- melt(Reduce("+", lapply(somipa.verb$matrices, function(x) {x$matrix})) / length(somipa.verb$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")

#=============================================================================================

# make combined plots

somipa.all <- somipa.all %>% mutate(Category = "A, all contacts")
somipa.phys <- somipa.phys %>% mutate(Category = "B, physical contacts")
somipa.verb <- somipa.verb %>% mutate(Category = "C, non-physical contacts")


A <- rbind(somipa.all, somipa.phys, somipa.verb) %>%
  mutate(part.age = if_else(Participant.age == 1L, "[0,1)",
                            if_else(Participant.age == 2L, "[1,5)",
                                    if_else(Participant.age == 3L, "[5,10)",
                                            if_else(Participant.age == 4L, "[10,15)",
                                                    if_else(Participant.age == 5L, "[15,20)",
                                                            if_else(Participant.age == 6L, "[20,25)",
                                                                    if_else(Participant.age == 7L, "[25,30)",
                                                                            if_else(Participant.age == 8L, "[30,35)",
                                                                                    if_else(Participant.age == 9L, "[35,40)",
                                                                                            if_else(Participant.age == 10L, "[40,45)",
                                                                                                    if_else(Participant.age == 11L, "[45,50)", "50+")))))))))))) %>%

ggplot(aes(x = factor(part.age,levels(factor(part.age))[c(1,2,11,3,4,5,6,7,8,9,10,12)]), y = Contact.age, fill = Mixing.rate)) + 
  geom_tile(color = "white") + 
  geom_text(aes(label = sprintf("%1.2f", Mixing.rate)), color = "white", size = 2) +
  scale_fill_gradient(low="lightgreen", high="red") +
  facet_grid(.~ Category) +
  theme_bw() +
  labs(title = "", x = "Participants age (years)", y = "Contacts age (years)") +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  theme(strip.text.x = element_text(size = 16)) +
  guides(fill=guide_legend(title="Average number\nof daily contacts")) +
  theme(legend.position = "right") + 
  geom_vline(xintercept = c(2.5, 5.5, 12), linetype="dashed", color = "black", size = 0.2) +
  geom_hline(yintercept = c(2.5, 5.5, 12), linetype="dashed", color = "black", size = 0.2)
#===========================================================================

ggsave(here::here("output", "Fig4_crude_contact_matrix.png"),
       plot = A,
       width = 18, height = 6, unit="in", dpi = 300)
