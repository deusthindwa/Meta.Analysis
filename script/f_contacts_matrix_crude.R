#Last edited by - Deus Thindwa
#Date - 28/10/2019

#==========================overall contact matrix

#create survey object by combining participants and contacts datasets
part.m <- rename(select(pp1.enrol, pid, hhid, agey, sex, sdate), "part_id" = pid, "part_age" = agey, "part_sex" = sex)
part.m <- part.m %>% mutate(country = "Malawi", year = 2021, dayofweek = weekdays(sdate))
part.m <- select(left_join(part.m, spatialhiv), part_id, hhid, part_age, part_sex, sdate, country, year, dayofweek, hiv)
part.m <- part.m %>% distinct()


cnt.m <- rename(select(contacts, pid, cnt_age, cnt_type, cnt_loc, cnt_plc), "part_id" = pid)
cnt.m <- cnt.m %>% arrange(part_id) %>% mutate(cnt_id = 1:n())
cnt.m$pid <- c(0,cumsum(as.numeric(with(cnt.m, part_id[1:(length(part_id)-1)] != part_id[2:length(part_id)])))) + 1
cnt.m <- cnt.m %>% select(cnt_id, part_id, pid, everything())

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

#prepare a participant population (for null model of probability of contact under random mixing)
survey.pop <- read.csv(here("data", "survey_pop.csv"))
survey.pop$lower.age.limit <- if_else(survey.pop$age < 1, 0,
                                      if_else(survey.pop$age >= 1 & survey.pop$age <= 4, 1,
                                              if_else(survey.pop$age > 4 & survey.pop$age <= 14, 5,
                                                      if_else(survey.pop$age > 14 & survey.pop$age <= 19, 15,
                                                              if_else(survey.pop$age > 19 & survey.pop$age <= 49, 20,50)))))
survey.pop <- survey.pop %>% group_by(lower.age.limit) %>% tally()
survey.pop <- rename(survey.pop, "population" = "n")

# build a contact matrix via sampling contact survey using bootstrapping
somipa.all <- contact_matrix(
  somipa.all,
  countries = c("Malawi"),
  survey.pop = survey.pop,
  age.limits = c(0, 1, 5, 15, 20, 50),
  filter = FALSE,
  n = 100,
  bootstrap = TRUE,
  counts = FALSE,
  symmetric = FALSE,
  split = FALSE,
  weigh.dayofweek = TRUE,
  sample.all.age.groups = FALSE,
  quiet = FALSE
)

#calculate the mean mixing rate of matrices generated through bopostrapping for uncertainty
somipa.all <- melt(Reduce("+", lapply(somipa.all$matrices, function(x) {x$matrix})) / length(somipa.all$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")

#ggplotting the matrices
A <- ggplot(somipa.all, aes(x = Contact.age, y = Participant.age, fill = Mixing.rate)) + 
  geom_tile(color = "white") + 
  labs(title = "A, all mixing events", x = "Contacts age", y = "Participants age") +
  theme(legend.position = "bottom") + 
  theme_bw()

#==========================physical contact matrix

#create survey object by combining separate male and female part and cnt datasets
somipa.phys <- survey(part.m, cnt.m %>% filter(cnt_type == "Physical"))

#build a contact matrix via sampling contact survey using bootstrapping
somipa.phys <- contact_matrix(
  somipa.phys,
  countries = c("Malawi"),
  survey.pop = survey.pop,
  age.limits = c(0, 1, 5, 15,20,50),
  filter = FALSE,
  n = 10,
  bootstrap = TRUE,
  counts = FALSE,
  symmetric = FALSE,
  split = FALSE,
  weigh.dayofweek = FALSE,
  sample.all.age.groups = FALSE,
  quiet = FALSE
)

#calculate the mean of matrices generated through bopostrapping for uncertainty
somipa.phys <- melt(Reduce("+", lapply(somipa.phys$matrices, function(x) {x$matrix})) / length(somipa.phys$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")

#ggplotting the matrices
B <- ggplot(somipa.phys, aes(x = Contact.age, y = Participant.age, fill = Mixing.rate)) + 
  geom_tile(color="white") + 
  labs(title = "B, physical", x = "Contacts age", y = "participants age") +
  theme(legend.position = "bottom") + 
  theme_bw()

#==========================non-physical contact matrix

#create survey object by combining separate male and female part and cnt datasets
somipa.verb <- survey(part.m, cnt.m %>% filter(cnt_type == "Non-physical"))

#build a contact matrix via sampling contact survey using bootstrapping
somipa.verb <- contact_matrix(
  somipa.verb,
  countries = c("Malawi"),
  survey.pop = survey.pop,
  age.limits = c(0, 1, 5, 15,20,50),
  filter = FALSE,
  n = 10,
  bootstrap = TRUE,
  counts = FALSE,
  symmetric = FALSE,
  split = FALSE,
  weigh.dayofweek = FALSE,
  sample.all.age.groups = FALSE,
  quiet = FALSE
)

#calculate the mean of matrices generated through bopostrapping for uncertainty
somipa.verb <- melt(Reduce("+", lapply(somipa.verb$matrices, function(x) {x$matrix})) / length(somipa.verb$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")

#ggplotting the matrices
C <- ggplot(somipa.verb, aes(x = Contact.age, y = Participant.age, fill = Mixing.rate)) + 
  geom_tile(color="white") + 
  labs(title = "C, non-physical", x = "Contacts age", y = "participants age") +
  theme(legend.position = "bottom") + 
  theme_bw()

#===========================================================================

A | B | C

ggsave(here("output", "Fig1_matrices.tiff"),
       plot = A | B | C,
       width = 14, height = 4, unit="in", dpi = 200)

