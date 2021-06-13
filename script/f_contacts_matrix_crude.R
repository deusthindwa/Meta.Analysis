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
survey.pop$lower.age.limit <- if_else(survey.pop$age < 1, 0,
                              if_else(survey.pop$age >= 1 & survey.pop$age <= 4, 1,
                              if_else(survey.pop$age > 4 & survey.pop$age <= 9, 5, 
                              if_else(survey.pop$age > 9 & survey.pop$age <= 14, 10, 
                              if_else(survey.pop$age > 14 & survey.pop$age <= 19, 15, 
                              if_else(survey.pop$age > 19 & survey.pop$age <= 24, 20,
                              if_else(survey.pop$age > 24 & survey.pop$age <= 29, 25, 
                              if_else(survey.pop$age > 29 & survey.pop$age <= 34, 30,
                              if_else(survey.pop$age > 34 & survey.pop$age <= 39, 35,
                              if_else(survey.pop$age > 39 & survey.pop$age <= 44, 40,
                              if_else(survey.pop$age > 44 & survey.pop$age <= 49, 45, 50)))))))))))
                              
survey.pop <- survey.pop %>% group_by(lower.age.limit) %>% tally()
survey.pop <- rename(survey.pop, "population" = "n")

# build a contact matrix via sampling contact survey using bootstrapping
somipa.all <- contact_matrix(
  somipa.all,
  countries = c("Malawi"),
  survey.pop = survey.pop,
  age.limits = c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50),
  filter = FALSE,
  n = 200,
  bootstrap = TRUE,
  counts = FALSE,
  symmetric = FALSE,
  split = FALSE,
  weigh.dayofweek = TRUE,
  sample.all.age.groups = FALSE,
  quiet = FALSE
)

# calculate the mean mixing rate of matrices generated through bopostrapping for uncertainty
Reduce("+", lapply(somipa.all$matrices, function(x) {x$matrix}))
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
  n = 200,
  bootstrap = TRUE,
  counts = FALSE,
  symmetric = FALSE,
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
  n = 200,
  bootstrap = TRUE,
  counts = FALSE,
  symmetric = FALSE,
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
ggplot(aes(x = Contact.age, y = Participant.age, fill = Mixing.rate)) + 
  geom_tile() + 
  theme_bw() +
  scale_fill_gradient(low="lightgreen", high="red") +
  facet_grid(.~ Category) +
  labs(title = "", x = "Contacts age (years)", y = "Participants age (years)") +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  theme(strip.text.x = element_text(size = 16)) +
  guides(fill=guide_legend(title="Average number\nof daily contacts")) +
  theme(legend.position = "right")

#===========================================================================

ggsave(here::here("output", "Fig1_matrices.tiff"),
       plot = A,
       width = 18, height = 6, unit="in", dpi = 200)

