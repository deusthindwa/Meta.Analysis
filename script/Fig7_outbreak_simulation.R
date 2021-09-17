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
                                           if_else(age > 4 & age <= 14, 5,
                                                   if_else(age > 14 & age <= 19, 15,
                                                           if_else(age > 19 & age <= 49, 20,50)))))) %>% 
  group_by(lower.age.limit) %>% tally() %>% rename("population" = n)

# build a contact matrix via sampling contact survey using bootstrapping
somipa.all <- contact_matrix(
  somipa.all,
  countries = c("Malawi"),
  survey.pop = survey.pop,
  age.limits = c(0, 1, 5, 15, 20, 50),
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



