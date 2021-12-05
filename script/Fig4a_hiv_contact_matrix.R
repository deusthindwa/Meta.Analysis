#Last edited by - Deus Thindwa
#Date - 28/10/2019

# prepare a participant population (for null model of probability of contact under random mixing)
survey.poph <- read.csv(here::here("data", "survey_pop.csv"))
survey.poph <- survey.poph %>% 
  mutate(lower.age.limit = if_else(age <=4, 0,
                                   if_else(age > 4 & age <= 9, 5,
                                           if_else(age > 9 & age <= 14, 10,
                                                   if_else(age > 14 & age <= 19, 15,
                                                           if_else(age > 19 & age <= 24, 20,
                                                                   if_else(age > 24 & age <= 29, 25,
                                                                           if_else(age > 29 & age <= 34, 30,
                                                                                   if_else(age > 34 & age <= 39, 35,
                                                                                           if_else(age > 39 & age <= 44, 40,
                                                                                                   if_else(age > 44 & age <= 49, 45, 50))))))))))) %>% 
  group_by(lower.age.limit) %>% tally() %>% rename("population" = n)

#==========================contact matrix for HIV positive participants

#create survey object by combining HIV-infected part and cnt datasets
somipa.pos <- survey(part.m %>% filter(part_hiv == "Positive on ART" & part_age >19), cnt.m)

#build a contact matrix via sampling contact survey using bootstrapping
set.seed = 1987
somipa.pos <- contact_matrix(
  somipa.pos,
  countries = c("Malawi"),
  survey.pop = survey.poph,
  #age.limits = c(0, 1, 5, 10, 18, 30, 35, 40, 45, 50, 60),
  age.limits = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50),
  filter = FALSE,
  n = 100,
  bootstrap = TRUE,
  counts = FALSE,
  symmetric = FALSE,
  split = FALSE,
  weigh.dayofweek = TRUE,
  sample.all.age.groups = FALSE,
  missing.participant.age = "remove",
  missing.contact.age = "sample",
  quiet = FALSE
)

#calculate the mean of matrices generated through bootstrapping for uncertainty
somipa.pos <- melt(Reduce("+", lapply(somipa.pos$matrices, function(x) {x$matrix})) / length(somipa.pos$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")

#---------------------------------------------------------------

# prepare a participant population (for null model of probability of contact under random mixing) for Q calculation
survey.popQ <- read.csv(here::here("data", "survey_pop.csv"))

survey.popQ <- survey.popQ %>%
  filter(age >19) %>%
  mutate(lower.age.limit = if_else(age > 19 & age <= 24, 20,
                                   if_else(age > 24 & age <= 29, 25,
                                           if_else(age > 29 & age <= 34, 30,
                                                   if_else(age > 34 & age <= 39, 35,
                                                           if_else(age > 39 & age <= 44, 40,
                                                                   if_else(age > 44 & age <= 49, 45, 50))))))) %>%
  group_by(lower.age.limit) %>% tally() %>% rename("population" = n)

#create survey object by combining HIV-infected part and cnt datasets
somipa.posQ <- survey(part.m %>% filter(part_hiv == "Positive on ART" & part_age >19), cnt.m %>% filter(cnt_age >19 ))

#build a contact matrix via sampling contact survey using bootstrapping
set.seed = 1987
somipa.posQ <- contact_matrix(
  somipa.posQ,
  countries = c("Malawi"),
  survey.pop = survey.popQ,
  age.limits = c(20, 25, 30, 35, 40, 45, 50),
  filter = FALSE,
  n = 1000,
  bootstrap = TRUE,
  counts = FALSE,
  symmetric = TRUE,
  split = FALSE,
  weigh.dayofweek = TRUE,
  sample.all.age.groups = TRUE,
  missing.participant.age = "remove",
  missing.contact.age = "sample",
  quiet = FALSE
)

# combine all 1000 matrices through mean
somipa.posQ <- Reduce("+", lapply(somipa.posQ$matrices, function(x) {x$matrix})) / length(somipa.posQ$matrices)

# column-wise matrix normalization method 1 (P) and assortative index (Q)
P <- (somipa.posQ)/rowSums(somipa.posQ)[row(somipa.posQ)]
Q7 <- (sum(diag(P), na.rm = TRUE)-1)/(dim(P)[1]-1)


#==========================contact matrix for HIV negative participants

#create survey object by combining HIV-infected part and cnt datasets
somipa.neg <- survey(part.m %>% filter(part_hiv == "Negative" & part_age >19), cnt.m)

#build a contact matrix via sampling contact survey using bootstrapping
set.seed = 1987
somipa.neg <- contact_matrix(
  somipa.neg,
  countries = c("Malawi"),
  survey.pop = survey.poph,
  #age.limits = c(0, 1, 5, 10, 18, 30, 35, 40, 45, 50, 60),
  age.limits = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50),
  filter = FALSE,
  n = 1000,
  bootstrap = TRUE,
  counts = FALSE,
  symmetric = FALSE,
  split = FALSE,
  weigh.dayofweek = TRUE,
  sample.all.age.groups = FALSE,
  missing.participant.age = "remove",
  missing.contact.age = "sample",
  quiet = FALSE
)

#calculate the mean of matrices generated through bootstrapping for uncertainty
somipa.neg <- melt(Reduce("+", lapply(somipa.neg$matrices, function(x) {x$matrix})) / length(somipa.neg$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")

#---------------------------------------------------------------

#create survey object by combining HIV-infected part and cnt datasets
somipa.negQ <- survey(part.m %>% filter(part_hiv == "Negative" & part_age >19), cnt.m %>% filter(cnt_age >19 ))

#build a contact matrix via sampling contact survey using bootstrapping
set.seed = 1987
somipa.negQ <- contact_matrix(
  somipa.negQ,
  countries = c("Malawi"),
  survey.pop = survey.popQ,
  age.limits = c(20, 25, 30, 35, 40, 45, 50),
  filter = FALSE,
  n = 1000,
  bootstrap = TRUE,
  counts = FALSE,
  symmetric = TRUE,
  split = FALSE,
  weigh.dayofweek = TRUE,
  sample.all.age.groups = TRUE,
  missing.participant.age = "remove",
  missing.contact.age = "sample",
  quiet = FALSE
)

# combine all 1000 matrices through mean
somipa.negQ <- Reduce("+", lapply(somipa.negQ$matrices, function(x) {x$matrix})) / length(somipa.negQ$matrices)

# column-wise matrix normalization method 1 (P) and assortative index (Q)
P <- (somipa.negQ)/rowSums(somipa.negQ)[row(somipa.negQ)]
Q8 <- (sum(diag(P), na.rm = TRUE)-1)/(dim(P)[1]-1)

