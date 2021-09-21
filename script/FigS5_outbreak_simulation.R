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
                                   if_else(age >= 1 & age < 5, 1,
                                           if_else(age >= 5 & age < 15, 5,
                                                   if_else(age >= 15 & age < 20, 15,
                                                           if_else(age >= 20 & age <50, 20, 50)))))) %>% 
  
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
somipa.all <- somipa.all %>%
  mutate(Participant.age = if_else(Participant.age == 1L, "[0,1)",
                            if_else(Participant.age == 2L, "[1,5)",
                                    if_else(Participant.age == 3L, "[5,15)",
                                            if_else(Participant.age == 4L, "[15,20)",
                                                    if_else(Participant.age == 5L, "[20,50)", "50+"))))))

# plot the contact matirix
somipa.all %>%
ggplot(aes(x = factor(Participant.age,levels(factor(Participant.age))[c(1,2,5,3,4,6)]), y = Contact.age, fill = Mixing.rate)) + 
  geom_tile(color = "white") + 
  geom_text(aes(label = sprintf("%1.2f", Mixing.rate)), color = "white", size = 2) +
  scale_fill_gradient(low="lightgreen", high="red") +
  theme_bw() +
  labs(title = "", x = "Participant age (years)", y = "Contactee age (years)") +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  theme(strip.text.x = element_text(size = 16)) +
  theme(legend.position = "right") + 
  geom_vline(xintercept = c(2.5, 5.5, 12), linetype="dashed", color = "black", size = 0.2) +
  geom_hline(yintercept = c(2.5, 5.5, 12), linetype="dashed", color = "black", size = 0.2)



