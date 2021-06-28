#Last edited by - Deus Thindwa
#Date - 28/10/2019

# prepare a participant population (for null model of probability of contact under random mixing)
survey.poph <- read.csv(here::here("data", "survey_pop.csv"))
<<<<<<< HEAD
survey.poph <- survey.poph %>% 
  mutate(lower.age.limit = if_else(age >= 0 & age < 1, 0, 
                                   if_else(age >= 1 & age <= 4, 1,
                                           if_else(age > 4 & age <= 9, 5, 
                                                   if_else(age > 9 & age <= 17, 10, 
                                                           if_else(age > 17 & age <= 24, 18,
                                                                   if_else(age > 24 & age <= 34, 25,
                                                                           if_else(age > 34 & age <= 44, 35, 
                                                                                   if_else(age > 44 & age <= 54, 45, 55))))))))) %>% 
  group_by(lower.age.limit) %>% tally() %>% rename("population" = n)


#==========================contact matrix for HIV positive participants
=======
survey.poph$lower.age.limit <- if_else(survey.poph$age >= 0 & survey.poph$age < 1, 0,
                                      if_else(survey.poph$age >= 1 & survey.poph$age <= 4, 1,
                                              if_else(survey.poph$age > 4 & survey.poph$age <= 9, 5, 
                                                      if_else(survey.poph$age > 9 & survey.poph$age <= 17, 10, 
                                                              if_else(survey.poph$age > 17 & survey.poph$age <= 29, 18, 
                                                                      if_else(survey.poph$age > 29 & survey.poph$age <= 39, 30, 
                                                                              if_else(survey.poph$age > 39 & survey.poph$age <= 49, 40, 
                                                                                      if_else(survey.poph$age > 49 & survey.poph$age <= 59, 50, 60))))))))

#==========================contact matrix for HIV negative participants
>>>>>>> 817de1ac7a314dab3741e19db8914b64062ef14e

#create survey object by combining HIV-infected part and cnt datasets
somipa.pos <- survey(part.m %>% filter(hiv == "Positive on ART"), cnt.m)

#build a contact matrix via sampling contact survey using bootstrapping
somipa.pos <- contact_matrix(
  somipa.pos,
  countries = c("Malawi"),
  survey.pop = survey.poph,
<<<<<<< HEAD
  age.limits = c(0, 1, 5, 10, 18, 25, 35, 45, 55),
=======
  age.limits = c(0, 1, 5, 10, 18, 30, 40, 50, 60),
>>>>>>> 817de1ac7a314dab3741e19db8914b64062ef14e
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

#calculate the mean of matrices generated through bopostrapping for uncertainty
somipa.pos <- melt(Reduce("+", lapply(somipa.pos$matrices, function(x) {x$matrix})) / length(somipa.pos$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")


#==========================contact matrix for HIV negative participants

# create survey object by combining HIV-uninfected part and cnt datasets
somipa.neg <- survey(part.m %>% filter(hiv == "Negative"), cnt.m)

# build a contact matrix via sampling contact survey using bootstrapping
somipa.neg <- contact_matrix(
  somipa.neg,
  countries = c("Malawi"),
  survey.pop = survey.poph,
<<<<<<< HEAD
  age.limits = c(0, 1, 5, 10, 18, 25, 35, 45, 55),
=======
  age.limits = c(0, 1, 5, 10, 18, 30, 40, 50, 60),
>>>>>>> 817de1ac7a314dab3741e19db8914b64062ef14e
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
somipa.neg <- melt(Reduce("+", lapply(somipa.neg$matrices, function(x) {x$matrix})) / length(somipa.neg$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")

# ggplotting the matrices
somipa.neg <- somipa.neg %>% mutate(Category = "A, HIV-uninfected")
somipa.pos <- somipa.pos %>% mutate(Category = "B, HIV-infected on ART")

B <- rbind(somipa.neg, somipa.pos) %>%
  ggplot(aes(x = Participant.age, y = Contact.age, fill = Mixing.rate)) + 
  geom_tile() + 
  theme_bw() +
  scale_fill_gradient(low="lightgreen", high="red") +
  facet_grid(.~ Category) +
  labs(title = "", x = "Participants age (years)", y = "Contacts age (years)") +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  theme(strip.text.x = element_text(size = 14)) +
  guides(fill=guide_legend(title="Average number\nof daily contacts")) +
  theme(legend.position = "right") + 
  geom_hline(yintercept = c(2, 4, 7), linetype="dashed", color = "black", size = 0.2)

#===========================================================================

ggsave(here::here("output", "Fig6_matrices.tiff"),
       plot = B,
       width = 14, height = 5, unit="in", dpi = 200)
