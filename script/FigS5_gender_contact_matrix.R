#Last edited by - Deus Thindwa
#Date - 28/10/2019

#==========================contact matrix for males participants

#recreate contact sex dataset e.g cnt.m.sex
cnt.m.sex <- left_join(cnt.m, cn.labeled %>% select(somipa_pid, cnt_age, cnt_sex) %>% rename("part_id" = "somipa_pid"))

# prepare a participant population (for null model of probability of contact under random mixing)
survey.pop <- read.csv(here::here("data", "survey_pop.csv"))
survey.pop <- survey.pop %>% 
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

#==========================contact matrix for male participants male contacts

#create survey object by combining separate male participants and their male comntacts
somipa.sexmm <- survey(part.m %>% filter(part_sex == "Male"), cnt.m.sex %>% filter(cnt_sex == "Male"))

#build a contact matrix via sampling contact survey using bootstrapping
somipa.sexmm <- contact_matrix(
  somipa.sexmm,
  countries = c("Malawi"),
  survey.pop = survey.pop,
  age.limits = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50),
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
somipa.sexmm <- Reduce("+", lapply(somipa.sexmm$matrices, function(x) {x$matrix})) / length(somipa.sexmm$matrices)

# column-wise matrix normalization method 1 (P) and assortative index (Q)
P <- (somipa.sexmm)/rowSums(somipa.sexmm)[row(somipa.sexmm)]
Q1 <- (sum(diag(P))-1)/(dim(P)[1]-1)

#calculate the mean of matrices generated through bootstrapping for uncertainty
somipa.sexmm <- melt(somipa.sexmm, varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")


#==========================contact matrix for female participants male contacts

#create survey object by combining separate female participants and their male comntacts
somipa.sexfm <- survey(part.m %>% filter(part_sex == "Female"), cnt.m.sex %>% filter(cnt_sex == "Male"))

#build a contact matrix via sampling contact survey using bootstrapping
somipa.sexfm <- contact_matrix(
  somipa.sexfm,
  countries = c("Malawi"),
  survey.pop = survey.pop,
  age.limits = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50),
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
somipa.sexfm <- Reduce("+", lapply(somipa.sexfm$matrices, function(x) {x$matrix})) / length(somipa.sexfm$matrices)

# column-wise matrix normalization method 1 (P) and assortative index (Q)
P <- (somipa.sexfm)/rowSums(somipa.sexfm)[row(somipa.sexfm)]
Q2 <- (sum(diag(P))-1)/(dim(P)[1]-1)

#calculate the mean of matrices generated through bootstrapping for uncertainty
somipa.sexfm <- melt(somipa.sexfm, varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")


#==========================contact matrix for male participants female contacts

#create survey object by combining separate male participants and their female contacts
somipa.sexmf <- survey(part.m %>% filter(part_sex == "Male"), cnt.m.sex %>% filter(cnt_sex == "Female"))

#build a contact matrix via sampling contact survey using bootstrapping
somipa.sexmf <- contact_matrix(
  somipa.sexmf,
  countries = c("Malawi"),
  survey.pop = survey.pop,
  age.limits = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50),
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
somipa.sexmf <- Reduce("+", lapply(somipa.sexmf$matrices, function(x) {x$matrix})) / length(somipa.sexmf$matrices)

# column-wise matrix normalization method 1 (P) and assortative index (Q)
P <- (somipa.sexmf)/rowSums(somipa.sexmf)[row(somipa.sexmf)]
Q3 <- (sum(diag(P))-1)/(dim(P)[1]-1)

#calculate the mean of matrices generated through bootstrapping for uncertainty
somipa.sexmf <- melt(somipa.sexmf, varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")


#==========================contact matrix for female participants female contacts

#create survey object by combining separate male participants and their female contacts
somipa.sexff <- survey(part.m %>% filter(part_sex == "Female"), cnt.m.sex %>% filter(cnt_sex == "Female"))

#build a contact matrix via sampling contact survey using bootstrapping
somipa.sexff <- contact_matrix(
  somipa.sexff,
  countries = c("Malawi"),
  survey.pop = survey.pop,
  age.limits = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50),
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
somipa.sexff <- Reduce("+", lapply(somipa.sexff$matrices, function(x) {x$matrix})) / length(somipa.sexff$matrices)

# column-wise matrix normalization method 1 (P) and assortative index (Q)
P <- (somipa.sexff)/rowSums(somipa.sexff)[row(somipa.sexff)]
Q4 <- (sum(diag(P))-1)/(dim(P)[1]-1)

#calculate the mean of matrices generated through bootstrapping for uncertainty
somipa.sexff <- melt(somipa.sexff, varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")


#==========================combine all the datasets for ggplotting

# ggplotting the matrices
somipa.sexmm <- somipa.sexmm %>% mutate(part_cat = "Male", cnt_cat = "Male", Assortativity = paste0("Male-Male, ", round(Q1, 3)))
somipa.sexfm <- somipa.sexfm %>% mutate(part_cat = "Female", cnt_cat = "Male", Assortativity = paste0("Female-Male, ", round(Q2, 3)))
somipa.sexmf <- somipa.sexmf %>% mutate(part_cat = "Male", cnt_cat = "Female", Assortativity = paste0("Male-Female, ", round(Q3, 3)))
somipa.sexff <- somipa.sexff %>% mutate(part_cat = "Female", cnt_cat = "Female", Assortativity = paste0("Female-Female, ", round(Q4, 3)))

#somipa.sexmm <- somipa.sexmm %>% mutate(Category = paste0("A, Male, Q=", round(Q1, 3)), " | ", "Male, Q=", round(Q3, 3)), subtitle = "Male")
#somipa.sexfm <- somipa.sexfm %>% mutate(Category = paste0("A, Female, Q=", round(Q2, 3), " | ", "Female, Q=", round(Q4, 3)), subtitle = "Male")
#somipa.sexmf <- somipa.sexmf %>% mutate(Category = paste0("A, Male, Q=", round(Q1, 3), " | ", "Male, Q=", round(Q3, 3)), subtitle = "Female")
#somipa.sexff <- somipa.sexff %>% mutate(Category = paste0("A, Female, Q=", round(Q2, 3), " | ", "Female, Q=", round(Q4, 3)), subtitle = "Male")

A <- rbind(somipa.sexmm, somipa.sexfm, somipa.sexmf, somipa.sexff) %>%
  mutate(part.age = if_else(Participant.age == 1L, "[0,5)",
                            if_else(Participant.age == 2L, "[5,10)",
                                    if_else(Participant.age == 3L, "[10,15)",
                                            if_else(Participant.age == 4L, "[15,20)",
                                                    if_else(Participant.age == 5L, "[20,25)",
                                                            if_else(Participant.age == 6L, "[25,30)",
                                                                    if_else(Participant.age == 7L, "[30,35)",
                                                                            if_else(Participant.age == 8L, "[35,40)",
                                                                                    if_else(Participant.age == 9L, "[40,45)",
                                                                                            if_else(Participant.age == 10L, "[45,50)", "50+")))))))))),
         `Daily average contacts` = if_else(is.na(Mixing.rate), 0, Mixing.rate),
         `Assortativity index, Q` = Assortativity) %>%
  
  ggplot(aes(x = factor(part.age, levels(factor(part.age))[c(1,10,2,3,4,5,6,7,8,9,11)]), y = Contact.age, fill = `Daily average contacts`)) + 
  geom_tile(aes(color = `Assortativity index, Q`), size = 0) + 
  geom_text(aes(label = sprintf("%1.2f", `Daily average contacts`)), color = "white", size = 4) +
  scale_fill_gradient(low = "gray30", high = "red") +
  facet_grid(part_cat ~ cnt_cat) +
  theme_bw() +
  labs(title = "", x = "Participant age (years)", y = "Contact age (years)") +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  theme(strip.text.x = element_text(size = 18), strip.text.y = element_text(size = 18), strip.background = element_rect(fill="white")) +
  theme(legend.position = "right", legend.text=element_text(size = 12), legend.title = element_text(size = 12))


#===========================================================================

ggsave(here::here("output", "FigS5_gender_contact_matrix.png"),
       plot = (A),
       width = 20, height = 12, unit="in", dpi = 300)
