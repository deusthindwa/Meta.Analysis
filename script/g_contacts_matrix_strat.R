#Last edited by - Deus Thindwa
#Date - 28/10/2019

#==========================contact matrix for HIV negative participants

#create survey object by combining separate male and female part and cnt datasets
somipa.neg <- survey(part.m %>% filter(hiv == "Negative"), cnt.m)

#build a contact matrix via sampling contact survey using bootstrapping
somipa.neg <- contact_matrix(
  somipa.neg,
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
somipa.neg <- melt(Reduce("+", lapply(somipa.neg$matrices, function(x) {x$matrix})) / length(somipa.neg$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")


# contact matrix for HIV positive participants

# create survey object by combining separate male and female part and cnt datasets
somipa.pos <- survey(part.m %>% filter(hiv == "Positive on ART"), cnt.m)

# build a contact matrix via sampling contact survey using bootstrapping
somipa.pos <- contact_matrix(
  somipa.pos,
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

# calculate the mean of matrices generated through bopostrapping for uncertainty
somipa.pos <- melt(Reduce("+", lapply(somipa.pos$matrices, function(x) {x$matrix})) / length(somipa.pos$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")


# contact matrix for within household

# create survey object for household location
somipa.whh <- survey(part.m, cnt.m %>% filter(cnt_loc == "Home"))

# build a contact matrix via sampling contact survey using bootstrapping
somipa.whh <- contact_matrix(
  somipa.whh,
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

# calculate the mean of matrices generated through bopostrapping for uncertainty
somipa.whh <- melt(Reduce("+", lapply(somipa.whh$matrices, function(x) {x$matrix})) / length(somipa.whh$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")


# contact matrix for outside household

# create survey object for household location
somipa.ohh <- survey(part.m, cnt.m %>% filter(cnt_loc != "Home"))

# build a contact matrix via sampling contact survey using bootstrapping
somipa.ohh <- contact_matrix(
  somipa.ohh,
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

# calculate the mean of matrices generated through bopostrapping for uncertainty
somipa.ohh <- melt(Reduce("+", lapply(somipa.ohh$matrices, function(x) {x$matrix})) / length(somipa.ohh$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")


# contact matrix for within community

# create survey object community location
somipa.wcom <- survey(part.m, cnt.m %>% filter(cnt_plc != "Within community"))

# build a contact matrix via sampling contact survey using bootstrapping
somipa.wcom <- contact_matrix(
  somipa.wcom,
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

# calculate the mean of matrices generated through bopostrapping for uncertainty
somipa.wcom <- melt(Reduce("+", lapply(somipa.wcom$matrices, function(x) {x$matrix})) / length(somipa.wcom$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")


# contact matrix for outside community

# create survey object for community location
somipa.ocom <- survey(part.m, cnt.m %>% filter(cnt_plc != "Outside community"))

# build a contact matrix via sampling contact survey using bootstrapping
somipa.ocom <- contact_matrix(
  somipa.ocom,
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

# calculate the mean of matrices generated through bopostrapping for uncertainty
somipa.ocom <- melt(Reduce("+", lapply(somipa.ocom$matrices, function(x) {x$matrix})) / length(somipa.ocom$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")

# ggplotting the matrices
somipa.neg <- somipa.neg %>% mutate(Category = "A, HIV-uninfected")
somipa.pos <- somipa.pos %>% mutate(Category = "B, HIV-infected")
somipa.whh <- somipa.whh %>% mutate(Category = "C, within household")
somipa.ohh <- somipa.ohh %>% mutate(Category = "D, outside household")
somipa.wcom <- somipa.wcom %>% mutate(Category = "E, within community")
somipa.ocom <- somipa.ocom %>% mutate(Category = "F, outside community")

A <- rbind(somipa.neg, somipa.whh, somipa.wcom) %>%
  ggplot(aes(x = Contact.age, y = Participant.age, fill = Mixing.rate)) + 
  geom_tile() + 
  theme_bw() +
  scale_fill_gradient(low="lightgreen", high="red") +
  facet_grid(.~ Category) +
  labs(title = "", x = "Contacts age", y = "Participants age") +
  theme(axis.text.x = element_text(face = "bold", size = 12), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(strip.text.x = element_text(size = 14)) +
  theme(legend.position = "right")

B <- rbind(somipa.pos, somipa.ohh, somipa.ocom) %>%
  ggplot(aes(x = Contact.age, y = Participant.age, fill = Mixing.rate)) + 
  geom_tile() + 
  theme_bw() +
  scale_fill_gradient(low="lightgreen", high="red") +
  facet_grid(.~ Category) +
  labs(title = "", x = "Contacts age", y = "Participants age") +
  theme(axis.text.x = element_text(face = "bold", size = 12), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(strip.text.x = element_text(size = 14)) +
  theme(legend.position = "right")

#===========================================================================


A / B

ggsave(here("output", "Fig2_matrices.tiff"),
       plot = (A / B),
       width = 14, height = 7, unit="in", dpi = 200)
