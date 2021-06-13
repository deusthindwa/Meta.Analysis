#Last edited by - Deus Thindwa
#Date - 28/10/2019

#==========================contact matrix for HIV negative participants

#create survey object by combining separate male and female part and cnt datasets
somipa.pos <- survey(part.m %>% filter(hiv == "Positive on ART"), cnt.m)

#build a contact matrix via sampling contact survey using bootstrapping
somipa.pos <- contact_matrix(
  somipa.pos,
  countries = c("Malawi"),
  survey.pop = survey.pop,
  age.limits = c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50),
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
somipa.pos <- melt(Reduce("+", lapply(somipa.pos$matrices, function(x) {x$matrix})) / length(somipa.pos$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")


#==========================contact matrix for HIV negative participants

# create survey object by combining separate male and female part and cnt datasets
somipa.neg <- survey(part.m %>% filter(hiv == "Negative"), cnt.m)

# build a contact matrix via sampling contact survey using bootstrapping
somipa.neg <- contact_matrix(
  somipa.neg,
  countries = c("Malawi"),
  survey.pop = survey.pop,
  age.limits = c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50),
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
  labs(title = "", x = "", y = "Contacts age") +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  theme(strip.text.x = element_text(size = 14)) +
  guides(fill=guide_legend(title="Average number\nof daily contacts")) +
  theme(legend.position = "right") + 
  geom_vline(xintercept = c(2.5, 5.5, 12), linetype="dashed", color = "black", size = 0.2) +
  geom_hline(yintercept = c(2.5, 5.5, 12), linetype="dashed", color = "black", size = 0.2)

#===========================================================================

ggsave(here::here("output", "Fig5_matrices.tiff"),
       plot = B,
       width = 14, height = 5, unit="in", dpi = 200)
