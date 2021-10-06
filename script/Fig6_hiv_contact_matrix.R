#Last edited by - Deus Thindwa
#Date - 28/10/2019

# prepare a participant population (for null model of probability of contact under random mixing)
survey.poph <- read.csv(here::here("data", "survey_pop.csv"))

survey.poph$lower.age.limit <- if_else(survey.poph$age >= 0 & survey.poph$age < 1, 0,
                                      if_else(survey.poph$age >= 1 & survey.poph$age <= 4, 1,
                                              if_else(survey.poph$age > 4 & survey.poph$age <= 9, 5, 
                                                      if_else(survey.poph$age > 9 & survey.poph$age <= 17, 10, 
                                                              if_else(survey.poph$age > 17 & survey.poph$age <= 29, 18, 
                                                                      if_else(survey.poph$age > 29 & survey.poph$age <= 34, 30, 
                                                                              if_else(survey.poph$age >34 & survey.poph$age <= 39, 35,
                                                                                      if_else(survey.poph$age > 39 & survey.poph$age <= 44, 40,
                                                                                              if_else(survey.poph$age > 44 & survey.poph$age <= 49, 45, 
                                                                                                      if_else(survey.poph$age > 49 & survey.poph$age <= 59, 50, 60))))))))))

survey.poph <- survey.poph %>% group_by(lower.age.limit) %>% tally() %>% rename("population" = n)

#==========================contact matrix for HIV positive participants

#create survey object by combining HIV-infected part and cnt datasets
somipa.pos <- survey(part.m %>% filter(part_hiv == "Positive on ART"), cnt.m)

#build a contact matrix via sampling contact survey using bootstrapping
somipa.pos <- contact_matrix(
  somipa.pos,
  countries = c("Malawi"),
  survey.pop = survey.poph,
  age.limits = c(0, 1, 5, 10, 18, 30, 35, 40, 45, 50, 60),
  filter = FALSE,
  n = 1000,
  bootstrap = TRUE,
  counts = FALSE,
  symmetric = FALSE,
  split = FALSE,
  weigh.dayofweek = TRUE,
  sample.all.age.groups = FALSE,
  quiet = FALSE
)

# compute assortative index Q
Q_vec <- NA
for(i in 1:1000){
  Q_vec[i] <- (sum(diag(somipa.pos$matrices[[i]]$matrix), na.rm = TRUE)-1)/(dim(somipa.pos$matrices[[i]]$matrix)[1]*dim(somipa.pos$matrices[[i]]$matrix)[2]-1)
}
Q_vec <- as.data.frame(Q_vec)

QConf <- function (x, ci = 0.95){
  Margin_Error <- abs(qnorm((1-ci)/2))* sd(x)/sqrt(length(x))
  df_out <- data.frame( Mean=mean(x), 'LCI' = (mean(x) - Margin_Error), 'UCI' = (mean(x) + Margin_Error)) %>% 
    tidyr::pivot_longer(names_to = "Measurements", values_to ="values", 1:3 )
  return(df_out)
}
QConf(Q_vec$Q_vec)


#calculate the mean of matrices generated through bopostrapping for uncertainty
somipa.pos <- melt(Reduce("+", lapply(somipa.pos$matrices, function(x) {x$matrix})) / length(somipa.pos$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")


#==========================contact matrix for HIV negative participants

# create survey object by combining HIV-uninfected part and cnt datasets
somipa.neg <- survey(part.m %>% filter(part_hiv == "Negative"), cnt.m)

# build a contact matrix via sampling contact survey using bootstrapping
somipa.neg <- contact_matrix(
  somipa.neg,
  countries = c("Malawi"),
  survey.pop = survey.poph,
  age.limits = c(0, 1, 5, 10, 18, 30, 35, 40, 45, 50, 60),
  filter = FALSE,
  n = 1000,
  bootstrap = TRUE,
  counts = FALSE,
  symmetric = FALSE,
  split = FALSE,
  weigh.dayofweek = TRUE,
  sample.all.age.groups = FALSE,
  quiet = FALSE
)

# compute assortative index Q
Q_vec <- NA
for(i in 1:1000){
  Q_vec[i] <- (sum(diag(somipa.neg$matrices[[i]]$matrix), na.rm = TRUE)-1)/(dim(somipa.neg$matrices[[i]]$matrix)[1]*dim(somipa.neg$matrices[[i]]$matrix)[2]-1)
}
Q_vec <- as.data.frame(Q_vec)

QConf <- function (x, ci = 0.95){
  Margin_Error <- abs(qnorm((1-ci)/2))* sd(x)/sqrt(length(x))
  df_out <- data.frame( Mean=mean(x), 'LCI' = (mean(x) - Margin_Error), 'UCI' = (mean(x) + Margin_Error)) %>% 
    tidyr::pivot_longer(names_to = "Measurements", values_to ="values", 1:3 )
  return(df_out)
}
QConf(Q_vec$Q_vec)


# calculate the mean of matrices generated through bopostrapping for uncertainty
somipa.neg <- melt(Reduce("+", lapply(somipa.neg$matrices, function(x) {x$matrix})) / length(somipa.neg$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")

# ggplotting the matrices
somipa.neg <- somipa.neg %>% mutate(Category = "A, HIV-uninfected (Q=0.114)")
somipa.pos <- somipa.pos %>% mutate(Category = "B, HIV-infected on ART (Q=0.112)")


A <- filter(rbind(somipa.neg, somipa.pos), !is.na(Mixing.rate)) %>%
  ggplot(aes(x = Participant.age, y = Contact.age, fill = Mixing.rate)) + 
  geom_tile(color = "white") + 
  geom_text(aes(label = sprintf("%1.2f", Mixing.rate)), color = "white", size = 2) +
  theme_bw() +
  scale_fill_gradient(low="lightgreen", high="red") +
  facet_grid(.~ Category) +
  labs(title = "", x = "Participant age (years)", y = "Contactee age (years)") +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  theme(strip.text.x = element_text(size = 14), strip.text.y = element_text(size = 14), strip.background=element_rect(fill="white")) +
  scale_color_grey() +
  geom_hline(yintercept = c(2.5, 4.5, 5.5, 7.5), linetype="dashed", color = "black", size = 0.2)

#===========================================================================

ggsave(here::here("output", "Fig6_hiv_contact_matrix.png"),
       plot = A,
       width = 14, height = 5, unit="in", dpi = 300)
