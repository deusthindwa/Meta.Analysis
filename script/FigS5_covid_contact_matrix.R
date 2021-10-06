#Last edited by - Deus Thindwa
#Date - 28/10/2019

# prepare a participant population (for null model of probability of contact under random mixing)
survey.poph <- read.csv(here::here("data", "survey_pop.csv"))

survey.poph <- survey.poph %>% mutate(lower.age.limit = if_else(age >= 0 & age < 5, 0,
                                                                if_else(age >= 5 & age < 16, 5,
                                                                        if_else(age >= 16 & age < 20, 16,
                                                                                if_else(age >= 20 & age < 30, 20,
                                                                                        if_else(age >= 30 & age < 40, 30,
                                                                                                if_else(age >= 40 & age <50, 40, 50)))))))
  
survey.poph <- survey.poph %>% group_by(lower.age.limit) %>% tally() %>% rename("population" = n)

#==========================contact matrix for low COVID-19 incidence

#create survey object by combining part and cnt datasets
somipa.c19no <- survey(part.m %>% mutate(datex = dmy(substr(date, 1, 10))) %>% filter(datex < date('2021-06-14')), cnt.m)

#build a contact matrix via sampling contact survey using bootstrapping
somipa.c19no <- contact_matrix(
  somipa.c19no,
  countries = c("Malawi"),
  survey.pop = survey.poph,
  age.limits = c(0, 5, 16, 20, 30, 40, 50),
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

# compute assortative index Q
Q_vec <- NA
for(i in 1:1000){
  Q_vec[i] <- (sum(diag(somipa.c19no$matrices[[i]]$matrix), na.rm = TRUE)-1)/(dim(somipa.c19no$matrices[[i]]$matrix)[1]*dim(somipa.c19no$matrices[[i]]$matrix)[2]-1)
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
somipa.c19no <- melt(Reduce("+", lapply(somipa.c19no$matrices, function(x) {x$matrix})) / length(somipa.c19no$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")


#==========================contact matrix for HIV negative participants

# create survey object by combining HIV-uninfected part and cnt datasets
somipa.c19yes <- survey(part.m %>% mutate(datex = dmy(substr(date, 1, 10))) %>% filter(datex >= date('2021-06-14')), cnt.m)

# build a contact matrix via sampling contact survey using bootstrapping
somipa.c19yes <- contact_matrix(
  somipa.c19yes,
  countries = c("Malawi"),
  survey.pop = survey.poph,
  age.limits = c(0, 5, 16, 20, 30, 40, 50),
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

# compute assortative index Q
Q_vec <- NA
for(i in 1:1000){
  Q_vec[i] <- (sum(diag(somipa.c19yes$matrices[[i]]$matrix), na.rm = TRUE)-1)/(dim(somipa.c19yes$matrices[[i]]$matrix)[1]*dim(somipa.c19yes$matrices[[i]]$matrix)[2]-1)
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
somipa.c19yes <- melt(Reduce("+", lapply(somipa.c19yes$matrices, function(x) {x$matrix})) / length(somipa.c19yes$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")

# ggplotting the matrices
somipa.c19no <- somipa.c19no %>% mutate(Category = "A, During low COVID-19 incidence (Q=0.383)")
somipa.c19yes <- somipa.c19yes %>% mutate(Category = "B, During high COVID-19 incidence (Q=0.407)")


A <- rbind(somipa.c19no, somipa.c19yes) %>%
  mutate(Participant.age = if_else(Participant.age == 2L, "[0,5)",
                                   if_else(Participant.age == 3L, "[5,16)",
                                           if_else(Participant.age == 4L, "[16,20)",
                                                   if_else(Participant.age == 5L, "[20,30)",
                                                           if_else(Participant.age == 6L, "[30,40)",
                                                                   if_else(Participant.age == 7L, "[40,50)", "50+"))))))) %>%
         
  ggplot(aes(x = factor(Participant.age,levels(factor(Participant.age))[c(1,6,2,3,4,5,7)]), y = Contact.age, fill = Mixing.rate)) + 
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

ggsave(here::here("output", "FigS5_covid_contacts_matrix.png"),
       plot = A,
       width = 14, height = 5, unit="in", dpi = 300)
