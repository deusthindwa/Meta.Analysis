# Last edited by - Deus Thindwa
# Date - 28/10/2019

# overall contact matrix
somipa.all <- survey(part.m %>% filter(part_cvd == "Yes"), cnt.m)

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
survey.pop <- survey.pop %>% mutate(lower.age.limit = if_else(age >= 0 & age < 5, 0,
                                                              if_else(age >= 5 & age < 16, 5,
                                                                      if_else(age >= 16 & age < 20, 16,
                                                                              if_else(age >= 20 & age < 30, 20,
                                                                                      if_else(age >= 30 & age < 40, 30,
                                                                                              if_else(age >= 40 & age <50, 40, 50)))))))

survey.pop <- survey.pop %>% group_by(lower.age.limit) %>% tally() %>% rename("population" = n)

# build a contact matrix via sampling contact survey using bootstrapping
somipa.all <- contact_matrix(
  somipa.all,
  countries = c("Malawi"),
  survey.pop = survey.pop,
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
  Q_vec[i] <- (sum(diag(somipa.all$matrices[[i]]$matrix), na.rm = TRUE)-1)/(dim(somipa.all$matrices[[i]]$matrix)[1]*dim(somipa.all$matrices[[i]]$matrix)[2]-1)
}
Q_vec <- as.data.frame(Q_vec)

QConf <- function (x, ci = 0.95){
  Margin_Error <- abs(qnorm((1-ci)/2))* sd(x)/sqrt(length(x))
  df_out <- data.frame( Mean=mean(x), 'LCI' = (mean(x) - Margin_Error), 'UCI' = (mean(x) + Margin_Error)) %>% 
    tidyr::pivot_longer(names_to = "Measurements", values_to ="values", 1:3 )
  return(df_out)
}
QConf(Q_vec$Q_vec)

# calculate the mean mixing rate of matrices generated through bootstrapping for uncertainty
somipa.all <- melt(Reduce("+", lapply(somipa.all$matrices, function(x) {x$matrix})) / length(somipa.all$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")

#=============================================================================================

# physical contact matrix

# create survey object by combining part and cnt datasets for those who reported COVID19 restrictions
somipa.phys <- survey(part.m %>% filter(part_cvd == "Yes"), cnt.m %>% filter(cnt_type == "Physical"))

# build a contact matrix via sampling contact survey using bootstrapping
somipa.phys <- contact_matrix(
  somipa.phys,
  countries = c("Malawi"),
  survey.pop = survey.pop,
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
  Q_vec[i] <- (sum(diag(somipa.phys$matrices[[i]]$matrix), na.rm = TRUE)-1)/(dim(somipa.phys$matrices[[i]]$matrix)[1]*dim(somipa.phys$matrices[[i]]$matrix)[2]-1)
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
somipa.phys <- melt(Reduce("+", lapply(somipa.phys$matrices, function(x) {x$matrix})) / length(somipa.phys$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")

#=============================================================================================

# non-physical contact matrix

# create survey object by combining part and cnt datasets for those who reported COVID19 restrictions
somipa.verb <- survey(part.m %>% filter(part_cvd == "Yes"), cnt.m %>% filter(cnt_type == "Non-physical"))

# build a contact matrix via sampling contact survey using bootstrapping
somipa.verb <- contact_matrix(
  somipa.verb,
  countries = c("Malawi"),
  survey.pop = survey.pop,
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
  Q_vec[i] <- (sum(diag(somipa.verb$matrices[[i]]$matrix), na.rm = TRUE)-1)/(dim(somipa.verb$matrices[[i]]$matrix)[1]*dim(somipa.verb$matrices[[i]]$matrix)[2]-1)
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
somipa.verb <- melt(Reduce("+", lapply(somipa.verb$matrices, function(x) {x$matrix})) / length(somipa.verb$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")

#=============================================================================================

# make combined dataset for COVID-19 restrictions

somipa.all <- somipa.all %>% mutate(Category = "A, All contacts (Q=0.425 | Q=0.398)")
somipa.phys <- somipa.phys %>% mutate(Category = "B, Physical contacts (Q=0.275 | Q=0.250)")
somipa.verb <- somipa.verb %>% mutate(Category = "C, Non-physical contacts (Q=0.129 | Q=0.127)")
A <- rbind(somipa.all, somipa.phys, somipa.verb) %>%
  mutate(Participant.age = if_else(Participant.age == 2L, "[0,5)",
                                   if_else(Participant.age == 3L, "[5,16)",
                                           if_else(Participant.age == 4L, "[16,20)",
                                                   if_else(Participant.age == 5L, "[20,30)",
                                                           if_else(Participant.age == 6L, "[30,40)",
                                                                   if_else(Participant.age == 7L, "[40,50)", "50+")))))),
         
         subtit = "Affected by COVID-19")


#=============================================================================================
#=============================================================================================

# overall contact matrix
somipa.all <- survey(part.m %>% filter(part_cvd == "No"), cnt.m)

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
survey.pop <- survey.pop %>% mutate(lower.age.limit = if_else(age >= 0 & age < 5, 0,
                                                              if_else(age >= 5 & age < 16, 5,
                                                                      if_else(age >= 16 & age < 20, 16,
                                                                              if_else(age >= 20 & age < 30, 20,
                                                                                      if_else(age >= 30 & age < 40, 30,
                                                                                              if_else(age >= 40 & age <50, 40, 50)))))))
survey.pop <- survey.pop %>% group_by(lower.age.limit) %>% tally() %>% rename("population" = n)

# build a contact matrix via sampling contact survey using bootstrapping
somipa.all <- contact_matrix(
  somipa.all,
  countries = c("Malawi"),
  survey.pop = survey.pop,
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
  Q_vec[i] <- (sum(diag(somipa.all$matrices[[i]]$matrix), na.rm = TRUE)-1)/(dim(somipa.all$matrices[[i]]$matrix)[1]*dim(somipa.all$matrices[[i]]$matrix)[2]-1)
}
Q_vec <- as.data.frame(Q_vec)

QConf <- function (x, ci = 0.95){
  Margin_Error <- abs(qnorm((1-ci)/2))* sd(x)/sqrt(length(x))
  df_out <- data.frame( Mean=mean(x), 'LCI' = (mean(x) - Margin_Error), 'UCI' = (mean(x) + Margin_Error)) %>% 
    tidyr::pivot_longer(names_to = "Measurements", values_to ="values", 1:3 )
  return(df_out)
}
QConf(Q_vec$Q_vec)

# calculate the mean mixing rate of matrices generated through bootstrapping for uncertainty
somipa.all <- melt(Reduce("+", lapply(somipa.all$matrices, function(x) {x$matrix})) / length(somipa.all$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")

#=============================================================================================

# physical contact matrix

# create survey object by combining separate male and female part and cnt datasets
somipa.phys <- survey(part.m %>% filter(part_cvd == "No"), cnt.m %>% filter(cnt_type == "Physical"))

# build a contact matrix via sampling contact survey using bootstrapping
somipa.phys <- contact_matrix(
  somipa.phys,
  countries = c("Malawi"),
  survey.pop = survey.pop,
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
  Q_vec[i] <- (sum(diag(somipa.phys$matrices[[i]]$matrix), na.rm = TRUE)-1)/(dim(somipa.phys$matrices[[i]]$matrix)[1]*dim(somipa.phys$matrices[[i]]$matrix)[2]-1)
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
somipa.phys <- melt(Reduce("+", lapply(somipa.phys$matrices, function(x) {x$matrix})) / length(somipa.phys$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")

#=============================================================================================

# non-physical contact matrix

# create survey object by combining separate male and female part and cnt datasets
somipa.verb <- survey(part.m %>% filter(part_cvd == "No"), cnt.m %>% filter(cnt_type == "Non-physical"))

# build a contact matrix via sampling contact survey using bootstrapping
somipa.verb <- contact_matrix(
  somipa.verb,
  countries = c("Malawi"),
  survey.pop = survey.pop,
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
  Q_vec[i] <- (sum(diag(somipa.verb$matrices[[i]]$matrix), na.rm = TRUE)-1)/(dim(somipa.verb$matrices[[i]]$matrix)[1]*dim(somipa.verb$matrices[[i]]$matrix)[2]-1)
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
somipa.verb <- melt(Reduce("+", lapply(somipa.verb$matrices, function(x) {x$matrix})) / length(somipa.verb$matrices), varnames = c("Participant.age", "Contact.age"), value.name = "Mixing.rate")

#=============================================================================================

# make combined plots

somipa.all <- somipa.all %>% mutate(Category = "A, All contacts (Q=0.425 | Q=0.398)")
somipa.phys <- somipa.phys %>% mutate(Category = "B, Physical contacts (Q=0.275 | Q=0.250)")
somipa.verb <- somipa.verb %>% mutate(Category = "C, Non-physical contacts (Q=0.129 | Q=0.127)")
B <- rbind(somipa.all, somipa.phys, somipa.verb) %>%
  mutate(Participant.age = if_else(Participant.age == 2L, "[0,5)",
                                           if_else(Participant.age == 3L, "[5,16)",
                                                   if_else(Participant.age == 4L, "[16,20)",
                                                           if_else(Participant.age == 5L, "[20,30)",
                                                                   if_else(Participant.age == 6L, "[30,40)",
                                                                           if_else(Participant.age == 7L, "[40,50)", "50+")))))),
  
  subtit = "Not affected by COVID-19")
  
#=============================================================================================

X <-  rbind(A, B) %>% filter(!is.na(Mixing.rate) & Participant.age != "[0,1)") %>%
ggplot(aes(x = factor(Participant.age,levels(factor(Participant.age))[c(1,6,2,3,4,5,7)]), y = Contact.age, fill = Mixing.rate)) + 
  geom_tile(color = "white") + 
  geom_text(aes(label = sprintf("%1.2f", Mixing.rate)), color = "white", size = 2) +
  scale_fill_gradient(low="lightgreen", high="red") +
  facet_grid(subtit ~ Category) +
  theme_bw() +
  labs(title = "", x = "Participant age (years)", y = "Contactee age (years)") +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  theme(strip.text.x = element_text(size = 16), strip.text.y = element_text(size = 16)) +
  theme(legend.position = "right") + 
  geom_vline(xintercept = c(2.5, 5.5, 12), linetype="dashed", color = "black", size = 0.2) +
  geom_hline(yintercept = c(2.5, 5.5, 12), linetype="dashed", color = "black", size = 0.2)

#===========================================================================

ggsave(here::here("output", "FigS4_crude_contact_matrix.png"),
       plot = X,
       width = 18, height = 9, unit="in", dpi = 300)
