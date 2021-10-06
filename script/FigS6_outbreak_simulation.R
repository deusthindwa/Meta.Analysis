# Last edited by - Deus Thindwa
# Date - 28/10/2019

# overall contact matrix
somipa.all <- survey(part.m, cnt.m %>% filter(cnt_type == "Physical"))

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
A <- somipa.all %>%
ggplot(aes(x = factor(Participant.age,levels(factor(Participant.age))[c(1,2,5,3,4,6)]), y = Contact.age, fill = Mixing.rate)) + 
  geom_tile(color = "white") + 
  geom_text(aes(label = sprintf("%1.2f", Mixing.rate)), color = "white", size = 2) +
  scale_fill_gradient(low="lightgreen", high="red") +
  theme_bw() +
  labs(title = "A, Physical mixing", x = "Participant age (years)", y = "Contactee age (years)") +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  theme(strip.text.x = element_text(size = 16)) +
  theme(legend.position = "right") + 
  geom_vline(xintercept = c(2.5, 5.5, 12), linetype="dashed", color = "black", size = 0.2) +
  geom_hline(yintercept = c(2.5, 5.5, 12), linetype="dashed", color = "black", size = 0.2)


# simulate the outbreak based on the matrix above and SIS model structure
SISmat <- function(time, state, pars){
  with(as.list(c(state, pars)), {
    S = state[1:n]
    I = state[(n+1):(2*n)]
    
    # sum across the columns of the matrix beta*I*S
    newinfections = colSums(beta*I%*%t(S))
    dS <- -newinfections + gamma*I
    dI <- newinfections - gamma*I
    return(list(c(dS, dI)))
  })
}

# set the number of age groups
n = 6

# set the transmission matrix
d1 = 1 #default values for diagonal matrix
B = matrix(0, nrow = n, ncol = n)
B = B + diag(d1, nrow = n, ncol = n)
B[1,1] = 0.03; B[2,1] = 0.69; B[3,1] = 1.66; B[4,1] = 1.12; B[5,1] = 4.62; B[6,1] = 0.32 
B[1,2] = 0.16; B[2,2] = 1.82; B[3,2] = 2.14; B[4,2] = 0.93; B[5,2] = 3.37; B[6,2] = 0.42 
B[1,3] = 0.16; B[2,3] = 0.85; B[3,3] = 5.32; B[4,3] = 1.14; B[5,3] = 2.42; B[6,3] = 0.60  
B[1,4] = 0.24; B[2,4] = 0.85; B[3,4] = 2.64; B[4,4] = 4.48; B[5,4] = 4.22; B[6,4] = 0.59 
B[1,5] = 0.26; B[2,5] = 0.81; B[3,5] = 1.47; B[4,5] = 1.11; B[5,5] = 6.71; B[6,5] = 0.66  
B[1,6] = 0.15; B[2,6] = 0.80; B[3,6] = 2.85; B[4,6] = 1.23; B[5,6] = 5.23; B[6,6] = 1.88

# initial conditions
S = rep(1, n)
I = rep(0, n)

#start with small amount of infection in the first subpopulation
S[2] = S[2] - 1e-6
I[2] = I[2] + 1e-6
init <- c(S = S, I = I)

parms <- list(n = n, beta = B*0.014, gamma = c(1/56.3, 1/56.3, 1/17.9, 1/17.9, 1/6.0, 1/6.0))
times <- seq(0, 1000, by = 0.01)

#run the model
out <- as.data.frame(lsoda(y = init, times = times, func = SISmat, parms = parms))

#reshape the output data frame and plot transmission dynamics overtime
B <- out %>% 
  pivot_longer(cols = I1:I6, names_to = "age") %>%
  dplyr::select(time, age, value) %>%
  mutate(agegp = if_else(age == "I1", "<1y",
                         if_else(age == "I2", "1-4y",
                                 if_else(age == "I3", "5-14y",
                                         if_else(age == "I4", "15-19y",
                                                 if_else(age == "I5", "20-49y", "50+y")))))) %>%
  group_by(time, agegp) %>%
  
  ggplot(aes(time, value, color = agegp)) +
  geom_line(size = 1) +
  theme_bw() +
  xlim(0,350) +
  labs(title = "B, Steady state proportion infected", x = "Time (days)", y = "Proportion infected") + 
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 0, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  theme(legend.position = "none")

#reshape the output data frame and plot transmission dynamics by age groups
X <- out %>% 
  pivot_longer(cols = I1:I6, names_to = "age") %>%
  dplyr::select(age, value) %>%
  mutate(agegp = if_else(age == "I1", "<1y",
                         if_else(age == "I2", "1-4y",
                                 if_else(age == "I3", "5-14y",
                                         if_else(age == "I4", "15-19y",
                                                 if_else(age == "I5", "20-49y", "50+y")))))) %>%
  group_by(agegp) %>% 
  
  ggplot(aes(factor(agegp,levels(factor(agegp))[c(1,2,5,3,4,6)]), value, color = agegp)) +
  geom_line(size = 1) +
  theme_bw() +
  ylim(0,1) +
  labs(title = "", x = "Age group in years (y)", y = "") +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  theme(legend.position = "none")


#reshape the output data frame and plot epidemic by age groups
C <- left_join(out %>% 
  pivot_longer(cols = I1:I6, names_to = "age") %>%
  dplyr::select(time, age, value) %>%
  mutate(agegp = if_else(age == "I1", "<1y",
                         if_else(age == "I2", "1-4y",
                                 if_else(age == "I3", "5-14y",
                                         if_else(age == "I4", "15-19y",
                                                 if_else(age == "I5", "20-49y", "50+y")))))) %>%
  dplyr::filter(time == 1000),
survey.pop %>%
  mutate(agegp = if_else(lower.age.limit == 0, "<1y",
                         if_else(lower.age.limit == 1, "1-4y",
                                 if_else(lower.age.limit == 5, "5-14y",
                                         if_else(lower.age.limit == 15, "15-19y",
                                                 if_else(lower.age.limit == 20, "20-49y", "50+y")))))) %>%
  dplyr::select(agegp, population)) %>%
  
  ggplot(aes(factor(agegp, levels(factor(agegp))[c(1,2,5,3,4,6)]), value*population, color = agegp)) +
  geom_bar(size = 0.6, stat = "identity", fill = "white") +
  theme_bw() +
  labs(title = "C, Epidemic size", x = "Age group in years (y)", y = "Number infected\n(Psize = 97,331)") +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  theme(legend.position = "none")

#reshape the output data frame and plot epidemic by age groups
D <- out %>% pivot_longer(cols = I1:I6, names_to = "age") %>%
  dplyr::select(time, age, value) %>%
  dplyr::filter(time == max(time)) %>%
  mutate(agegp = if_else(age == "I1", "<1y",
                         if_else(age == "I2", "1-4y",
                                 if_else(age == "I3", "5-14y",
                                         if_else(age == "I4", "15-19y",
                                                 if_else(age == "I5", "20-49y", "50+y"))))),
         Rnote = 1/(1-value)) %>%

  ggplot(aes(factor(agegp, levels(factor(agegp))[c(1,2,5,3,4,6)]), Rnote, color = agegp)) +
  geom_point(size = 2.8, fill = "black") +
  theme_bw() +
  ylim(1,5) +
  labs(title = "D, Basic reproduction number", x = "Age group in years (y)", y = "Rnote") +
  theme(axis.text.x = element_text(face = "bold", size = 12, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 12)) +
  theme(axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16)) +
  theme(legend.position = "none")


#===========================================================================

ggsave(here::here("output", "FigS5_outbreak_simulation.png"),
       plot = (A | B | inset_element(X, right = 0.5, left = 0.01, bottom = 0.3, top = 0.95) | C | D | plot_layout(ncol = 4, width = c(3,4,2,2))),
       width = 22, height = 6, unit="in", dpi = 300)

