# Last edited by - Deus Thindwa
# Date - 28/10/2019

#===========================================================================

# number of participant (Table 1 column 1)
 pp.gee <- pp.labeled

# age
 pp.gee <- pp.gee %>% 
  mutate(agey = if_else(agey < 1, "<1y", 
                         if_else(agey >= 1 & agey < 5, "1-4y", 
                                 if_else(agey >= 5 & agey < 15, "5-14y", 
                                         if_else(agey >= 15 & agey < 20, "15-19y", 
                                                 if_else(agey >= 20 & agey < 50, "20-49y", "50+y"))))))
 pp.gee %>% 
   group_by(agey) %>% 
   tally()
  
# sex
 pp.gee %>% 
  group_by(sex) %>% 
  tally()

# occupation
 pp.gee <- pp.gee %>% 
  filter(!is.na(occup)) %>% 
  mutate(occup = 
           if_else(occup == "Business", "Business", 
                   if_else(occup == "Shop", "Business",
                           if_else(occup == "Agriculture", "Workers", 
                                   if_else(occup == "Domestic", "Workers", 
                                           if_else(occup == "Manual", "Workers",
                                                   if_else(occup == "Office", "Workers",
                                                           if_else(occup == "Other", "Other",
                                                                   if_else(occup == "Preschool", "Preschool",
                                                                           if_else(occup == "Retired", "Retired", 
                                                                                   if_else(occup == "School", "School", "Unemployed")))))))))))
  
 pp.gee %>% 
   group_by(occup) %>% 
   tally()

# hiv
pp.gee %>% 
  group_by(hiv) %>% 
  tally()

# day  of interview
pp.gee <- pp.gee %>% 
  mutate(datex = dmy(str_sub(date, 1, 10)), dow = weekdays(datex-1), dowgp = if_else(dow == "Saturday" | dow == "Sunday", "Weekend", "Weekday"))  

pp.gee %>% group_by(dowgp) %>% 
  tally()

# COVID19 behavioural change
pp.gee %>% 
  group_by(cvdcnt) %>%
  tally()

# number of household members
hh.gee <- hh.labeled %>% 
  dplyr::select(hhid, nlive) %>% 
  rename("somipa_hhid" = hhid) 

glm.gee <- left_join(pp.gee, hh.gee) %>%
  #rename("hhsize" = nlive) %>%
  mutate(hhsize = if_else(nlive <= 3, "1-3", 
                          if_else(nlive == 4 | nlive == 5, "4-5",
                                          if_else(nlive == 6, "6", "7+"))))

glm.gee %>% group_by(hhsize) %>% 
  tally()

# final dataset
glm.gee <- glm.gee %>% 
  dplyr::select(somipa_hhid, somipa_pid, cntno, agey, sex, occup, educ, hiv, dowgp, cvdcnt, hhsize)


#===========================================================================

# mean number of contacts (Table 1 column 2)
glm.gee$cntno <- as.integer(glm.gee$cntno)

gee_agey <- c("<1y", "1-4y", "5-14y", "15-19y", "20-49y", "50+y")
gee_sex <- c("Male", "Female")
gee_occup <- c("Business", "Workers", "Preschool", "School", "Retired", "Unemployed", "Other")
gee_educ <- c("No education", "Primary", "Secondary", "College")
gee_hiv <- c("Negative", "Positive on ART")
gee_dowgp <- c("Weekday", "Weekend")
gee_cvd <- c("No", "Yes")
gee_hhsize <- c("1-3", "4-5", "6", "7+")

for(i in gee_agey){
  set.seed(1988)
  j = boot(subset(glm.gee, agey == i)$cntno, function(x,i) mean(x[i]), R = 1000)
  print(i)
  print(j); print(boot.ci(j, conf = 0.95, type = c("bca"))) 
  print("-----------------------------------------------------------")
}

for(i in gee_sex){
  set.seed(1988)
  j = boot(subset(glm.gee, sex == i)$cntno, function(x,i) mean(x[i]), R = 1000)
  print(i)
  print(j); print(boot.ci(j, conf = 0.95, type = c("bca"))) 
  print("-----------------------------------------------------------")
}

for(i in gee_occup){
  set.seed(1988)
  j = boot(subset(glm.gee, occup == i)$cntno, function(x,i) mean(x[i]), R = 1000)
  print(i)
  print(j); print(boot.ci(j, conf = 0.95, type = c("bca"))) 
  print("-----------------------------------------------------------")
}

for(i in gee_educ){
  set.seed(1988)
  j = boot(subset(glm.gee, educ == i)$cntno, function(x,i) mean(x[i]), R = 1000)
  print(i)
  print(j); print(boot.ci(j, conf = 0.95, type = c("bca"))) 
  print("-----------------------------------------------------------")
}

for(i in gee_hiv){
  set.seed(1988)
  j = boot(subset(glm.gee, hiv == i)$cntno, function(x,i) mean(x[i]), R = 1000)
  print(i)
  print(j); print(boot.ci(j, conf = 0.95, type = c("bca"))) 
  print("-----------------------------------------------------------")
}

for(i in gee_dowgp){
  set.seed(1988)
  j = boot(subset(glm.gee, dowgp == i)$cntno, function(x,i) mean(x[i]), R = 1000)
  print(i)
  print(j); print(boot.ci(j, conf = 0.95, type = c("bca"))) 
  print("-----------------------------------------------------------")
}

for(i in gee_cvd){
  set.seed(1988)
  j = boot(subset(glm.gee, cvdcnt == i)$cntno, function(x,i) mean(x[i]), R = 1000)
  print(i)
  print(j); print(boot.ci(j, conf = 0.95, type = c("bca"))) 
  print("-----------------------------------------------------------")
}

for(i in gee_hhsize){
  set.seed(1988)
  j = boot(subset(glm.gee, hhsize == i)$cntno, function(x,i) mean(x[i]), R = 1000)
  print(i)
  print(j); print(boot.ci(j, conf = 0.95, type = c("bca"))) 
  print("-----------------------------------------------------------")
}

# mean and standard deviation (alternative to confidence intervals)
with(glm.gee, tapply(cntno, agey, function(x){sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))}))
with(glm.gee, tapply(cntno, sex, function(x){sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))}))
with(glm.gee, tapply(cntno, occup, function(x){sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))}))
with(glm.gee, tapply(cntno, educ, function(x){sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))}))
with(glm.gee, tapply(cntno, hiv, function(x){sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))}))
with(glm.gee, tapply(cntno, dowgp, function(x){sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))}))
with(glm.gee, tapply(cntno, cvdcnt, function(x){sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))}))
with(glm.gee, tapply(cntno, hhsize, function(x){sprintf("M (SD) = %1.2f (%1.2f)", mean(x), sd(x))}))

# fit adjusted negative binomial mixed model for crude contact rate ratio (CRR)
cmodel <- glmm.nb(cntno ~ agey, random =  ~ 1 | somipa_hhid, na.action = na.omit, data = glm.gee, verbose = TRUE)
summary(cmodel); intervals(cmodel)

cmodel <- glmm.nb(cntno ~ sex, random =  ~ 1 | somipa_hhid, na.action = na.omit, data = glm.gee, verbose = TRUE)
summary(cmodel); intervals(cmodel)

cmodel <- glmm.nb(cntno ~ occup, random =  ~ 1 | somipa_hhid, na.action = na.omit, data = glm.gee, verbose = TRUE)
summary(cmodel); intervals(cmodel)

cmodel <- glmm.nb(cntno ~ educ, random =  ~ 1 | somipa_hhid, na.action = na.omit, data = glm.gee, verbose = TRUE)
summary(cmodel); intervals(cmodel)

cmodel <- glmm.nb(cntno ~ hiv, random =  ~ 1 | somipa_hhid, na.action = na.omit, data = glm.gee, verbose = TRUE)
summary(cmodel); intervals(cmodel)

cmodel <- glmm.nb(cntno ~ dowgp, random =  ~ 1 | somipa_hhid, na.action = na.omit, data = glm.gee, verbose = TRUE)
summary(cmodel); intervals(cmodel)

cmodel <- glmm.nb(cntno ~ cvdcnt, random =  ~ 1 | somipa_hhid, na.action = na.omit, data = glm.gee, verbose = TRUE)
summary(cmodel); intervals(cmodel)

cmodel <- glmm.nb(cntno ~ hhsize, random =  ~ 1 | somipa_hhid, na.action = na.omit, data = glm.gee, verbose = TRUE)
summary(cmodel); intervals(cmodel)

# fit adjusted negative binomial mixed model for adjusted contact rate ratio (CRR)
cmodel <- glmm.nb(cntno ~ agey + sex + occup + cvdcnt + hhsize , random =  ~ 1 | somipa_hhid, na.action = na.omit, data = glm.gee, verbose = TRUE)
summary(cmodel); intervals(cmodel)
