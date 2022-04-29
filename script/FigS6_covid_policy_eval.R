# Last edited by - Deus Thindwa
# Date - 28/10/2019

#===========================================================================

#sensitivity analysis of the study time and COVID 19 policies

#covid19 levels
pp.gee <- pp.labeled

pp.gee <- pp.gee %>% mutate(cvdlevel = month(date(date)),
                            cvdlevel = if_else(cvdlevel >5, "level3", "level1"))

# number of contacts outside home
cn.gee <- cn.labeled %>%
  group_by(somipa_pid, cnt_loc) %>%
  tally() %>%
  filter(cnt_loc == "Home")

# join number of contacts outside home to overall contacts
covidPolicy <- pp.gee %>% 
  left_join(glm.cn) %>% 
  mutate(n = if_else(is.na(n), 0L, n), nonHcnt = cntno-n) %>%
  dplyr::select(somipa_pid, cvdlevel, cvdcnt, nonHcnt)

# tab participants in level 1 and 3
covidPolicy %>% 
  filter(nonHcnt !=0) %>%
  group_by(cvdlevel) %>% 
  tally()

# tab contacts in level 1 and 3
covidPolicy %>% 
  group_by(cvdlevel) %>% 
  tally(nonHcnt)

# tab contacts by covid policy and participant hypothetical response
covidPolicy %>% 
  filter(nonHcnt !=0) %>%
  group_by(cvdlevel, cvdcnt) %>% 
  tally()

# mean number of contacts
gee_cvdlevel <- c("level1", "level3")

set.seed(1988)
for(i in gee_cvdlevel){
  j = boot(subset(covidPolicy, cvdlevel == i)$nonHcnt, function(x,i) mean(x[i]), R = 1000)
  print(i)
  print(j); print(boot.ci(j, conf = 0.95, type = c("bca"))) 
  print("-----------------------------------------------------------")
}

#===========================================================================

#sensitivity analysis of the study time and COVID 19 policies

#covid19 levels
pp.gee <- pp.labeled

pp.gee <- pp.gee %>% mutate(cvdlevel = month(date(date)),
                            cvdlevel = if_else(cvdlevel >5, "level3", "level1"))

# join number of contacts outside home to overall contacts
covidPolicy <- pp.gee %>% 
  dplyr::select(somipa_pid, cvdlevel, cvdcnt, cntno)

# tab participants in level 1 and 3
covidPolicy %>% 
  group_by(cvdlevel) %>% 
  tally()

# tab contacts in level 1 and 3
covidPolicy %>% 
  group_by(cvdlevel) %>% 
  tally(cntno)

# tab contacts by covid policy and participant hypothetical response
covidPolicy %>% 
  group_by(cvdlevel, cvdcnt) %>% 
  tally()

# mean number of contacts
gee_cvdlevel <- c("level1", "level3")

set.seed(1988)
for(i in gee_cvdlevel){
  j = boot(subset(covidPolicy, cvdlevel == i)$cntno, function(x,i) mean(x[i]), R = 1000)
  print(i)
  print(j); print(boot.ci(j, conf = 0.95, type = c("bca"))) 
  print("-----------------------------------------------------------")
}

