# Last edited by - Deus Thindwa
# Date - 28/10/2019

# load all required datasets
hh.unlabel <- read_csv(here::here("data", "hh.unlabel.csv"))
hh.labeled <- read_csv(here::here("data", "hh.labeled.csv"))
pp.unlabel <- read_csv(here::here("data", "pp.unlabel.csv"))
pp.labeled <- read_csv(here::here("data", "pp.labeled.csv"))
cn.unlabel <- read_csv(here::here("data", "cn.unlabel.csv"))
cn.labeled <- read_csv(here::here("data", "cn.labeled.csv"))

# create survey object by combining participants and contacts datasets
part.m <- rename(select(pp.labeled, date, somipa_pid, scale_pid, agey, sex, cvdcnt, hiv), "part_age" = agey, "part_sex" = sex, "part_cvd" = cvdcnt, "part_hiv" = hiv)
part.m <- part.m %>% mutate(country = "Malawi", year = 2021, dayofweek = wday(dmy_hm(date)))

cnt.m <- select(cn.labeled, somipa_pid, cnt_age, cnt_type, cnt_loc, cnt_plc, cnt_rel)
cnt.m <- cnt.m %>% arrange(somipa_pid) %>% mutate(cnt_id = 1:n())
cnt.m$cnt_pid <- c(0, cumsum(as.numeric(with(cnt.m, somipa_pid[1:(length(somipa_pid)-1)] != somipa_pid[2:length(somipa_pid)])))) + 1
cnt.m <- cnt.m %>% select(cnt_id, cnt_pid, somipa_pid, everything())

#rename contact matrix merging variable from somipa_pid to part_id
part.m <- part.m %>% rename("part_id" = somipa_pid)
cnt.m <- cnt.m %>% rename("part_id" = somipa_pid)

