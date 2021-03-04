#Last edited by - Deus Thindwa
#Date - 28/10/2019

# load SCALE dataset
scale <- read_csv(here::here("data", "scale.csv"))

# data manipulation
scale <- rename(scale, "cluster" = "s02cl_id", "sex" = "s07sex",  "hiv" = "d12sumres", "age" = "s09age", 
                "lat" = "gps_lat", "lon" = "gps_lng", "alt" = "gps_alt", "acc" = "gps_acc")
scale <- select(scale, -dup)

scale <- filter(scale, hiv == 2 | hiv == 3, !is.na(cluster), !is.na(age), !is.na(lon), !is.na(lat))

scale$agegp <- if_else(scale$age >= 18 & scale$age <= 49, "18-49y", "50y+")

scale$sex <- if_else(scale$sex == 1, "Male", "Female")

scale$hiv <- if_else(scale$hiv == 2, "Positive on ART", "Negative")

scale$comm <- if_else(scale$cluster %in% 1:14, "Ndirande",
              if_else(scale$cluster %in% 15:16, "Zingwangwa",
              if_else(scale$cluster %in% 17, "Chimwankhunda",
              if_else(scale$cluster %in% 18:21, "Chilobwe",
              if_else(scale$cluster %in% 22:26, "Manase",
              if_else(scale$cluster %in% 27:30, "Soche",
              if_else(scale$cluster %in% 31:33, "BCA",
              if_else(scale$cluster %in% 34:43, "Bangwe",
              if_else(scale$cluster %in% 44:47, "Khama",
              if_else(scale$cluster %in% 48:49, "Kachere",
              if_else(scale$cluster %in% 50:56, "Machinjiri",
              if_else(scale$cluster %in% 57:65, "Chirimba",
              if_else(scale$cluster %in% 66:69, "Mbayani", "Chilomoni")))))))))))))

scale <- select(scale, hh_id, ind_id, comm, cluster, age, sex, agegp, hiv, lat, lon, alt, acc )

# perform stratified sampling
set.seed(1988)
somipa <- stratified(subset(scale, comm == "Ndirande"), c("agegp", "hiv"), 150)
write_csv(select(somipa, hh_id, ind_id, comm, cluster, lon, lat, age, sex, hiv), here("data", "OKD_DS.csv"))

