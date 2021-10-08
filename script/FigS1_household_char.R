# Last edited by - Deus Thindwa
# Date - 28/10/2019

#suppress warnings
defaultW <- getOption("warn")
options(warn = -1)

#----------convert shape file map into dataframe for ggplot.
dlnmtmp <- tempfile()
download.file("https://raw.githubusercontent.com/deusthindwa/social.contact.rates.estimation.hiv.malawi/master/data/ndix_map.zip", destfile = dlnmtmp)
unzip(dlnmtmp, exdir = ".")
ndix.map <- rgdal::readOGR(".","ndix_map")


  
  
  
  
  
  
  #----------load shape file of malawi map.
  dlnmtmp <- tempfile()
  download.file("https://raw.githubusercontent.com/deusthindwa/dlnm.typhoid.nts.climate.blantyre.malawi/master/data/malawi_map.zip", destfile=dlnmtmp)
  unzip(dlnmtmp, exdir = ".")
  malawi.map <- rgdal::readOGR(".","malawi_map")
  
  
  #----------subsetting to get blantyre map only.
  blantyre1.map <- malawi.map@data$OBJECTID >289 & malawi.map@data$OBJECTID <297 #id from 290 to 296 
  blantyre2.map <- malawi.map@data$OBJECTID >308 & malawi.map@data$OBJECTID <311 #id from 309 to 310
  blantyre3.map <- malawi.map@data$OBJECTID >342  #id fom 243
  
  
  #----------convert shape file map into dataframe for ggplot.
  blantyre.map <- rbind(fortify(malawi.map[blantyre1.map,]), fortify(malawi.map[blantyre2.map,]), fortify(malawi.map[blantyre3.map,]))
  blantyre.map$id <- as.integer(blantyre.map$id)
  
  #----------merge blantyre map dataset with location attributes dataset.
  blantyre.demog <- read.csv(curl("https://raw.githubusercontent.com/deusthindwa/dlnm.typhoid.nts.climate.blantyre.malawi/master/data/blantyre_demog.csv"))
  map.features <- read.csv(curl("https://raw.githubusercontent.com/deusthindwa/dlnm.typhoid.nts.climate.blantyre.malawi/master/data/blantyre_features.csv"))
  blantyre.demog$id <- as.integer(blantyre.demog$id)
  map.all <- merge(x=blantyre.map, y=blantyre.demog, by="id", x.all=TRUE)
  rm(list = ls()[grep("^blantyre", ls())])
  
  #----------plot blantyre map with 1998-2008 population census.
  ggplot() + 
    geom_polygon(data=map.all, aes(x=long, y=lat, group=group, fill=popc), colour="gray50") + 
    theme_classic() + 
    theme(axis.text.x = element_text(face="bold", size=10, color="black"), axis.text.y = element_text(face="bold", size=10, color="black")) + 
    labs(fill="(1998 - 2008) Population censuses") + xlab("Longitude") + ylab("Latitude") + 
    geom_point(data=map.features, aes(x =long, y =lat, shape=Geolocation, size=Geolocation), color="black") +
    scale_shape_manual(values=c(17, 16, 3)) +
    scale_size_manual(values=c(2,4,3)) + 
    theme(legend.key.height=unit(0.8,"line")) + 
    theme(legend.key.width=unit(0.8,"line"))
  
  
  
  
  
  
  

#----------plot blantyre map with 1998-2008 population census.
ggplot() + 
  geom_polygon(data=Ndix_map, aes(x=cluster, y=geometry), colour="gray50") + 
  theme_classic() + 
  theme(axis.text.x = element_text(face="bold", size=10, color="black"), axis.text.y = element_text(face="bold", size=10, color="black")) + 
  labs(fill="(1998 - 2008) Population censuses") + xlab("Longitude") + ylab("Latitude")  
  #geom_point(data=map.features, aes(x =long, y =lat, shape=Geolocation, size=Geolocation), color="black") +
  #scale_shape_manual(values=c(17, 16, 3)) + 
  #scale_size_manual(values=c(2,4,3)) + 
  #theme(legend.key.height=unit(0.8,"line")) + 
  #theme(legend.key.width=unit(0.8,"line"))

Ndix_map$geometry










#===========================================================================

# distribution of household cluster
clustersize <- tibble(cluster = c(1, 2, 3,4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
                      N = c(193, 208, 151, 211, 195, 211, 197, 210, 140, 174, 175, 201, 170, 190))

A <- hh.labeled %>%
  filter(!is.na(cluster)) %>% group_by(cluster) %>% tally() %>%
  left_join(clustersize) %>%
  mutate(perc = n/sum(n), 
         lci = exactci(n, sum(n), 0.95)$conf.int[1:14], 
         uci = exactci(n, sum(n), 0.95)$conf.int[15:28],
         p = sprintf("%1.1f%%", n/N*100)) %>%
  
  ggplot(aes(x = cluster, y = perc)) + 
  geom_bar(stat = "identity", color = "black", size = 1, fill = brocolors("crayons")["Tan"]) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.3, position = position_dodge(0.9), size = 1.3) +
  geom_text(aes(label = n), color = "black", position = position_stack(vjust = 0.5), size = 3) +
  geom_text(aes(label = p), color = "black", position = position_stack(vjust = 0.4), size = 3) +
  scale_y_continuous(breaks = seq(0, 0.16, 0.02), labels = scales::percent_format(accuracy = 1)) + 
  scale_x_continuous(breaks = seq(1, 14, 1)) + 
  theme_bw() +
  labs(title = "A", x = "Cluster number", y = "Proportion of households") +
  theme(axis.text.x = element_text(face = "bold", size = 10), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = "none")

#===========================================================================

# distribution of number of rooms and sleeping rooms
B <- hh.labeled %>%  
  ggplot() + 
  geom_density(aes(x = room), alpha = 0.3, size = 1, fill = brocolors("crayons")["Goldenrod"]) +
  geom_density(aes(x = sroom), alpha = 0.3, size = 1, fill = brocolors("crayons")["Sky Blue"]) +
  geom_text(aes(x = 2, y = 0.2, label = "BR"), color = "black", size = 3) +
  geom_text(aes(x = 4, y = 0.2, label = "HR"), color = "black", size = 3) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  labs(title = "B", x = "Houserooms (HR) v bedrooms (BR)", y = "Probability density") +
  theme(axis.text.x = element_text(face = "bold", size = 10), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = "right")

#===========================================================================

# distribution of participant by household size
C <- hh.labeled %>%  
  ggplot() + 
  geom_density(aes(x = nlive), alpha = 0.3, size = 1, fill = brocolors("crayons")["Sky Blue"]) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 20, 2)) +
  labs(title = "C", x = "Household size", y = "Probability density") +
  theme(axis.text.x = element_text(face = "bold", size = 10), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = "none")

X <- filter(hh.labeled, !is.na(smoke) & smoke !="Dont know") %>% 
  group_by(smoke) %>% tally() %>% 
  mutate(perc = n/sum(n), lci = exactci(n, sum(n), 0.95)$conf.int[1:2], uci = exactci(n, sum(n), 0.95)$conf.int[3:4]) %>%
  
  ggplot(aes(x = smoke, y = perc)) + 
  geom_bar(stat = "identity", color = "black", size = 1, fill = brocolors("crayons")["Sky Blue"], alpha = 0.3) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.3, position = position_dodge(0.9), size = 1.3) +
  geom_text(aes(label = n), color = "black", position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(breaks = seq(0, 2, 0.1), labels = scales::percent_format(accuracy = 1)) + 
  theme_bw() +
  labs(title = "", x = "Household smokers", y = "Proportion of households") +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 0, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = "none")

#===========================================================================

# household source of energy
D <- hh.labeled %>% 
  dplyr::select(gas:diesel) %>%
  pivot_longer(gas:diesel, names_to = "question", values_to = "response") %>% 
  group_by(question, response) %>% 
  tally() %>% 
  mutate(perc = n/sum(n), 
         questionc = if_else(question == "electric", "Electricity",
                                     if_else(question == "firewood", "Firewood",
                                             if_else(question == "charcoal", "Charcoal", 
                                                     if_else(question == "gas", "Gas",
                                                             if_else(question == "diesel", "Diesel", "Kerosene")))))) %>%
  
ggplot(mapping = aes(x = factor(questionc,levels(factor(questionc))[c(1,4,3,6,2,5)]), y = perc, color = response, fill = response)) + 
  geom_bar(stat = "identity", color = "black", size = 0.7) +
  scale_fill_brewer() +
  geom_text(aes(label = n), color = "black", position = position_stack(vjust = 0.5)) +
  theme_bw() +
  labs(title = "D", x = "Source of household energy", y = "Proportion of households") +
  scale_y_continuous(breaks = seq(0, 1, 0.2), labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 30, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  guides(fill = guide_legend(title="")) +
  theme(legend.position = c(0.58, 0.75))

#===========================================================================

# household ventilation
E <- hh.labeled %>% 
  group_by(indoor) %>% tally() %>% 
  mutate(perc = n/sum(n), lci = exactci(n, sum(n), 0.95)$conf.int[1:2], uci = exactci(n, sum(n), 0.95)$conf.int[3:4]) %>%
  
  ggplot(aes(x = indoor, y = perc)) + 
  geom_bar(stat = "identity", color = "black", size = 1, fill = brocolors("crayons")["Tumbleweed"]) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.3, position = position_dodge(0.9), size = 1.3) +
  geom_text(aes(label = n), color = "black", position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), labels = scales::percent_format(accuracy = 1)) + 
  theme_bw() +
  labs(title = "E", x = "Indoor cooking", y = "Proportion of households") +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 0, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = "none")

#===========================================================================

F <- filter(hh.labeled, !is.na(vent)) %>% 
  group_by(vent) %>% tally() %>% 
  mutate(perc = n/sum(n), lci = exactci(n, sum(n), 0.95)$conf.int[1:2], uci = exactci(n, sum(n), 0.95)$conf.int[3:4]) %>%
  
  ggplot(aes(x = vent, y = perc)) + 
  geom_bar(stat = "identity", color = "black", size = 1, fill = brocolors("crayons")["Sunglow"]) +
  geom_errorbar(aes(ymin = lci, ymax = uci), width = 0.3, position = position_dodge(0.9), size = 1.3) +
  geom_text(aes(label = n), color = "black", position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(breaks = seq(0, 1, 0.1), labels = scales::percent_format(accuracy = 1)) + 
  theme_bw() +
  labs(title = "F", x = "Indoor ventilation", y = "Proportion of households") +
  theme(axis.text.x = element_text(face = "bold", size = 10, angle = 0, vjust = 0.5, hjust = 0.3), axis.text.y = element_text(face = "bold", size = 11)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  theme(legend.position = "none")

#===========================================================================

#turn on warnings
options(warn = defaultW)

#combined plots
ggsave(here("output", "FigS1_household_char.png"),
       plot = (A | B | plot_layout(ncol = 2, width = c(4,2))) / (C | inset_element(X, right = 0.9, left = 0.6, bottom = 0.2, top = 0.9) | D | E | F | plot_layout(ncol = 4, width = c(3,3,1,1))),
       width = 21, height = 11, unit="in", dpi = 300)
