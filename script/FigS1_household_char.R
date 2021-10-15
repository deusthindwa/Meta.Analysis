# Last edited by - Deus Thindwa
# Date - 28/10/2019

#suppress warnings
defaultW <- getOption("warn")
options(warn = -1)

# distribution of household clusters to inform plot A
clustersize <- tibble(cluster = c(1, 2, 3,4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
                      N = c(193, 208, 151, 211, 195, 211, 197, 210, 140, 174, 175, 201, 170, 190))

hh.labeled %>%
  filter(!is.na(cluster)) %>% group_by(cluster) %>% tally() %>%
  left_join(clustersize) %>%
  mutate(perc = n/sum(n), 
         lci = exactci(n, sum(n), 0.95)$conf.int[1:14], 
         uci = exactci(n, sum(n), 0.95)$conf.int[15:28],
         p = sprintf("%1.1f%%", n/N*100))

#map ndirande
ndix_map  <- st_read(here("data", "ndix_map", "bt_scale_ndirande.shp"))

A <- ndix_map %>% 
  mutate(num = if_else(cluster == "c1", "38 (10.1%)",
                       if_else(cluster == "c2", "38 (10.1%)",
                               if_else(cluster == "c3", "19 (5.0%)",
                                       if_else(cluster == "c4", "34 (9.0%)",
                                               if_else(cluster == "c5", "20 (5.3%)",
                                                       if_else(cluster == "c6", "27 (7.1%)",
                                                               if_else(cluster == "c7", "37 (9.8%)",
                                                                       if_else(cluster == "c8", "27 (7.1%)",
                                                                               if_else(cluster == "c9", "20 (5.3%)",
                                                                                       if_else(cluster == "c10", "24 (6.4%)",
                                                                                               if_else(cluster == "c11", "21 (5.6%)",
                                                                                                       if_else(cluster == "c12", "30 (7.9%)",
                                                                                                               if_else(cluster == "c13", "28 (7.4%)", "15 (4.0%)"))))))))))))),
         nump = if_else(cluster == "c1", "19.7%",
                       if_else(cluster == "c2", "18.3%",
                               if_else(cluster == "c3", "12.6%",
                                       if_else(cluster == "c4", "16.1%",
                                               if_else(cluster == "c5", "10.3%",
                                                       if_else(cluster == "c6", "12.8%",
                                                               if_else(cluster == "c7", "18.8%",
                                                                       if_else(cluster == "c8", "12.9%",
                                                                               if_else(cluster == "c9", "14.3%",
                                                                                       if_else(cluster == "c10", "13.8%",
                                                                                               if_else(cluster == "c11", "12.0%",
                                                                                                       if_else(cluster == "c12", "14.9%",
                                                                                                               if_else(cluster == "c13", "16.5%", "7.9%"))))))))))))) ) %>%
ggplot() +
  geom_sf() + 
  coord_sf(expand = TRUE) +
  geom_sf(aes(fill = num)) +
  geom_sf_label(aes(label = nump), label.size = 0.05) +
  labs(title = "A", x = "Longitude", y = "Latitude") +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.25, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw() +
  guides(fill = guide_legend(title = "Households included\nacross clusters")) + 
  theme(legend.position = c(0.8, 0.75)) +
  theme(plot.title = element_text(size = 22), axis.title.x = element_text(face = "bold", size = 11), axis.title.y = element_text(face = "bold", size = 11)) +
  theme(axis.text.x = element_text(face = "bold", size = 10), axis.text.y = element_text(face = "bold", size = 11))
  #guides(fill = guide_legend(title = "Sample"), shape = guide_legend(override.aes = list(size = 9)), color = guide_legend(override.aes = list(size = 9))) +
  #theme(legend.title = element_text(size = 9), legend.text  = element_text(size = 9), legend.key.size = unit(0.9, "lines"))

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
       plot = (A | ((B | (C | inset_element(X, right = 0.9, left = 0.6, bottom = 0.2, top = 0.9)))) / (D | E | F | plot_layout(ncol = 3, width = c(3,1,1)))),
       width = 21, height = 11, unit="in", dpi = 300)

