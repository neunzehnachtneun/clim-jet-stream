

ggp_mean_jet_chebyshev <-  
  ggplot(data = tb_jets_month %>%
           # filter(season == "DJF") %>%
           filter(method == "Chebyshev") %>%
           filter(class %in% c("PFJ", "SJ")),
         mapping = aes(x = lon, y = lat)) +
  geom_density_2d(aes(colour = "#d7191c")) +
  geom_density_2d(data = tb_jets_month %>%
                    # filter(season == "DJF") %>%
                    filter(method == "Chebyshev") %>%
                    filter(class %in% c("STJ", "SJ")),
                  mapping = aes(colour = "#2c7bb6")) +
  scale_x_continuous(name = "Längengrad",
                     breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
  scale_y_continuous(name = "Breitengrad",
                     breaks = c(0, 30, 60, 90)) +
  coord_cartesian(xlim = c(-180,180), ylim = c(0,90)) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group),
               data = map_nh, fill = "gray50", alpha = 0.35) +
  theme_bw() + theme(legend.position = "none")
print(ggp_mean_jet_chebyshev)


ggp_mean_jet_dijkstra <-
  ggplot(data = tb_jets_month %>%
           # filter(season == "DJF") %>%
           filter(method == "Dijkstra") %>%
           filter(class %in% c("PFJ", "SJ")),
         mapping = aes(x = lon, y = lat)) +
  geom_density_2d(aes(colour = "#d7191c")) +
  geom_density_2d(data = tb_jets_month %>%
                    # filter(season == "DJF") %>%
                    filter(method == "Dijkstra") %>%
                    filter(class %in% c("STJ", "SJ")),
                  mapping = aes(colour = "#2c7bb6")) +
  scale_x_continuous(name = "Längengrad",
                     breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
  scale_y_continuous(name = "Breitengrad",
                     breaks = c(0, 30, 60, 90)) +
  coord_cartesian(xlim = c(-180,180), ylim = c(0,90)) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group),
               data = map_nh, fill = "gray50", alpha = 0.35) +
  theme_bw() + theme(legend.position = "none")
print(ggp_mean_jet_dijkstra)

# save_plot(plt = ggp_mean_jet_chebyshev, 
#          width = 135, height = 75, pointsize = 9,
#          filepath = paste0(save_dir, "10-praes/"), 
#          filename = "mean-cheb-djf")


