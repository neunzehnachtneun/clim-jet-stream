
# Initiieren einer passenden Weltkarte
map_nh <- map_data("world")

# Plot der Nordhemisphäre // Untersuchungsgebiet
ggp_nh <-
  ggplot() + geom_polygon(data = map_nh, mapping = aes(x = long, y = lat, group = group), fill = "gray50") +
  scale_x_continuous(name = "Längengrad in $^{\\circ}$ geographischer Länge",
                     breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
  scale_y_continuous(name = "Breitengrad in\n$^{\\circ}$ nördlicher Breite",
                     breaks = c(0, 30, 60, 90)) +
  coord_fixed(xlim = c(-180,180), ylim = c(0,90)) +
  theme_bw()
print(ggp_nh)

# Plot der Nordhemisphäre auf Mercator-Projektion
ggp_nh_merc <-
  ggplot() + geom_polygon(data = map_nh, mapping = aes(x = long, y = lat, group = group), fill = "gray50") +
  scale_x_continuous(name = "Längengrad in $^{\\circ}$ geographischer Länge",
                     breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
  scale_y_continuous(name = "Breitengrad in\n$^{\\circ}$ nördlicher Breite",
                     breaks = c(0, 30, 60, 90)) +
  coord_map(xlim = c(-180,180), ylim = c(0,90)) +
  theme_bw()
print(ggp_nh_merc)

# options(tikzDefaultEngine = "pdftex", tikzLatexPackages = "siunitx")
# save_plot(plt = ggp_nh, width = 135, height = 55, pointsize = 9, 
#          filepath = paste0(save_dir, "01-area"), 
#          filename = "north-hem")
# save_plot(plt = ggp_nh_merc, width = 135, height = 55, pointsize = 9, 
#          filepath = paste0(save_dir, "01-area"), 
#          filename = "north-hem-merc")


## Greife zufällig acht Zeitpunkte heraus, 
## Nebenbedingung: Jede Saison kommt zweimal vor.

repeat {
  i_step <- sample.int(n = length(dts), size = 8, replace = FALSE)
  # print(dts_season[t_step])
  if (length(which(dts_season[i_step] == "mam")) == 2 &
      length(which(dts_season[i_step] == "jja")) == 2 &
      length(which(dts_season[i_step] == "son")) == 2 &
      length(which(dts_season[i_step] == "djf")) == 2 &
      length(which(duplicated(dts_month[i_step]))) == 0 &
      length(which(duplicated(dts_year[i_step]))) == 0) {
    break
  }
}
print(sort(i_step))
print(dts[sort(i_step)])
i_step <- c(33, 52, 143, 394, 437, 577, 663, 692)
## Schleife über Stichprobe
for (t_step in i_step) {
  print(t_step)

  ## Plot des zonalen Windfeldes und der Position des maximalen Jets sowie des maximalen Chebyshev-Jets
  # Datenaufbereitung
  tb_subset <-
    tb_jets_month %>%
    filter(year == dts_year[t_step] &
             month == dts_month[t_step]) %>%
    group_by(year, month, lon) %>%
    select(dts, year, month, season, method, class, lon, lat) %>%
    filter(class == "MJ")
  #print(unique(tb_subset$dts))
  # Visualisierung
  ggp_nh_m1_m2b <-
    ggplot(data = tb_uv[which(tb_uv$dts == dts[t_step]),],
           mapping = aes(x = lon, y = lat, fill = uv)) +
    geom_tile() + scale_fill_distiller(palette = "RdYlBu") +
    geom_point(mapping = aes(x = lon, y = lat, shape = method, fill = NULL),
               data = tb_subset,
               fill = "black", size = 1.2, show.legend = TRUE) +
    labs(shape = "Methode", fill = "Betrag der hor. Wind-\ngeschwindigkeit in $m\\,s^{-1}$") +
    guides(fill = guide_colourbar(title.position = "top",
                                  direction = "horizontal",
                                  label.position = "bottom"), 
           shape = guide_legend(title.position = "top",
                                direction = "horizontal",
                                label.position = "bottom",
                                nrow = 1)) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$ geographischer Länge",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Breitengrad in\n$^{\\circ}$ nördlicher Breite",
                       breaks = c(0, 30, 60, 90)) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group),
                 data = map_nh, fill = "gray50", alpha = 0.35) +
    coord_fixed(xlim = c(-180,180), ylim = c(0,90)) + 
    theme_bw() + theme(legend.position = "bottom")
  print(ggp_nh_m1_m2b)
  
  
  # Plot des zonalen Windfeldes und der zwei stärksten Chebyshev-Maxima im Bereich [20,85]
  # Datenaufbereitung
  tb_subset <-
    tb_jets_month %>%
    filter(year == dts_year[t_step] &
             month == dts_month[t_step]) %>%
    group_by(year, month, lon) %>%
    select(dts, year, month, season, method, class, lon, lat) %>%
    filter(method == "Chebyshev", class != "MJ")
  #print(unique(tb_subset$dts))
  # Visualisierung
  ggp_nh_m2c <-
    ggplot(data = tb_uv[which(tb_uv$dts == dts[t_step]),],
           mapping = aes(x = lon, y = lat, fill = uv)) +
    geom_tile() + scale_fill_distiller(palette = "RdYlBu") +
    geom_point(mapping = aes(x = lon , y = lat, shape = factor(class), fill = NULL),
               data = tb_subset,
               fill = "black", size = 1.2, show.legend = TRUE) +
    labs(shape = "Klasse", fill = "Betrag der hor. Wind- \ngeschwindigkeit in $m\\,s^{-1}$") +
    guides(fill = guide_colourbar(title.position = "top",
                                  direction = "horizontal",
                                  label.position = "bottom"), 
           shape = guide_legend(title.position = "top",
                                direction = "horizontal",
                                label.position = "bottom",
                                nrow = 1)) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$ geographischer Länge",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Breitengrad in\n$^{\\circ}$ nördlicher Breite",
                       breaks = c(0, 30, 60, 90)) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group),
                 data = map_nh, fill = "gray50", alpha = 0.35) +
    coord_fixed(xlim = c(-180,180), ylim = c(0,90)) + 
    theme_bw() + theme(legend.position = "bottom")
  print(ggp_nh_m2c)
  
  # Plot des Betrags des horizontalen Windfeldes und Dijkstra-Jets
  # Datenaufbereitung
  tb_subset <-
    tb_jets_month %>%
    filter(year == dts_year[t_step] &
             month == dts_month[t_step]) %>%
    group_by(year, month, lon) %>%
    select(dts, year, month, season, method, class, lon, lat) %>%
    filter(method == "Dijkstra", class != "MJ")
  #print(unique(tb_subset$dts))
  # Visualisierung
  ggp_nh_m3 <-
    ggplot(data = tb_uv[which(tb_uv$dts == dts[t_step]),],
           mapping = aes(x = lon, y = lat, fill = uv)) +
    geom_tile() + scale_fill_distiller(palette = "RdYlBu") +
    geom_point(mapping = aes(x = lon , y = lat, shape = factor(class), fill = NULL),
               data = tb_subset,
               fill = "black", size = 1.2, show.legend = TRUE) +
    labs(shape = "Klasse", fill = "Betrag der hor. Wind- \ngeschwindigkeit in $m\\,s^{-1}$") +
    guides(fill = guide_colourbar(title.position = "top",
                                  direction = "horizontal",
                                  label.position = "bottom"), 
           shape = guide_legend(title.position = "top",
                                direction = "horizontal",
                                label.position = "bottom",
                                nrow = 1)) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$ geographischer Länge",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Breitengrad in\n$^{\\circ}$ nördlicher Breite",
                       breaks = c(0, 30, 60, 90)) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group),
                 data = map_nh, fill = "gray50", alpha = 0.35) +
    coord_fixed(xlim = c(-180,180), ylim = c(0,90)) + 
    theme_bw() + theme(legend.position = "bottom")
  print(ggp_nh_m3)
  
  # ## Speichern der Plots als pdfs und tex-files
  # save_plot(plt = ggp_nh.m1.m2b, 
  #          width = 135, height = 75, pointsize = 9,
  #          filepath = paste0(save_dir, "02-case/"), 
  #          filename = paste0(dts_year[t_step], "-", dts_month[t_step], "-m1-m2b"))
  # save_plot(plt = ggp_nh.m2c, 
  #          width = 135, height = 75, pointsize = 9,
  #          filepath = paste0(save_dir, "02-case/"), 
  #          filename = paste0(dts_year[t_step], "-", dts_month[t_step], "-m2c"))
  # save_plot(plt = ggp_nh.m3, 
  #          width = 135, height = 75, pointsize = 9,
  #          filepath = paste0(save_dir, "02-case/"), 
  #          filename = paste0(dts_year[t_step], "-", dts_month[t_step], "-m3"))
}
