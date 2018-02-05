
## Jährliches Mittel
tb_subset_mean <-
  tb_jets_month %>%
  filter(class != "MJ") %>%
  select(year, season, method, class, lon, lat, u, v) %>%
  group_by(method, class, year, season, lon) %>%
  summarise_all(funs("mean", mean, mean(., na.rm = TRUE)))
## Gleitendes Mittel Fünf Jahre
rollmean5 <- function(data) rollapply(data, 5, mean, na.rm = TRUE, fill = NA, partial = TRUE)
tb_subset_rollmean <-
  tb_subset_mean %>%
  group_by(method, class, season, lon) %>%
  mutate_at(.vars = vars(lat_mean, u_mean, v_mean), .funs = funs("rollmean5", rollmean5, rollmean5(.)))

## VISUALISIERUNG DER HOVMÖLLER-DIAGRAMME  ####
# Schleife über Jahreszeiten
for (i_season in c("djf", "mam", "jja", "son")) {
  print(i_season)
  
  ## Chebyshev Polarfrontjet ####
  ## Breitengrad
  ggp_hovm_m2_pfj_lat <-
    ggplot(data = tb_subset_rollmean %>%
             filter(method == "Chebyshev", class == "PFJ", season == i_season),
           mapping = aes(x = lon, y = year, fill = lat_mean_rollmean5)) +
    geom_tile() + scale_fill_distiller(palette = "RdYlBu", 
                                       direction = 1,
                                       limits = c(40,80)) +
    labs(fill = "Breitengrad in $^{\\circ}$\nnördlicher Breite") +
    guides(fill = guide_colourbar(title.position = "left",
                                  direction = "horizontal",
                                  label.position = "bottom")) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$ geographischer Länge",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Jahr",
                       breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
    theme_bw() + theme(legend.position = "bottom")
  print(ggp_hovm_m2_pfj_lat)
  ## Zonalwind
  ggp_hovm_m2_pfj_u <- 
    ggplot(data = tb_subset_rollmean %>%
             filter(method == "Chebyshev", class == "PFJ", season == i_season),
           mapping = aes(x = lon, y = year, fill = u_mean_rollmean5)) +
    geom_tile() + scale_fill_distiller(palette = "RdYlBu", 
                                       direction = -1,
                                       limits = c(-5,35)) +
    labs(fill = "Zonalwind in $m\\,s^{-1}$") +
    guides(fill = guide_colourbar(title.position = "left",
                                  direction = "horizontal",
                                  label.position = "bottom")) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$ geographischer Länge",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Jahr",
                       breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
    theme_bw() + theme(legend.position = "bottom")
  print(ggp_hovm_m2_pfj_u)
  ## Meridionalwind
  ggp_hovm_m2_pfj_v <-
    ggplot(data = tb_subset_rollmean %>%
             filter(method == "Chebyshev", class == "PFJ", season == i_season),
           mapping = aes(x = lon, y = year, fill = v_mean_rollmean5)) +
    geom_tile() + scale_fill_distiller(palette = "RdYlBu",
                                       limits = c(-17,17)) +
    labs(fill = "Meridionalwind in $m\\,s^{-1}$") +
    guides(fill = guide_colourbar(title.position = "left",
                                  direction = "horizontal",
                                  label.position = "bottom")) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$ geographischer Länge",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Jahr",
                       breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
    theme_bw() + theme(legend.position = "bottom")
  print(ggp_hovm_m2_pfj_v)
  
  ## Chebyshev Subtropenjet ####
  ## Breitengrad
  ggp_hovm_m2_stj_lat <-
    ggplot(data = tb_subset_rollmean %>%
             filter(method == "Chebyshev", class == "STJ", season == i_season),
           mapping = aes(x = lon, y = year, fill = lat_mean_rollmean5)) +
    geom_tile() + scale_fill_distiller(palette = "RdYlBu", direction = 1,
                                       limits = c(20,50)) +
    labs(fill = "Breitengrad in $^{\\circ}$\nnördlicher Breite") +
    guides(fill = guide_colourbar(title.position = "left",
                                  direction = "horizontal",
                                  label.position = "bottom")) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$ geographischer Länge",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Jahr",
                       breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
    theme_bw() + theme(legend.position = "bottom")
  print(ggp_hovm_m2_stj_lat)
  ## Zonalwind
  ggp_hovm_m2_stj_u <-
    ggplot(data = tb_subset_rollmean %>%
             filter(method == "Chebyshev", class == "STJ", season == i_season),
           mapping = aes(x = lon, y = year, fill = u_mean_rollmean5)) +
    geom_tile() + scale_fill_distiller(palette = "RdYlBu",
                                       limits = c(10,80)) +
    labs(fill = "Zonalwind in $m\\,s^{-1}$") +
    guides(fill = guide_colourbar(title.position = "left",
                                  direction = "horizontal",
                                  label.position = "bottom")) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$ geographischer Länge",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Jahr",
                       breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
    theme_bw() + theme(legend.position = "bottom")
  print(ggp_hovm_m2_stj_u)
  ## Meridionalwind
  ggp_hovm_m2_stj_v <-
    ggplot(data = tb_subset_rollmean %>%
             filter(method == "Chebyshev", class == "STJ", season == i_season),
           mapping = aes(x = lon, y = year, fill = v_mean_rollmean5)) +
    geom_tile() + scale_fill_distiller(palette = "RdYlBu",
                                       limits = c(-15,15)) +
    labs(fill = "Meridionalwind in $m\\,s^{-1}$") +
    guides(fill = guide_colourbar(title.position = "left",
                                  direction = "horizontal",
                                  label.position = "bottom")) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$ geographischer Länge",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Jahr",
                       breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
    theme_bw() + theme(legend.position = "bottom")
  print(ggp_hovm_m2_stj_v)
  
  ## Dijkstra Polarfrontjet ####
  ## Breitengrad
  ggp_hovm_m3_pfj_lat <-
    ggplot(data = tb_subset_rollmean %>%
             filter(method == "Dijkstra", class == "PFJ", season == i_season),
           mapping = aes(x = lon, y = year, fill = lat_mean_rollmean5)) +
    geom_tile() + scale_fill_distiller(palette = "RdYlBu", direction = 1,
                                       limits = c(45,85)) +
    labs(fill = "Breitengrad in $^{\\circ}$\nnördlicher Breite") +
    guides(fill = guide_colourbar(title.position = "left",
                                  direction = "horizontal",
                                  label.position = "bottom")) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$ geographischer Länge",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Jahr",
                       breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
    theme_bw() + theme(legend.position = "bottom")
  print(ggp_hovm_m3_pfj_lat)
  ## Zonalwind
  ggp_hovm_m3_pfj_u <-
    ggplot(data = tb_subset_rollmean %>%
             filter(method == "Dijkstra", class == "PFJ", season == i_season),
           mapping = aes(x = lon, y = year, fill = u_mean_rollmean5)) +
    geom_tile() + scale_fill_distiller(palette = "RdYlBu",
                                       limits = c(-5, 40)) +
    labs(fill = "Breitengrad in $^{\\circ}$") +
    guides(fill = guide_colourbar(title.position = "left",
                                  direction = "horizontal",
                                  label.position = "bottom")) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$ geographischer Länge",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Jahr",
                       breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
    theme_bw() + theme(legend.position = "bottom")
  print(ggp_hovm_m3_pfj_u)
  ## Meridionalwind
  ggp_hovm_m3_pfj_v <-
    ggplot(data = tb_subset_rollmean %>%
             filter(method == "Dijkstra", class == "PFJ", season == i_season),
           mapping = aes(x = lon, y = year, fill = v_mean_rollmean5)) +
    geom_tile() + scale_fill_distiller(palette = "RdYlBu",
                                       limits = c(-20, 20)) +
    labs(fill = "Breitengrad in $^{\\circ}$") +
    guides(fill = guide_colourbar(title.position = "left",
                                  direction = "horizontal",
                                  label.position = "bottom")) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$ geographischer Länge",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Jahr",
                       breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
    theme_bw() + theme(legend.position = "bottom")
  print(ggp_hovm_m3_pfj_v)
  
  ## Dijkstra Subtropenjet ####
  ## Breitengrad
  ggp_hovm_m3_stj_lat <- 
    ggplot(data = tb_subset_rollmean %>%
             filter(method == "Dijkstra", class == "STJ", season == i_season),
           mapping = aes(x = lon, y = year, fill = lat_mean_rollmean5)) +
    geom_tile() + scale_fill_distiller(palette = "RdYlBu", direction = 1,
                                       limits = c(15, 50)) +
    labs(fill = "Breitengrad in $^{\\circ}$") +
    guides(fill = guide_colourbar(title.position = "left",
                                  direction = "horizontal",
                                  label.position = "bottom")) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$ geographischer Länge",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Jahr",
                       breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
    theme_bw() + theme(legend.position = "bottom")
  print(ggp_hovm_m3_stj_lat)
  ## Zonalwind
  ggp_hovm_m3_stj_u <- 
    ggplot(data = tb_subset_rollmean %>%
             filter(method == "Dijkstra", class == "STJ", season == i_season),
           mapping = aes(x = lon, y = year, fill = u_mean_rollmean5)) +
    geom_tile() + scale_fill_distiller(palette = "RdYlBu",
                                       limits = c(0, 80)) +
    labs(fill = "Zonalwind in $m\\,s^{-1}$") +
    guides(fill = guide_colourbar(title.position = "left",
                                  direction = "horizontal",
                                  label.position = "bottom")) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$ geographischer Länge",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Jahr",
                       breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
    theme_bw() + theme(legend.position = "bottom")
  print(ggp_hovm_m3_stj_u)
  ## Meridionalwind
  ggp_hovm_m3_stj_v <-
    ggplot(data = tb_subset_rollmean %>%
             filter(method == "Dijkstra", class == "STJ", season == i_season),
           mapping = aes(x = lon, y = year, fill = v_mean_rollmean5)) +
    geom_tile() + scale_fill_distiller(palette = "RdYlBu",
                                       limits = c(-15, 15)) +
    labs(fill = "Meridionalwind in $m\\,s^{-1}$") +
    guides(fill = guide_colourbar(title.position = "left",
                                  direction = "horizontal",
                                  label.position = "bottom")) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$ geographischer Länge",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Jahr",
                       breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
    theme_bw() + theme(legend.position = "bottom")
  print(ggp_hovm_m3_stj_v)
  
  
  # # Speichern der Plots ####
  # # Breitengrade
  # save_plot(plt = ggp_hovm_m2_pfj_lat, width = 140, height = 100, pointsize = 9,
  #          filepath = paste0(save_dir, "05-hovm-lat/"),
  #          filename = paste0("hovm-chebyshev-pfj-", i_season))
  # save_plot(plt = ggp_hovm_m2_stj_lat, width = 140, height = 100, pointsize = 9,
  #          filepath = paste0(save_dir, "05-hovm-lat/"),
  #          filename = paste0("hovm-chebyshev-stj-", i_season))
  # save_plot(plt = ggp_hovm_m3_pfj_lat, width = 140, height = 100, pointsize = 9,
  #          filepath = paste0(save_dir, "05-hovm-lat/"),
  #          filename = paste0("hovm-dijkstra-pfj-", i_season))
  # save_plot(plt = ggp_hovm_m3_stj_lat, width = 140, height = 100, pointsize = 9,
  #          filepath = paste0(save_dir, "05-hovm-lat/"),
  #          filename = paste0("hovm-dijkstra-stj-", i_season))
  # # Zonalwind
  # save_plot(plt = ggp_hovm_m2_pfj_u, width = 140, height = 100, pointsize = 9,
  #          filepath = paste0(save_dir, "06-hovm-u/"),
  #          filename = paste0("hovm-chebyshev-pfj-", i_season))
  # save_plot(plt = ggp_hovm_m2_stj_u, width = 140, height = 100, pointsize = 9,
  #          filepath = paste0(save_dir, "06-hovm-u/"),
  #          filename = paste0("hovm-chebyshev-stj-", i_season))
  # save_plot(plt = ggp_hovm_m3_pfj_u, width = 140, height = 100, pointsize = 9,
  #          filepath = paste0(save_dir, "06-hovm-u/"),
  #          filename = paste0("hovm-dijkstra-pfj-", i_season))
  # save_plot(plt = ggp_hovm_m3_stj_u, width = 140, height = 100, pointsize = 9,
  #          filepath = paste0(save_dir, "06-hovm-u/"),
  #          filename = paste0("hovm-dijkstra-stj-", i_season))
  # # Meridionalwind
  # save_plot(plt = ggp_hovm_m2_pfj_v, width = 140, height = 100, pointsize = 9,
  #          filepath = paste0(save_dir, "07-hovm-v/"),
  #          filename = paste0("hovm-chebyshev-pfj-", i_season))
  # save_plot(plt = ggp_hovm_m2_stj_v, width = 140, height = 100, pointsize = 9,
  #          filepath = paste0(save_dir, "07-hovm-v/"),
  #          filename = paste0("hovm-chebyshev-stj-", i_season))
  # save_plot(plt = ggp_hovm_m3_pfj_v, width = 140, height = 100, pointsize = 9,
  #          filepath = paste0(save_dir, "07-hovm-v/"),
  #          filename = paste0("hovm-dijkstra-pfj-", i_season))
  # save_plot(plt = ggp_hovm_m3_stj_v, width = 140, height = 100, pointsize = 9,
  #          filepath = paste0(save_dir, "07-hovm-v/"),
  #          filename = paste0("hovm-dijkstra-stj-", i_season))
  
}

rm(tb_subset_rollmean, tb_subset_mean)
