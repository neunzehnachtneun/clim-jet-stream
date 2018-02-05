
## 1. Mittelwert
tb_subset_yr_mean <-
  tb_jets_month %>%
  filter(class != "MJ") %>%
#  filter(lon >= -90 & lon <= -0) %>%
  select(year, season, month, dts, method, class, lon, lat, u, v, extent) %>%
  group_by(method, class, year, month, dts) %>%
  summarise_at(.vars = vars(lat, u, v), .funs = funs("mean", mean, mean(., na.rm = TRUE)))
tb_subset_yr_mean <- left_join(tb_subset_yr_mean, tb_sea_ice, by = c("year", "month"))
tb_subset_yr_mean <- 
  tb_subset_yr_mean %>%
  group_by(month) %>%
  mutate(lat_mon_mean = mean(lat_mean, na.rm = TRUE),
         u_mon_mean = mean(u_mean, na.rm = TRUE),
         v_mon_mean = mean(v_mean, na.rm = TRUE),
         extent_mon_mean = mean(extent, na.rm = TRUE),
         area_mon_mean = mean(area, na.rm = TRUE)) %>%
  mutate(lat_anom = lat_mean - lat_mon_mean,
         u_anom = u_mean - u_mon_mean,
         v_anom = v_mean - v_mon_mean,
         extent_anom = extent - extent_mon_mean,
         area_anom = area - area_mon_mean) %>%
  select(year, month, dts, method, class, lat_anom, u_anom, v_anom, extent_anom, extent)



## 2. Standardabweichung
tb_subset_yr_sdev <-
  tb_jets_month %>%
  filter(class != "MJ") %>%
#  filter(lon >= -90 & lon <= -0) %>%
  select(year, month, dts, method, class, lon, lat, u, v, extent) %>%
  group_by(method, class, year, month, dts) %>%
  summarise_at(.vars = vars(lat, u, v), .funs = funs("sd", sd, sd(., na.rm = TRUE)))
tb_subset_yr_sdev <- left_join(tb_subset_yr_sdev, tb_sea_ice, by = c("year", "month"))
tb_subset_yr_sdev <- 
  tb_subset_yr_sdev %>%
  group_by(month) %>%
  mutate(lat_mon_mean = mean(lat_sd, na.rm = TRUE),
         u_mon_mean = mean(u_sd, na.rm = TRUE),
         v_mon_mean = mean(v_sd, na.rm = TRUE),
         extent_mon_mean = mean(extent, na.rm = TRUE),
         area_mon_mean = mean(area, na.rm = TRUE)) %>%
  mutate(lat_anom = lat_sd - lat_mon_mean,
         u_anom = u_sd - u_mon_mean,
         v_anom = v_sd - v_mon_mean,
         extent_anom = extent - extent_mon_mean,
         area_anom = area - area_mon_mean) %>%
  select(year, month, dts, method, class, lat_anom, u_anom, v_anom, extent_anom, extent)

## Seeeisentwicklung allgemein
ggp_seaice <- 
  ggplot(data = tb_subset_yr_mean %>%
           filter(year >= 1978),
         mapping = aes(x = dts, y = extent)) + 
  geom_line(size = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, color = "black") +
  scale_x_datetime(name = "Jahr") +
  scale_y_continuous(name = "Meereis-Ausdehnung in $10^{6}\\,km^{2}$",
                     breaks = c(4,8,12,16)) +
  theme_bw() + theme(legend.position = "bottom")
print(ggp_seaice)


## Chebyshev
## Mean
ggp_seaice_chebyshev_mean <-
  ggplot(data = tb_subset_yr_mean %>%
           filter(method == "Chebyshev" & class == "PFJ"), 
         mapping = aes(x = extent_anom, y = lat_anom)) + 
  geom_point(size = 0.2) + scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  geom_smooth(method = "lm", formula = y ~ x, color = "black") +
  scale_x_reverse(name = "Meereis-Ausdehnung in $10^{6}\\,km^{2}$") +
  scale_y_continuous(name = "Positionen in\n$^{\\circ}$ nördlicher Breite") +
  theme_bw() +
  theme(legend.position = "bottom")
print(ggp_seaice_chebyshev_mean)

## Sdev
ggp_seaice_chebyshev_sdev <-
  ggplot(data =   tb_subset_yr_sdev %>%
           filter(method == "Chebyshev" & class == "PFJ"), 
         mapping = aes(x = extent_anom, y = lat_anom)) + 
         # mapping = aes(x = extent, y = lat_sd)) + 
  geom_point(size = 0.2) + scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  geom_smooth(method = "lm", formula = y ~ x, color = "black") +
  scale_x_reverse(name = "Meereis-Ausdehnung in $10^{6}\\,km^{2}$") +
  scale_y_continuous(name = "Positionen in\n$^{\\circ}$ nördlicher Breite") +
  theme_bw() +
  theme(legend.position = "bottom")
print(ggp_seaice_chebyshev_sdev)

## Dijkstra
## Mean
ggp_seaice_dijkstra_mean <-
  ggplot(data =   tb_subset_yr_mean %>%
           filter(method == "Dijkstra" & class == "PFJ"), 
         mapping = aes(x = extent_anom, y = lat_anom)) + 
  geom_point(size = 0.2) + scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  geom_smooth(method = "lm", formula = y ~ x, color = "black") +
  scale_x_reverse(name = "Meereis-Ausdehnung in $10^{6}\\,km^{2}$") +
  scale_y_continuous(name = "Positionen in\n$^{\\circ}$ nördlicher Breite") +
  theme_bw() +
  theme(legend.position = "bottom")
print(ggp_seaice_dijkstra_mean)
## Sdev
ggp_seaice_dijkstra_sdev <-
  ggplot(data =   tb_subset_yr_sdev %>%
           filter(method == "Dijkstra" & class == "PFJ"), 
         mapping = aes(x = extent_anom, y = lat_anom)) + 
  geom_point(size = 0.2) + scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  geom_smooth(method = "lm", formula = y ~ x, color = "black") +
  scale_x_reverse(name = "Meereis-Ausdehnung in $10^{6}\\,km^{2}$") +
  scale_y_continuous(name = "Positionen in\n$^{\\circ}$ nördlicher Breite") +
  theme_bw() +
  theme(legend.position = "bottom")
print(ggp_seaice_dijkstra_sdev)


# save_plot(plt = ggp.seaice, 
#          width = 135, height = 70, pointsize = 9,
#          filepath = paste0(save_dir, "08-sea-ice/"), 
#          filename = "seaice-timeseries")
# 
# save_plot(plt = ggp.seaice.chebyshev.mean, 
#          width = 135, height = 70, pointsize = 9,
#          filepath = paste0(save_dir, "08-sea-ice/"), 
#          filename = "seaice-cheb-mean")
# 
# save_plot(plt = ggp.seaice.chebyshev.sdev, 
#          width = 135, height = 70, pointsize = 9,
#          filepath = paste0(save_dir, "08-sea-ice/"), 
#          filename = "seaice-cheb-sdev")
# 
# save_plot(plt = ggp.seaice.dijkstra.mean, 
#          width = 135, height = 70, pointsize = 9,
#          filepath = paste0(save_dir, "08-sea-ice/"), 
#          filename = "seaice-dijk-mean")
# 
# save_plot(plt = ggp.seaice.dijkstra.sdev, 
#          width = 135, height = 70, pointsize = 9,
#          filepath = paste0(save_dir, "08-sea-ice/"), 
#          filename = "seaice-dijk-sdev")


## Korrelationen nach Pearson zwischen arktischem Seeeis und * ####
## 
# Chebyshev Mean
tb_subset_yr_mean %>%
  filter(method == "Chebyshev") %$%
  cor.test(extent_anom, lat_anom) %>%
  tidy
# Chebyshev Standardabweichung
tb_subset_yr_sdev %>%
  filter(method == "Chebyshev") %$%
  cor.test(extent_anom, lat_anom) %>%
  tidy
# Dijkstra Mittel
tb_subset_yr_mean %>%
  filter(method == "Dijkstra") %$%
  cor.test(extent_anom, lat_anom) %>%
  tidy
# Dijkstra Standardabweichung
tb_subset_yr_sdev %>%
  filter(method == "Dijkstra") %$%
  cor.test(extent_anom, lat_anom) %>%
  tidy
