
ggp_anom_tropics_u <-
  ggplot(data = tb_uv %>%
           select(lon, lat, dts, u,v) %>%
           filter(lat >= 0 & lat < 23.5) %>%
           group_by(dts) %>%
           summarise_at(.vars = vars(u, v), .funs = funs("mean", mean, mean(., na.rm = TRUE))) %>%
           mutate(year = year(dts), month = month(dts)) %>%
           group_by(month) %>%
           mutate(u_mon_mean = mean(u_mean, na.rm = TRUE), v_mon_mean = mean(v_mean, na.rm = TRUE)) %>%
           mutate(u_anom = u_mean - u_mon_mean, v_anom = v_mean - v_mon_mean),
         mapping = aes(x = dts, y = u_anom)) +
  geom_line(size = 0.4) +
  geom_vline(xintercept = dts[257], linetype = 4) +
  scale_x_datetime(name = "Jahr", 
                   date_breaks = "10 years", date_minor_breaks = "1 year",
                   date_labels = "%Y") + 
  scale_y_continuous(name = "Anomalien der zonalen \nWindkomponente in $m\\,s^{-1}$") +
  theme_bw() + theme(legend.position = "bottom")
print(ggp_anom_tropics_u)

ggp_anom_tropics_v <-
  ggplot(data = tb_uv %>%
           select(lon, lat, dts, u,v) %>%
           filter(lat >= 0 & lat < 23.5) %>%
           group_by(dts) %>%
           summarise_at(.vars = vars(u, v), .funs = funs("mean", mean, mean(., na.rm = TRUE))) %>%
           mutate(year = year(dts), month = month(dts)) %>%
           group_by(month) %>%
           mutate(u_mon_mean = mean(u_mean, na.rm = TRUE), v_mon_mean = mean(v_mean, na.rm = TRUE)) %>%
           mutate(u_anom = u_mean - u_mon_mean, v_anom = v_mean - v_mon_mean),
         mapping = aes(x = dts, y = v_anom)) +
  geom_line(size = 0.4) +
  geom_vline(xintercept = dts[257], linetype = 4) +
  scale_x_datetime(name = "Jahr", 
                   date_breaks = "10 years", date_minor_breaks = "1 year",
                   date_labels = "%Y") + 
  scale_y_continuous(name = "Anomalien der meridionalen \nWindkomponente in $m\\,s^{-1}$") +
  theme_bw() + theme(legend.position = "bottom")
print(ggp_anom_tropics_v)

ggp_anom_midlat_u <-
  ggplot(data = tb_uv %>%
           select(lon, lat, dts, u,v) %>%
           filter(lat >= 23.5 & lat < 66.5) %>%
           group_by(dts) %>%
           summarise_at(.vars = vars(u, v), .funs = funs("mean", mean, mean(., na.rm = TRUE))) %>%
           mutate(year = year(dts), month = month(dts)) %>%
           group_by(month) %>%
           mutate(u_mon_mean = mean(u_mean, na.rm = TRUE), v_mon_mean = mean(v_mean, na.rm = TRUE)) %>%
           mutate(u_anom = u_mean - u_mon_mean, v_anom = v_mean - v_mon_mean),
         mapping = aes(x = dts, y = u_anom)) +
  geom_line(size = 0.4) +
  geom_vline(xintercept = dts[257], linetype = 4) +
  scale_x_datetime(name = "Jahr", 
                   date_breaks = "10 years", date_minor_breaks = "1 year",
                   date_labels = "%Y") + 
  scale_y_continuous(name = "Anomalien der zonalen \nWindkomponente in $m\\,s^{-1}$") +
  theme_bw() + theme(legend.position = "bottom")
print(ggp_anom_midlat_u)

ggp_anom_midlat_v <-
  ggplot(data = tb_uv %>%
           select(lon, lat, dts, u,v) %>%
           filter(lat >= 23.5 & lat < 66.5) %>%
           group_by(dts) %>%
           summarise_at(.vars = vars(u, v), .funs = funs("mean", mean, mean(., na.rm = TRUE))) %>%
           mutate(year = year(dts), month = month(dts)) %>%
           group_by(month) %>%
           mutate(u_mon_mean = mean(u_mean, na.rm = TRUE), v_mon_mean = mean(v_mean, na.rm = TRUE)) %>%
           mutate(u_anom = u_mean - u_mon_mean, v_anom = v_mean - v_mon_mean),
         mapping = aes(x = dts, y = v_anom)) +
  geom_line(size = 0.4) +
  geom_vline(xintercept = dts[257], linetype = 4) +
  scale_x_datetime(name = "Jahr", 
                   date_breaks = "10 years", date_minor_breaks = "1 year",
                   date_labels = "%Y") + 
  scale_y_continuous(name = "Anomalien der meridionalen \nWindkomponente in $m\\,s^{-1}$") +
  theme_bw() + theme(legend.position = "bottom")
print(ggp_anom_midlat_v)

ggp_anom_polar_u <-
  ggplot(data = tb_uv %>%
           select(lon, lat, dts, u,v) %>%
           filter(lat >= 66.5 & lat < 90) %>%
           group_by(dts) %>%
           summarise_at(.vars = vars(u, v), .funs = funs("mean", mean, mean(., na.rm = TRUE))) %>%
           mutate(year = year(dts), month = month(dts)) %>%
           group_by(month) %>%
           mutate(u_mon_mean = mean(u_mean, na.rm = TRUE), v_mon_mean = mean(v_mean, na.rm = TRUE)) %>%
           mutate(u_anom = u_mean - u_mon_mean, v_anom = v_mean - v_mon_mean),
         mapping = aes(x = dts, y = u_anom)) +
  geom_line(size = 0.4) +
  geom_vline(xintercept = dts[257], linetype = 4) +
  scale_x_datetime(name = "Jahr", 
                   date_breaks = "10 years", date_minor_breaks = "1 year",
                   date_labels = "%Y") + 
  scale_y_continuous(name = "Anomalien der zonalen \nWindkomponente in $m\\,s^{-1}$") +
  theme_bw() + theme(legend.position = "bottom")
print(ggp_anom_polar_u)

ggp_anom_polar_v <-
  ggplot(data = tb_uv %>%
           select(lon, lat, dts, u,v) %>%
           filter(lat >= 66.5 & lat < 90) %>%
           group_by(dts) %>%
           summarise_at(.vars = vars(u, v), .funs = funs("mean", mean, mean(., na.rm = TRUE))) %>%
           mutate(year = year(dts), month = month(dts)) %>%
           group_by(month) %>%
           mutate(u_mon_mean = mean(u_mean, na.rm = TRUE), v_mon_mean = mean(v_mean, na.rm = TRUE)) %>%
           mutate(u_anom = u_mean - u_mon_mean, v_anom = v_mean - v_mon_mean),
         mapping = aes(x = dts, y = v_anom)) +
  geom_line(size = 0.4) +
  geom_vline(xintercept = dts[257], linetype = 4) +
  scale_x_datetime(name = "Jahr", 
                   date_breaks = "10 years", date_minor_breaks = "1 year",
                   date_labels = "%Y") + 
  scale_y_continuous(name = "Anomalien der meridionalen \nWindkomponente in $m\\,s^{-1}$") +
  theme_bw() + theme(legend.position = "bottom")
print(ggp_anom_polar_v)


# save_plot(plt = ggp_anom_tropics_u, 
#          width = 135, height = 55, pointsize = 9,
#          filepath = paste0(save_dir, "10-praes/"), 
#          filename = "tropics-u")
# save_plot(plt = ggp_anom_tropics_v, 
#          width = 135, height = 55, pointsize = 9,
#          filepath = paste0(save_dir, "10-praes/"), 
#          filename = "tropics-v")
# 
# save_plot(plt = ggp_anom_midlat_u, 
#          width = 135, height = 55, pointsize = 9,
#          filepath = paste0(save_dir, "10-praes/"), 
#          filename = "midlat-u")
# save_plot(plt = ggp_anom_midlat_v, 
#          width = 135, height = 55, pointsize = 9,
#          filepath = paste0(save_dir, "10-praes/"), 
#          filename = "midlat-v")
# 
# save_plot(plt = ggp_anom_polar_u, 
#          width = 135, height = 55, pointsize = 9,
#          filepath = paste0(save_dir, "10-praes/"), 
#          filename = "polar-u")
# save_plot(plt = ggp_anom_polar_v, 
#          width = 135, height = 55, pointsize = 9,
#          filepath = paste0(save_dir, "10-praes/"), 
#          filename = "polar-v")
