
## Mittel -> method, class, year ####
## Für Klimatrends!
## 1. Mittelwert
tb_subset_yr_mean <-
  tb_jets_month %>%
  filter(class != "MJ") %>%
  select(year, season, method, class, lon, lat, u, v) %>%
  group_by(method, class, year) %>%
  summarise_at(.vars = vars(lat, u, v), .funs = funs("mean", mean, mean(., na.rm = TRUE)))
## 2. Standardabweichung
tb_subset_yr_sdev <-
  tb_jets_month %>%
  filter(class != "MJ") %>%
  select(year, method, class, lon, lat, u, v) %>%
  group_by(method, class, year) %>%
  summarise_at(.vars = vars(lat, u, v), .funs = funs("sd", sd, sd(., na.rm = TRUE)))

## Kernergebnisse (Mittelwerte, Standardabweichung, Trends) ####
## 
View(tb_jets_month %>%
       filter(class == "PFJ" | class == "STJ") %>%
       select(year, season, method, class, lon, lat, u, v) %>%
       group_by(method, class, season) %>%
       summarise_at(.vars = vars(lat, u, v), .funs = funs("mean", mean, mean(., na.rm = TRUE)))
)

View(tb_jets_month %>%
       filter(class == "PFJ" | class == "STJ") %>%
       select(year, season, method, class, lon, lat, u, v) %>%
       group_by(method, class, season) %>%
       summarise_at(.vars = vars(lat, u, v), .funs = funs("sd", sd, sd(., na.rm = TRUE)))
)

## Trends
View(tb_jets_month %>%
       filter(class == "PFJ" | class == "STJ") %>%
       select(year, season, method, class, lon, lat, u, v) %>%
       group_by(method, class) %>%
       do(model = lm(u ~ year, data = .)) %>% 
       mutate(Intercept = coef(model)[1], Slope = 10*coef(model)[2]) %>%
       select(-model)
)

View(tb_jets_month %>%
       filter(class == "PFJ" | class == "STJ") %>%
       select(year, season, method, class, lon, lat, u, v) %>%
       group_by(method, class, season) %>%
       do(model = lm(lat ~ year, data = .)) %>% 
       mutate(Intercept = coef(model)[1], Slope = coef(model)[2]) %>%
       select(-model)
)


## Klimatrend Breitengrad Position ####
# Trend Positionen Polarfrontjet
ggp_clim_pfj_lat_mn <- 
ggplot(data = tb_subset_yr_mean %>%
         filter(class == "PFJ"),
       mapping = aes(x = year, y = lat_mean, 
                     shape = method))  + geom_point() + 
  stat_smooth(mapping = aes(color = method),
              method = "lm", formula = y ~ x,
              se = TRUE, level = 0.95) +
  labs(shape = "Methode", color = "Lineares Modell") +
  guides(shape = guide_legend(order = 1,
                              title.position = "top", 
                              reverse = TRUE),
         colour = guide_legend(order = 2,
                               title.position = "top", 
                               reverse = TRUE)) +     
  scale_x_continuous(name = "Jahr",
                     breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
  scale_y_continuous(name = "Breitengrad in\n$^{\\circ}$ nördlicher Breite",
                     breaks = c(50, 55, 60, 65, 70)) +
  theme_bw() + theme(legend.position = "bottom")
print(ggp_clim_pfj_lat_mn)
# Variabilität
ggp_clim_pfj_lat_sd <- 
  ggplot(data = tb_subset_yr_sdev %>%
           filter(class == "PFJ"),
         mapping = aes(x = year, y = lat_sd, 
                       shape = method))  + geom_point() + 
  geom_smooth(mapping = aes(color = method),
              method = "lm", formula = y ~ x) +
  labs(shape = "Methode", color = "Lineares Modell") +
  guides(shape = guide_legend(order = 1,
                              title.position = "top", 
                              reverse = TRUE),
         colour = guide_legend(order = 2,
                               title.position = "top", 
                               reverse = TRUE)) +  
  scale_x_continuous(name = "Jahr",
                     breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
  scale_y_continuous(name = "Breitengrad in\n$^{\\circ}$nördlicher Breite") +
  theme_bw() + theme(legend.position = "bottom")
print(ggp_clim_pfj_lat_sd)
# Trend Positionen Subtropenjet
ggp_clim_stj_lat_mn <- 
ggplot(data = tb_subset_yr_mean %>%
         filter(class == "STJ"),
       mapping = aes(x = year, y = lat_mean, 
                     shape = method)) + geom_point() + 
  geom_smooth(mapping = aes(color = method),
              method = "lm", formula = y ~ x) +
  labs(shape = "Methode", color = "Lineares Modell") +
  guides(shape = guide_legend(order = 1,
                              title.position = "top", 
                              reverse = TRUE),
         colour = guide_legend(order = 2,
                               title.position = "top", 
                               reverse = TRUE)) +  
  scale_x_continuous(name = "Jahr",
                     breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
  scale_y_continuous(name = "Breitengrad in\n$^{\\circ}$nördlicher Breite",
                     breaks = c(32,34,36)) +
  theme_bw() + theme(legend.position = "bottom")
print(ggp_clim_stj_lat_mn)
# Variabilität
ggp_clim_stj_lat_sd <- 
  ggplot(data = tb_subset_yr_sdev %>%
           filter(class == "STJ"),
         mapping = aes(x = year, y = lat_sd, 
                       shape = method)) + geom_point() + 
  geom_smooth(mapping = aes(color = method),
              method = "lm", formula = y ~ x) +
  labs(shape = "Methode", color = "Lineares Modell") +
  guides(shape = guide_legend(order = 1,
                              title.position = "top", 
                              reverse = TRUE),
         colour = guide_legend(order = 2,
                               title.position = "top", 
                               reverse = TRUE)) +   
  scale_x_continuous(name = "Jahr",
                     breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
  scale_y_continuous(name = "Breitengrad in $^{\\circ}$\nnördlicher Breite") +
  theme_bw() + theme(legend.position = "bottom")
print(ggp_clim_stj_lat_sd)

## Klimatrend Zonalwindgeschwindigkeit ####
# Trend Zonalwind Polarfrontjet
ggp_clim_pfj_u_mn <- 
  ggplot(data = tb_subset_yr_mean %>%
           filter(class == "PFJ"),
         mapping = aes(x = year, y = u_mean, 
                       shape = method))  + geom_point() + 
  geom_smooth(mapping = aes(color = method),
              method = "lm", formula = y ~ x) +
  labs(shape = "Methode", color = "Lineares Modell") +
  guides(shape = guide_legend(order = 1,
                              title.position = "top", 
                              reverse = TRUE),
         colour = guide_legend(order = 2,
                               title.position = "top", 
                               reverse = TRUE)) +   
  scale_x_continuous(name = "Jahr",
                     breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
  scale_y_continuous(name = "Zonalwind in $m\\,s^{-1}$") +
  theme_bw() + theme(legend.position = "bottom")
print(ggp_clim_pfj_u_mn)
# Variabilität
ggp_clim_pfj_u_sd <- 
  ggplot(data = tb_subset_yr_sdev %>%
           filter(class == "PFJ"),
         mapping = aes(x = year, y = u_sd, 
                       shape = method))  + geom_point() + 
  geom_smooth(mapping = aes(color = method),
              method = "lm", formula = y ~ x) +
  labs(shape = "Methode", color = "Lineares Modell") +
  guides(shape = guide_legend(order = 1,
                              title.position = "top", 
                              reverse = TRUE),
         colour = guide_legend(order = 2,
                               title.position = "top", 
                               reverse = TRUE)) +  
  scale_x_continuous(name = "Jahr",
                     breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
  scale_y_continuous(name = "Zonalwind in $m\\,s^{-1}$") +
  theme_bw() + theme(legend.position = "bottom")
print(ggp_clim_pfj_u_sd)

# Trend Zonalwind Subtropenjet
ggp_clim_stj_u_mn <- 
  ggplot(data = tb_subset_yr_mean %>%
           filter(class == "STJ"),
         mapping = aes(x = year, y = u_mean, 
                       shape = method)) + geom_point() + 
  geom_smooth(mapping = aes(color = method),
              method = "lm", formula = y ~ x) +
  labs(shape = "Methode", color = "Lineares Modell") +
  guides(shape = guide_legend(order = 1,
                              title.position = "top", 
                              reverse = TRUE),
         colour = guide_legend(order = 2,
                               title.position = "top", 
                               reverse = TRUE)) +  
  scale_x_continuous(name = "Jahr",
                     breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
  scale_y_continuous(name = "Zonalwind in $m\\,s^{-1}$") +
  theme_bw() + theme(legend.position = "bottom")
print(ggp_clim_stj_u_mn)
# Variabilität
ggp_clim_stj_u_sd <- 
  ggplot(data = tb_subset_yr_sdev %>%
           filter(class == "STJ"),
         mapping = aes(x = year, y = u_sd, 
                       shape = method)) + geom_point() + 
  geom_smooth(mapping = aes(color = method),
              method = "lm", formula = y ~ x) +
  labs(shape = "Methode", color = "Lineares Modell") +
  guides(shape = guide_legend(order = 1,
                              title.position = "top", 
                              reverse = TRUE),
         colour = guide_legend(order = 2,
                               title.position = "top", 
                               reverse = TRUE)) +  
  scale_x_continuous(name = "Jahr",
                     breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
  scale_y_continuous(name = "Zonalwind in $m\\,s^{-1}$") +
  theme_bw() + theme(legend.position = "bottom")
print(ggp_clim_stj_u_sd)

## Klimatrend Meridionalwindgeschwindigkeit ####
# Trend Meridionalwind Polarfrontjet
ggp_clim_pfj_v_mn <- 
  ggplot(data = tb_subset_yr_mean %>%
           filter(class == "PFJ"),
         mapping = aes(x = year, y = v_mean, 
                       shape = method))  + geom_point() + 
  geom_smooth(mapping = aes(color = method),
              method = "lm", formula = y ~ x) +
  labs(shape = "Methode", color = "Lineares Modell") +
  guides(shape = guide_legend(order = 1,
                              title.position = "top", 
                              reverse = TRUE),
         colour = guide_legend(order = 2,
                               title.position = "top", 
                               reverse = TRUE)) +  
  scale_x_continuous(name = "Jahr",
                     breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
  scale_y_continuous(name = "Meridionalwind in $m\\,s^{-1}$") +
  theme_bw() + theme(legend.position = "bottom")
print(ggp_clim_pfj_v_mn)
# Variabilität
ggp_clim_pfj_v_sd <- 
  ggplot(data = tb_subset_yr_sdev %>%
           filter(class == "PFJ"),
         mapping = aes(x = year, y = v_sd, 
                       shape = method))  + geom_point() + 
  geom_smooth(mapping = aes(color = method),
              method = "lm", formula = y ~ x) +
  labs(shape = "Methode", color = "Lineares Modell") +
  guides(shape = guide_legend(order = 1,
                              title.position = "top", 
                              reverse = TRUE),
         colour = guide_legend(order = 2,
                               title.position = "top", 
                               reverse = TRUE)) +  
  scale_x_continuous(name = "Jahr",
                     breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
  scale_y_continuous(name = "Meridionalwind in $m\\,s^{-1}$") +
  theme_bw() + theme(legend.position = "bottom")
print(ggp_clim_pfj_v_sd)

# Trend Meridionalwind Subtropenjet
ggp_clim_stj_v_mn <- 
  ggplot(data = tb_subset_yr_mean %>%
           filter(class == "STJ"),
         mapping = aes(x = year, y = v_mean, 
                       shape = method)) + geom_point() + 
  geom_smooth(mapping = aes(color = method),
              method = "lm", formula = y ~ x) +
  labs(shape = "Methode", color = "Lineares Modell") +
  guides(shape = guide_legend(order = 1,
                              title.position = "top", 
                              reverse = TRUE),
         colour = guide_legend(order = 2,
                               title.position = "top", 
                               reverse = TRUE)) +  
  scale_x_continuous(name = "Jahr",
                     breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
  scale_y_continuous(name = "Meridionalwind in $m\\,s^{-1}$") +
  theme_bw() + theme(legend.position = "bottom")
print(ggp_clim_stj_v_mn)
# Variabilität
ggp_clim_stj_v_sd <- 
  ggplot(data = tb_subset_yr_sdev %>%
           filter(class == "STJ"),
         mapping = aes(x = year, y = v_sd, 
                       shape = method)) + geom_point() + 
  geom_smooth(mapping = aes(color = method),
              method = "lm", formula = y ~ x) +
  labs(shape = "Methode", color = "Lineares Modell") +
  guides(shape = guide_legend(order = 1,
                              title.position = "top", 
                              reverse = TRUE),
         colour = guide_legend(order = 2,
                               title.position = "top", 
                               reverse = TRUE)) +  
  scale_x_continuous(name = "Jahr",
                     breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
  scale_y_continuous(name = "Meridionalwind in $m\\,s^{-1}$") +
  theme_bw() + theme(legend.position = "bottom")
print(ggp_clim_stj_v_sd)


# ## Speichern der Klimatrendplots ####
# ## 
# save_plot(plt = ggp_clim_stj_lat_mn, 
#          width = 135, height = 65, pointsize = 9, 
#          filepath = paste0(save_dir, "04-clim/"), 
#          filename = "stj-lat-mn")
# save_plot(plt = ggp_clim_stj_lat_sd, 
#          width = 135, height = 65, pointsize = 9, 
#          filepath = paste0(save_dir, "04-clim/"), 
#          filename = "stj-lat-sd")
# 
# save_plot(plt = ggp_clim_pfj_lat_mn, 
#          width = 135, height = 65, pointsize = 9, 
#          filepath = paste0(save_dir, "04-clim/"), 
#          filename = "pfj-lat-mn")
# save_plot(plt = ggp_clim_pfj_lat_sd, 
#          width = 135, height = 65, pointsize = 9, 
#          filepath = paste0(save_dir, "04-clim/"), 
#          filename = "pfj-lat-sd")
# 
# save_plot(plt = ggp_clim_pfj_u_mn, 
#          width = 135, height = 65, pointsize = 9, 
#          filepath = paste0(save_dir, "04-clim/"), 
#          filename = "pfj-u-mn")
# save_plot(plt = ggp_clim_pfj_u_sd, 
#          width = 135, height = 65, pointsize = 9, 
#          filepath = paste0(save_dir, "04-clim/"), 
#          filename = "pfj-u-sd")
# 
# save_plot(plt = ggp_clim_stj_u_mn, 
#          width = 135, height = 65, pointsize = 9, 
#          filepath = paste0(save_dir, "04-clim/"), 
#          filename = "stj-u-mn")
# save_plot(plt = ggp_clim_stj_u_sd, 
#          width = 135, height = 65, pointsize = 9, 
#          filepath = paste0(save_dir, "04-clim/"), 
#          filename = "stj-u-sd")
# 
# save_plot(plt = ggp_clim_pfj_v_mn, 
#          width = 135, height = 65, pointsize = 9, 
#          filepath = paste0(save_dir, "04-clim/"), 
#          filename = "pfj-v-mn")
# save_plot(plt = ggp_clim_pfj_v_sd, 
#          width = 135, height = 65, pointsize = 9, 
#          filepath = paste0(save_dir, "04-clim/"), 
#          filename = "pfj-v-sd")
# 
# save_plot(plt = ggp_clim_stj_v_mn, 
#          width = 135, height = 65, pointsize = 9, 
#          filepath = paste0(save_dir, "04-clim/"), 
#          filename = "stj-v-mn")
# save_plot(plt = ggp_clim_stj_v_sd, 
#          width = 135, height = 65, pointsize = 9, 
#          filepath = paste0(save_dir, "04-clim/"), 
#          filename = "stj-v-sd")

rm(tb_subset_yr_sdev, tb_subset_yr_mean)

