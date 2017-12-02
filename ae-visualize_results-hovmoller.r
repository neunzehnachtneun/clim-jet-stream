## source("ae-visualize_results-hovmoller.r")
## 
## ANALYSE VON MONATLICHEN MITTELWERTEN ####
## 1957 - 2016
####

## WORKING DIRECTORY & FIRST THINGS FIRST ####
## 
setwd("~/01-Master-Thesis/02-code-git/")
# getwd()


## LADEN DES DATENSATZES ####
rm(list = ls())
# Laden
load("stp-b.RData")
ls()

## Pfad zum Speichern von Abbildungen festlegen:
#save.dir <- "/home/skiefer/01-Master-Thesis/02-code-git/05-visu-pdf-tikz/"
save.dir <- "05-visu-pdf-tikz/"

# Nachladen der Packages
# library(ncdf4); library(chron); library(parallel); library(foreach); library(doParallel);

## VISUALISIERUNG GGPLOT2() ####
##
## Nötige Pakete
## Nötige Pakete
library(lubridate) # Datumsformate
library(dplyr) # Datenbearbeitung
library(zoo) # Zeitreihenanalyse
library(ggplot2) # Visualisierung
#library(ggsci) # Farbskala
library(tikzDevice) # Plot für Weiterverarbeitung in Latex
source("f-help-functions.r")


## Mittel -> method, class, year ####
## Für Klimatrends!
## 1. Mittelwert
tb.subset.yr.mean <-
  tb.jets.month %>%
  filter(class != "MJ") %>%
  select(year, season, method, class, lon, lat, u, v) %>%
  group_by(method, class, year) %>%
  summarise_at(.vars = vars(lat, u, v), .funs = funs("mean", mean, mean(., na.rm = TRUE)))
## 2. Standardabweichung
tb.subset.yr.sdev <-
  tb.jets.month %>%
  filter(class != "MJ") %>%
  select(year, method, class, lon, lat, u, v) %>%
  group_by(method, class, year) %>%
  summarise_at(.vars = vars(lat, u, v), .funs = funs("sd", sd, sd(., na.rm = TRUE)))

## Kernergebnisse (Mittelwerte, Standardabweichung, Trends) ####
## 
View(tb.jets.month %>%
       filter(class == "PFJ" | class == "STJ") %>%
       select(year, season, method, class, lon, lat, u, v) %>%
       group_by(method, class, season) %>%
       summarise_at(.vars = vars(lat, u, v), .funs = funs("mean", mean, mean(., na.rm = TRUE)))
)

View(tb.jets.month %>%
       filter(class == "PFJ" | class == "STJ") %>%
       select(year, season, method, class, lon, lat, u, v) %>%
       group_by(method, class, season) %>%
       summarise_at(.vars = vars(lat, u, v), .funs = funs("sd", sd, sd(., na.rm = TRUE)))
)

## Trends


View(tb.jets.month %>%
       filter(class == "PFJ" | class == "STJ") %>%
       select(year, season, method, class, lon, lat, u, v) %>%
       group_by(method, class) %>%
       do(model = lm(u ~ year, data=.)) %>% 
       mutate(Intercept=coef(model)[1], Slope=10*coef(model)[2]) %>%
       select(-model)
)

View(tb.jets.month %>%
       filter(class == "PFJ" | class == "STJ") %>%
       select(year, season, method, class, lon, lat, u, v) %>%
       group_by(method, class, season) %>%
       do(model = lm(lat ~ year, data=.)) %>% 
       mutate(Intercept=coef(model)[1], Slope=coef(model)[2]) %>%
       select(-model)
)


## Klimatrend Breitengrad Position ####
# Trend Positionen Polarfrontjet
ggp.clim.pfj.lat.mn <- 
ggplot(data = tb.subset.yr.mean %>%
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
print(ggp.clim.pfj.lat.mn)
# Variabilität
ggp.clim.pfj.lat.sd <- 
  ggplot(data = tb.subset.yr.sdev %>%
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
print(ggp.clim.pfj.lat.sd)
# Trend Positionen Subtropenjet
ggp.clim.stj.lat.mn <- 
ggplot(data = tb.subset.yr.mean %>%
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
print(ggp.clim.stj.lat.mn)
# Variabilität
ggp.clim.stj.lat.sd <- 
  ggplot(data = tb.subset.yr.sdev %>%
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
print(ggp.clim.stj.lat.sd)

## Klimatrend Zonalwindgeschwindigkeit ####
# Trend Zonalwind Polarfrontjet
ggp.clim.pfj.u.mn <- 
  ggplot(data = tb.subset.yr.mean %>%
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
print(ggp.clim.pfj.u.mn)
# Variabilität
ggp.clim.pfj.u.sd <- 
  ggplot(data = tb.subset.yr.sdev %>%
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
print(ggp.clim.pfj.u.sd)

# Trend Zonalwind Subtropenjet
ggp.clim.stj.u.mn <- 
  ggplot(data = tb.subset.yr.mean %>%
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
print(ggp.clim.stj.u.mn)
# Variabilität
ggp.clim.stj.u.sd <- 
  ggplot(data = tb.subset.yr.sdev %>%
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
print(ggp.clim.stj.u.sd)

## Klimatrend Meridionalwindgeschwindigkeit ####
# Trend Meridionalwind Polarfrontjet
ggp.clim.pfj.v.mn <- 
  ggplot(data = tb.subset.yr.mean %>%
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
print(ggp.clim.pfj.v.mn)
# Variabilität
ggp.clim.pfj.v.sd <- 
  ggplot(data = tb.subset.yr.sdev %>%
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
print(ggp.clim.pfj.v.sd)

# Trend Meridionalwind Subtropenjet
ggp.clim.stj.v.mn <- 
  ggplot(data = tb.subset.yr.mean %>%
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
print(ggp.clim.stj.v.mn)
# Variabilität
ggp.clim.stj.v.sd <- 
  ggplot(data = tb.subset.yr.sdev %>%
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
print(ggp.clim.stj.v.sd)


## Speichern der Klimatrendplots ####
## 
plt.save(plt = ggp.clim.stj.lat.mn, 
         width = 135, height = 65, pointsize = 9, 
         filepath = paste0(save.dir, "04-clim/"), 
         filename = "stj-lat-mn")
plt.save(plt = ggp.clim.stj.lat.sd, 
         width = 135, height = 65, pointsize = 9, 
         filepath = paste0(save.dir, "04-clim/"), 
         filename = "stj-lat-sd")

plt.save(plt = ggp.clim.pfj.lat.mn, 
         width = 135, height = 65, pointsize = 9, 
         filepath = paste0(save.dir, "04-clim/"), 
         filename = "pfj-lat-mn")
plt.save(plt = ggp.clim.pfj.lat.sd, 
         width = 135, height = 65, pointsize = 9, 
         filepath = paste0(save.dir, "04-clim/"), 
         filename = "pfj-lat-sd")

plt.save(plt = ggp.clim.pfj.u.mn, 
         width = 135, height = 65, pointsize = 9, 
         filepath = paste0(save.dir, "04-clim/"), 
         filename = "pfj-u-mn")
plt.save(plt = ggp.clim.pfj.u.sd, 
         width = 135, height = 65, pointsize = 9, 
         filepath = paste0(save.dir, "04-clim/"), 
         filename = "pfj-u-sd")

plt.save(plt = ggp.clim.stj.u.mn, 
         width = 135, height = 65, pointsize = 9, 
         filepath = paste0(save.dir, "04-clim/"), 
         filename = "stj-u-mn")
plt.save(plt = ggp.clim.stj.u.sd, 
         width = 135, height = 65, pointsize = 9, 
         filepath = paste0(save.dir, "04-clim/"), 
         filename = "stj-u-sd")

plt.save(plt = ggp.clim.pfj.v.mn, 
         width = 135, height = 65, pointsize = 9, 
         filepath = paste0(save.dir, "04-clim/"), 
         filename = "pfj-v-mn")
plt.save(plt = ggp.clim.pfj.v.sd, 
         width = 135, height = 65, pointsize = 9, 
         filepath = paste0(save.dir, "04-clim/"), 
         filename = "pfj-v-sd")

plt.save(plt = ggp.clim.stj.v.mn, 
         width = 135, height = 65, pointsize = 9, 
         filepath = paste0(save.dir, "04-clim/"), 
         filename = "stj-v-mn")
plt.save(plt = ggp.clim.stj.v.sd, 
         width = 135, height = 65, pointsize = 9, 
         filepath = paste0(save.dir, "04-clim/"), 
         filename = "stj-v-sd")



## Mittel -> method, class, year, season, lon ####
## Für Hovmöller!
## 1. Jährlich
tb.subset.mean <-
  tb.jets.month %>%
  filter(class != "MJ") %>%
  select(year, season, method, class, lon, lat, u, v) %>%
  group_by(method, class, year, season, lon) %>%
  summarise_all(funs("mean", mean, mean(., na.rm = TRUE)))
## 2. Running Mean über Fünf Jahre
rollmean5 <- function(data) rollapply(data, 5, mean, na.rm = TRUE, fill = NA, partial = TRUE)
tb.subset.rollmean <-
  tb.subset.mean %>%
  group_by(method, class, season, lon) %>%
  mutate_at(.vars = vars(lat_mean, u_mean, v_mean), .funs = funs("rollmean5", rollmean5, rollmean5(.)))

## VISUALISIERUNG DER HOVMÖLLER-DIAGRAMME  ####
# Schleife über Jahreszeiten
for (i.ssn in c("djf", "mam", "jja", "son")) {
  print(i.ssn)
  
  ## Chebyshev Polarfrontjet ####
  ## Breitengrad
  ggp.hovm.m2.pfj.lat <-
    ggplot(data = tb.subset.rollmean %>%
             filter(method == "Chebyshev", class == "PFJ", season == i.ssn),
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
  print(ggp.hovm.m2.pfj.lat)
  ## Zonalwind
  ggp.hovm.m2.pfj.u <- 
    ggplot(data = tb.subset.rollmean %>%
             filter(method == "Chebyshev", class == "PFJ", season == i.ssn),
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
  print(ggp.hovm.m2.pfj.u)
  ## Meridionalwind
  ggp.hovm.m2.pfj.v <-
    ggplot(data = tb.subset.rollmean %>%
             filter(method == "Chebyshev", class == "PFJ", season == i.ssn),
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
  print(ggp.hovm.m2.pfj.v)
  
  ## Chebyshev Subtropenjet ####
  ## Breitengrad
  ggp.hovm.m2.stj.lat <-
    ggplot(data = tb.subset.rollmean %>%
             filter(method == "Chebyshev", class == "STJ", season == i.ssn),
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
  print(ggp.hovm.m2.stj.lat)
  ## Zonalwind
  ggp.hovm.m2.stj.u <-
    ggplot(data = tb.subset.rollmean %>%
             filter(method == "Chebyshev", class == "STJ", season == i.ssn),
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
  print(ggp.hovm.m2.stj.u)
  ## Meridionalwind
  ggp.hovm.m2.stj.v <-
    ggplot(data = tb.subset.rollmean %>%
             filter(method == "Chebyshev", class == "STJ", season == i.ssn),
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
  print(ggp.hovm.m2.stj.v)
  
  ## Dijkstra Polarfrontjet ####
  ## Breitengrad
  ggp.hovm.m3.pfj.lat <-
    ggplot(data = tb.subset.rollmean %>%
             filter(method == "Dijkstra", class == "PFJ", season == i.ssn),
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
  print(ggp.hovm.m3.pfj.lat)
  ## Zonalwind
  ggp.hovm.m3.pfj.u <-
    ggplot(data = tb.subset.rollmean %>%
             filter(method == "Dijkstra", class == "PFJ", season == i.ssn),
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
  print(ggp.hovm.m3.pfj.u)
  ## Meridionalwind
  ggp.hovm.m3.pfj.v <-
    ggplot(data = tb.subset.rollmean %>%
             filter(method == "Dijkstra", class == "PFJ", season == i.ssn),
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
  print(ggp.hovm.m3.pfj.v)
  
  ## Dijkstra Subtropenjet ####
  ## Breitengrad
  ggp.hovm.m3.stj.lat <- 
    ggplot(data = tb.subset.rollmean %>%
             filter(method == "Dijkstra", class == "STJ", season == i.ssn),
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
  print(ggp.hovm.m3.stj.lat)
  ## Zonalwind
  ggp.hovm.m3.stj.u <- 
    ggplot(data = tb.subset.rollmean %>%
             filter(method == "Dijkstra", class == "STJ", season == i.ssn),
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
  print(ggp.hovm.m3.stj.u)
  ## Meridionalwind
  ggp.hovm.m3.stj.v <-
    ggplot(data = tb.subset.rollmean %>%
             filter(method == "Dijkstra", class == "STJ", season == i.ssn),
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
  print(ggp.hovm.m3.stj.v)
  
  
  # Speichern der Plots ####
  # Breitengrade
  plt.save(plt = ggp.hovm.m2.pfj.lat, width = 140, height = 100, pointsize = 9,
           filepath = paste0(save.dir, "05-hovm-lat/"),
           filename = paste0("hovm-chebyshev-pfj-", i.ssn))
  plt.save(plt = ggp.hovm.m2.stj.lat, width = 140, height = 100, pointsize = 9,
           filepath = paste0(save.dir, "05-hovm-lat/"),
           filename = paste0("hovm-chebyshev-stj-", i.ssn))
  plt.save(plt = ggp.hovm.m3.pfj.lat, width = 140, height = 100, pointsize = 9,
           filepath = paste0(save.dir, "05-hovm-lat/"),
           filename = paste0("hovm-dijkstra-pfj-", i.ssn))
  plt.save(plt = ggp.hovm.m3.stj.lat, width = 140, height = 100, pointsize = 9,
           filepath = paste0(save.dir, "05-hovm-lat/"),
           filename = paste0("hovm-dijkstra-stj-", i.ssn))
  # Zonalwind
  plt.save(plt = ggp.hovm.m2.pfj.u, width = 140, height = 100, pointsize = 9,
           filepath = paste0(save.dir, "06-hovm-u/"),
           filename = paste0("hovm-chebyshev-pfj-", i.ssn))
  plt.save(plt = ggp.hovm.m2.stj.u, width = 140, height = 100, pointsize = 9,
           filepath = paste0(save.dir, "06-hovm-u/"),
           filename = paste0("hovm-chebyshev-stj-", i.ssn))
  plt.save(plt = ggp.hovm.m3.pfj.u, width = 140, height = 100, pointsize = 9,
           filepath = paste0(save.dir, "06-hovm-u/"),
           filename = paste0("hovm-dijkstra-pfj-", i.ssn))
  plt.save(plt = ggp.hovm.m3.stj.u, width = 140, height = 100, pointsize = 9,
           filepath = paste0(save.dir, "06-hovm-u/"),
           filename = paste0("hovm-dijkstra-stj-", i.ssn))
  # Meridionalwind
  plt.save(plt = ggp.hovm.m2.pfj.v, width = 140, height = 100, pointsize = 9,
           filepath = paste0(save.dir, "07-hovm-v/"),
           filename = paste0("hovm-chebyshev-pfj-", i.ssn))
  plt.save(plt = ggp.hovm.m2.stj.v, width = 140, height = 100, pointsize = 9,
           filepath = paste0(save.dir, "07-hovm-v/"),
           filename = paste0("hovm-chebyshev-stj-", i.ssn))
  plt.save(plt = ggp.hovm.m3.pfj.v, width = 140, height = 100, pointsize = 9,
           filepath = paste0(save.dir, "07-hovm-v/"),
           filename = paste0("hovm-dijkstra-pfj-", i.ssn))
  plt.save(plt = ggp.hovm.m3.stj.v, width = 140, height = 100, pointsize = 9,
           filepath = paste0(save.dir, "07-hovm-v/"),
           filename = paste0("hovm-dijkstra-stj-", i.ssn))
  
}

## ENDE ENDE ENDE ####
