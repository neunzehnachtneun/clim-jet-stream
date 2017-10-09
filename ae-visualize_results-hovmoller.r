## source("ac-visualize_results.r")
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
library(ggsci) # Farbskala
library(tikzDevice) # Plot für Weiterverarbeitung in Latex
source("f-help-functions.r")

## mean
tb.subset.mean <-
  tb.jets.month %>%
  filter(class != "MJ") %>%
  select(year, season, method, class, lon, lat, u, v) %>%
  group_by(method, class, year, season, lon) %>%
  summarise_all(funs("mean", mean, mean(., na.rm = TRUE)))


## running mean über fünf Jahre
rollmean5 <- function(data) rollapply(data, 5, mean, na.rm = TRUE, fill = NA, partial = TRUE)
tb.subset.rollmean <-
  tb.subset.mean %>%
  group_by(method, class, season, lon) %>%
  mutate_at(.vars = vars(lat_mean, u_mean, v_mean), .funs = funs("rollmean5", rollmean5, rollmean5(.)))

## Plot des meridionalen Mittels ####
## 
ggp.clim.pfj <- 
ggplot(data = tb.subset.mean %>%
         group_by(method, class, year) %>%
         select(method, class, year, lat_mean, u_mean, v_mean) %>%
         summarise_all(funs("mean", mean, mean(., na.rm = TRUE))) %>%
         filter(class == "PFJ"),
       mapping = aes(x = year, y = lat_mean_mean, 
                     shape = method))  + geom_point() + 
  geom_smooth(mapping = aes(color = method),
              method = "lm", formula = y ~ x) +
  labs(shape = "Methode", color = "LM") +
  guides(shape = guide_legend(order = 1),
         colour = guide_legend(order = 2)) +    
  scale_x_continuous(name = "Jahr",
                     breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
  scale_y_continuous(name = "Breitengrad in $^{\\circ}$",
                     breaks = c(50, 55, 60, 65, 70)) +
  theme_bw() + theme(legend.position = "bottom")

ggp.clim.stj <- 
ggplot(data = tb.subset.mean %>%
         group_by(method, class, year) %>%
         select(method, class, year, lat_mean, u_mean, v_mean) %>%
         summarise_all(funs("mean", mean, mean(., na.rm = TRUE))) %>%
         filter(class == "STJ"),
       mapping = aes(x = year, y = lat_mean_mean, 
                     shape = method)) + geom_point() + 
  geom_smooth(mapping = aes(color = method),
              method = "lm", formula = y ~ x) +
  labs(shape = "Methode", color = "LM") +
  guides(shape = guide_legend(order = 1),
         colour = guide_legend(order = 2)) +    
  scale_x_continuous(name = "Jahr",
                     breaks = c(1960, 1970, 1980, 1990, 2000, 2010)) +
  scale_y_continuous(name = "Breitengrad in $^{\\circ}$",
                     breaks = c(32,34,36)) +
  theme_bw() + theme(legend.position = "bottom")

plt.save(plt = ggp.clim.pfj, width = 140, height = 70, pointsize = 11, 
         filepath = "/home/skiefer/01-Master-Thesis/02-code-git/05-visu-pdf/04-clim/", 
         filename = "mean_clim_pfj")
plt.save(plt = ggp.clim.stj, width = 140, height = 70, pointsize = 11, 
         filepath = "/home/skiefer/01-Master-Thesis/02-code-git/05-visu-pdf/04-clim/", 
         filename = "mean_clim_stj")

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
    geom_tile() + scale_fill_distiller(palette = "RdYlBu") +
    labs(fill = "Breitengrad in $^{\\circ}$") +
    guides(fill = guide_colourbar(title.position = "left",
                                  direction = "horizontal",
                                  label.position = "bottom")) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Jahr",
                       breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
    theme_bw() + theme(legend.position = "bottom")
  ## Zonalwind
  ggp.hovm.m2.pfj.u <- 
    ggplot(data = tb.subset.rollmean %>%
             filter(method == "Chebyshev", class == "PFJ", season == i.ssn),
           mapping = aes(x = lon, y = year, fill = u_mean_rollmean5)) +
    geom_tile() + scale_fill_distiller(palette = "RdYlBu") +
    labs(fill = "Zonalwind in $\\frac{m}{s}$") +
    guides(fill = guide_colourbar(title.position = "left",
                                  direction = "horizontal",
                                  label.position = "bottom")) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Jahr",
                       breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
    theme_bw() + theme(legend.position = "bottom")
  ## Meridionalwind
  ggp.hovm.m2.pfj.v <-
    ggplot(data = tb.subset.rollmean %>%
             filter(method == "Chebyshev", class == "PFJ", season == i.ssn),
           mapping = aes(x = lon, y = year, fill = v_mean_rollmean5)) +
    geom_tile() + scale_fill_distiller(palette = "RdYlBu") +
    labs(fill = "Meridionalwind in $\\frac{m}{s}$") +
    guides(fill = guide_colourbar(title.position = "left",
                                  direction = "horizontal",
                                  label.position = "bottom")) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Jahr",
                       breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
    theme_bw() + theme(legend.position = "bottom")
  
  ## Chebyshev Subtropenjet ####
  ## Breitengrad
  ggp.hovm.m2.stj.lat <-
    ggplot(data = tb.subset.rollmean %>%
             filter(method == "Chebyshev", class == "STJ", season == i.ssn),
           mapping = aes(x = lon, y = year, fill = lat_mean_rollmean5)) +
    geom_tile() + scale_fill_distiller(palette = "RdYlBu") +
    labs(fill = "Breitengrad in $^{\\circ}$") +
    guides(fill = guide_colourbar(title.position = "left",
                                  direction = "horizontal",
                                  label.position = "bottom")) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Jahr",
                       breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
    theme_bw() + theme(legend.position = "bottom")
  ## Zonalwind
  ggp.hovm.m2.stj.u <-
    ggplot(data = tb.subset.rollmean %>%
             filter(method == "Chebyshev", class == "STJ", season == i.ssn),
           mapping = aes(x = lon, y = year, fill = u_mean_rollmean5)) +
    geom_tile() + scale_fill_distiller(palette = "RdYlBu") +
    labs(fill = "Zonalwind in $\\frac{m}{s}$") +
    guides(fill = guide_colourbar(title.position = "left",
                                  direction = "horizontal",
                                  label.position = "bottom")) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Jahr",
                       breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
    theme_bw() + theme(legend.position = "bottom")
  ## Meridionalwind
  ggp.hovm.m2.stj.v <-
    ggplot(data = tb.subset.rollmean %>%
             filter(method == "Chebyshev", class == "STJ", season == i.ssn),
           mapping = aes(x = lon, y = year, fill = v_mean_rollmean5)) +
    geom_tile() + scale_fill_distiller(palette = "RdYlBu") +
    labs(fill = "Meridionalwind in $\\frac{m}{s}$") +
    guides(fill = guide_colourbar(title.position = "left",
                                  direction = "horizontal",
                                  label.position = "bottom")) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Jahr",
                       breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
    theme_bw() + theme(legend.position = "bottom")
  
  ## Dijkstra Polarfrontjet ####
  ## Breitengrad
  ggp.hovm.m3.pfj.lat <-
    ggplot(data = tb.subset.rollmean %>%
             filter(method == "Dijkstra", class == "PFJ", season == i.ssn),
           mapping = aes(x = lon, y = year, fill = lat_mean_rollmean5)) +
    geom_tile() + scale_fill_distiller(palette = "RdYlBu") +
    labs(fill = "Breitengrad in $^{\\circ}$") +
    guides(fill = guide_colourbar(title.position = "left",
                                  direction = "horizontal",
                                  label.position = "bottom")) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Jahr",
                       breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
    theme_bw() + theme(legend.position = "bottom")
  ## Zonalwind
  ggp.hovm.m3.pfj.u <-
    ggplot(data = tb.subset.rollmean %>%
             filter(method == "Dijkstra", class == "PFJ", season == i.ssn),
           mapping = aes(x = lon, y = year, fill = u_mean_rollmean5)) +
    geom_tile() + scale_fill_distiller(palette = "RdYlBu") +
    labs(fill = "Breitengrad in $^{\\circ}$") +
    guides(fill = guide_colourbar(title.position = "left",
                                  direction = "horizontal",
                                  label.position = "bottom")) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Jahr",
                       breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
    theme_bw() + theme(legend.position = "bottom")
  ## Meridionalwind
  ggp.hovm.m3.pfj.v <-
    ggplot(data = tb.subset.rollmean %>%
             filter(method == "Dijkstra", class == "PFJ", season == i.ssn),
           mapping = aes(x = lon, y = year, fill = v_mean_rollmean5)) +
    geom_tile() + scale_fill_distiller(palette = "RdYlBu") +
    labs(fill = "Breitengrad in $^{\\circ}$") +
    guides(fill = guide_colourbar(title.position = "left",
                                  direction = "horizontal",
                                  label.position = "bottom")) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Jahr",
                       breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
    theme_bw() + theme(legend.position = "bottom")
  
  ## Dijkstra Subtropenjet ####
  ## Breitengrad
  ggp.hovm.m3.stj.lat <- 
    ggplot(data = tb.subset.rollmean %>%
             filter(method == "Dijkstra", class == "STJ", season == i.ssn),
           mapping = aes(x = lon, y = year, fill = lat_mean_rollmean5)) +
    geom_tile() + scale_fill_distiller(palette = "RdYlBu") +
    labs(fill = "Breitengrad in $^{\\circ}$") +
    guides(fill = guide_colourbar(title.position = "left",
                                  direction = "horizontal",
                                  label.position = "bottom")) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Jahr",
                       breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
    theme_bw() + theme(legend.position = "bottom")
  ## Zonalwind
  ggp.hovm.m3.stj.u <- 
    ggplot(data = tb.subset.rollmean %>%
             filter(method == "Dijkstra", class == "STJ", season == i.ssn),
           mapping = aes(x = lon, y = year, fill = u_mean_rollmean5)) +
    geom_tile() + scale_fill_distiller(palette = "RdYlBu") +
    labs(fill = "Zonalwind in $\\frac{m}{s}$") +
    guides(fill = guide_colourbar(title.position = "left",
                                  direction = "horizontal",
                                  label.position = "bottom")) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Jahr",
                       breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
    theme_bw() + theme(legend.position = "bottom")
  ## Meridionalwind
  ggp.hovm.m3.stj.v <-
    ggplot(data = tb.subset.rollmean %>%
             filter(method == "Dijkstra", class == "STJ", season == i.ssn),
           mapping = aes(x = lon, y = year, fill = v_mean_rollmean5)) +
    geom_tile() + scale_fill_distiller(palette = "RdYlBu") +
    labs(fill = "Meridionalwind in $\\frac{m}{s}$") +
    guides(fill = guide_colourbar(title.position = "left",
                                  direction = "horizontal",
                                  label.position = "bottom")) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Jahr",
                       breaks = c(1960, 1970, 1980, 1990, 2000, 2010, 2020)) +
    theme_bw() + theme(legend.position = "bottom")
  
  
  ## Speichern der Plots ####
  # Breitengrade
  plt.save(plt = ggp.hovm.m2.pfj.lat, width = 140, height = 100, pointsize = 11, 
           filepath = "/home/skiefer/01-Master-Thesis/02-code-git/05-visu-pdf/05-hovm-lat/", 
           filename = paste0("hovm_chebyshev_pfj_", i.ssn))
  plt.save(plt = ggp.hovm.m2.stj.lat, width = 140, height = 100, pointsize = 11, 
           filepath = "/home/skiefer/01-Master-Thesis/02-code-git/05-visu-pdf/05-hovm-lat/", 
           filename = paste0("hovm_chebyshev_stj_", i.ssn))
  plt.save(plt = ggp.hovm.m3.pfj.lat, width = 140, height = 100, pointsize = 11, 
           filepath = "/home/skiefer/01-Master-Thesis/02-code-git/05-visu-pdf/05-hovm-lat/", 
           filename = paste0("hovm_dijkstra_pfj_", i.ssn))
  plt.save(plt = ggp.hovm.m3.stj.lat, width = 140, height = 100, pointsize = 11, 
           filepath = "/home/skiefer/01-Master-Thesis/02-code-git/05-visu-pdf/05-hovm-lat/", 
           filename = paste0("hovm_dijkstra_stj_", i.ssn))
  # Zonalwind
  plt.save(plt = ggp.hovm.m2.pfj.u, width = 140, height = 100, pointsize = 11, 
           filepath = "/home/skiefer/01-Master-Thesis/02-code-git/05-visu-pdf/06-hovm-u/", 
           filename = paste0("hovm_chebyshev_pfj_", i.ssn))
  plt.save(plt = ggp.hovm.m2.stj.u, width = 140, height = 100, pointsize = 11, 
           filepath = "/home/skiefer/01-Master-Thesis/02-code-git/05-visu-pdf/06-hovm-u/", 
           filename = paste0("hovm_chebyshev_stj_", i.ssn))
  plt.save(plt = ggp.hovm.m3.pfj.u, width = 140, height = 100, pointsize = 11, 
           filepath = "/home/skiefer/01-Master-Thesis/02-code-git/05-visu-pdf/06-hovm-u/", 
           filename = paste0("hovm_dijkstra_pfj_", i.ssn))
  plt.save(plt = ggp.hovm.m3.stj.u, width = 140, height = 100, pointsize = 11, 
           filepath = "/home/skiefer/01-Master-Thesis/02-code-git/05-visu-pdf/06-hovm-u/", 
           filename = paste0("hovm_dijkstra_stj_", i.ssn))
  # Meridionalwind
  plt.save(plt = ggp.hovm.m2.pfj.v, width = 140, height = 100, pointsize = 11, 
           filepath = "/home/skiefer/01-Master-Thesis/02-code-git/05-visu-pdf/07-hovm-v/", 
           filename = paste0("hovm_chebyshev_pfj_", i.ssn))
  plt.save(plt = ggp.hovm.m2.stj.v, width = 140, height = 100, pointsize = 11, 
           filepath = "/home/skiefer/01-Master-Thesis/02-code-git/05-visu-pdf/07-hovm-v/", 
           filename = paste0("hovm_chebyshev_stj_", i.ssn))
  plt.save(plt = ggp.hovm.m3.pfj.v, width = 140, height = 100, pointsize = 11, 
           filepath = "/home/skiefer/01-Master-Thesis/02-code-git/05-visu-pdf/07-hovm-v/", 
           filename = paste0("hovm_dijkstra_pfj_", i.ssn))
  plt.save(plt = ggp.hovm.m3.stj.v, width = 140, height = 100, pointsize = 11, 
           filepath = "/home/skiefer/01-Master-Thesis/02-code-git/05-visu-pdf/07-hovm-v/", 
           filename = paste0("hovm_dijkstra_stj_", i.ssn))
}

## ENDE ENDE ENDE ####
