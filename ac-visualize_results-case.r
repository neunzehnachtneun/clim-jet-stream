## source("ac-visualize_results-case.r")
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
save.dir <-  "05-visu-pdf-tikz/"

# Nachladen der Packages
# library(ncdf4); library(chron); library(parallel); library(foreach); library(doParallel);

## VISUALISIERUNG GGPLOT2() ####
##
## Nötige Pakete
library(lubridate) # Datumsformate
library(dplyr) # Datenbearbeitung
library(ggplot2) # Visualisierung
library(ggsci) # Farbskala
library(tikzDevice) # Plot für Weiterverarbeitung in Latex

## Nötige Hilfsfunktionen
source("f-help-functions.r")

## VISUALISIEREN DER BESTIMMTEN JETPOSITIONEN ####
##

# Initiieren einer passenden Weltkarte
map_nh <- map_data("world")

# Plot der Nordhemisphäre // Untersuchungsgebiet
ggp.nh <-
  ggplot() + geom_polygon(data = map_nh, mapping = aes(x = long, y = lat, group = group), fill = "gray50") +
  scale_x_continuous(name = "Längengrad in $^{\\circ}$",
                     breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
  scale_y_continuous(name = "Breitengrad in $^{\\circ}$",
                     breaks = c(0, 30, 60, 90)) +
  coord_fixed(xlim = c(-180,180), ylim = c(0,90)) +
  theme_bw()

# Plot der Nordhemisphäre auf Mercator-Projektion
ggp.nh.merc <-
  ggplot() + geom_polygon(data = map_nh, mapping = aes(x = long, y = lat, group = group), fill = "gray50") +
  scale_x_continuous(name = "Längengrad in $^{\\circ}$",
                     breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
  scale_y_continuous(name = "Breitengrad in $^{\\circ}$",
                     breaks = c(0, 30, 60, 90)) +
  coord_map(xlim = c(-180,180), ylim = c(0,90)) +
  theme_bw()

plt.save(plt = ggp.nh, width = 135, height = 55, pointsize = 11, 
         filepath = paste0(save.dir, "01-area"), 
         filename = "north-hem")
plt.save(plt = ggp.nh.merc, width = 135, height = 55, pointsize = 11, 
         filepath = paste0(save.dir, "01-area"), 
         filename = "north-hem-merc")


## Greife zufällig acht Zeitpunkte heraus, 
## Bedingung: Jede Saison kommt zweimal vor.

repeat {
  i.stp <- sample.int(n = length(dts), size = 8, replace = FALSE)
  # print(dts.season[t.stp])
  if (length(which(dts.season[i.stp] == "mam")) == 2 &
      length(which(dts.season[i.stp] == "jja")) == 2 &
      length(which(dts.season[i.stp] == "son")) == 2 &
      length(which(dts.season[i.stp] == "djf")) == 2 &
      length(which(duplicated(dts.month[i.stp]))) == 0 &
      length(which(duplicated(dts.year[i.stp]))) == 0) {
    break
  }
}
print(sort(i.stp))
print(dts[sort(i.stp)])
i.stp <- c(33, 52, 143, 394, 437, 577, 663, 692)
## Schleife über Stichprobe
for (t.stp in i.stp) {
  print(t.stp)

  ## Plot des zonalen Windfeldes und der Position des maximalen Jets sowie des maximalen Chebyshev-Jets
  # Datenaufbereitung
  tb.subset <-
    tb.jets.month %>%
    filter(year == dts.year[t.stp] &
             month == dts.month[t.stp]) %>%
    group_by(year, month, lon) %>%
    select(dts, year, month, season, method, class, lon, lat) %>%
    filter(class == "MJ")
  #print(unique(tb.subset$dts))
  # Visualisierung
  ggp.nh.m1.m2b <-
    ggplot(data = tb.uv[which(tb.uv$dts == dts[t.stp]),],
           mapping = aes(x = lon, y = lat, fill = uv)) +
    geom_tile() + scale_fill_distiller(palette = "RdYlBu") +
    geom_point(mapping = aes(x = lon, y = lat, shape = method, fill = NULL),
               data = tb.subset,
               fill = "black", size = 1.4, show.legend = TRUE) +
    labs(shape = "Methode", fill = "$\\sqrt{u^2+v^2}$") +
    guides(fill = guide_colourbar(title.position = "top",
                                  direction = "horizontal",
                                  label.position = "bottom"), 
           shape = guide_legend(title.position = "top",
                                direction = "horizontal",
                                label.position = "bottom",
                                nrow = 1)) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Breitengrad in $^{\\circ}$",
                       breaks = c(0, 30, 60, 90)) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group),
                 data = map_nh, fill = "gray50", alpha = 0.35) +
    coord_fixed(xlim = c(-180,180), ylim = c(0,90)) + 
    theme_bw() + theme(legend.position = "bottom")
  
  
  # Plot des zonalen Windfeldes und der zwei stärksten Chebyshev-Maxima im Bereich [20,85]
  # Datenaufbereitung
  tb.subset <-
    tb.jets.month %>%
    filter(year == dts.year[t.stp] &
             month == dts.month[t.stp]) %>%
    group_by(year, month, lon) %>%
    select(dts, year, month, season, method, class, lon, lat) %>%
    filter(method == "Chebyshev", class != "MJ")
  #print(unique(tb.subset$dts))
  # Visualisierung
  ggp.nh.m2c <-
    ggplot(data = tb.uv[which(tb.uv$dts == dts[t.stp]),],
           mapping = aes(x = lon, y = lat, fill = uv)) +
    geom_tile() + scale_fill_distiller(palette = "RdYlBu") +
    geom_point(mapping = aes(x = lon , y = lat, shape = factor(class), fill = NULL),
               data = tb.subset,
               fill = "black", size = 1.4, show.legend = TRUE) +
    labs(shape = "Methode", fill = "$\\sqrt{u^2+v^2}$") +
    guides(fill = guide_colourbar(title.position = "top",
                                  direction = "horizontal",
                                  label.position = "bottom"), 
           shape = guide_legend(title.position = "top",
                                direction = "horizontal",
                                label.position = "bottom",
                                nrow = 1)) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Breitengrad in $^{\\circ}$",
                       breaks = c(0, 30, 60, 90)) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group),
                 data = map_nh, fill = "gray50", alpha = 0.35) +
    coord_fixed(xlim = c(-180,180), ylim = c(0,90)) + 
    theme_bw() + theme(legend.position = "bottom")
  
  
  # Plot des Betrags des horizontalen Windfeldes und Dijkstra-Jets
  # Datenaufbereitung
  tb.subset <-
    tb.jets.month %>%
    filter(year == dts.year[t.stp] &
             month == dts.month[t.stp]) %>%
    group_by(year, month, lon) %>%
    select(dts, year, month, season, method, class, lon, lat) %>%
    filter(method == "Dijkstra", class != "MJ")
  #print(unique(tb.subset$dts))
  # Visualisierung
  ggp.nh.m3 <-
    ggplot(data = tb.uv[which(tb.uv$dts == dts[t.stp]),],
           mapping = aes(x = lon, y = lat, fill = uv)) +
    geom_tile() + scale_fill_distiller(palette = "RdYlBu") +
    geom_point(mapping = aes(x = lon , y = lat, shape = factor(class), fill = NULL),
               data = tb.subset,
               fill = "black", size = 1.4, show.legend = TRUE) +
    labs(shape = "Methode", fill = "$\\sqrt{u^2+v^2}$") +
    guides(fill = guide_colourbar(title.position = "top",
                                  direction = "horizontal",
                                  label.position = "bottom"), 
           shape = guide_legend(title.position = "top",
                                direction = "horizontal",
                                label.position = "bottom",
                                nrow = 1)) +
    scale_x_continuous(name = "Längengrad in $^{\\circ}$",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Breitengrad in $^{\\circ}$",
                       breaks = c(0, 30, 60, 90)) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group),
                 data = map_nh, fill = "gray50", alpha = 0.35) +
    coord_fixed(xlim = c(-180,180), ylim = c(0,90)) + 
    theme_bw() + theme(legend.position = "bottom")
  
  
  ## Speichern der Plots als pdfs und tex-files
  plt.save(plt = ggp.nh.m1.m2b, 
           width = 135, height = 55, pointsize = 11,
           filepath = paste0(save.dir, "02-case/"), 
           filename = paste0(dts.year[t.stp], "-", dts.month[t.stp], "-m1-m2b"))
  plt.save(plt = ggp.nh.m2c, 
           width = 135, height = 55, pointsize = 11,
           filepath = paste0(save.dir, "02-case/"), 
           filename = paste0(dts.year[t.stp], "-", dts.month[t.stp], "-m2c"))
  plt.save(plt = ggp.nh.m3, 
           width = 135, height = 55, pointsize = 11,
           filepath = paste0(save.dir, "02-case/"), 
           filename = paste0(dts.year[t.stp], "-", dts.month[t.stp], "-m3"))
}



## ENDE ENDE ENDE ####
