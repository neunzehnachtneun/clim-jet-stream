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
  scale_y_continuous(name = "Breitengrad", breaks = c(0, 30, 60, 90)) +
  coord_fixed(xlim = c(-180,180), ylim = c(0,90))
# Plot der Nordhemisphäre auf Mercator-Projektion
ggp.nh.merc <-
  ggplot() + geom_polygon(data = map_nh, mapping = aes(x = long, y = lat, group = group), fill = "gray50") +
  scale_y_continuous(name = "Breitengrad", breaks = c(0, 30, 60, 90)) +
  coord_map(xlim = c(-180,180), ylim = c(0,90))


for (t.stp in round(seq(1,length(dts), length.out = 12))) {
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
    geom_tile() + scale_fill_gsea(name = "$\\sqrt(u^2+v^2)$") +
    geom_point(mapping = aes(x = lon , y = lat, shape = method, fill = NULL),
               data = tb.subset,
               fill = "black", size = 1.2, show.legend = TRUE) +
    scale_x_continuous(name = "Längengrad",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Breitengrad",
                       breaks = c(0, 30, 60, 90)) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group),
                 data = map_nh, fill = "gray50", alpha = 0.35) +
    ggtitle("Zonales Windfeld und Position der meridionalen Zonalwindmaxima und des absoluten Chebyshev-Maximums", 
            subtitle = paste0(dts.year[t.stp], "-", dts.month[t.stp])) +
    coord_fixed(xlim = c(-180,180), ylim = c(0,90)) + theme_classic() 

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
    geom_tile() + scale_fill_gsea() + #scale_fill_distiller(palette = "RdYlBu") +
    geom_point(mapping = aes(x = lon , y = lat, shape = factor(class), fill = NULL),
               data = tb.subset,
               fill = "black", size = 1.2, show.legend = TRUE) +
    scale_x_continuous(name = "Längengrad",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Breitengrad",
                       breaks = c(0, 30, 60, 90)) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group),
                 data = map_nh, fill = "gray50", alpha = 0.35) +
    ggtitle("Zonales Windfeld und Position der zwei stärksten Chebyshev-Maxima", 
            subtitle = paste0(dts.year[t.stp], "-", dts.month[t.stp])) +
    coord_fixed(xlim = c(-180,180), ylim = c(0,90)) + theme_classic()
  
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
    geom_tile() + scale_fill_gsea() + #scale_fill_distiller(palette = "RdYlBu") +
    geom_point(mapping = aes(x = lon , y = lat, shape = factor(class), fill = NULL),
               data = tb.subset,
               fill = "black", size = 1.2, show.legend = TRUE) +
    scale_x_continuous(name = "Längengrad",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Breitengrad",
                       breaks = c(0, 30, 60, 90)) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group),
                 data = map_nh, fill = "gray50", alpha = 0.35) +
    ggtitle("Betrag des horizontalen Windfeldes und Position des Dijkstra-Jets", 
            subtitle = paste0(dts.year[t.stp], "-", dts.month[t.stp])) +
    coord_fixed(xlim = c(-180,180), ylim = c(0,90)) + theme_classic()
  
  ## Speichern der Plots als pdfs und tex-files
  
  plt.save(plt = ggp.nh.m1.m2b, 
           width = 150, height = 75, pointsize = 11,
           filepath = "05-visu-pdf/01-case/", 
           filename = paste0(dts.year[t.stp], "-", dts.month[t.stp], "-m1-m2b"))
  plt.save(plt = ggp.nh.m2c, 
           width = 150, height = 75, pointsize = 11,
           filepath = "05-visu-pdf/01-case/", 
           filename = paste0(dts.year[t.stp], "-", dts.month[t.stp], "-m2c"))
  plt.save(plt = ggp.nh.m3, 
           width = 150, height = 75, pointsize = 11,
           filepath = "05-visu-pdf/01-case/", 
           filename = paste0(dts.year[t.stp], "-", dts.month[t.stp], "-m3"))
  
}


## ENDE ENDE ENDE ####
