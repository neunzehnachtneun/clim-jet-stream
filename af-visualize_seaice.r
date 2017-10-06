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
library(lubridate) # Datumsformate
library(dplyr) # Datenbearbeitung
library(ggplot2) # Visualisierung
library(ggsci) # Farbskala
library(tikzDevice) # Plot für Weiterverarbeitung in Latex
source("f-help-functions.r")

## DATENAUFBEREITUNG DPLYR() ####
tb.jets.month$season <- factor(tb.jets.month$season, levels = c("djf", "mam", "jja", "son"),
                               labels = c("DJF", "MAM", "JJA", "SON"))
tb.jets.month$method <- factor(tb.jets.month$method, levels = c("Max", "Chebyshev", "Dijkstra"), 
                               labels = c("Maximum", "Chebyshev", "Dijkstra"))
tb.jets.month$class <- factor(tb.jets.month$class, levels = c("MJ", "PFJ", "SJ", "STJ"), 
                              labels = c("MJ", "PFJ", "SJ", "STJ"))
tb.jets.month$extent[tb.jets.month$extent < 0] <- NA
tb.jets.month$area[tb.jets.month$area < 0] <- NA

tb.subset <- 
  tb.jets.month %>%
  filter(lon >= -90 & lon <= -0) %>%
  filter(method == "Chebyshev" & class == "PFJ")




## VERGLEICH CHEBYSHEV VS DIJKSTRA ####
## 

## Chebyshev
## 
ggp.seaice.chebyshev <-
  ggplot(data =   tb.jets.month %>%
           filter(lon >= -90 & lon <= -0) %>%
           filter(method == "Chebyshev" & class == "PFJ"), 
         mapping = aes(x = extent, y = lat, col = season)) + 
  geom_jitter(size = 0.2) + scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  scale_x_reverse(name = "Seeeis-Ausdehnung") +
  scale_y_continuous(name = "Positionen Chebyshev") +
  theme_classic() +
  theme(legend.position = "bottom")
print(ggp.seaice.chebyshev)
##


## Dijkstra
## 
ggp.seaice.dijkstra <- 
  ggplot(data =   tb.jets.month %>%
           filter(lon >= -90 & lon <= -0) %>%
           filter(method == "Dijkstra" & class == "PFJ"), 
         mapping = aes(x = extent, y = lat, col = season)) + 
  geom_jitter(size = 0.2) + scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  scale_x_reverse(name = "Seeeis-Ausdehnung in km$^{2}$") +
  scale_y_continuous(name = "Positionen Dijkstra") +
  theme_classic() +
  theme(legend.position = "bottom")
print(ggp.seaice.chebyshev)
##
ggp.seaice.dijkstra <- 
  ggplot(data =   tb.jets.month %>%
           filter(lon >= -90 & lon <= -0) %>%
           filter(method == "Dijkstra", class == "PFJ", season == "DJF") %>%
           select(year, month, lat, extent) %>%
           group_by(year, month) %>%
           summarise_all(funs("mean", mean, mean(., na.rm = TRUE))), 
         mapping = aes(x = extent, y = lat, col = season)) + 
  geom_jitter(size = 0.2) + scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  scale_x_reverse(name = "Seeeis-Ausdehnung in km$^{2}$") +
  scale_y_continuous(name = "Positionen Dijkstra") +
  theme_classic() +
  theme(legend.position = "bottom")
print(ggp.seaice.chebyshev)



## ENDE ENDE ENDE ####
