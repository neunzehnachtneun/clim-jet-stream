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

save.dir <- "05-visu-pdf-tikz/"

# Nachladen der Packages
# library(ncdf4); library(chron); library(parallel); library(foreach); library(doParallel);

## VISUALISIERUNG GGPLOT2() ####
##
## Nötige Pakete
library(lubridate) # Datumsformate
library(dplyr) # Datenbearbeitung
library(magrittr)
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

# tb.subset <- 
#   tb.jets.month %>%
#   filter(lon >= -90 & lon <= -0) %>%
#   filter(method == "Chebyshev" & class == "PFJ")


## Mittel -> method, class, year ####
## Für Klimatrends!
## 1. Mittelwert
tb.subset.yr.mean <-
  tb.jets.month %>%
  filter(class != "MJ") %>%
#  filter(lon >= -90 & lon <= -0) %>%
  select(year, season, month, dts, method, class, lon, lat, u, v, extent) %>%
  group_by(method, class, year, month, dts) %>%
  summarise_at(.vars = vars(lat, u, v), .funs = funs("mean", mean, mean(., na.rm = TRUE)))
tb.subset.yr.mean <- left_join(tb.subset.yr.mean, tb.sea.ice, by = c("year", "month"))


## 2. Standardabweichung
tb.subset.yr.sdev <-
  tb.jets.month %>%
  filter(class != "MJ") %>%
#  filter(lon >= -90 & lon <= -0) %>%
  select(year, month, dts, method, class, lon, lat, u, v, extent) %>%
  group_by(method, class, year, month, dts) %>%
  summarise_at(.vars = vars(lat, u, v), .funs = funs("sd", sd, sd(., na.rm = TRUE)))
tb.subset.yr.sdev <- left_join(tb.subset.yr.sdev, tb.sea.ice, by = c("year", "month"))


## Seeeisentwicklung allgemein
ggp.seaice <- 
  ggplot(data = tb.subset.yr.mean %>%
           filter(year >= 1978),
         mapping = aes(x = dts, y = extent)) + 
  geom_line(size = 0.2) +
  geom_smooth(method = "lm", formula = y ~ x, color = "black") +
  scale_x_datetime(name = "Jahr") +
  scale_y_continuous(name = "Meereis-Ausdehnung in $10^{6}\\,km^{2}$",
                     breaks = c(4,8,12,16)) +
  theme_bw() + theme(legend.position = "bottom")
print(ggp.seaice)


## Chebyshev
## Mean
ggp.seaice.chebyshev.mean <-
  ggplot(data = tb.subset.yr.mean %>%
           filter(method == "Chebyshev" & class == "PFJ"), 
         mapping = aes(x = extent, y = lat_mean)) + 
  geom_point(size = 0.2) + scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  geom_smooth(method = "lm", formula = y ~ x, color = "black") +
  scale_x_reverse(name = "Meereis-Ausdehnung in $10^{6}\\,km^{2}$") +
  scale_y_continuous(name = "Positionen in\n$^{\\circ}$ nördlicher Breite") +
  theme_bw() +
  theme(legend.position = "bottom")
print(ggp.seaice.chebyshev.mean)

## Sdev
ggp.seaice.chebyshev.sdev <-
  ggplot(data =   tb.subset.yr.sdev %>%
           filter(method == "Chebyshev" & class == "PFJ"), 
         mapping = aes(x = extent, y = lat_sd)) + 
  geom_point(size = 0.2) + scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  geom_smooth(method = "lm", formula = y ~ x, color = "black") +
  scale_x_reverse(name = "Meereis-Ausdehnung in $10^{6}\\,km^{2}$") +
  scale_y_continuous(name = "Positionen in\n$^{\\circ}$ nördlicher Breite") +
  theme_bw() +
  theme(legend.position = "bottom")
print(ggp.seaice.chebyshev.sdev)

## Dijkstra
## Mean
ggp.seaice.dijkstra.mean <-
  ggplot(data =   tb.subset.yr.mean %>%
           filter(method == "Dijkstra" & class == "PFJ"), 
         mapping = aes(x = extent, y = lat_mean)) + 
  geom_point(size = 0.2) + scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  geom_smooth(method = "lm", formula = y ~ x, color = "black") +
  scale_x_reverse(name = "Meereis-Ausdehnung in $10^{6}\\,km^{2}$") +
  scale_y_continuous(name = "Positionen in\n$^{\\circ}$ nördlicher Breite") +
  theme_bw() +
  theme(legend.position = "bottom")
print(ggp.seaice.dijkstra.mean)
## Sdev
ggp.seaice.dijkstra.sdev <-
  ggplot(data =   tb.subset.yr.sdev %>%
           filter(method == "Dijkstra" & class == "PFJ"), 
         mapping = aes(x = extent, y = lat_sd)) + 
  geom_point(size = 0.2) + scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  geom_smooth(method = "lm", formula = y ~ x, color = "black") +
  scale_x_reverse(name = "Meereis-Ausdehnung in $10^{6}\\,km^{2}$") +
  scale_y_continuous(name = "Positionen in\n$^{\\circ}$ nördlicher Breite") +
  theme_bw() +
  theme(legend.position = "bottom")
print(ggp.seaice.dijkstra.sdev)


plt.save(plt = ggp.seaice, 
         width = 135, height = 70, pointsize = 9,
         filepath = paste0(save.dir, "08-sea-ice/"), 
         filename = "seaice-timeseries")

plt.save(plt = ggp.seaice.chebyshev.mean, 
         width = 135, height = 70, pointsize = 9,
         filepath = paste0(save.dir, "08-sea-ice/"), 
         filename = "seaice-cheb-mean")

plt.save(plt = ggp.seaice.chebyshev.sdev, 
         width = 135, height = 70, pointsize = 9,
         filepath = paste0(save.dir, "08-sea-ice/"), 
         filename = "seaice-cheb-sdev")

plt.save(plt = ggp.seaice.dijkstra.mean, 
         width = 135, height = 70, pointsize = 9,
         filepath = paste0(save.dir, "08-sea-ice/"), 
         filename = "seaice-dijk-mean")

plt.save(plt = ggp.seaice.dijkstra.sdev, 
         width = 135, height = 70, pointsize = 9,
         filepath = paste0(save.dir, "08-sea-ice/"), 
         filename = "seaice-dijk-sdev")


## Korrelationen nach Pearson zwischen arktischem Seeeis und * ####
## 
library(broom)
# Chebyshev Mean
tb.subset.yr.mean %>%
  filter(method == "Chebyshev") %$%
  cor.test(extent, lat_mean) %>%
  tidy
# Chebyshev Standardabweichung
tb.subset.yr.sdev %>%
  filter(method == "Chebyshev") %$%
  cor.test(extent, lat_sd) %>%
  tidy
# Dijkstra Mittel
tb.subset.yr.mean %>%
  filter(method == "Dijkstra") %$%
  cor.test(extent, lat_mean) %>%
  tidy
# Dijkstra Standardabweichung
tb.subset.yr.sdev %>%
  filter(method == "Dijkstra") %$%
  cor.test(extent, lat_sd) %>%
  tidy


## ENDE ENDE ENDE ####
