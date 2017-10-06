## source("ad-visualize_results-comp-dijkcheb.r")
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
  filter(method == "Dijkstra" & class == "STJ") %>%
  select(dts, year, season, month, lon, lat) %>%
  rename(Dijkstra.STJ = lat) %>%
  arrange(dts, lon)

tb.subset[,7] <- 
  tb.jets.month %>%
  filter(method == "Dijkstra" & class == "PFJ") %>%
  select(dts, year, season, month, lon, lat) %>%
  rename(Dijkstra.PFJ = lat) %>%
  arrange(dts, lon) %>%
  select(Dijkstra.PFJ)

tb.subset[,8] <- 
  tb.jets.month %>%
  filter(method == "Dijkstra" & class == "SJ") %>%
  select(dts, year, season, month, lon, lat) %>%
  rename(Dijkstra.SJ = lat) %>%
  arrange(dts, lon) %>%
  select(Dijkstra.SJ)

tb.subset[,9] <- 
  tb.jets.month %>%
  filter(method == "Chebyshev" & class == "STJ") %>%
  select(dts, year, season, month, lon, lat) %>%
  rename(Chebyshev.STJ = lat) %>%
  arrange(dts, lon) %>%
  select(Chebyshev.STJ)

tb.subset[,10] <- 
  tb.jets.month %>%
  filter(method == "Chebyshev" & class == "PFJ") %>%
  select(dts, year, season, month, lon, lat) %>%
  rename(Chebyshev.PFJ = lat) %>%
  arrange(dts, lon) %>%
  select(Chebyshev.PFJ)

tb.subset[,11] <- 
  tb.jets.month %>%
  filter(method == "Chebyshev" & class == "SJ") %>%
  select(dts, year, season, month, lon, lat) %>%
  rename(Chebyshev.SJ = lat) %>%
  arrange(dts, lon) %>%
  select(Chebyshev.SJ)



## VERGLEICH CHEBYSHEV VS DIJKSTRA ####
## 

## Polarfrontjet
## 
ggp.comp.pfj.pt <-
  ggplot(data = tb.subset, mapping = aes(x = Chebyshev.PFJ, y = Dijkstra.PFJ, col = season)) + 
  geom_jitter(size = 0.2) + scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash") +
  labs(colour = "Jahreszeit") +
  guides(colour = guide_legend(title.position = "top",
                              direction = "horizontal",
                              label.position = "bottom",
                              nrow = 1)) +
  scale_x_continuous(name = "Breitengrad$_{Chebyshev}$ in $^{\\circ}$") +
  scale_y_continuous(name = "Breitengrad$_{Dijkstra}$ in $^{\\circ}$") +
  coord_fixed(xlim = c(35, 85), ylim = c(35,85)) + 
  theme_bw() + theme(legend.position = "bottom")

tikz(file = "test.tex", width = 14, height = 17, pointsize = 11)
print(ggp.comp.pfj.pt)
dev.off()


##
ggp.comp.pfj.ell <-
  ggplot(data = tb.subset, mapping = aes(x = Chebyshev.PFJ, y = Dijkstra.PFJ)) + 
  stat_ellipse(type = "norm", level = 0.90) + 
  stat_ellipse(type = "norm", level = 0.95) + 
  stat_ellipse(type = "norm", level = 0.99) +
  geom_point(mapping = aes(x = mean(tb.subset$Chebyshev.PFJ, na.rm = TRUE),
                           y = mean(tb.subset$Dijkstra.PFJ, na.rm = TRUE))) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash") +
  scale_x_continuous(name = "Breitengrad$_{Chebyshev}$ in $^{\\circ}$") +
  scale_y_continuous(name = "Breitengrad$_{Dijkstra}$ in $^{\\circ}$") +
  coord_fixed(xlim = c(35, 85), ylim = c(35,85)) + 
  theme_bw()


## Subtropenjet
## 
ggp.comp.stj.pt <-
  ggplot(data = tb.subset, mapping = aes(x = Chebyshev.STJ, y = Dijkstra.STJ, col = season)) + 
  geom_jitter(size = 0.2) + scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash") +
  labs(colour = "Jahreszeit") +
  guides(colour = guide_legend(title.position = "top",
                               direction = "horizontal",
                               label.position = "bottom",
                               nrow = 1)) +
  scale_x_continuous(name = "Breitengrad$_{Chebyshev}$ in $^{\\circ}$") +
  scale_y_continuous(name = "Breitengrad$_{Dijkstra}$ in $^{\\circ}$") +
  coord_fixed(xlim = c(15, 70), ylim = c(15,70)) + 
  theme_bw() + theme(legend.position = "bottom")

##
ggp.comp.stj.ell <-
  ggplot(data = tb.subset, mapping = aes(x = Chebyshev.STJ, y = Dijkstra.STJ)) + 
  stat_ellipse(type = "norm", level = 0.90) + 
  stat_ellipse(type = "norm", level = 0.95) + 
  stat_ellipse(type = "norm", level = 0.99) +
  geom_point(mapping = aes(x = mean(tb.subset$Chebyshev.STJ, na.rm = TRUE),
                           y = mean(tb.subset$Dijkstra.STJ, na.rm = TRUE))) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash") +
  scale_x_continuous(name = "Breitengrad$_{Chebyshev}$ in $^{\\circ}$") +
  scale_y_continuous(name = "Breitengrad$_{Dijkstra}$ in $^{\\circ}$") +
  coord_fixed(xlim = c(15, 80), ylim = c(15,80)) + 
  theme_bw() + theme(legend.position = "bottom")

## Single-Jet
## 
ggp.comp.sj.pt <-
  ggplot(data = tb.subset, mapping = aes(x = Chebyshev.SJ, y = Dijkstra.SJ, col = season)) + 
  geom_jitter(size = 0.2) + scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash") +
  labs(colour = "Jahreszeit") +
  guides(colour = guide_legend(title.position = "top",
                               direction = "horizontal",
                               label.position = "bottom",
                               nrow = 1)) +
  scale_x_continuous(name = "Breitengrad$_{Chebyshev}$ in $^{\\circ}$") +
  scale_y_continuous(name = "Breitengrad$_{Dijkstra}$ in $^{\\circ}$") +
  coord_fixed(xlim = c(15, 70), ylim = c(15,70)) + 
  theme_bw() + theme(legend.position = "bottom")


##
ggp.comp.sj.ell <-
  ggplot(data = tb.subset, mapping = aes(x = Chebyshev.SJ, y = Dijkstra.SJ)) + 
  stat_ellipse(type = "norm", level = 0.90) + 
  stat_ellipse(type = "norm", level = 0.95) + 
  stat_ellipse(type = "norm", level = 0.99) +
  geom_point(mapping = aes(x = mean(tb.subset$Chebyshev.SJ, na.rm = TRUE),
                           y = mean(tb.subset$Dijkstra.SJ, na.rm = TRUE))) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash") +
  scale_x_continuous(name = "Breitengrad$_{Chebyshev}$ in $^{\\circ}$") +
  scale_y_continuous(name = "Breitengrad$_{Dijkstra}$ in $^{\\circ}$") +
  coord_fixed(xlim = c(15, 80), ylim = c(15,80)) + 
  theme_bw() + theme(legend.position = "bottom")
  

## SPEICHERN DER PLOTS ####
## 

plt.save(plt = ggp.comp.pfj.pt, 
         width = 140, height = 140, pointsize = 11,
         filepath = "/home/skiefer/01-Master-Thesis/02-code-git/05-visu-pdf/03-comp/", 
         filename = "PFJ_scat")
plt.save(plt = ggp.comp.pfj.ell, 
         width = 140, height = 140, pointsize = 11,
         filepath = "/home/skiefer/01-Master-Thesis/02-code-git/05-visu-pdf/03-comp/", 
         filename = "PFJ_ell")

plt.save(plt = ggp.comp.stj.pt, 
         width = 140, height = 140, pointsize = 11,
         filepath = "/home/skiefer/01-Master-Thesis/02-code-git/05-visu-pdf/03-comp/", 
         filename = "STJ_scat")
plt.save(plt = ggp.comp.stj.ell, 
         width = 140, height = 140, pointsize = 11,
         filepath = "/home/skiefer/01-Master-Thesis/02-code-git/05-visu-pdf/03-comp/", 
         filename = "STJ_ell")

plt.save(plt = ggp.comp.sj.pt, 
         width = 140, height = 140, pointsize = 11,
         filepath = "/home/skiefer/01-Master-Thesis/02-code-git/05-visu-pdf/03-comp/", 
         filename = "SJ_scat")
plt.save(plt = ggp.comp.sj.ell, 
         width = 140, height = 140, pointsize = 11,
         filepath = "/home/skiefer/01-Master-Thesis/02-code-git/05-visu-pdf/03-comp/", 
         filename = "SJ_ell")

## ENDE ENDE ENDE ####
