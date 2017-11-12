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

## Pfad zum Speichern von Abbildungen festlegen:
#save.dir <- "/home/skiefer/01-Master-Thesis/02-code-git/05-visu-pdf-tikz/"
save.dir <- "05-visu-pdf-tikz/"

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
# tb.jets.month$method <- factor(tb.jets.month$method, levels = c("Maximum", "Chebyshev", "Dijkstra"), 
                               # labels = c("Maximum", "Chebyshev", "Dijkstra"))
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

tb.subset[,12] <- 
  tb.jets.month %>%
  filter(method == "Chebyshev" & class == "MJ") %>%
  select(dts, year, season, month, lon, lat) %>%
  rename(Chebyshev.MJ = lat) %>%
  arrange(dts, lon) %>%
  select(Chebyshev.MJ)

tb.subset[,13] <- 
  tb.jets.month %>%
  filter(method == "Maximum" & class == "MJ") %>%
  select(dts, year, season, month, lon, lat) %>%
  rename(Maximum.MJ = lat) %>%
  arrange(dts, lon) %>%
  select(Maximum.MJ)


## VERGLEICH VERSCHIEDENER JETS NACH METHODEN ####
##

## Maximum Jetstream
ggp.comp.mj.pt <-
  ggplot(data = tb.subset, mapping = aes(x = Chebyshev.MJ, y = Maximum.MJ, col = season)) + 
  geom_jitter(size = 0.2) + scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash") +
  labs(colour = "Jahreszeit") +
  guides(colour = guide_legend(title.position = "top",
                               direction = "horizontal",
                               label.position = "bottom",
                               nrow = 1)) +
  scale_x_continuous(name = "Position$_{Chebyshev}$ in $^{\\circ}$ nördlicher Breite") +
  scale_y_continuous(name = "Position$_{Maximum}$ in $^{\\circ}$ nördlicher Breite") +
  coord_fixed(xlim = c(5, 85), ylim = c(5,85)) + 
  theme_bw() + theme(legend.position = "bottom")

ggp.comp.mj.cont <-
  ggplot(data = tb.subset, mapping = aes(x = Chebyshev.MJ, y = Maximum.MJ)) + 
  geom_density_2d(aes(color = season)) + 
  scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash") +
  labs(colour = "Jahreszeit") +
  guides(colour = guide_legend(title.position = "top",
                               direction = "horizontal",
                               label.position = "bottom",
                               nrow = 1)) +
  scale_x_continuous(name = "Position$_{Chebyshev}$ in $^{\\circ}$ nördlicher Breite") +
  scale_y_continuous(name = "Position$_{Maximum}$ in $^{\\circ}$ nördlicher Breite") +
  coord_fixed(xlim = c(15, 60), ylim = c(15,60)) + 
  theme_bw() + theme(legend.position = "bottom")


## Polarfrontjetstreams
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
  scale_x_continuous(name = "Position$_{Chebyshev}$ in $^{\\circ}$ nördlicher Breite") +
  scale_y_continuous(name = "Position$_{Dijkstra}$ in $^{\\circ}$ nördlicher Breite") +
  coord_fixed(xlim = c(35, 85), ylim = c(35,85)) + 
  theme_bw() + theme(legend.position = "bottom")

ggp.comp.pfj.cont <-
  ggplot(data = tb.subset, mapping = aes(x = Chebyshev.PFJ, y = Dijkstra.PFJ)) + 
  geom_density_2d(aes(color = season)) + 
  scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash") +
  labs(colour = "Jahreszeit") +
  guides(colour = guide_legend(title.position = "top",
                               direction = "horizontal",
                               label.position = "bottom",
                               nrow = 1)) +
  scale_x_continuous(name = "Position$_{Chebyshev}$ in $^{\\circ}$ nördlicher Breite") +
  scale_y_continuous(name = "Position$_{Dijkstra}$ in $^{\\circ}$ nördlicher Breite") +
  coord_fixed(xlim = c(35, 85), ylim = c(35,85)) + 
  theme_bw() + theme(legend.position = "bottom")

## Subtropenjetstreams
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
  scale_x_continuous(name = "Position$_{Chebyshev}$ in $^{\\circ}$ nördlicher Breite") +
  scale_y_continuous(name = "Position$_{Dijkstra}$ in $^{\\circ}$ nördlicher Breite") +
  coord_fixed(xlim = c(15, 70), ylim = c(15,70)) + 
  theme_bw() + theme(legend.position = "bottom")

ggp.comp.stj.cont <-
  ggplot(data = tb.subset, mapping = aes(x = Chebyshev.STJ, y = Dijkstra.STJ)) + 
  geom_density_2d(aes(color = season)) + 
  scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash") +
  labs(colour = "Jahreszeit") +
  guides(colour = guide_legend(title.position = "top",
                               direction = "horizontal",
                               label.position = "bottom",
                               nrow = 1)) +
  scale_x_continuous(name = "Position$_{Chebyshev}$ in $^{\\circ}$ nördlicher Breite") +
  scale_y_continuous(name = "Position$_{Dijkstra}$ in $^{\\circ}$ nördlicher Breite") +
  coord_fixed(xlim = c(15, 70), ylim = c(15,70)) + 
  theme_bw() + theme(legend.position = "bottom")


## Single-Jetstreams
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
  scale_x_continuous(name = "Position$_{Chebyshev}$ in $^{\\circ}$ nördlicher Breite") +
  scale_y_continuous(name = "Position$_{Dijkstra}$ in $^{\\circ}$ nördlicher Breite") +
  coord_fixed(xlim = c(15, 70), ylim = c(15,70)) + 
  theme_bw() + theme(legend.position = "bottom")

ggp.comp.sj.cont <-
  ggplot(data = tb.subset, mapping = aes(x = Chebyshev.SJ, y = Dijkstra.SJ)) + 
  geom_density_2d(aes(color = season)) + 
  scale_colour_brewer(palette = "RdYlBu", direction = -1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash") +
  labs(colour = "Jahreszeit") +
  guides(colour = guide_legend(title.position = "top",
                               direction = "horizontal",
                               label.position = "bottom",
                               nrow = 1)) +
  scale_x_continuous(name = "Position$_{Chebyshev}$ in $^{\\circ}$ nördlicher Breite") +
  scale_y_continuous(name = "Position$_{Dijkstra}$ in $^{\\circ}$ nördlicher Breite") +
  coord_fixed(xlim = c(15, 70), ylim = c(15,70)) + 
  theme_bw() + theme(legend.position = "bottom")
  

## SPEICHERN DER PLOTS ####
## 

plt.save(plt = ggp.comp.mj.pt, 
         width = 90, height = 90, pointsize = 11,
         filepath = paste0(save.dir, "03-comp/"), 
         filename = "MJ_scat")
plt.save(plt = ggp.comp.mj.cont, 
         width = 90, height = 90, pointsize = 11,
         filepath = paste0(save.dir, "03-comp/"), 
         filename = "MJ_cont")

plt.save(plt = ggp.comp.pfj.pt, 
         width = 90, height = 90, pointsize = 11,
         filepath = paste0(save.dir, "03-comp/"), 
         filename = "PFJ_scat")
plt.save(plt = ggp.comp.pfj.cont, 
         width = 90, height = 90, pointsize = 11,
         filepath = paste0(save.dir, "03-comp/"), 
         filename = "PFJ_cont")

plt.save(plt = ggp.comp.stj.pt, 
         width = 90, height = 90, pointsize = 11,
         filepath = paste0(save.dir, "03-comp/"), 
         filename = "STJ_scat")
plt.save(plt = ggp.comp.stj.cont, 
         width = 90, height = 90, pointsize = 11,
         filepath = paste0(save.dir, "03-comp/"), 
         filename = "STJ_cont")

plt.save(plt = ggp.comp.sj.pt, 
         width = 90, height = 65, pointsize = 11,
         filepath = paste0(save.dir, "03-comp/"), 
         filename = "SJ_scat")
plt.save(plt = ggp.comp.sj.cont, 
         width = 90, height = 90, pointsize = 11,
         filepath = paste0(save.dir, "/03-comp"), 
         filename = "SJ_cont")

## Jahreszeitweise Plot mit points/jitter + density_2d
## 
ssn <- c("DJF", "MAM", "JJA", "SON")
clr.ssn <- c("#2c7bb6", "#fdae61", "#d7191c", "#abd9e9")
for (i.ssn in seq_along(ssn)) {
  print(i.ssn)
  
  ## Polarfrontjetstreams
  ## 
  ggp.comp.pfj.pt.cont <-
    ggplot(data = tb.subset %>%
             filter(season == ssn[i.ssn]), 
           mapping = aes(x = Chebyshev.PFJ, y = Dijkstra.PFJ)) + 
    geom_jitter(colour = clr.ssn[i.ssn], size = 0.2) + geom_density_2d(colour = "black") + 
    geom_abline(intercept = 0, slope = 1, linetype = "dotdash") +
    labs(colour = "Jahreszeit") +
    guides(colour = guide_legend(title.position = "top",
                                 direction = "horizontal",
                                 label.position = "bottom",
                                 nrow = 1)) +
    scale_x_continuous(name = "Position$_{Chebyshev}$ in $^{\\circ}$ nördlicher Breite") +
    scale_y_continuous(name = "Position$_{Dijkstra}$ in $^{\\circ}$ nördlicher Breite") +
    coord_fixed(xlim = c(35, 85), ylim = c(35,85)) + 
    theme_bw() + theme(legend.position = "bottom")
  print(ggp.comp.pfj.pt.cont)
  ## Subtropenjetstreams
  ## 
  ggp.comp.stj.pt.cont <-
    ggplot(data = tb.subset %>%
             filter(season == ssn[i.ssn]), 
           mapping = aes(x = Chebyshev.STJ, y = Dijkstra.STJ)) + 
    geom_jitter(colour = clr.ssn[i.ssn], size = 0.2) + geom_density_2d(colour = "black") + 
    geom_abline(intercept = 0, slope = 1, linetype = "dotdash") +
    labs(colour = "Jahreszeit") +
    guides(colour = guide_legend(title.position = "top",
                                 direction = "horizontal",
                                 label.position = "bottom",
                                 nrow = 1)) +
    scale_x_continuous(name = "Position$_{Chebyshev}$ in $^{\\circ}$ nördlicher Breite") +
    scale_y_continuous(name = "Position$_{Dijkstra}$ in $^{\\circ}$ nördlicher Breite") +
    coord_fixed(xlim = c(15, 70), ylim = c(15,70)) + 
    theme_bw() + theme(legend.position = "bottom")
  print(ggp.comp.stj.pt.cont)
  
  ## Speichern der Plots
  plt.save(plt = ggp.comp.pfj.pt.cont, 
           width = 90, height = 90, pointsize = 11,
           filepath = paste0(save.dir, "03-comp/"), 
           filename = paste0("PFJ_ptcont_", ssn[i.ssn]))
  plt.save(plt = ggp.comp.stj.pt.cont, 
           width = 90, height = 90, pointsize = 11,
           filepath = paste0(save.dir, "03-comp/"), 
           filename = paste0("STJ_ptcont_", ssn[i.ssn]))
}


## ENDE ENDE ENDE ####
