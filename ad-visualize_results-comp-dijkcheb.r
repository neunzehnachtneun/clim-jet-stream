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


## DATENAUFBEREITUNG DPLYR() ####
tb.jets.month$method <- factor(tb.jets.month$method, levels = c("Max", "Chebyshev", "Dijkstra"), 
                               labels = c("Maximum", "Chebyshev", "Dijkstra"))
tb.jets.month$class <- factor(tb.jets.month$class, levels = c("MJ", "PFJ", "SJ", "STJ"), 
                              labels = c("MJ", "PFJ", "SJ", "STJ"))

tb.subset <- 
  tb.jets.month %>%
  filter(method == "Dijkstra" & class == "STJ") %>%
  select(dts, year, month, lon, lat) %>%
  rename(Dijkstra.STJ = lat) %>%
  arrange(dts, lon)

tb.subset[,6] <- 
  tb.jets.month %>%
  filter(method == "Dijkstra" & class == "PFJ") %>%
  select(dts, year, month, lon, lat) %>%
  rename(Dijkstra.PFJ = lat) %>%
  arrange(dts, lon) %>%
  select(Dijkstra.PFJ)

tb.subset[,7] <- 
  tb.jets.month %>%
  filter(method == "Dijkstra" & class == "SJ") %>%
  select(dts, year, month, lon, lat) %>%
  rename(Dijkstra.SJ = lat) %>%
  arrange(dts, lon) %>%
  select(Dijkstra.SJ)

tb.subset[,8] <- 
  tb.jets.month %>%
  filter(method == "Chebyshev" & class == "STJ") %>%
  select(dts, year, month, lon, lat) %>%
  rename(Chebyshev.STJ = lat) %>%
  arrange(dts, lon) %>%
  select(Chebyshev.STJ)

tb.subset[,9] <- 
  tb.jets.month %>%
  filter(method == "Chebyshev" & class == "PFJ") %>%
  select(dts, year, month, lon, lat) %>%
  rename(Chebyshev.PFJ = lat) %>%
  arrange(dts, lon) %>%
  select(Chebyshev.PFJ)

tb.subset[,10] <- 
  tb.jets.month %>%
  filter(method == "Chebyshev" & class == "SJ") %>%
  select(dts, year, month, lon, lat) %>%
  rename(Chebyshev.SJ = lat) %>%
  arrange(dts, lon) %>%
  select(Chebyshev.SJ)



## VERGLEICH CHEBYSHEV VS DIJKSTRA ####
## 



## Polarfrontjet
## 
ggp.comp.pfj.pt <-
  ggplot(data = tb.subset, mapping = aes(x = Chebyshev.PFJ, y = Dijkstra.PFJ, col = lon)) + 
  geom_jitter(size = 0.2) + scale_colour_gsea() +
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash") +
  scale_x_continuous(name = "Positionen Chebyshev") +
  scale_y_continuous(name = "Positionen Dijkstra")
##
ggp.comp.pfj.ell <-
  ggplot(data = tb.subset, mapping = aes(x = Chebyshev.PFJ, y = Dijkstra.PFJ)) + 
  stat_ellipse(type = "norm", level = 0.90) + 
  stat_ellipse(type = "norm", level = 0.95) + 
  stat_ellipse(type = "norm", level = 0.99) +
  # geom_vline(xintercept = mean(tb.subset$Chebyshev.PFJ, na.rm = TRUE)) +
  # geom_hline(yintercept = mean(tb.subset$Dijkstra.PFJ, na.rm = TRUE)) +
  geom_point(mapping = aes(x = mean(tb.subset$Chebyshev.PFJ, na.rm = TRUE),
                           y = mean(tb.subset$Dijkstra.PFJ, na.rm = TRUE))) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash") +
  scale_x_continuous(name = "Positionen Chebyshev") + #+ geom_point()
  scale_y_continuous(name = "Positionen Dijkstra")

## Subtropenjet
## 
ggp.comp.stj.pt <-
  ggplot(data = tb.subset, mapping = aes(x = Chebyshev.STJ, y = Dijkstra.STJ, col = lon)) + 
  geom_jitter(size = 0.2) + scale_colour_gsea() +
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash") +
  scale_x_continuous(name = "Positionen Chebyshev") +
  scale_y_continuous(name = "Positionen Dijkstra")
##
ggp.comp.stj.ell <-
  ggplot(data = tb.subset, mapping = aes(x = Chebyshev.STJ, y = Dijkstra.STJ)) + 
  stat_ellipse(type = "norm", level = 0.90) + 
  stat_ellipse(type = "norm", level = 0.95) + 
  stat_ellipse(type = "norm", level = 0.99) +
  # geom_vline(xintercept = mean(tb.subset$Chebyshev.PFJ, na.rm = TRUE)) +
  # geom_hline(yintercept = mean(tb.subset$Dijkstra.PFJ, na.rm = TRUE)) +
  geom_point(mapping = aes(x = mean(tb.subset$Chebyshev.STJ, na.rm = TRUE),
                           y = mean(tb.subset$Dijkstra.STJ, na.rm = TRUE))) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash") +
  scale_x_continuous(name = "Positionen Chebyshev") + #+ geom_point()
  scale_y_continuous(name = "Positionen Dijkstra")

## Single-Jet
## 
ggp.comp.sj.pt <-
  ggplot(data = tb.subset, mapping = aes(x = Chebyshev.SJ, y = Dijkstra.SJ, col = lon)) + 
  geom_jitter(size = 0.2) + scale_colour_gsea() +
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash") +
  scale_x_continuous(name = "Positionen Chebyshev") +
  scale_y_continuous(name = "Positionen Dijkstra")
##
ggp.comp.sj.ell <-
  ggplot(data = tb.subset, mapping = aes(x = Chebyshev.SJ, y = Dijkstra.SJ)) + 
  stat_ellipse(type = "norm", level = 0.90) + 
  stat_ellipse(type = "norm", level = 0.95) + 
  stat_ellipse(type = "norm", level = 0.99) +
  geom_point(mapping = aes(x = mean(tb.subset$Chebyshev.SJ, na.rm = TRUE),
                           y = mean(tb.subset$Dijkstra.SJ, na.rm = TRUE))) +
  geom_abline(intercept = 0, slope = 1, linetype = "dotdash") +
  scale_x_continuous(name = "Positionen Chebyshev") + #+ geom_point()
  scale_y_continuous(name = "Positionen Dijkstra")
  

## SPEICHERN DER PLOTS ####
## 

## als tex-dateien
filepath <- "05-visu-pdf/06-comp-cheb-dijk/pfj_points.tex"
tikz(file = filepath)
print(ggp.comp.pfj.pt)
dev.off()
filepath <- "05-visu-pdf/06-comp-cheb-dijk/pfj_ellipse.tex"
tikz(file = filepath)
print(ggp.comp.pfj.ell)
dev.off()
filepath <- "05-visu-pdf/06-comp-cheb-dijk/stj_points.tex"
tikz(file = filepath)
print(ggp.comp.stj.pt)
dev.off()
filepath <- "05-visu-pdf/06-comp-cheb-dijk/stj_ellipse.tex"
tikz(file = filepath)
print(ggp.comp.stj.ell)
dev.off()
filepath <- "05-visu-pdf/06-comp-cheb-dijk/sj_points.tex"
tikz(file = filepath)
print(ggp.comp.sj.pt)
dev.off()
filepath <- "05-visu-pdf/06-comp-cheb-dijk/sj_ellipse.tex"
tikz(file = filepath)
print(ggp.comp.sj.ell)
dev.off()

# ggsave(filename = "sj_ellipse.tex",
#        plot = ggp.comp.sj.ell, device = tikz, path = "05-visu-pdf/06-comp-cheb-dijk/", 
#        dpi = 600, width = 297, height = 210, units = "mm")



## ENDE ENDE ENDE ####
