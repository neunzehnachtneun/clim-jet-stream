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

## mean
tb.subset <-
  tb.jets.month %>%
  filter(method == "Chebyshev" & class != "MJ") %>%
  select(year, season, method, class, lon, lat, u, v) %>%
  group_by(method, class, year, season, lon) %>%
  summarise_all(funs(mean, "mean", mean(., na.rm = TRUE)))


## running mean
tb.subset <-
  tb.jets.month %>%
  filter(method == "Chebyshev" & class != "MJ" & season == "son") %>%
  select(year, lon, lat, u, v) %>%
  group_by(year, lon) %>%
  mutate(RUNMEAN = rollmean(lat, k = 5, fill = NA))


## VISUALISIERUNG DER HOVMÖLLER-DIAGRAMME ** PFJ ####
## POLARFRONT JETSTREAM

# Schleife über Jahreszeiten
for (i.ssn in c("djf", "mam", "jja", "son")) {
  print(i.ssn)
  
  # Positionen Breitengrad Chebyshev
  hovm.pfj.lat.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                                mapping = aes(x = Longitude, y = Year, fill = PFJ.lat.m2c)) +
    geom_tile() + scale_fill_gsea()
  # Positionen Breitengrad Dijkstra
  hovm.pfj.lat.m3.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                                mapping = aes(x = Longitude, y = Year, fill = PFJ.lat.m3)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Zonalwind Chebyshev
  hovm.pfj.u.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                              mapping = aes(x = Longitude, y = Year, fill = PFJ.u.m2c)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Zonalwind Dijkstra
  hovm.pfj.u.m3.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                              mapping = aes(x = Longitude, y = Year, fill = PFJ.u.m3)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Meridionalwind Chebyshev
  hovm.pfj.v.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                              mapping = aes(x = Longitude, y = Year, fill = PFJ.v.m2c)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Meridionalwind Dijkstra
  hovm.pfj.v.m3.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                              mapping = aes(x = Longitude, y = Year, fill = PFJ.v.m3)) +
    geom_tile() + scale_fill_gsea()

  
  ## RELATIV ZU ZONALEM MITTEL
  # Positionen Breitengrad Chebyshev
  hovm.pfj.lat.m2.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                                mapping = aes(x = Longitude, y = Year, fill = PFJ.lat.m2c)) +
    geom_tile() + scale_fill_gsea()
  # Positionen Breitengrad Dijkstra
  hovm.pfj.lat.m3.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                                mapping = aes(x = Longitude, y = Year, fill = PFJ.lat.m3)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Zonalwind Chebyshev
  hovm.pfj.u.m2.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                              mapping = aes(x = Longitude, y = Year, fill = PFJ.u.m2c)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Zonalwind Dijkstra
  hovm.pfj.u.m3.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                              mapping = aes(x = Longitude, y = Year, fill = PFJ.u.m3)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Meridionalwind Chebyshev
  hovm.pfj.v.m2.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                              mapping = aes(x = Longitude, y = Year, fill = PFJ.v.m2c)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Meridionalwind Dijkstra
  hovm.pfj.v.m3.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                              mapping = aes(x = Longitude, y = Year, fill = PFJ.v.m3)) +
    geom_tile() + scale_fill_gsea()

  
  ## Speichern der Plots
  # Breitengrade
  ggsave(filename = paste0("pfj-", i.ssn, "-m2-abs.pdf"),
         plot = hovm.pfj.lat.m2.abs, device = pdf, path = "05-visu-pdf/02-hovm-lat/", 
         dpi = 600, width = 297, height = 210, units = "mm")
  ggsave(filename = paste0("pfj-", i.ssn, "-m3-abs.pdf"),
         plot = hovm.pfj.lat.m3.abs, device = pdf, path = "05-visu-pdf/02-hovm-lat/", 
         dpi = 600, width = 297, height = 210, units = "mm")
  ggsave(filename = paste0("pfj-", i.ssn, "-m2-rel.pdf"),
         plot = hovm.pfj.lat.m2.rel, device = pdf, path = "05-visu-pdf/02-hovm-lat/", 
         dpi = 600, width = 297, height = 210, units = "mm")
  ggsave(filename = paste0("pfj-", i.ssn, "-m3-rel.pdf"),
         plot = hovm.pfj.lat.m3.rel, device = pdf, path = "05-visu-pdf/02-hovm-lat/", 
         dpi = 600, width = 297, height = 210, units = "mm")
  # Zonalwind
  ggsave(filename = paste0("pfj-", i.ssn, "-m2-abs.pdf"),
         plot = hovm.pfj.u.m2.abs, device = pdf, path = "05-visu-pdf/03-hovm-u/", 
         dpi = 600, width = 297, height = 210, units = "mm")
  ggsave(filename = paste0("pfj-", i.ssn, "-m3-abs.pdf"),
         plot = hovm.pfj.u.m3.abs, device = pdf, path = "05-visu-pdf/03-hovm-u/", 
         dpi = 600, width = 297, height = 210, units = "mm")
  ggsave(filename = paste0("pfj-", i.ssn, "-m2-rel.pdf"),
         plot = hovm.pfj.u.m2.rel, device = pdf, path = "05-visu-pdf/03-hovm-u/", 
         dpi = 600, width = 297, height = 210, units = "mm")
  ggsave(filename = paste0("pfj-", i.ssn, "-m3-rel.pdf"),
         plot = hovm.pfj.u.m3.rel, device = pdf, path = "05-visu-pdf/03-hovm-u/", 
         dpi = 600, width = 297, height = 210, units = "mm")
  # Meridionalwind
  ggsave(filename = paste0("pfj-", i.ssn, "-m2-abs.pdf"),
         plot = hovm.pfj.v.m2.abs, device = pdf, path = "05-visu-pdf/04-hovm-v/", 
         dpi = 600, width = 297, height = 210, units = "mm")
  ggsave(filename = paste0("pfj-", i.ssn, "-m3-abs.pdf"),
         plot = hovm.pfj.v.m3.abs, device = pdf, path = "05-visu-pdf/04-hovm-v/", 
         dpi = 600, width = 297, height = 210, units = "mm")
  ggsave(filename = paste0("pfj-", i.ssn, "-m2-rel.pdf"),
         plot = hovm.pfj.v.m2.rel, device = pdf, path = "05-visu-pdf/04-hovm-v/", 
         dpi = 600, width = 297, height = 210, units = "mm")
  ggsave(filename = paste0("pfj-", i.ssn, "-m3-rel.pdf"),
         plot = hovm.pfj.v.m3.rel, device = pdf, path = "05-visu-pdf/04-hovm-v/", 
         dpi = 600, width = 297, height = 210, units = "mm")
}
## HOVMÖLLER-DIAGRAMME ** STJ ####
## SUBTROPISCHER JETSTREAM

# Schleife über Jahreszeiten
for (i.ssn in c("djf", "mam", "jja", "son")) {
  print(i.ssn)
  
  # Positionen Breitengrad Chebyshev
  hovm.stj.lat.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                                mapping = aes(x = Longitude, y = Year, fill = STJ.lat.m2c)) +
    geom_tile() + scale_fill_gsea() 
  # Positionen Breitengrad Dijkstra
  hovm.stj.lat.m3.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                                mapping = aes(x = Longitude, y = Year, fill = STJ.lat.m3)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Zonalwind Chebyshev
  hovm.stj.u.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                              mapping = aes(x = Longitude, y = Year, fill = STJ.u.m2c)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Zonalwind Dijkstra
  hovm.stj.u.m3.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                              mapping = aes(x = Longitude, y = Year, fill = STJ.u.m3)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Meridionalwind Chebyshev
  hovm.stj.v.m2.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                              mapping = aes(x = Longitude, y = Year, fill = STJ.v.m2c)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Meridionalwind Dijkstra
  hovm.stj.v.m3.abs <- ggplot(data = df.jets.season[which(df.jets.season$Season == i.ssn),],
                              mapping = aes(x = Longitude, y = Year, fill = STJ.v.m3)) +
    geom_tile() + scale_fill_gsea()
  
  
  ## RELATIV ZU ZONALEM MITTEL
  # Positionen Breitengrad Chebyshev
  hovm.stj.lat.m2.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                                mapping = aes(x = Longitude, y = Year, fill = STJ.lat.m2c)) +
    geom_tile() + scale_fill_gsea()
  # Positionen Breitengrad Dijkstra
  hovm.stj.lat.m3.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                                mapping = aes(x = Longitude, y = Year, fill = STJ.lat.m3)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Zonalwind Chebyshev
  hovm.stj.u.m2.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                              mapping = aes(x = Longitude, y = Year, fill = STJ.u.m2c)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Zonalwind Dijkstra
  hovm.stj.u.m3.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                              mapping = aes(x = Longitude, y = Year, fill = STJ.u.m3)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Meridionalwind Chebyshev
  hovm.stj.v.m2.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                              mapping = aes(x = Longitude, y = Year, fill = STJ.v.m2c)) +
    geom_tile() + scale_fill_gsea()
  # Intensität Meridionalwind Dijkstra
  hovm.stj.v.m3.rel <- ggplot(data = df.jets.season.rel[which(df.jets.season$Season == i.ssn),],
                              mapping = aes(x = Longitude, y = Year, fill = STJ.v.m3)) +
    geom_tile() + scale_fill_gsea()
  
  
  ## Speichern der Plots
  # Breitengrade
  ggsave(filename = paste0("stj-", i.ssn, "-m2-abs.pdf"),
         plot = hovm.pfj.lat.m2.abs, device = pdf, path = "05-visu-pdf/02-hovm-lat/", 
         dpi = 600, width = 297, height = 210, units = "mm")
  ggsave(filename = paste0("stj-", i.ssn, "-m3-abs.pdf"),
         plot = hovm.pfj.lat.m3.abs, device = pdf, path = "05-visu-pdf/02-hovm-lat/", 
         dpi = 600, width = 297, height = 210, units = "mm")
  ggsave(filename = paste0("stj-", i.ssn, "-m2-rel.pdf"),
         plot = hovm.pfj.lat.m2.rel, device = pdf, path = "05-visu-pdf/02-hovm-lat/", 
         dpi = 600, width = 297, height = 210, units = "mm")
  ggsave(filename = paste0("stj-", i.ssn, "-m3-rel.pdf"),
         plot = hovm.pfj.lat.m3.rel, device = pdf, path = "05-visu-pdf/02-hovm-lat/", 
         dpi = 600, width = 297, height = 210, units = "mm")
  # Zonalwind
  ggsave(filename = paste0("stj-", i.ssn, "-m2-abs.pdf"),
         plot = hovm.pfj.u.m2.abs, device = pdf, path = "05-visu-pdf/03-hovm-u/", 
         dpi = 600, width = 297, height = 210, units = "mm")
  ggsave(filename = paste0("stj-", i.ssn, "-m3-abs.pdf"),
         plot = hovm.pfj.u.m3.abs, device = pdf, path = "05-visu-pdf/03-hovm-u/", 
         dpi = 600, width = 297, height = 210, units = "mm")
  ggsave(filename = paste0("stj-", i.ssn, "-m2-rel.pdf"),
         plot = hovm.pfj.u.m2.rel, device = pdf, path = "05-visu-pdf/03-hovm-u/", 
         dpi = 600, width = 297, height = 210, units = "mm")
  ggsave(filename = paste0("stj-", i.ssn, "-m3-rel.pdf"),
         plot = hovm.pfj.u.m3.rel, device = pdf, path = "05-visu-pdf/03-hovm-u/", 
         dpi = 600, width = 297, height = 210, units = "mm")
  # Meridionalwind
  ggsave(filename = paste0("stj-", i.ssn, "-m2-abs.pdf"),
         plot = hovm.pfj.v.m2.abs, device = pdf, path = "05-visu-pdf/04-hovm-v/", 
         dpi = 600, width = 297, height = 210, units = "mm")
  ggsave(filename = paste0("stj-", i.ssn, "-m3-abs.pdf"),
         plot = hovm.pfj.v.m3.abs, device = pdf, path = "05-visu-pdf/04-hovm-v/", 
         dpi = 600, width = 297, height = 210, units = "mm")
  ggsave(filename = paste0("stj-", i.ssn, "-m2-rel.pdf"),
         plot = hovm.pfj.v.m2.rel, device = pdf, path = "05-visu-pdf/04-hovm-v/", 
         dpi = 600, width = 297, height = 210, units = "mm")
  ggsave(filename = paste0("stj-", i.ssn, "-m3-rel.pdf"),
         plot = hovm.pfj.v.m3.rel, device = pdf, path = "05-visu-pdf/04-hovm-v/", 
         dpi = 600, width = 297, height = 210, units = "mm")
}


## ENDE ENDE ENDE ####
