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
library(ggplot2) # Grundpaket
# library(RColorBrewer)
library(ggsci) # Farbskala
# library(maps)
# library(gridExtra)
# library(egg)

## Schriftarten
library(extrafont)
fonts()
fonttable()
loadfonts(device = "postscript")

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

for (t.stp in round(seq(1,length(dts), length.out = 6))) {
  print(t.stp)
  
  # Plot des zonalen Windfeldes und der Position aller gefundenen Chebyshev-Jets
  ggp.nh.m2a <-
    ggplot(data = df.uv[which(df.uv$t.stp == dts[t.stp]),],
           mapping = aes(x = lon, y = lat, fill = u)) +
    geom_tile() + scale_fill_gsea() + #scale_fill_distiller(palette = 'RdYlBu') +
    geom_point(mapping = aes(x = lon , y = J.lat.m2a.a, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 20, fill = "black", size = 1) +
    geom_point(mapping = aes(x = lon , y = J.lat.m2a.b, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 20, fill = "black", size = 1) +
    geom_point(mapping = aes(x = lon , y = J.lat.m2a.c, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 20, fill = "black", size = 1) +
    geom_point(mapping = aes(x = lon , y = J.lat.m2a.d, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 20, fill = "black", size = 1) +
    scale_x_continuous(name = "Längengrad",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Breitengrad",
                       breaks = c(0, 30, 60, 90)) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group),
                 data = map_nh, fill = "gray50", alpha = 0.35) +
    ggtitle("Zonales Windfeld und Position aller gefundenen Chebyshev-Maxima", 
            subtitle = paste0(dts.year[t.stp], "-", dts.month[t.stp])) +
    coord_fixed(xlim = c(-180,180), ylim = c(0,90)) + theme_classic(base_family = "Droid Serif")

  
  # Plot des zonalen Windfeldes und der Position des maximalen Jets sowie des maximalen Chebyshev-Jets
  ggp.nh.m1.m2b <-
    ggplot(data = df.uv[which(df.uv$t.stp == dts[t.stp]),],
           mapping = aes(x = lon, y = lat, fill = u)) +
    geom_tile() + scale_fill_gsea() + #scale_fill_distiller(palette = 'RdYlBu') +
    geom_point(mapping = aes(x = lon , y = J.lat.m1, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 16, fill = "black", size = 1, show.legend = TRUE) +
    geom_point(mapping = aes(x = lon , y = J.lat.m2b, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 18, fill = "black", size = 1.5, show.legend = TRUE) +
    scale_x_continuous(name = "Längengrad",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Breitengrad",
                       breaks = c(0, 30, 60, 90)) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group),
                 data = map_nh, fill = "gray50", alpha = 0.35) +
    ggtitle("Zonales Windfeld und Position der meridionalen Zonalwindmaxima und des absoluten Chebyshev-Maximums", 
            subtitle = paste0(dts.year[t.stp], "-", dts.month[t.stp])) +
    coord_fixed(xlim = c(-180,180), ylim = c(0,90)) + theme_classic(base_family = "Droid Serif")
  
  # Plot des zonalen Windfeldes und der zwei stärksten Chebyshev-Maxima im Bereich [20,85]
  ggp.nh.m2c <-
    ggplot(data = df.uv[which(df.uv$t.stp == dts[t.stp]),],
           mapping = aes(x = lon, y = lat, fill = u)) +
    geom_tile() + scale_fill_gsea() + #scale_fill_distiller(palette = 'RdYlBu') +
    geom_point(mapping = aes(x = lon , y = PFJ.lat.m2c, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 24, fill = "black", size = 1) +
    geom_point(mapping = aes(x = lon , y = STJ.lat.m2c, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 25, fill = "black", size = 1) +
    scale_x_continuous(name = "Längengrad",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Breitengrad",
                       breaks = c(0, 30, 60, 90)) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group),
                 data = map_nh, fill = "gray50", alpha = 0.35) +
    ggtitle("Zonales Windfeld und Position der zwei stärksten Chebyshev-Maxima", 
            subtitle = paste0(dts.year[t.stp], "-", dts.month[t.stp])) +
    coord_fixed(xlim = c(-180,180), ylim = c(0,90)) + theme_classic(base_family = "Droid Serif")
  
  # Plot des Betrags des horizontalen Windfeldes und Dijkstra-Jets
  ggp.nh.m3 <-
    ggplot(data = df.uv[which(df.uv$t.stp == dts[t.stp]),],
           mapping = aes(x = lon, y = lat, fill = uv)) +
    geom_tile() + scale_fill_gsea() + #scale_fill_distiller(palette = 'RdYlBu') +
    geom_point(mapping = aes(x = lon , y = PFJ.lat.m3, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 24, fill = "black", size = 1) +
    geom_point(mapping = aes(x = lon , y = STJ.lat.m3, fill = NULL),
               data = df.jets.month[which(df.jets.month$dts == dts[t.stp]),],
               shape = 25, fill = "black", size = 1) +
    scale_x_continuous(name = "Längengrad",
                       breaks = c(-180, -135, -90, -45, 0, 45, 90, 135, 180)) +
    scale_y_continuous(name = "Breitengrad",
                       breaks = c(0, 30, 60, 90)) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group),
                 data = map_nh, fill = "gray50", alpha = 0.35) +
    ggtitle("Betrag des horizontalen Windfeldes und Position des Dijkstra-Jets", 
            subtitle = paste0(dts.year[t.stp], "-", dts.month[t.stp])) +
    coord_fixed(xlim = c(-180,180), ylim = c(0,90)) + theme_classic(base_family = "Droid Serif")
  
  ## Speichern der Plots als pdfs
  # ggp.jets <- grid.arrange(ggp.u.m0, ggp.u.m1a, ggp.u.m1b, ggp.u.m1c, ggp.uv.m2, ncol = 1)
  ggsave(filename = paste0('case-', dts.year[t.stp], "-", dts.month[t.stp], "-m1-m2b.pdf"),
         plot = ggp.nh.m1.m2b, device = pdf, path = "05-visu-pdf/01-case/",
         dpi = 600, width = 297, height = 210, units = "mm")
  ggsave(filename = paste0('case-',dts.year[t.stp], "-", dts.month[t.stp], "-m2a.pdf"),
         plot = ggp.nh.m2a, device = pdf, path = "05-visu-pdf/01-case/", 
         dpi = 600, width = 297, height = 210, units = "mm")
  ggsave(filename = paste0('case-',dts.year[t.stp], "-", dts.month[t.stp], "-m2c.pdf"),
         plot = ggp.nh.m2c, device = pdf, path = "05-visu-pdf/01-case/", 
         dpi = 600, width = 297, height = 210, units = "mm")
  ggsave(filename = paste0('case-',dts.year[t.stp], "-", dts.month[t.stp], "-m3.pdf"),
         plot = ggp.nh.m3, device = pdf, path = "05-visu-pdf/01-case/", 
         dpi = 600, width = 297, height = 210, units = "mm")
}


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


## VERGLEICH CHEBYSHEV VS DIJKSTRA ####
## 

ggplot(data = df.jets.season, mapping = aes(x = PFJ.lat.m2c, y = PFJ.lat.m3)) + 
  stat_ellipse(type = "norm", level = 0.90) + 
  stat_ellipse(type = "norm", level = 0.95) + 
  stat_ellipse(type = "norm", level = 0.99) +
  scale_x_continuous(name = "Positionen Chebyshev") + #+ geom_point()
  scale_y_continuous(name = "Positionen Dijkstra")

ggplot(data = df.jets.season, mapping = aes(x = STJ.lat.m2c, y = STJ.lat.m3)) + 
  stat_ellipse(type = "norm", level = 0.90) + 
  stat_ellipse(type = "norm", level = 0.95) + 
  stat_ellipse(type = "norm", level = 0.99) +
  scale_x_continuous(name = "Positionen Chebyshev") + #+ geom_point()
  scale_y_continuous(name = "Positionen Dijkstra")


ggplot(data = df.jets.season, mapping = aes(x = PFJ.u.m2c, y = PFJ.u.m3)) + 
  stat_ellipse(type = "norm", level = 0.90) + 
  stat_ellipse(type = "norm", level = 0.95) + 
  stat_ellipse(type = "norm", level = 0.99) #+ geom_point()

ggplot(data = df.jets.season, mapping = aes(x = STJ.u.m2c, y = STJ.u.m3)) + 
  stat_ellipse(type = "norm", level = 0.90) + 
  stat_ellipse(type = "norm", level = 0.95) + 
  stat_ellipse(type = "norm", level = 0.99) #+ geom_point()

ggplot(data = df.jets.season, mapping = aes(x = PFJ.v.m2c, y = PFJ.v.m3)) + 
  stat_ellipse(type = "norm", level = 0.90) + 
  stat_ellipse(type = "norm", level = 0.95) + 
  stat_ellipse(type = "norm", level = 0.99) #+ geom_point()

ggplot(data = df.jets.season, mapping = aes(x = STJ.v.m2c, y = STJ.v.m3)) + 
  stat_ellipse(type = "norm", level = 0.90) + 
  stat_ellipse(type = "norm", level = 0.95) + 
  stat_ellipse(type = "norm", level = 0.99) +
  geom_abline(slope = 1, intercept = 0) +
  geom_vline(xintercept = mean(df.jets.season$STJ.v.m2c)) +
  geom_hline(yintercept = mean(df.jets.season$STJ.v.m3)) +
  geom_point(data = df.jets.season, mapping = aes(x = mean(STJ.v.m2c), y = mean(STJ.v.m3))) 
  
#+ geom_point()

## ENDE ENDE ENDE ####
