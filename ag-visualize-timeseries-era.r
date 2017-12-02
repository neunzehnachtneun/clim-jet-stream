## 
## 
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
save.dir <- "05-visu-pdf-tikz/"

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
#library(ggsci) # Farbskala
library(tikzDevice) # Plot für Weiterverarbeitung in Latex
source("f-help-functions.r")



ggp.anom.tropics.u <-
  ggplot(data = tb.uv %>%
           select(lon, lat, dts, u,v) %>%
           filter(lat >= 0 & lat < 23.5) %>%
           group_by(dts) %>%
           summarise_at(.vars = vars(u, v), .funs = funs("mean", mean, mean(., na.rm = TRUE))) %>%
           mutate(u_mean_mean = mean(u_mean, na.rm = TRUE), v_mean_mean = mean(v_mean, na.rm = TRUE)) %>%
           mutate(u_anom = u_mean - u_mean_mean, v_anom = v_mean - v_mean_mean),
         mapping = aes(x = dts, y = u_anom)) +
  geom_line(size = 0.4) +
  geom_vline(xintercept = dts[257], linetype=4) +
  scale_x_datetime(name = "Jahr", 
                   date_breaks = "10 years", date_minor_breaks = "1 year",
                   date_labels = "%Y") + 
  scale_y_continuous(name = "Anomalien der zonalen \nWindkomponente in $m\\,s^{-1}$") +
  theme_bw() + theme(legend.position = "bottom")
print(ggp.anom.tropics.u)

ggp.anom.tropics.v <-
  ggplot(data = tb.uv %>%
           select(lon, lat, dts, u,v) %>%
           filter(lat >= 0 & lat < 23.5) %>%
           group_by(dts) %>%
           summarise_at(.vars = vars(u, v), .funs = funs("mean", mean, mean(., na.rm = TRUE))) %>%
           mutate(u_mean_mean = mean(u_mean, na.rm = TRUE), v_mean_mean = mean(v_mean, na.rm = TRUE)) %>%
           mutate(u_anom = u_mean - u_mean_mean, v_anom = v_mean - v_mean_mean),
         mapping = aes(x = dts, y = v_anom)) +
  geom_line(size = 0.4) +
  geom_vline(xintercept = dts[257], linetype=4) +
  scale_x_datetime(name = "Jahr", 
                   date_breaks = "10 years", date_minor_breaks = "1 year",
                   date_labels = "%Y") + 
  scale_y_continuous(name = "Anomalien der meridionalen \nWindkomponente in $m\\,s^{-1}$") +
  theme_bw() + theme(legend.position = "bottom")
print(ggp.anom.tropics.v)

ggp.anom.midlat.u <-
  ggplot(data = tb.uv %>%
           select(lon, lat, dts, u,v) %>%
           filter(lat >= 23.5 & lat < 66.5) %>%
           group_by(dts) %>%
           summarise_at(.vars = vars(u, v), .funs = funs("mean", mean, mean(., na.rm = TRUE))) %>%
           mutate(u_mean_mean = mean(u_mean, na.rm = TRUE), v_mean_mean = mean(v_mean, na.rm = TRUE)) %>%
           mutate(u_anom = u_mean - u_mean_mean, v_anom = v_mean - v_mean_mean),
         mapping = aes(x = dts, y = u_anom)) +
  geom_line(size = 0.4) +
  geom_vline(xintercept = dts[257], linetype=4) +
  scale_x_datetime(name = "Jahr", 
                   date_breaks = "10 years", date_minor_breaks = "1 year",
                   date_labels = "%Y") + 
  scale_y_continuous(name = "Anomalien der zonalen \nWindkomponente in $m\\,s^{-1}$") +
  theme_bw() + theme(legend.position = "bottom")
print(ggp.anom.midlat.u)

ggp.anom.midlat.v <-
  ggplot(data = tb.uv %>%
           select(lon, lat, dts, u,v) %>%
           filter(lat >= 23.5 & lat < 66.5) %>%
           group_by(dts) %>%
           summarise_at(.vars = vars(u, v), .funs = funs("mean", mean, mean(., na.rm = TRUE))) %>%
           mutate(u_mean_mean = mean(u_mean, na.rm = TRUE), v_mean_mean = mean(v_mean, na.rm = TRUE)) %>%
           mutate(u_anom = u_mean - u_mean_mean, v_anom = v_mean - v_mean_mean),
         mapping = aes(x = dts, y = v_anom)) +
  geom_line(size = 0.4) +
  geom_vline(xintercept = dts[257], linetype=4) +
  scale_x_datetime(name = "Jahr", 
                   date_breaks = "10 years", date_minor_breaks = "1 year",
                   date_labels = "%Y") + 
  scale_y_continuous(name = "Anomalien der meridionalen \nWindkomponente in $m\\,s^{-1}$") +
  theme_bw() + theme(legend.position = "bottom")
print(ggp.anom.midlat.v)

ggp.anom.polar.u <-
  ggplot(data = tb.uv %>%
           select(lon, lat, dts, u,v) %>%
           filter(lat >= 66.5 & lat < 90) %>%
           group_by(dts) %>%
           summarise_at(.vars = vars(u, v), .funs = funs("mean", mean, mean(., na.rm = TRUE))) %>%
           mutate(u_mean_mean = mean(u_mean, na.rm = TRUE), v_mean_mean = mean(v_mean, na.rm = TRUE)) %>%
           mutate(u_anom = u_mean - u_mean_mean, v_anom = v_mean - v_mean_mean),
         mapping = aes(x = dts, y = u_anom)) +
  geom_line(size = 0.4) +
  geom_vline(xintercept = dts[257], linetype = 4) +
  scale_x_datetime(name = "Jahr", 
                   date_breaks = "10 years", date_minor_breaks = "1 year",
                   date_labels = "%Y") + 
  scale_y_continuous(name = "Anomalien der zonalen \nWindkomponente in $m\\,s^{-1}$") +
  theme_bw() + theme(legend.position = "bottom")
print(ggp.anom.polar.u)

ggp.anom.polar.v <-
  ggplot(data = tb.uv %>%
           select(lon, lat, dts, u,v) %>%
           filter(lat >= 66.5 & lat < 90) %>%
           group_by(dts) %>%
           summarise_at(.vars = vars(u, v), .funs = funs("mean", mean, mean(., na.rm = TRUE))) %>%
           mutate(u_mean_mean = mean(u_mean, na.rm = TRUE), v_mean_mean = mean(v_mean, na.rm = TRUE)) %>%
           mutate(u_anom = u_mean - u_mean_mean, v_anom = v_mean - v_mean_mean),
         mapping = aes(x = dts, y = v_anom)) +
  geom_line(size = 0.4) +
  geom_vline(xintercept = dts[257], linetype = 4) +
  scale_x_datetime(name = "Jahr", 
                   date_breaks = "10 years", date_minor_breaks = "1 year",
                   date_labels = "%Y") + 
  scale_y_continuous(name = "Anomalien der meridionalen \nWindkomponente in $m\\,s^{-1}$") +
  theme_bw() + theme(legend.position = "bottom")
print(ggp.anom.polar.v)


plt.save(plt = ggp.anom.tropics.u, 
         width = 135, height = 55, pointsize = 9,
         filepath = paste0(save.dir, "09-timeseries-era/"), 
         filename = "tropics-u")
plt.save(plt = ggp.anom.tropics.v, 
         width = 135, height = 55, pointsize = 9,
         filepath = paste0(save.dir, "09-timeseries-era/"), 
         filename = "tropics-v")


plt.save(plt = ggp.anom.midlat.u, 
         width = 135, height = 55, pointsize = 9,
         filepath = paste0(save.dir, "09-timeseries-era/"), 
         filename = "midlat-u")
plt.save(plt = ggp.anom.midlat.v, 
         width = 135, height = 55, pointsize = 9,
         filepath = paste0(save.dir, "09-timeseries-era/"), 
         filename = "midlat-v")

plt.save(plt = ggp.anom.polar.u, 
         width = 135, height = 55, pointsize = 9,
         filepath = paste0(save.dir, "09-timeseries-era/"), 
         filename = "polar-u")
plt.save(plt = ggp.anom.polar.v, 
         width = 135, height = 55, pointsize = 9,
         filepath = paste0(save.dir, "09-timeseries-era/"), 
         filename = "polar-v")

