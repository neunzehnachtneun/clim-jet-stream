## source("c-prepare_seaice_data.R")
## Aufbereitung der monatlichen Sea Ice Extent Dataset der NOAA
## 
## https://nsidc.org/data/seaice_index/
## ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/north/monthly/data/





## Einlesen der monatsweise abgelegten Daten von FTP-Server ####
## Checken, ob Dateinamen oder Server verändert sind. -> Link siehe oben
##
path = "04-data-nc-csv/"
url <- "ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/north/monthly/data/"
files <- c("N_01_extent_v3.0.csv",
           "N_02_extent_v3.0.csv",
           "N_03_extent_v3.0.csv",
           "N_04_extent_v3.0.csv",
           "N_05_extent_v3.0.csv",
           "N_06_extent_v3.0.csv",
           "N_07_extent_v3.0.csv",
           "N_08_extent_v3.0.csv",
           "N_09_extent_v3.0.csv",
           "N_10_extent_v3.0.csv",
           "N_11_extent_v3.0.csv",
           "N_12_extent_v3.0.csv")

## Schleife über einzelne Monate und Einlesen von FTP-Server
for (i in 1:12) {
  # print(i)
  # Laden des spezifischen Monats
  data.i <- read.csv(file = paste0(url, files[i]))
  # Mergen des Monats mit dem Gesamtdatensatz
  data <- if (i == 1) data.i else merge(x = data, y = data.i, all = TRUE)
}

## Definieren der NA-Werte zur Weiterverarbeitung
data$extent[which(data$extent == -9999.00)] <- NA
data$area[  which(data$area   == -9999.00)] <- NA

## Speichern des Datensatzes als CSV-Datei
write.csv(x = data, file = paste0(path, "c-sea-ice-index-arctic-noaa.csv"))

