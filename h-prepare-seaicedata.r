## source("h-prepare-seaicedata.r")
## Aufbereitung der monatlichen Sea Ice Extent Dataset der NOAA
#### 
## 

## WORKING DIRECTORY & FIRST THINGS FIRST ####
##
setwd("~/01-Master-Thesis/02-code-git/")

## Einlesen der monatsweise abgelegten Daten ####
##
path = "04-data-nc/"
files = list.files(path = path, pattern = "N_")

# Schleife über Monate
for (i in 1:12) {
  # print(i)
  # Laden des spezifischen Monats
  data.i <- read.csv(file = paste0(path, files[i]))
  # Mergen des Monats mit dem Gesamtdatensatz
  data <- if (i == 1) data.i else merge(x = data, y = data.i, all = TRUE)
}

write.csv(x = data, file = paste0(path, "c-sea-ice-index-arctic-noaa.csv"))

