## source('~/Master_Thesis/02-r-code-git/d-analyse_seasonal_change.r')
## 
## BERECHNUNG VON GLEITENDEM MITTEL U SD ####
## ÜBER FÜNF JAHRE U SAISONAL              ## 
####
####

####
## AUFRUF WICHTIGER BIBLIOTHEKEN UND PAKETE ####
####

library(RColorBrewer)
library(fields)
library(ncdf4)

setwd("~/01-Master-Thesis/02-r-code-git/")
path <- "03-data-nc/"
filename <- "e4ei-t63-geopotential.nc"


nc <- nc_open(paste(path, filename, sep = ""))
h <- ncvar_get(nc, "z")
nc_close(nc)


g0 <- 9.80665
z <- h/g0