## source('~/Master_Thesis/02-r-code-git/e-get-orography.r')
## 
## BERECHNUNG DER GEOPOTENZIELLEN HÖHE DER OROGRAPHIE ####

# Definition einer Funktion zur Ausgabe der Orographie als dataframe
get.orography.e4ei <- function(arg = NA) {

library(ncdf4)

#setwd("~/01-Master-Thesis/02-r-code-git/")

# Einlesen
nc  <- nc_open("04-data-nc/e4ei-t63-orography.nc")
z   <- ncvar_get(nc, "z")
lon <- ncvar_get(nc, "lon")
lat <- ncvar_get(nc, "lat")
nc_close(nc); rm(nc)

# Nordhemisphäre
lat <- lat[49:96]
z   <- z[,49:96]

# Eurozentrisches Gitter
lon.hlp <- lon - 180
lon[1]; lon.hlp[97]; lon <- lon.hlp
z <- z[c(97:192,1:96),]; dimnames(z) <- list(lon, lat)

g0 <- 9.80665
h <- z/g0

# Dataframe
library(reshape2)
df.zh <- melt(z, varnames = c("lon", "lat"), value.name = "z")
df.zh$h <- melt(h, value.name = "h")$h

# Übergabe
return(df.zh)
}

# Test der Funktion
# df.zh <- get.orography.e4ei()
