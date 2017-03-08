## source('~/01-Master-Thesis/02-code-git/d-read_era_ncdf.r')
##
## ROUTINE ZUM EINLESEN VON ERA-DATEN (ZONAL-WIND) IM NCDF-FORMAT ####
####
####

####
## AUFRUF WICHTIGER BIBLIOTHEKEN UND PAKETE ####
####

read.era.nc <- function(path, file) {

  library(ncdf4)
  library(chron)

  nc <- nc_open(paste(path, file, sep = ""))
  u.era <- ncvar_get(nc, "u") # U-Wind-Komponente
  v.era <- ncvar_get(nc, "v") # V-Wind-Komponente
  
  lon <- ncvar_get(nc, "lon") # Längengrad
  lat <- ncvar_get(nc, "lat") # Breitengrad
  lev <- ncvar_get(nc, "level") # Drucklevel
  date.help <- ncvar_get(nc, "time")
  
  nc_close(nc)
  rm(nc)

#   ## Räumliche Auflösung
#   lat.len <- length(lat)
#   lon.len <- length(lon)
  
  ## Zeitliche Auflösung
  dts = as.POSIXct(date.help*3600, origin = '1900-01-01 00:00', tz = 'UTC')
  dts.month <- months(dts, abbreviate = TRUE)
  dts.year <- years(dts)
  
  ## Betrag der Windgeschwindigkeit
  uv.era <- sqrt( u.era ** 2 + v.era ** 2 )
  
  ## verarbeite Datensätze als liste
  lst <- list(u.era = u.era, v.era = v.era, uv.era = uv.era, lon = lon, lat = lat, lev = lev, dts = dts, dts.month = dts.month, dts.year = dts.year)
  
  ## übergabe d variablen
  return(lst)
}

