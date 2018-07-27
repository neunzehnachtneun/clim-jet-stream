#' ==========================================================================================================
#' EINLESEROUTINE FÜR NCDF-DATEN DER ERA40- UND ERA-INTERIM-DATENSÄTZE                                     ==
#' EINGELESEN WERDEN DER ZONALWIND U, DER MERIDIONALWIND V, DAS GEOPOTENZIAL Z SOWIE                       ==
#' DIE ACHSEN DES DATENSATZES, ALSO LÄNGEN- UND BREITENGRAD UND ZEITACHSE                                  ==
#' DIE ROHDATEN LIEGEN AUF EINEM PAZIFIKZENTRISCHEN GITTER VOR,                                            ==
#' DIESES WIRD AUF EIN EUROPAZENTRISCHES TRANSFORMIERT                                                     ==
#' ==========================================================================================================

## =============================================================================
## Einlesen der ERA-Interim-Datensätze =========================================
## =============================================================================

timestep <- seq(time_begin[i], time_end[i])

#' Einlesen der Daten mit ncdf4-Routinen
nc <- nc_open("../../a-e4ei-1deg-1958-2017-uv.nc")
u <- ncvar_get(nc, "eastward_wind")[,,,timestep]
v <- ncvar_get(nc, "northward_wind")[,,,timestep]
lon <- ncvar_get(nc, "longitude")
lat <- ncvar_get(nc, "latitude")
lev <- ncvar_get(nc, "air_pressure")
tm_help <- ncvar_get(nc, "time")[timestep]
nc_close(nc); rm(nc)

#' Umschreiben der Zeitachse in Posix-Format
dts <- as_date(tm_help / 24, origin = "1900-01-01 UTC", tz = "UTC")
dts_month <- as.character(month(dts, label = TRUE))
dts_year <- year(dts)


## =============================================================================
## Eckdaten der Datensätze =====================================================
## =============================================================================


#' Auflösungen des Datensatzes
#' Räumliche Auflösung
n_lon <- dim(u)[1]
n_lat <- dim(u)[2]
n_lev <- dim(u)[3]
#' Zeitliche Auflösung
n_dts <- dim(u)[4]


#' Indizes der Matrizen für spätere Verarbeitung
dimnames(u) <- list(lon, lat, lev, dts)
dimnames(v) <- list(lon, lat, lev, dts)
# dimnames(z) <- list(lon, lat, lev, dts)


 
## =============================================================================
## Definieren von kalten/warmen Jahreszeiten nach Molnos et al. ================
## =============================================================================

#' Vektor mit Markern für warme und kalte Monate
#' Notwendig für Dijkstra-Methode
dts_cold_warm <- rep(NA, length.out = length(dts))
dts_cold_warm[which(dts_month == "Nov" | dts_month == "Dec" | 
                      dts_month == "Jan" | dts_month == "Feb" | 
                      dts_month == "Mar" | dts_month == "Apr")] <- "cold"
dts_cold_warm[which(dts_month == "May" | dts_month == "Jun" | 
                      dts_month == "Jul" | dts_month == "Aug" | 
                      dts_month == "Sep" | dts_month == "Oct")] <- "warm"

#' Unterscheidung von Jahreszeiten
dts_season <- rep(NA, length.out = length(dts))
dts_season[which(dts_month == "Dec" | 
                   dts_month == "Jan" | dts_month == "Feb")] <- "djf"
dts_season[which(dts_month == "Mar" | 
                   dts_month == "Apr" | dts_month == "May")] <- "mam"
dts_season[which(dts_month == "Jun" | 
                   dts_month == "Jul" | dts_month == "Aug")] <- "jja"
dts_season[which(dts_month == "Sep" | 
                   dts_month == "Oct" | dts_month == "Nov")] <- "son"

## =============================================================================
## Festlegen des zu untersuchenden Druckniveaus ================================
## =============================================================================
pressure_level <- which(lev == geopotential)

## =============================================================================
## Berechnen des massengewichteten Mittels für u und v für Dijkstra-Methode ====
## =============================================================================

lev_mask_e4 <- c(3,5,7,8,10,12)
lev_mask_ei <- c(3:12)

## Zonalwindkomponente
u_molten <- as_tibble(melt(u, varnames = c("lon", "lat", "lev", "dts"), value.name = "u"))
u_average <- u_molten %>%
  mutate(dts = as_date(dts, origin = "1970-01-01 UTC")) %>%
  filter(lev >= 150, lev <= 500) %>%
  group_by(lon, lat, dts) %>%
  mutate(press_u = lev * u) %>%
  summarise_at(.vars = vars(press_u), .funs = funs("sum", sum, sum(., na.rm = TRUE)))

u_average$sum[which(year(u_average$dts) <  1979)] <- 
  u_average$sum[which(year(u_average$dts) <  1979)] / sum(lev[lev_mask_e4])
u_average$sum[which(year(u_average$dts) >= 1979)] <- 
  u_average$sum[which(year(u_average$dts) >= 1979)] / sum(lev[lev_mask_ei])

# print(u_average)
u_cast <- acast(u_average, lon ~ lat ~ dts)

## Meridionalwindkomponente
v_molten <- as_tibble(melt(v, varnames = c("lon", "lat", "lev", "dts"), value.name = "v"))
v_average <- v_molten %>%
  mutate(dts = as_date(dts, origin = "1970-01-01 UTC")) %>%
  filter(lev >= 150, lev <= 500) %>%
  group_by(lon, lat, dts) %>%
  mutate(press_v = lev * v) %>%
  summarise_at(.vars = vars(press_v), .funs = funs("sum", sum, sum(., na.rm = TRUE)))

v_average$sum[which(year(v_average$dts) <  1979)] <- 
  v_average$sum[which(year(v_average$dts) <  1979)] / sum(lev[lev_mask_e4])
v_average$sum[which(year(v_average$dts) >= 1979)] <- 
  v_average$sum[which(year(v_average$dts) >= 1979)] / sum(lev[lev_mask_ei])

print(v_average)
v_cast <- acast(v_average, lon ~ lat ~ dts)

