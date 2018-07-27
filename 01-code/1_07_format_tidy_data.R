
## LADEN DES SEA ICE DATENSATZTES ####
##
df_sea_ice <- read.csv("04-data-nc-csv/d-sea-ice-index-arctic-noaa.csv")
df_sea_ice$seaice_extent <- df_sea_ice$extent
df_sea_ice$seaice_area   <- df_sea_ice$area

df_sea_ice$dts_sii <- as.Date(paste0(df_sea_ice$year,"-",df_sea_ice$mo,"-1"))
df_sea_ice$year <- year(df_sea_ice$dts_sii)
df_sea_ice$month <- as.character(month(df_sea_ice$dts_sii, label = TRUE, locale = "en_GB.UTF-8"))

names(df_sea_ice)
df_sea_ice <- df_sea_ice[c(2, 11, 8, 9, 10)]
df_sea_ice$seaice_area[which(df_sea_ice$area < 0)] <- NA
df_sea_ice$seaice_extent[which(df_sea_ice$extent < 0)] <- NA
names(df_sea_ice)

tb_sea_ice <- as_tibble(df_sea_ice)

## FORMATIEREN DER DATENSATZE ####
## 


## Globales Maximum M1
# Maximaljet
m1_J_lat    <- sapply(m1, "[[", "MaxJ_lat"); colnames(m1_J_lat)  <- dts; rownames(m1_J_lat) <- lon;
# maximaler Chebyshev-Jet
m2b_J_lat <- sapply(m2, "[[", "MaxJ_lat");   colnames(m2b_J_lat) <- dts; rownames(m2b_J_lat) <- lon;

## Chebyshev
## Polarfrontjet
m2c_PFJ_lat <- sapply(m2, "[[", "PFJ_lat"); colnames(m2c_PFJ_lat) <- dts; rownames(m2c_PFJ_lat) <- lon;
m2c_PFJ_u   <- sapply(m2, "[[", "PFJ_u");   colnames(m2c_PFJ_u)   <- dts; rownames(m2c_PFJ_u)   <- lon;
m2c_PFJ_v   <- sapply(m2, "[[", "PFJ_v");   colnames(m2c_PFJ_v)   <- dts; rownames(m2c_PFJ_v)   <- lon;
m2c_PFJ_z   <- sapply(m2, "[[", "PFJ_z");   colnames(m2c_PFJ_z)   <- dts; rownames(m2c_PFJ_z)   <- lon;
## Subtropenjet
m2c_STJ_lat <- sapply(m2, "[[", "STJ_lat"); colnames(m2c_STJ_lat) <- dts; rownames(m2c_STJ_lat) <- lon;
m2c_STJ_u   <- sapply(m2, "[[", "STJ_u");   colnames(m2c_STJ_u)   <- dts; rownames(m2c_STJ_u)   <- lon;
m2c_STJ_v   <- sapply(m2, "[[", "STJ_v");   colnames(m2c_STJ_v)   <- dts; rownames(m2c_STJ_v)   <- lon;
m2c_STJ_z   <- sapply(m2, "[[", "STJ_z");   colnames(m2c_STJ_z)   <- dts; rownames(m2c_STJ_z)   <- lon;
# ## Single-Jetstream
# m2c_SJS_lat <- sapply(m2, "[[", "SJS_lat"); colnames(m2c_SJS_lat) <- dts; rownames(m2c_SJS_lat) <- lon;
# m2c_SJS_u   <- sapply(m2, "[[", "SJS_u");   colnames(m2c_SJS_u)   <- dts; rownames(m2c_SJS_u)   <- lon;
# m2c_SJS_v   <- sapply(m2, "[[", "SJS_v");   colnames(m2c_SJS_v)   <- dts; rownames(m2c_SJS_v)   <- lon;
# m2c_SJS_z   <- sapply(m2, "[[", "SJS_z");   colnames(m2c_SJS_z)   <- dts; rownames(m2c_SJS_z)   <- lon;

## Dijkstra
## Polarfrontjet
m3_PFJ_lat  <- sapply(m3, "[[", "PFJ_lat"); colnames(m3_PFJ_lat) <- dts; rownames(m3_PFJ_lat) <- lon;
m3_PFJ_u    <- sapply(m3, "[[", "PFJ_u");   colnames(m3_PFJ_u)   <- dts; rownames(m3_PFJ_u)   <- lon;
m3_PFJ_v    <- sapply(m3, "[[", "PFJ_v");   colnames(m3_PFJ_v)   <- dts; rownames(m3_PFJ_v)   <- lon;
## Subtropenjet
m3_STJ_lat  <- sapply(m3, "[[", "STJ_lat"); colnames(m3_STJ_lat) <- dts; rownames(m3_STJ_lat) <- lon;
m3_STJ_u    <- sapply(m3, "[[", "STJ_u");   colnames(m3_STJ_u)   <- dts; rownames(m3_STJ_u)   <- lon;
m3_STJ_v    <- sapply(m3, "[[", "STJ_v");   colnames(m3_STJ_v)   <- dts; rownames(m3_STJ_v)   <- lon;
# ## Single-Jetstream
# m3_SJS_lat  <- sapply(m3, "[[", "SJS_lat"); colnames(m3_SJS_lat) <- dts; rownames(m3_SJS_lat) <- lon;
# m3_SJS_u    <- sapply(m3, "[[", "SJS_u");   colnames(m3_SJS_u)   <- dts; rownames(m3_SJS_u)   <- lon;
# m3_SJS_v    <- sapply(m3, "[[", "SJS_v");   colnames(m3_SJS_v)   <- dts; rownames(m3_SJS_v)   <- lon;

## MELTEN DER DATENSÄTZE (RESHAPE2::MELT) FÜR GGPLOT2 ####
## Maximaljet max()
df_jets_month <- melt(m1_J_lat, varnames = c("lon", "dts"),  value.name = "m1_maxj_lat" )
df_jets_month$dts          <- rep(dts, each = n_lon)          # Datum
df_jets_month$year         <- rep(dts_year, each = n_lon)     # Jahr
df_jets_month$month        <- rep(dts_month, each = n_lon)    # Monat
df_jets_month$season       <- rep(dts_season, each = n_lon)   # Jahreszeit/Saison

# Umsortieren der Spalten des Datensatzes
df_jets_month <- df_jets_month[,c("dts", "year", "month", "season", "lon", "m1_maxj_lat")]
df_jets_month$m1_maxj_u  <- NA
df_jets_month$m1_maxj_v  <- NA
df_jets_month$m1_maxj_uv <- NA
# df_jets_month_m1_mj$m1_maxj_z  <- NA

## Maximaljet Chebyshev
df_jets_month$m2_maxj_lat <- 
  melt(m2b_J_lat, varnames = c("lon", "dts"),value.name = "m2_maxj_lat")$m2_maxj_lat
df_jets_month$m2_maxj_u  <- NA
df_jets_month$m2_maxj_v  <- NA
df_jets_month$m2_maxj_uv <- NA
# df_jets_month$m2_maxj_z  <- NA

## Polarfrontjet || Chebyshev- Methode 
df_jets_month$m2_pfj_lat <- 
  melt(m2c_PFJ_lat, varnames = c("lon", "dts"), value.name = "m2_pfj_lat")$m2_pfj_lat
df_jets_month$m2_pfj_u  <- melt(m2c_PFJ_u)$value
df_jets_month$m2_pfj_v  <- melt(m2c_PFJ_v)$value
df_jets_month$m2_pfj_uv <- 
  sqrt(df_jets_month$m2_pfj_u**2 + df_jets_month$m2_pfj_v**2)
# df_jets_month$m2_pfj_z  <- melt(m2c_PFJ_z)$value

## Subtropenjet || Chebyshev- Methode 
df_jets_month$m2_stj_lat <- 
  melt(m2c_STJ_lat, varnames = c("lon", "dts"),value.name = "m2_stj_lat")$m2_stj_lat
df_jets_month$m2_stj_u  <- melt(m2c_STJ_u)$value
df_jets_month$m2_stj_v  <- melt(m2c_STJ_v)$value
df_jets_month$m2_stj_uv <- 
  sqrt(df_jets_month$m2_stj_u**2 + df_jets_month$m2_stj_v**2)
# df_jets_month$m2_stj_z  <- melt(m2c_STJ_z)$value

## Polarfrontjet || Dijkstra- Methode 
df_jets_month$m3_pfj_lat <- 
  melt(m3_PFJ_lat, varnames = c("lon", "dts"), value.name = "m3_pfj_lat")$m3_pfj_lat
df_jets_month$m3_pfj_u  <- melt(m3_PFJ_u)$value
df_jets_month$m3_pfj_v  <- melt(m3_PFJ_v)$value
df_jets_month$m3_pfj_uv <- 
  sqrt(df_jets_month$m3_pfj_u**2 + df_jets_month$m3_pfj_v**2)
# df_jets_month$m3_pfj_z  <- NA

## Subtropenjet || Dijkstra- Methode 
df_jets_month$m3_stj_lat <- 
  melt(m3_STJ_lat, varnames = c("lon", "dts"), value.name = "m3_stj_lat")$m3_stj_lat
df_jets_month$m3_stj_u  <- melt(m3_STJ_u)$value
df_jets_month$m3_stj_v  <- melt(m3_STJ_v)$value
df_jets_month$m3_stj_uv <- 
  sqrt(df_jets_month$m3_stj_u**2 + df_jets_month$m3_stj_v**2)
# df_jets_month$m3_stj_z  <- NA


## MERGEN DER DATAFRAMES ####
## 
df_jets_month$year <- as.integer(as.character(df_jets_month$year))

## Mergen des Jet-Dataframes mit dem Seeeis-Dataframe
df_jets_month <- merge(df_jets_month, df_sea_ice, 
                       by.x = c("year", "month"), 
                       by.y = c("year", "month"), 
                       all.x = TRUE)
# Sortieren nach Datum und Längengrad
df_jets_month <- df_jets_month[order(df_jets_month$dts, df_jets_month$lon),]


# ## U-V-Datensatz ####
# # Melten
# df_uv <- melt(u[,,pressure_level,], varnames = c("lon", "lat", "dts"), value.name = "u")
# df_uv$dts <- rep(dts, each = n_lon*n_lat)
# df_uv$v <- melt(data = v[,,pressure_level,])$value
# # df_uv$uv <- sqrt( df_uv$u ** 2 + df_uv$v ** 2 )
# # df_uv$z <- melt(data = z[,,pressure_level,])$value


## DATAFRAMES ALS TIBBLES ####
## 
tb_jets_month <- as_tibble(df_jets_month)
tb_uv <- as_tibble(df_uv)


## Sonstige Formatierungen ####
## 
tb_jets_month$year <- as.integer(as.character(tb_jets_month$year))
tb_jets_month$season <- factor(tb_jets_month$season, levels = c("djf", "mam", "jja", "son"),
                               labels = c("DJF", "MAM", "JJA", "SON"))


## LÖSCHEN UNNÖTIGER VARIABLEN ####
## 

rm(list = setdiff(ls(), list("tb_jets_month", "tb_uv", "tb_sea_ice", 
                             "dts", "dts_cold_warm", "dts_month", "dts_season", "dts_year", 
                             "get.orography.df", "get.orography.ll", 
                             "i.stp", "lat", "lev", "lon", "n_lat", "n_lon", "n.seas", "n.years", "pressure_level")))
ls()
