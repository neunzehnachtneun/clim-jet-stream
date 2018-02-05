
## LADEN DES SEA ICE DATENSATZTES ####
##
df_sea_ice <- read.csv("04-data-nc-csv/d-sea-ice-index-arctic-noaa.csv")

df_sea_ice$dts <- as.Date(paste0(df_sea_ice$year,"-",df_sea_ice$mo,"-1"))
df_sea_ice$year <- year(df_sea_ice$dts)
df_sea_ice$month <- as.character(month(df_sea_ice$dts, label = TRUE))

names(df_sea_ice)
df_sea_ice <- df_sea_ice[c(2, 9, 7, 6)]
df_sea_ice$area[which(df_sea_ice$area < 0)] <- NA
df_sea_ice$extent[which(df_sea_ice$extent < 0)] <- NA
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
## Single-Jetstream
m2c_SJS_lat <- sapply(m2, "[[", "SJS_lat"); colnames(m2c_SJS_lat) <- dts; rownames(m2c_SJS_lat) <- lon;
m2c_SJS_u   <- sapply(m2, "[[", "SJS_u");   colnames(m2c_SJS_u)   <- dts; rownames(m2c_SJS_u)   <- lon;
m2c_SJS_v   <- sapply(m2, "[[", "SJS_v");   colnames(m2c_SJS_v)   <- dts; rownames(m2c_SJS_v)   <- lon;
m2c_SJS_z   <- sapply(m2, "[[", "SJS_z");   colnames(m2c_SJS_z)   <- dts; rownames(m2c_SJS_z)   <- lon;

## Dijkstra
## Polarfrontjet
m3_PFJ_lat  <- sapply(m3, "[[", "PFJ_lat"); colnames(m3_PFJ_lat) <- dts; rownames(m3_PFJ_lat) <- lon;
m3_PFJ_u    <- sapply(m3, "[[", "PFJ_u");   colnames(m3_PFJ_u)   <- dts; rownames(m3_PFJ_u)   <- lon;
m3_PFJ_v    <- sapply(m3, "[[", "PFJ_v");   colnames(m3_PFJ_v)   <- dts; rownames(m3_PFJ_v)   <- lon;
## Subtropenjet
m3_STJ_lat  <- sapply(m3, "[[", "STJ_lat"); colnames(m3_STJ_lat) <- dts; rownames(m3_STJ_lat) <- lon;
m3_STJ_u    <- sapply(m3, "[[", "STJ_u");   colnames(m3_STJ_u)   <- dts; rownames(m3_STJ_u)   <- lon;
m3_STJ_v    <- sapply(m3, "[[", "STJ_v");   colnames(m3_STJ_v)   <- dts; rownames(m3_STJ_v)   <- lon;
## Single-Jetstream
m3_SJS_lat  <- sapply(m3, "[[", "SJS_lat"); colnames(m3_SJS_lat) <- dts; rownames(m3_SJS_lat) <- lon;
m3_SJS_u    <- sapply(m3, "[[", "SJS_u");   colnames(m3_SJS_u)   <- dts; rownames(m3_SJS_u)   <- lon;
m3_SJS_v    <- sapply(m3, "[[", "SJS_v");   colnames(m3_SJS_v)   <- dts; rownames(m3_SJS_v)   <- lon;

## MELTEN DER DATENSÄTZE (RESHAPE2::MELT) FÜR GGPLOT2 ####
## Maximaljet max()
df_jets_month_m1_mj <- melt(m1_J_lat, varnames = c("lon", "dts"),  value.name = "lat" )
df_jets_month_m1_mj$dts          <- rep(dts, each = n_lon)          # Datum
df_jets_month_m1_mj$year         <- rep(dts_year, each = n_lon)     # Jahr
df_jets_month_m1_mj$month        <- rep(dts_month, each = n_lon)    # Monat
df_jets_month_m1_mj$season       <- rep(dts_season, each = n_lon)   # Jahreszeit/Saison
## Methode und Klassifizierung
df_jets_month_m1_mj$method <- "Max"
df_jets_month_m1_mj$class <- "MJ"
# Umsortieren der Spalten des Datensatzes
df_jets_month_m1_mj <- df_jets_month_m1_mj[,c("dts", "year", "month", "season", "method", "class", "lon", "lat")]
# Einfügen der u, v, z -Reihen
df_jets_month_m1_mj$u  <- NA
df_jets_month_m1_mj$v  <- NA
df_jets_month_m1_mj$uv <- NA
df_jets_month_m1_mj$z  <- NA


## Maximaljet Chebyshev
df_jets_month_m2_mj <- melt(m2b_J_lat, varnames = c("lon", "dts"),value.name = "lat" )
df_jets_month_m2_mj$dts          <- rep(dts, each = n_lon)          # Datum
df_jets_month_m2_mj$year         <- rep(dts_year, each = n_lon)     # Jahr
df_jets_month_m2_mj$month        <- rep(dts_month, each = n_lon)    # Monat
df_jets_month_m2_mj$season       <- rep(dts_season, each = n_lon)   # Jahreszeit/Saison
## Methode und Klassifizierung
df_jets_month_m2_mj$method <- "Chebyshev"
df_jets_month_m2_mj$class <- "MJ"
# Umsortieren der Spalten des Datensatzes
df_jets_month_m2_mj <- df_jets_month_m2_mj[,c("dts", "year", "month", "season", "method", "class", "lon", "lat")]
# Einfügen der u, v, z -Reihen
df_jets_month_m2_mj$u  <- NA
df_jets_month_m2_mj$v  <- NA
df_jets_month_m2_mj$uv <- NA
df_jets_month_m2_mj$z  <- NA

## PFJ || Chebyshev- Methode 
df_jets_month_m2_pfj <- melt(m2c_PFJ_lat, varnames = c("lon", "dts"),value.name = "lat" )
df_jets_month_m2_pfj$dts          <- rep(dts, each = n_lon)          # Datum
df_jets_month_m2_pfj$year         <- rep(dts_year, each = n_lon)     # Jahr
df_jets_month_m2_pfj$month        <- rep(dts_month, each = n_lon)    # Monat
df_jets_month_m2_pfj$season       <- rep(dts_season, each = n_lon)   # Jahreszeit/Saison
## Methode und Klassifizierung
df_jets_month_m2_pfj$method <- "Chebyshev"
df_jets_month_m2_pfj$class <- "PFJ"
# Umsortieren der Spalten des Datensatzes
df_jets_month_m2_pfj <- df_jets_month_m2_pfj[,c("dts", "year", "month", "season", "method", "class", "lon", "lat")]
# Einfügen der u, v, z -Reihen
df_jets_month_m2_pfj$u  <- melt(m2c_PFJ_u)$value
df_jets_month_m2_pfj$v  <- melt(m2c_PFJ_v)$value
df_jets_month_m2_pfj$uv <- sqrt(df_jets_month_m2_pfj$u**2 + df_jets_month_m2_pfj$v**2)
df_jets_month_m2_pfj$z  <- melt(m2c_PFJ_z)$value


## STJ || Chebyshev- Methode 
df_jets_month_m2_stj <- melt(m2c_STJ_lat, varnames = c("lon", "dts"),value.name = "lat" )
df_jets_month_m2_stj$dts          <- rep(dts, each = n_lon)          # Datum
df_jets_month_m2_stj$year         <- rep(dts_year, each = n_lon)     # Jahr
df_jets_month_m2_stj$month        <- rep(dts_month, each = n_lon)    # Monat
df_jets_month_m2_stj$season       <- rep(dts_season, each = n_lon)   # Jahreszeit/Saison
## Methode und Klassifizierung
df_jets_month_m2_stj$method <- "Chebyshev"
df_jets_month_m2_stj$class <- "STJ"
# Umsortieren der Spalten des Datensatzes
df_jets_month_m2_stj <- df_jets_month_m2_stj[,c("dts", "year", "month", "season", "method", "class", "lon", "lat")]
# Einfügen der u, v, z -Reihen
df_jets_month_m2_stj$u  <- melt(m2c_STJ_u)$value
df_jets_month_m2_stj$v  <- melt(m2c_STJ_v)$value
df_jets_month_m2_stj$uv <- sqrt(df_jets_month_m2_stj$u**2 + df_jets_month_m2_stj$v**2)
df_jets_month_m2_stj$z  <- melt(m2c_STJ_z)$value


## SJS || Chebyshev- Methode 
df_jets_month_m2_sjs <- melt(m2c_SJS_lat, varnames = c("lon", "dts"),value.name = "lat" )
df_jets_month_m2_sjs$dts          <- rep(dts, each = n_lon)          # Datum
df_jets_month_m2_sjs$year         <- rep(dts_year, each = n_lon)     # Jahr
df_jets_month_m2_sjs$month        <- rep(dts_month, each = n_lon)    # Monat
df_jets_month_m2_sjs$season       <- rep(dts_season, each = n_lon)   # Jahreszeit/Saison
## Methode und Klassifizierung
df_jets_month_m2_sjs$method <- "Chebyshev"
df_jets_month_m2_sjs$class <- "SJ"
# Umsortieren der Spalten des Datensatzes
df_jets_month_m2_sjs <- df_jets_month_m2_sjs[,c("dts", "year", "month", "season", "method", "class", "lon", "lat")]
# Einfügen der u, v, z -Reihen
df_jets_month_m2_sjs$u  <- melt(m2c_SJS_u)$value
df_jets_month_m2_sjs$v  <- melt(m2c_SJS_v)$value
df_jets_month_m2_sjs$uv <- sqrt(df_jets_month_m2_sjs$u**2 + df_jets_month_m2_sjs$v**2)
df_jets_month_m2_sjs$z  <- melt(m2c_SJS_z)$value


## PFJ || Dijkstra- Methode 
df_jets_month_m3_pfj <- melt(m3_PFJ_lat, varnames = c("lon", "dts"),value.name = "lat" )
df_jets_month_m3_pfj$dts          <- rep(dts, each = n_lon)          # Datum
df_jets_month_m3_pfj$year         <- rep(dts_year, each = n_lon)     # Jahr
df_jets_month_m3_pfj$month        <- rep(dts_month, each = n_lon)    # Monat
df_jets_month_m3_pfj$season       <- rep(dts_season, each = n_lon)   # Jahreszeit/Saison
## Methode und Klassifizierung
df_jets_month_m3_pfj$method <- "Dijkstra"
df_jets_month_m3_pfj$class <- "PFJ"
# Umsortieren der Spalten des Datensatzes
df_jets_month_m3_pfj <- df_jets_month_m3_pfj[,c("dts", "year", "month", "season", "method", "class", "lon", "lat")]
# Einfügen der u, v, z -Reihen
df_jets_month_m3_pfj$u  <- melt(m3_PFJ_u)$value
df_jets_month_m3_pfj$v  <- melt(m3_PFJ_v)$value
df_jets_month_m3_pfj$uv <- sqrt(df_jets_month_m3_pfj$u**2 + df_jets_month_m3_pfj$v**2)
df_jets_month_m3_pfj$z  <- NA


## STJ || Dijkstra- Methode 
df_jets_month_m3_stj <- melt(m3_STJ_lat, varnames = c("lon", "dts"),value.name = "lat" )
df_jets_month_m3_stj$dts          <- rep(dts, each = n_lon)          # Datum
df_jets_month_m3_stj$year         <- rep(dts_year, each = n_lon)     # Jahr
df_jets_month_m3_stj$month        <- rep(dts_month, each = n_lon)    # Monat
df_jets_month_m3_stj$season       <- rep(dts_season, each = n_lon)   # Jahreszeit/Saison
## Methode und Klassifizierung
df_jets_month_m3_stj$method <- "Dijkstra"
df_jets_month_m3_stj$class <- "STJ"
# Umsortieren der Spalten des Datensatzes
df_jets_month_m3_stj <- df_jets_month_m3_stj[,c("dts", "year", "month", "season", "method", "class", "lon", "lat")]
# Einfügen der u, v, z -Reihen
df_jets_month_m3_stj$u  <- melt(m3_STJ_u)$value
df_jets_month_m3_stj$v  <- melt(m3_STJ_v)$value
df_jets_month_m3_stj$uv <- sqrt(df_jets_month_m3_stj$u**2 + df_jets_month_m3_stj$v**2)
df_jets_month_m3_stj$z  <- NA


## SJS || Dijkstra- Methode 
df_jets_month_m3_sjs <- melt(m3_SJS_lat, varnames = c("lon", "dts"),value.name = "lat" )
df_jets_month_m3_sjs$dts          <- rep(dts, each = n_lon)          # Datum
df_jets_month_m3_sjs$year         <- rep(dts_year, each = n_lon)     # Jahr
df_jets_month_m3_sjs$month        <- rep(dts_month, each = n_lon)    # Monat
df_jets_month_m3_sjs$season       <- rep(dts_season, each = n_lon)   # Jahreszeit/Saison
## Methode und Klassifizierung
df_jets_month_m3_sjs$method <- "Dijkstra"
df_jets_month_m3_sjs$class <- "SJ"
# Umsortieren der Spalten des Datensatzes
df_jets_month_m3_sjs <- df_jets_month_m3_sjs[,c("dts", "year", "month", "season", "method", "class", "lon", "lat")]
# Einfügen der u, v, z -Reihen
df_jets_month_m3_sjs$u  <- melt(m3_SJS_u)$value
df_jets_month_m3_sjs$v  <- melt(m3_SJS_v)$value
df_jets_month_m3_sjs$uv <- sqrt(df_jets_month_m3_sjs$u**2 + df_jets_month_m3_sjs$v**2)
df_jets_month_m3_sjs$z  <- NA



## MERGEN DER DATAFRAMES ####
## 
## Mergen der einzelnen Dataframes der unterschiedlichen Methoden
df.jets.month <- rbind(df_jets_month_m1_mj, df_jets_month_m2_mj,
                       df_jets_month_m2_pfj, df_jets_month_m2_stj, df_jets_month_m2_sjs,
                       df_jets_month_m3_pfj, df_jets_month_m3_stj, df_jets_month_m3_sjs)
# Sortieren nach Datum und Längengrad
df.jets.month <- df.jets.month[order(df.jets.month$dts, df.jets.month$lon),]
# Umformatieren der year-Datenreihe als numeric 
df.jets.month$year <- as.numeric(as.character(df.jets.month$year))


## Mergen des Jet-Dataframes mit dem Seeeis-Dataframe
df.jets.month <- merge(df.jets.month, df_sea_ice, 
                       by.x = c("year", "month"), 
                       by.y = c("year", "month"), 
                       all.x = TRUE)
# Sortieren nach Datum und Längengrad
df.jets.month <- df.jets.month[order(df.jets.month$dts, df.jets.month$lon),]


## U-V-Datensatz ####
# Melten
df_uv <- melt(u[,,pressure_level,], varnames = c("lon", "lat", "dts"), value.name = "u")
df_uv$dts <- rep(dts, each = n_lon*n_lat)
df_uv$v <- melt(data = v[,,pressure_level,])$value
df_uv$uv <- sqrt( df_uv$u ** 2 + df_uv$v ** 2 )
df_uv$z <- melt(data = z[,,pressure_level,])$value


## DATAFRAMES ALS TIBBLES ####
## 
tb_jets_month <- as_tibble(df.jets.month)
tb_uv <- as_tibble(df_uv)

tb_jets_month$method <- factor(tb_jets_month$method, levels = c("Max", "Chebyshev", "Dijkstra"), 
                               labels = c("Maximum", "Chebyshev", "Dijkstra"))
tb_jets_month$class <- factor(tb_jets_month$class, levels = c("MJ", "PFJ", "SJ", "STJ"), 
                              labels = c("MJ", "PFJ", "SJ", "STJ"))
tb_jets_month$season <- factor(tb_jets_month$season, levels = c("djf", "mam", "jja", "son"),
                               labels = c("DJF", "MAM", "JJA", "SON"))


## LÖSCHEN UNNÖTIGER VARIABLEN ####
## 

rm(list = setdiff(ls(), list("tb_jets_month", "tb_uv", "tb_sea_ice", 
                             "dts", "dts_cold_warm", "dts_month", "dts_season", "dts_year", 
                             "get.orography.df", "get.orography.ll", 
                             "i.stp", "lat", "lev", "lon", "n_lat", "n_lon", "n.seas", "n.years", "pressure_level")))
ls()
