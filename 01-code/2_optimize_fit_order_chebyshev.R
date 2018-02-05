#' ==========================================================================================================
#' OPTIMIEREN DER ORDNUNG DES CHEBYSHEV-POLYNOM-FITS ANHAND EINER OBJEKTIVEN METHODE                       ==
#' UM DIE BESTMOEGLICHE ABBILDUNG DER JETSTREAMS ZU GEWAEHRLEISTEN, OPTIMIERE ICH DIE ORDNUNG DES          ==
#' POLYNOMFITS ÜBER DIE MINIMIERUNG DER BREITENGRAD-ABSTAENDE ZWISCHEN MAXIMAL-JET UND MAXIMALEM           ==
#' CHEBYSHEV-JET (OPTIMALER FIT == MINIMAL ABSTAND)                                                        ==
#' ==========================================================================================================
#' ABSTÄNDE SIND BEI ORDNUNG N=24 MINIMAL. HÄLFTE DER GITTERPUNKTE IST AUCH 24. WIE ERWARTET.              ==
#' HINWEIS: ROUTINE SOLLTE NUR AUF MASCHINEN MIT MEHR ALS ACHT (8) KERNEN LAUFEN!                          ==
#' ==========================================================================================================


#' Setzen des Working Directories
setwd("~/01-Master-Thesis/02-code-git/01-code/")


#' Laden wichtiger Pakete -----------------------------------------------------------------------------------
#' 
library(ncdf4)
library(ChebyshevMax)
library(rootSolve)
library(igraph)
library(parallel)
library(foreach)
library(doParallel)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tikzDevice)


source("1_02_jetstream_detection_schemes.R")
source("1_03_help_functions.R")

#' Festlegen von Konstanten für Routinen ---------------------------------------
geopotential <- 250
bound_lower_chebyshev <- 20
bound_upper_chebyshev <- 80
order_polynomial_fit <- 24
threshold_single_jet <- 10


#' EINLESEN DER DATEN ---------------------------------------------------------------------------------------
#'
source("1_05_read_era_data.R")


#' VORBEREITUNGEN FUER SCHLEIFE
#' Setzen der Anzahl der anzusteuernden Kerne für paralleles Rechnen.
#' Nicht mehr Kerne ansprechen, da andernfalls der Server in die Knie geht.
n_cluster <- 8

#' Aufbauen des Dataframes zum Festhalten und Speichern der Ergebnisse
i_order <- seq(from = 8, to = 32)
tb_results_order <- tibble(order = i_order, rmse = -9999,  dist = -9999)


#' Berechnen der Maxima nach der Maximum-Methode
cl_fork_1 <- makeCluster(n_cluster, type = "FORK")
m1 <- parApply(cl_fork_1, X = u[, , pressure_level, ], MARGIN = 3, FUN = find_jet_via_maximum_2d, axis = lat)
stopCluster(cl_fork_1); rm(cl_fork_1)
#' Auftrennen der Listen und Übertragen der gefundenen Jetstream-Positionen in Matrizen für
#' 1. Globales Maximum
m1_J_lat    <- sapply(m1, "[[", "MaxJ_lat"); colnames(m1_J_lat)  <- dts; rownames(m1_J_lat) <- lon;

#' SCHLEIFE UEBER FIT-ORDNUNGEN VON 8-32 --------------------------------------------------------------------
#' 
for (ii in seq_along(i_order)) {
  print(i_order[ii])
  
  
  #' Berechnen der Maxima nach der Chebyshev-Methode
  cl_fork_2 <- makeCluster(n_cluster, type = "FORK")
  registerDoParallel(cl_fork_2)
  m2 <- foreach(t.stp = seq_along(dts)) %dopar% {
    find_jets_via_chebpoly_2d(matrix_u = u[, , pressure_level, t.stp],
                              matrix_v = v[, , pressure_level, t.stp],
                              matrix_z = z[, , pressure_level, t.stp],
                              axis_x = lon, axis_y = lat, 
                              bound_lower = bound_lower_chebyshev,
                              bound_upper = bound_upper_chebyshev,
                              order_polyfit = i_order[ii] )}
  stopCluster(cl_fork_2); rm(cl_fork_2)


  #' Auftrennen der Listen und Übertragen der gefundenen Jetstream-Positionen in Matrizen für
  #' 2. Maximalen Chebyshev-Jet
  m2b_J_lat <- sapply(m2, "[[", "MaxJ_lat");   colnames(m2b_J_lat) <- dts; rownames(m2b_J_lat) <- lon;
  #' 3. Polarfrontjet
  m2c_PFJ_lat <- sapply(m2, "[[", "PFJ_lat"); colnames(m2c_PFJ_lat) <- dts; rownames(m2c_PFJ_lat) <- lon;
  #' 4. Subtropenjet
  m2c_STJ_lat <- sapply(m2, "[[", "STJ_lat"); colnames(m2c_STJ_lat) <- dts; rownames(m2c_STJ_lat) <- lon;

  #' Root Mean Square Error wird als Maß für die Abweichung genutzt. Distanzen sind
  #' 1. Differenz von Maximaljet und maximalem Chebyshev-Jet
  rmse <- sqrt(mean( (m1_J_lat - m2b_J_lat) ** 2, na.rm = TRUE))
  #' 2. Differenz von Polarfront- und Subtropenjet
  dist <- sqrt(mean( (m2c_STJ_lat - m2c_PFJ_lat) ** 2, na.rm = TRUE))
  #' Zum späteren Aufruf werden die Größen in ein Dataframe und eine csv-Datei geschrieben.
  tb_results_order$rmse[ii] <- rmse
  tb_results_order$dist[ii] <- dist
  print(tb_results_order)
  write.table(tb_results_order, file = "2_results_optimized_fit_order.csv")
}


#' Visualisierungen -----------------------------------------------------------------------------------------
#' Abstände zwischen Maximaljet und maximalem Chebyshev-Jetstream
ggp_order_rmse <-
  ggplot(data = tb_results_order,
         mapping = aes(x = order, y = rmse)) +
  geom_point() +
  scale_x_continuous(name = "Ordnung des Fits") +
  scale_y_continuous(name = "Betrag der mittleren Distanz \n der Maximal-Jetstreams in $^{\\circ}$") +
  geom_vline(xintercept = 24, linetype = "dotdash") +
  theme_bw()
print(ggp_order_rmse)

#' Abstände zwischen Polarfront- und Subtropenjet nach Chebyshev
ggp_order_dist <-
  ggplot(data = tb_results_order,
         mapping = aes(x = order, y = dist)) +
  geom_point() +
  scale_x_continuous(name = "Ordnung des Fits") +
  scale_y_continuous(name = "Betrag der mittleren Distanz \n zwischen Polarfront- und \n Subtropen-Jetstream in $^{\\circ}$") +
  geom_vline(xintercept = 24, linetype = "dotdash") +
  theme_bw()
print(ggp_order_dist)

#' Speichern der Plots
# save_plot(plt = ggp_order_rmse,
#          width = 135, height = 70, pointsize = 11,
#          filepath = paste0(save_dir, "00-fit-order/"),
#          filename = "order_rmse")
# save_plot(plt = ggp_order_dist,
#          width = 135, height = 70, pointsize = 11,
#          filepath = paste0(save_dir, "00-fit-order/"),
#          filename = "order_dist")
