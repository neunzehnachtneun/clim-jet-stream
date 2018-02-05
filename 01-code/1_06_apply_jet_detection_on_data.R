# source("a-c-apply_jet_detection_on_data.R")
## ANWENDEN DER JETSTREAM-DETEKTIONS-SCHEMATA AUF DEN ERA-40-/ERA-INTERIM-DATENSATZ
## GERECHNET WIRD IN DIESEM FALL AUF DEM RECHENSERVER SV31
## DIESER HAT 32 KERNE UND 128GB RAM, WESHALB SICH HIER PARALLELES RECHNEN ANBIETET.
## ES WERDEN MAXIMAL 16 KERNE ANGESTEUERT, MEHR SOLLTEN ES NICHT SEIN, DA
## SONST DER RAM KNAPP WIRD UND ANDERE NUTZER*INNEN EINGESCHRAENKT WERDEN.



## METHODE 1: find_jet_via_maximum_2d ####
## 
library(parallel)
cl_fork_1 <- makeCluster(n_cluster, type = "FORK")
# find_jet_via_maximum_2d(matrix = u[,,pressure_level, t_stp], axis = lat)
m1 <- parApply(cl_fork_1, X = u[,,pressure_level,], MARGIN = 3, FUN = find_jet_via_maximum_2d, axis = lat)
stopCluster(cl_fork_1); rm(cl_fork_1)


## METHODE 2: find.jet.chebpoly.fit.2d ####
##
library(foreach); library(doParallel)
cl_fork_2 <- makeCluster(n_cluster, type = "FORK")
registerDoParallel(cl_fork_2)
m2 <- foreach(t_stp = 1:length(dts)) %dopar% {
  find_jets_via_chebpoly_2d(matrix_u = u[,, pressure_level, t_stp], 
                            matrix_v = v[,, pressure_level, t_stp],
                            matrix_z = z[,, pressure_level, t_stp],
                            axis_x = lon, axis_y = lat, 
                            bound_lower = bound_lower_chebyshev, 
                            bound_upper = bound_upper_chebyshev,
                            order_polyfit = order_polynomial_fit,
                            threshold_single_jet = threshold_single_jet)}
stopCluster(cl_fork_2); rm(cl_fork_2)


## METHODE 3: find_jet_via_dijkstra_2d ####
## 
cl_psock_1 <- makeCluster(n_cluster, type = "PSOCK")
registerDoParallel(cl_psock_1)
m3 <- foreach(t_stp = 1:length(dts), .packages = "igraph") %dopar% {
  find_jets_via_dijkstra_2d(matrix_u = u[,, pressure_level, t_stp], 
                            matrix_v = v[,, pressure_level, t_stp], 
                            axis_x = lon, axis_y = lat, 
                            season = dts_cold_warm[t_stp],
                            threshold_single_jet = threshold_single_jet) }
stopCluster(cl_psock_1); rm(cl_psock_1)


## ZWISCHENSPEICHERN DER WERTE DES DATENSATZES ####
# Speichern
save.image("stp-a.RData")

## ENDE ENDE ENDE ####
