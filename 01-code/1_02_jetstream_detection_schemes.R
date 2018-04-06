## source("d-jetstream_detection_schemes.r")
##
## ROUTINEN ZUM AUFFINDEN DES/DER JETSTREAMS! ####
## MAXIMUM, CHEBYSHEV-LS-FIT, DIJKSTRA
####
####

## BENÖTIGTE PACKAGES
# install.packages("ChebyshevMax_0.9.9.tar.gz", repos = NULL, type = "source")
# library(ChebyshevMax) # Least Squares Fit u Nullstellen d Ableitung
# library(rootSolve)
# library(igraph)


## Methode 1: Maximum des Zonalwinds in Meridionalrichtung 
find_jet_via_maximum_2d <- function(matrix, axis) {
  
  ## SUCHE DES MAXIMAS MITTELS APPLY
  vec_max_y <- apply(matrix, 1, max)
  
  ##
  vec_max_x <- rep(NA, length.out = length(vec_max_y))
  
  for (i in 1:length(vec_max_x)) {
    #print(i)
    vec_max_x[i] <- if (!is.na(vec_max_y[i])) axis[which(vec_max_y[i] == matrix[i,])]
  }
  
  # Vorbereiten der Liste für Übergabe
  list_max_jet <- list("MaxJ_lat" = vec_max_x, "MaxJ_u" = vec_max_y)
  return(list_max_jet)
  
}


## Methode 2: Polynomfit des Zonalwinds in Merdionalrichtung im Sektor [20,85]
## Übergebene Variablen: Alle gefundenen Maxima, globales Maximum, zwei lokale Maxima in Sektor
find_jets_via_chebpoly_2d <- function(matrix_u, matrix_v, matrix_z,
                                      axis_x, axis_y,
                                      bound_lower, bound_upper,
                                      order_polyfit, threshold_single_jet = 10) {

  # Dimensionen der Matrix
  n_axis_x <- length(axis_x)
  n_axis_y <- length(axis_y)
  
  # Least-Squares-Fit mit Chebyshev-Polynomen an Zonalwind für jeden Meridian
  # Übergabe der Maxima
  list_max <- apply(matrix_u, 1, cheb_find_max, x_axis = axis_y, n = order_polyfit)
  list_max_len <- length(list_max) ###
  
  # Längengrad und Zonalwind der gefundenen Maxima
  list_lat <- sapply(list_max, "[[", 1)
  list_u <- sapply(list_max, "[[", 2)
  ## Transformieren der Liste in Array, Auffüllen mit NAs.
  n_max_max <- order_polyfit
  array_lat <- sapply(list_lat, fun_fill_vec, n = n_max_max)
  array_u <- sapply(list_u, fun_fill_vec, n = n_max_max)
  
  # Position und Zonalwind des globalen Maximums
  # MaxJ.ind <- apply(array.u, 2, which.max)
  MaxJ_lat <- rep(NA, nrow(matrix_u)); MaxJ_u <- rep(NA, nrow(matrix_u)); 
  for (i in seq_along(axis_x)) {
    # Index des Maximum im Raum der Maxima
    MaxJ_ind <- which.max(array_u[,i])
    # Index des Maximum im Raum der Längengrade # Nearest Neighbor Search
    MaxJ_ind_grd <- which.min(sqrt((array_lat[MaxJ_ind, i] - axis_y)**2))
    if (length(MaxJ_ind) >= 1) {
      MaxJ_lat[i] <- array_lat[MaxJ_ind, i]
    }
    if (length(MaxJ_ind_grd) >= 1) {
      MaxJ_u[i] <- matrix_u[i, MaxJ_ind_grd]
    }
  }
  
  ## Herausfiltern der Positionen innerhalb des Sektors [20, 85]
  sect_ind <- which(array_lat > bound_lower & array_lat < bound_upper, arr.ind = TRUE)
  array_lat_sect <- matrix(NA, nrow = n_max_max, ncol = list_max_len)
  array_lat_sect[sect_ind] <- array_lat[sect_ind]
  array_u_sect <- matrix(NA, nrow = n_max_max, ncol = list_max_len)
  array_u_sect[sect_ind] <- array_u[sect_ind]
  
  ## Filterung der zwei stärksten Maxima
  model_maxs <- apply(array_u_sect, 2, diff_max)
  # Positionen als Indizes im Raum der Maxima
  model_maxs_ind <- sapply(model_maxs, "[[", 1)
  
  ## Umrechnung von Indizes zu Breitengraden, Zonalwind- und Meridionalwindstärke
  model_maxs_lat <- matrix(NA, nrow = nrow(model_maxs_ind), ncol = n_axis_x)
  model_maxs_ind_grd <- matrix(NA, nrow(model_maxs_ind), n_axis_x)
  model_maxs_u <- matrix(NA, nrow(model_maxs_ind), n_axis_x)
  model_maxs_v <- matrix(NA, nrow(model_maxs_ind), n_axis_x)
  model_maxs_z <- matrix(NA, nrow(model_maxs_ind), n_axis_x)
  
  # Umrechnen von Indizes in Breitengrade
  for (i in seq_along(axis_x)) {
    model_maxs_lat[,i] <- array_lat_sect[model_maxs_ind[,i],i]
  }
  # Nächster Breitengrad-Gitterpunkt # nearest neighbor search
  for (i in seq(from = 1, to = length(model_maxs_ind))) {
    if (!is.na(model_maxs_ind[i])) {
      model_maxs_ind_grd[i] <- which.min(sqrt((model_maxs_lat[i] - axis_y)**2))
    }
  }
  # Abgreifen der dortigen Zonalwind- und Meridionalwind-Geschwindigkeiten
  for (i in seq_along(axis_x)) {
    #print(model_maxs_ind_grd[,i])
    model_maxs_u[,i] <- matrix_u[i, model_maxs_ind_grd[,i]]
    model_maxs_v[,i] <- matrix_v[i, model_maxs_ind_grd[,i]]
    model_maxs_z[,i] <- matrix_z[i, model_maxs_ind_grd[,i]]
  }
  
  # Werte der Maxima (U-Wind)
  # model_maxs_u <- sapply(model.maxs, "[[", 2) 
  
  ## Annahme: PFJ nördliches Maximum, STJ südliches Maximum
  PFJ_lat <- rep(NA, n_axis_x); 
  PFJ_u <- PFJ_lat; PFJ_v <- PFJ_lat; PFJ_z <- PFJ_lat
  STJ_lat <- rep(NA, n_axis_x); 
  STJ_u <- STJ_lat; STJ_v <- STJ_lat; STJ_z <- STJ_lat
  for (i in seq_along(axis_x)) {
    #print(i)
    PFJ_index <- which.max(model_maxs_lat[,i])
    STJ_index <- which.min(model_maxs_lat[,i])
    if (length(PFJ_index) >= 1 | length(STJ_index) >= 1) {
      # Polarfrontjet
      PFJ_lat[i] <- model_maxs_lat[PFJ_index,i]
      PFJ_u[i] <- model_maxs_u[PFJ_index,i]
      PFJ_v[i] <- model_maxs_v[PFJ_index,i]
      PFJ_z[i] <- model_maxs_z[PFJ_index,i]
      # Subtropenjet
      STJ_lat[i] <- model_maxs_lat[STJ_index,i]
      STJ_u[i] <- model_maxs_u[STJ_index,i]
      STJ_v[i] <- model_maxs_v[STJ_index,i]
      STJ_z[i] <- model_maxs_z[STJ_index,i]
    }
    PFJ_index <- NA; STJ_index <- NA;
  }
  
  ## Überprüfen, ob Jets weiter als 10 Grad auseinanderliegen.
  ## Falls nicht, Annahme für Single-Jetstream
  # SJS_index <- which(sqrt((PFJ_lat - STJ_lat)**2) < threshold_single_jet)
  # 
  # SJS_lat <- rep(NA, n_axis_x); 
  # SJS_u <- SJS_lat; SJS_v <- SJS_lat; SJS_z <- SJS_lat
  # 
  # if (length(SJS_index) != 0) {
  #   ## Nehme Mittelwert der beiden Werte
  #   SJS_lat[SJS_index]  <- apply(cbind(PFJ_lat[SJS_index], STJ_lat[SJS_index]), 1, mean)
  #   SJS_u[SJS_index]    <- apply(cbind(PFJ_u[SJS_index], STJ_u[SJS_index]), 1, mean)
  #   SJS_v[SJS_index]    <- apply(cbind(PFJ_v[SJS_index], STJ_v[SJS_index]), 1, mean)
  #   SJS_z[SJS_index]    <- apply(cbind(PFJ_z[SJS_index], STJ_z[SJS_index]), 1, mean)
  #   
  #   ## Setze korrespondierende PFJ.* und STJ.* gleich NA
  #   PFJ_lat[SJS_index] <- NA;  PFJ_u[SJS_index]   <- NA
  #   PFJ_v[SJS_index]   <- NA;  PFJ_v[SJS_index]   <- NA
  #   STJ_lat[SJS_index] <- NA;  STJ_u[SJS_index]   <- NA
  #   STJ_v[SJS_index]   <- NA;  STJ_z[SJS_index]   <- NA
  # }
  
  ## Übergabe der Variablen ## "all.max.lat" = array.lat, # "all.max.u" = array.u,
  list_model_jet <- list("MaxJ_lat" = MaxJ_lat, # "MaxJ_u" = MaxJ_u,
                         "PFJ_lat" = PFJ_lat, "PFJ_u" = PFJ_u, "PFJ_v" = PFJ_v, "PFJ_z" = PFJ_z,
                         "STJ_lat" = STJ_lat, "STJ_u" = STJ_u, "STJ_v" = STJ_v, "STJ_z" = STJ_z) #,
                         # "SJS_lat" = SJS_lat, "SJS_u" = SJS_u, "SJS_v" = SJS_u, "SJS_z" = SJS_z)
  return(list_model_jet)
}


## Methode 3: Kürzester Pfad mittels Dijkstra-Algorithmus. Nach Molnos/PIK.
find_jet_via_dijkstra_2d <- function(u, v, lon, lat, jet, season) {
  # Anpassen der Wichtungen
  if (jet == "STJ" & season == "cold") {
    w1 <- 0.044; w2 <- 0.0015; w3 <- 1 - w1 - w2; clim_jet <- 25.1;
  } else if (jet == "STJ" & season == "warm") {
    w1 <- 0.072; w2 <- 0.0015; w3 <- 1 - w1 - w2; clim_jet <- 29.8
  } else if (jet == "PFJ" & season == "cold") {
    w1 <- 0.044; w2 <- 0.0015; w3 <- 1 - w1 - w2; clim_jet <- 67.5;
  } else if (jet == "PFJ" & season == "warm") {
    w1 <- 0.043; w2 <- 0.0015; w3 <- 1 - w1 - w2; clim_jet <- 69.1;
  }
  # Dimensionen des 2d-Datensatzes und Festlegen von Konstanten
  n_lon <- length(lon); n_lat <- length(lat);
  d_lon <- lon[2] - lon[1]; d.lat <- lat[2] - lat[1];
  R <- 6371000
  # zur erfüllung der bedingung start = end wird der erste längengrad an den letzten kopiert
  u <- u[c(1:n_lon,1),]; v <- v[c(1:n_lon,1),]; len <- length(u);
  lon <- c(lon, lon[1] + 360); n_lon <- length(lon);
  mat_lat <- matrix(lat, nrow = n_lon, ncol = n_lat, byrow = TRUE)
  mat_lon <- matrix(lon, nrow = n_lon, ncol = n_lat)
  # aufbauen einer matrix zur definition der knotenpunkte und verbindenden kanten
  # erste spalte startknoten
  # zweite spalte zielknoten
  # dritte spalte kantengewicht (abh von windgeschwigkeit, windrichtung, breitengrad)
  nodes <- NULL
  nghbrs_xx <- c(-1, -1, -1, 
                 0,  0,  
                 1,  1,  1)
  nghbrs_yy <- c(-1,  0,  1, 
                 -1,  1,
                 -1,  0,  1)
  nghbrs_ttl <- c("nghbr_sw", "nghbr_ww", "nghbr_nw",
                  "nghbr_ss", "nghbr_nn", 
                  "nghbr_se", "nghbr_ee", "nghbr_ne")
  for (i in 1:len) {
    # festlegen der inizes der acht nachbarpunkte
    nghbr_sw <- if (i > n_lon & (i - 1) %% n_lon != 0) c(i, i - n_lon - 1)
    nghbr_ww <- if ((i - 1) %% n_lon != 0) c(i, i - 1)
    nghbr_nw <- if ((i - 1) %% n_lon != 0 & i <= len - n_lon) c(i, i + n_lon - 1)
    nghbr_ss <- if (i > n_lon) c(i, i - n_lon)
    nghbr_nn <- if (i <= len - n_lon ) c(i, i + n_lon)
    nghbr_se <- if (i > n_lon & i %% n_lon != 0) c(i, i - n_lon + 1)
    nghbr_ee <- if (i %% n_lon != 0) c(i, i + 1)
    nghbr_ne <- if (i <= len - n_lon & i %% n_lon != 0) c(i, i + n_lon + 1)
    # zusammenführen der knoten
    nodes <- rbind(nodes, nghbr_sw, nghbr_ww, nghbr_nw, nghbr_ss, nghbr_nn, nghbr_se, nghbr_ee, nghbr_ne)
  }
  nodes <- cbind(nodes, NA, NA, NA, NA)
  # parameter für windgeschwindigkeit
  max_uv <- max(sqrt(u ** 2 + v ** 2))
  nodes[,3] <- 1 - ((sqrt(u[nodes[,1]] ** 2 + v[nodes[,1]] ** 2) + sqrt(u[nodes[,2]] ** 2 + v[nodes[,2]] ** 2)) / (2 * max_uv))
  
  # parameter für windrichtung
  for (i in 1:8) {
    # print(c(nghbrs_ttl[i], nghbrs_xx[i], nghbrs_yy[i]))
    pnt_vec <- cbind(pi * R * cos(mat_lat[nodes[which(rownames(nodes) == nghbrs_ttl[i]),1]]*pi/180) * nghbrs_xx[i] * d_lon / 180, pi * R * nghbrs_yy[i] * d.lat / 180)
    pnt_vec_nrm <- t(apply(pnt_vec, 1, normalize_vec))
    wnd_vec <- cbind(u[nodes[which(rownames(nodes) == nghbrs_ttl[i]),1]], v[nodes[which(rownames(nodes) == nghbrs_ttl[i]),1]])
    wnd_vec_nrm <- t(apply(wnd_vec, 1, normalize_vec))
    nodes[which(rownames(nodes) == nghbrs_ttl[i]),4] <- (1 - rowSums(pnt_vec_nrm * wnd_vec_nrm)) / 2
  }
  
  # parameter für klimatisches mittel
  nodes[,5] <- (mat_lat[nodes[,1]] - clim_jet) ** 4 / max(clim_jet, 90 - clim_jet) ** 4
  
  # benennung der matrix
  colnames(nodes) <- c("root", "target", "strength", "direction", "climate", "weight")
  rownames(nodes) <- NULL
  
  # setzen der kantengewichte
  nodes[,6] <- w1 * nodes[,3] + w2 * nodes[,4] + w3 * nodes[,5]
  
  # definieren des graphen über knoten und kanten gewichte
  g <- add_edges(make_empty_graph(len), t(nodes[,1:2]), weight = nodes[,6])
  
  # start vektor bei lon[1]
  strt <- seq(1, len, n_lon)
  fnsh <- seq(n_lon, len, n_lon)
  
  # diagonale der distanzmatrix
  # distanzen bei gleichem breitengrad als start und ziel
  dist <- diag(distances(g, v = strt, to = fnsh, algorithm = "dijkstra"))
  min_dist <- which.min(dist)
  shrt_pth <- unlist(get.shortest.paths(g, from = strt[min_dist], to = fnsh[min_dist])$vpath)
  
  # berechnung von lon, lat, u, v an optimalen pfad
  lon_jet <- mat_lon[shrt_pth]; lat_jet <- mat_lat[shrt_pth];
  u_jet <- u[shrt_pth]; v_jet <- v[shrt_pth] 
  
  # übergabe der variablen jeweils ohne den letzten Wert
  list_model_jet <- list("SP_J_lon" = lon_jet[-n_lon], "SP_J_lat" = lat_jet[-n_lon],
                         "SP_J_u" = u_jet[-n_lon], "SP_J_v" = v_jet[-n_lon])
  return(list_model_jet)
}

find_jets_via_dijkstra_2d <- function(matrix_u, matrix_v, 
                                      axis_x, axis_y, season,
                                      threshold_single_jet = 10) {
  STJ <- find_jet_via_dijkstra_2d(matrix_u, matrix_v, axis_x, axis_y, jet = "STJ", season)
  PFJ <- find_jet_via_dijkstra_2d(matrix_u, matrix_v, axis_x, axis_y, jet = "PFJ", season)
  
  PFJ_lat <- PFJ$SP_J_lat; PFJ_u <- PFJ$SP_J_u; PFJ_v <- PFJ$SP_J_v; PFJ_z <- PFJ$SP_J_z;
  STJ_lat <- STJ$SP_J_lat; STJ_u <- STJ$SP_J_u; STJ_v <- STJ$SP_J_v; STJ_z <- STJ$SP_J_z;
  
  ## Überprüfen, ob Jets weiter als 10 Grad auseinanderliegen.
  ## Falls nicht, Annahme für Single-Jetstream
  # SJS_index <- which(sqrt((PFJ_lat - STJ_lat)**2) < threshold_single_jet)
  # 
  # SJS_lat <- rep(NA, nrow(matrix_u)); 
  # SJS_u <- SJS_lat; SJS_v <- SJS_lat; SJS_z <- SJS_lat
  # 
  # if (length(SJS_index) != 0) {
  #   ## Nehme Mittelwert der beiden Werte
  #   SJS_lat[SJS_index]  <- apply(cbind(PFJ_lat[SJS_index], STJ_lat[SJS_index]), 1, mean)
  #   SJS_u[SJS_index]    <- apply(cbind(PFJ_u[SJS_index], STJ_u[SJS_index]), 1, mean)
  #   SJS_v[SJS_index]    <- apply(cbind(PFJ_v[SJS_index], STJ_v[SJS_index]), 1, mean)
  # 
  #   ## Setze korrespondierende PFJ.* und STJ.* gleich NA
  #   PFJ_lat[SJS_index] <- NA;  PFJ_u[SJS_index]   <- NA;  PFJ_v[SJS_index]   <- NA;  
  #   STJ_lat[SJS_index] <- NA;  STJ_u[SJS_index]   <- NA;  STJ_v[SJS_index]   <- NA;  
  # }
  
  # Übergabe der Variablen
  list_model_jet <- list("PFJ_lat" = PFJ_lat, "PFJ_u" = PFJ_u, "PFJ_v" = PFJ_v, "PFJ_z" = PFJ_z,
                         "STJ_lat" = STJ_lat, "STJ_u" = STJ_u, "STJ_v" = STJ_v, "STJ_z" = STJ_z) #,
                         # "SJS_lat" = SJS_lat, "SJS_u" = SJS_u, "SJS_v" = SJS_u, "SJS_z" = SJS_z)
  return(list_model_jet)
}

