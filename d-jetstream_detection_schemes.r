## source("e-jetstream_detection_schemes.r")
##
## ROUTINEN ZUM AUFFINDEN DES/DER JETSTREAMS! ####
## MAXIMUM, CHEBYSHEV-LS-FIT, DIJKSTRA
####
####

## BENÖTIGTE PACKAGES
# install.packages("pckg.cheb_0.9.5-2.tar.gz", repos = NULL, type = "source")
library(pckg.cheb) # Least Squares Fit u Nullstellen d Ableitung
library(rootSolve)
library(igraph)


## Methode 1: Maximum des Zonalwinds in Meridionalrichtung 
find.jet.maximum.2d <- function(matrix, axis) {
  
  ## SUCHE DES MAXIMAS MITTELS APPLY
  vec.max.y <- apply(matrix, 1, max)
  
  ##
  vec.max.x <- rep(NA, length.out = length(vec.max.y))
  
  for (i in 1:length(vec.max.x)) {
    #print(i)
    vec.max.x[i] <- if (!is.na(vec.max.y[i])) axis[which(vec.max.y[i] == matrix[i,])]
  }
  
  # Vorbereiten der Liste für Übergabe
  list.max.jet <- list("MaxJ.lat" = vec.max.x, "MaxJ.u" = vec.max.y)
  return(list.max.jet)
  
}


## Methode 2: Polynomfit des Zonalwinds in Merdionalrichtung im Sektor [20,85]
## Übergebene Variablen: Alle gefundenen Maxima, globales Maximum, zwei lokale Maxima in Sektor
find.jets.chebpoly.2d <- function(matrix.u, matrix.v, matrix.z, axis.x, axis.y, n.order = 8) {

  # Dimensionen der Matrix
  n.axis.x <- length(axis.x)
  n.axis.y <- length(axis.y)
  
  # Least-Squares-Fit mit Chebyshev-Polynomen an Zonalwind für jeden Meridian
  # Übergabe der Maxima
  list.max <- apply(matrix.u, 1, cheb.find.max, x.axis = axis.y, n = n.order)
  list.max.len <- length(list.max) ###
  
  # Längengrad und Zonalwind der gefundenen Maxima
  list.lat <- sapply(list.max, "[[", 1)
  list.u <- sapply(list.max, "[[", 2)
  ## Transformieren der Liste in Array, Auffüllen mit NAs.
  n.max.max <- ceiling(n.order/2 + 1)
  array.lat <- sapply(list.lat, fun.fill, n = n.max.max)
  array.u <- sapply(list.u, fun.fill, n = n.max.max)
  
  # Position und Zonalwind des globalen Maximums
  # MaxJ.ind <- apply(array.u, 2, which.max)
  MaxJ.lat <- rep(NA, nrow(matrix.u)); MaxJ.u <- rep(NA, nrow(matrix.u)); 
  for (i in seq_along(axis.x)) {
    # Index des Maximum im Raum der Maxima
    MaxJ.ind <- which.max(array.u[,i])
    # Index des Maximum im Raum der Längengrade # Nearest Neighbor Search
    MaxJ.ind.grd <- which.min(sqrt((array.lat[MaxJ.ind, i] - axis.y)**2))
    if (length(MaxJ.ind) >= 1) {
      MaxJ.lat[i] <- array.lat[MaxJ.ind, i]
    }
    if (length(MaxJ.ind.grd) >= 1) {
      MaxJ.u[i] <- matrix.u[i, MaxJ.ind.grd]
    }
  }
  
  ## Herausfiltern der Positionen innerhalb des Sektors [20, 85]
  sect.ind <- which(array.lat > 20 & array.lat < 85, arr.ind = TRUE)
  array.lat.sect <- matrix(NA, nrow = n.max.max, ncol = list.max.len)
  array.lat.sect[sect.ind] <- array.lat[sect.ind]
  array.u.sect <- matrix(NA, nrow = n.max.max, ncol = list.max.len)
  array.u.sect[sect.ind] <- array.u[sect.ind]
  
  ## Filterung der zwei stärksten Maxima
  model.maxs <- apply(array.u.sect, 2, diff.max)
  # Positionen als Indizes im Raum der Maxima
  model.maxs.ind <- sapply(model.maxs, "[[", 1)
  
  ## Umrechnung von Indizes zu Breitengraden, Zonalwind- und Meridionalwindstärke
  model.maxs.lat <- matrix(NA, nrow = nrow(model.maxs.ind), ncol = n.axis.x)
  model.maxs.ind.grd <- matrix(NA, nrow(model.maxs.ind), n.axis.x)
  model.maxs.u <- matrix(NA, nrow(model.maxs.ind), n.axis.x)
  model.maxs.v <- matrix(NA, nrow(model.maxs.ind), n.axis.x)
  model.maxs.z <- matrix(NA, nrow(model.maxs.ind), n.axis.x)
  
  # Umrechnen von Indizes in Breitengrade
  for (i in seq_along(axis.x)) {
    model.maxs.lat[,i] <- array.lat.sect[model.maxs.ind[,i],i]
  }
  # Nächster Breitengrad-Gitterpunkt # nearest neighbor search
  for (i in seq(from = 1, to = length(model.maxs.ind))) {
    if (!is.na(model.maxs.ind[i])) {
      model.maxs.ind.grd[i] <- which.min(sqrt((model.maxs.lat[i] - axis.y)**2))
    }
  }
  # Abgreifen der dortigen Zonalwind- und Meridionalwind-Geschwindigkeiten
  for (i in seq_along(axis.x)) {
    #print(model.maxs.ind.grd[,i])
    model.maxs.u[,i] <- matrix.u[i,model.maxs.ind.grd[,i]]
    model.maxs.v[,i] <- matrix.v[i,model.maxs.ind.grd[,i]]
    model.maxs.z[,i] <- matrix.z[i,model.maxs.ind.grd[,i]]
  }
  
  # Werte der Maxima (U-Wind)
  # model.maxs.u <- sapply(model.maxs, "[[", 2) 
  
  ## Annahme: PFJ nördliches Maximum, STJ südliches Maximum
  PFJ.lat <- rep(NA, n.axis.x); 
  PFJ.u <- PFJ.lat; PFJ.v <- PFJ.lat; PFJ.z <- PFJ.lat
  STJ.lat <- rep(NA, n.axis.x); 
  STJ.u <- STJ.lat; STJ.v <- STJ.lat; STJ.z <- STJ.lat
  for (i in seq_along(axis.x)) {
    #print(i)
    PFJ.ind <- which.max(model.maxs.lat[,i])
    STJ.ind <- which.min(model.maxs.lat[,i])
    if (length(PFJ.ind) >= 1 | length(STJ.ind) >= 1) {
      # Polarfrontjet
      PFJ.lat[i] <- model.maxs.lat[PFJ.ind,i]
      PFJ.u[i] <- model.maxs.u[PFJ.ind,i]
      PFJ.v[i] <- model.maxs.v[PFJ.ind,i]
      PFJ.z[i] <- model.maxs.z[PFJ.ind,i]
      # Subtropenjet
      STJ.lat[i] <- model.maxs.lat[STJ.ind,i]
      STJ.u[i] <- model.maxs.u[STJ.ind,i]
      STJ.v[i] <- model.maxs.v[STJ.ind,i]
      STJ.z[i] <- model.maxs.z[STJ.ind,i]
    }
    PFJ.ind <- NA; STJ.ind <- NA;
  }
  
  ## Überprüfen, ob Jets weiter als 10 Grad auseinanderliegen.
  ## Falls nicht, Annahme für Single-Jetstream
  SJS.ind <- which(sqrt((PFJ.lat - STJ.lat)**2) < 10)
  
  SJS.lat <- rep(NA, n.axis.x); 
  SJS.u <- SJS.lat; SJS.v <- SJS.lat; SJS.z <- SJS.lat
  
  ## Nehme Mittelwert der beiden Werte
  SJS.lat[SJS.ind]  <- apply(cbind(PFJ.lat[SJS.ind], STJ.lat[SJS.ind]), 1, mean)
  SJS.u[SJS.ind]    <- apply(cbind(PFJ.u[SJS.ind], STJ.u[SJS.ind]), 1, mean)
  SJS.v[SJS.ind]    <- apply(cbind(PFJ.v[SJS.ind], STJ.v[SJS.ind]), 1, mean)
  SJS.z[SJS.ind]    <- apply(cbind(PFJ.z[SJS.ind], STJ.z[SJS.ind]), 1, mean)
  
  ## Setze korrespondierende PFJ.* und STJ.* gleich NA
  PFJ.lat[SJS.ind] <- NA;  PFJ.u[SJS.ind]   <- NA
  PFJ.v[SJS.ind]   <- NA;  PFJ.v[SJS.ind]   <- NA
  STJ.lat[SJS.ind] <- NA;  STJ.u[SJS.ind]   <- NA
  STJ.v[SJS.ind]   <- NA;  STJ.z[SJS.ind]   <- NA
  
  ## Übergabe der Variablen
  list.model.jet <- list("all.max.lat" = array.lat, # "all.max.u" = array.u,
                         "MaxJ.lat" = MaxJ.lat, # "MaxJ.u" = MaxJ.u,
                         "PFJ.lat" = PFJ.lat, "PFJ.u" = PFJ.u, "PFJ.v" = PFJ.v, "PFJ.z" = PFJ.z,
                         "STJ.lat" = STJ.lat, "STJ.u" = STJ.u, "STJ.v" = STJ.v, "STJ.z" = STJ.z,
                         "SJS.lat" = SJS.lat, "SJS.u" = SJS.u, "SJS.v" = SJS.u, "SJS.z" = SJS.z)
  return(list.model.jet)
}


## Methode 3: Kürzester Pfad mittels Dijkstra-Algorithmus. Nach Molnos/PIK.
find.jet.dijkstra.2d <- function(u, v, lon, lat, jet, season) {
  # Anpassen der Wichtungen
  if (jet == "STJ" & season == "cold") {
    w1 <- 0.044; w2 <- 0.0015; w3 <- 1 - w1 - w2; clim.jet <- 25.1;
  } else if (jet == "STJ" & season == "warm") {
    w1 <- 0.072; w2 <- 0.0015; w3 <- 1 - w1 - w2; clim.jet <- 29.8
  } else if (jet == "PFJ" & season == "cold") {
    w1 <- 0.044; w2 <- 0.0015; w3 <- 1 - w1 - w2; clim.jet <- 67.5;
  } else if (jet == "PFJ" & season == "warm") {
    w1 <- 0.043; w2 <- 0.0015; w3 <- 1 - w1 - w2; clim.jet <- 69.1;
  }
  # Dimensionen des 2d-Datensatzes und Festlegen von Konstanten
  nlon <- length(lon); nlat <- length(lat);
  d.lon <- lon[2] - lon[1]; d.lat <- lat[2] - lat[1];
  R <- 6371000
  # zur erfüllung der bedingung start = end wird der erste längengrad an den letzten kopiert
  u <- u[c(1:nlon,1),]; v <- v[c(1:nlon,1),]; len <- length(u);
  lon <- c(lon, lon[1] + 360); nlon <- length(lon);
  mat.lat <- matrix(lat, nrow = nlon, ncol = nlat, byrow = TRUE)
  mat.lon <- matrix(lon, nrow = nlon, ncol = nlat)
  # aufbauen einer matrix zur definition der knotenpunkte und verbindenden kanten
  # erste spalte startknoten
  # zweite spalte zielknoten
  # dritte spalte kantengewicht (abh von windgeschwigkeit, windrichtung, breitengrad)
  nodes <- NULL
  nghbrs.xx <- c(-1, -1, -1, 
                 0,  0,  
                 1,  1,  1)
  nghbrs.yy <- c(-1,  0,  1, 
                 -1,  1,
                 -1,  0,  1)
  nghbrs.ttl <- c("nghbr.sw", "nghbr.ww", "nghbr.nw",
                  "nghbr.ss", "nghbr.nn", 
                  "nghbr.se", "nghbr.ee", "nghbr.ne")
  for (i in 1:len) {
    # festlegen der inizes der acht nachbarpunkte
    nghbr.sw <- if (i > nlon & (i - 1) %% nlon != 0) c(i, i - nlon - 1)
    nghbr.ww <- if ((i - 1) %% nlon != 0) c(i, i - 1)
    nghbr.nw <- if ((i - 1) %% nlon != 0 & i <= len - nlon) c(i, i + nlon - 1)
    nghbr.ss <- if (i > nlon) c(i, i - nlon)
    nghbr.nn <- if (i <= len - nlon ) c(i, i + nlon)
    nghbr.se <- if (i > nlon & i %% nlon != 0) c(i, i - nlon + 1)
    nghbr.ee <- if (i %% nlon != 0) c(i, i + 1)
    nghbr.ne <- if (i <= len - nlon & i %% nlon != 0) c(i, i + nlon + 1)
    # zusammenführen der knoten
    nodes <- rbind(nodes, nghbr.sw, nghbr.ww, nghbr.nw, nghbr.ss, nghbr.nn, nghbr.se, nghbr.ee, nghbr.ne)
  }
  nodes <- cbind(nodes, NA, NA, NA, NA)
  # parameter für windgeschwindigkeit
  max.uv <- max(sqrt(u ** 2 + v ** 2))
  nodes[,3] <- 1 - ((sqrt(u[nodes[,1]] ** 2 + v[nodes[,1]] ** 2) + sqrt(u[nodes[,2]] ** 2 + v[nodes[,2]] ** 2)) / (2 * max.uv))
  
  # parameter für windrichtung
  for (i in 1:8) {
    # print(c(nghbrs.ttl[i], nghbrs.xx[i], nghbrs.yy[i]))
    pnt.vec <- cbind(pi * R * cos(mat.lat[nodes[which(rownames(nodes) == nghbrs.ttl[i]),1]]*pi/180) * nghbrs.xx[i] * d.lon / 180, pi * R * nghbrs.yy[i] * d.lat / 180)
    pnt.vec.nrm <- t(apply(pnt.vec, 1, norm.vec))
    wnd.vec <- cbind(u[nodes[which(rownames(nodes) == nghbrs.ttl[i]),1]], v[nodes[which(rownames(nodes) == nghbrs.ttl[i]),1]])
    wnd.vec.nrm <- t(apply(wnd.vec, 1, norm.vec))
    nodes[which(rownames(nodes) == nghbrs.ttl[i]),4] <- (1 - rowSums(pnt.vec.nrm * wnd.vec.nrm)) / 2
  }
  
  # parameter für klimatisches mittel
  nodes[,5] <- (mat.lat[nodes[,1]] - clim.jet) ** 4 / max(clim.jet, 90 - clim.jet) ** 4
  
  # benennung der matrix
  colnames(nodes) <- c("root", "target", "strength", "direction", "climate", "weight")
  rownames(nodes) <- NULL
  
  # setzen der kantengewichte
  nodes[,6] <- w1 * nodes[,3] + w2 * nodes[,4] + w3 * nodes[,5]
  
  # definieren des graphen über knoten und kanten gewichte
  g <- add_edges(make_empty_graph(len), t(nodes[,1:2]), weight = nodes[,6])
  
  # start vektor bei lon[1]
  strt <- seq(1, len, nlon)
  fnsh <- seq(nlon, len, nlon)
  
  # diagonale der distanzmatrix
  # distanzen bei gleichem breitengrad als start und ziel
  dist <- diag(distances(g, v = strt, to = fnsh, algorithm = "dijkstra"))
  min.dist <- which.min(dist)
  shrt.pth <- unlist(get.shortest.paths(g, from = strt[min.dist], to = fnsh[min.dist])$vpath)
  
  # berechnung von lon, lat, u, v an optimalen pfad
  lon.jet <- mat.lon[shrt.pth]; lat.jet <- mat.lat[shrt.pth];
  u.jet <- u[shrt.pth]; v.jet <- v[shrt.pth] 
  
  # übergabe der variablen jeweils ohne den letzten Wert
  list.model.jet <- list("SP.J.lon" = lon.jet[-nlon], "SP.J.lat" = lat.jet[-nlon],
                         "SP.J.u" = u.jet[-nlon], "SP.J.v" = v.jet[-nlon])
  return(list.model.jet)
}

find.jets.dijkstra.2d <- function(u, v, lon, lat, season) {
  STJ <- find.jet.dijkstra.2d(u, v, lon, lat, jet = "STJ", season)
  PFJ <- find.jet.dijkstra.2d(u, v, lon, lat, jet = "PFJ", season)
  
  PFJ.lat <- PFJ$SP.J.lon; PFJ.u <- PFJ$SP.J.u; PFJ.v <- PFJ$SP.J.v; PFJ.z <- PFJ$SP.J.z;
  STJ.lat <- STJ$SP.J.lon; STJ.u <- STJ$SP.J.u; STJ.v <- STJ$SP.J.v; STJ.z <- STJ$SP.J.z;
  
  ## Überprüfen, ob Jets weiter als 10 Grad auseinanderliegen.
  ## Falls nicht, Annahme für Single-Jetstream
  SJS.ind <- which(sqrt((PFJ.lat - STJ.lat)**2) < 10)
  
  SJS.lat <- rep(NA, n.axis.x); 
  SJS.u <- SJS.lat; SJS.v <- SJS.lat; SJS.z <- SJS.lat
  
  ## Nehme Mittelwert der beiden Werte
  SJS.lat[SJS.ind]  <- apply(cbind(PFJ.lat[SJS.ind], STJ.lat[SJS.ind]), 1, mean)
  SJS.u[SJS.ind]    <- apply(cbind(PFJ.u[SJS.ind], STJ.u[SJS.ind]), 1, mean)
  SJS.v[SJS.ind]    <- apply(cbind(PFJ.v[SJS.ind], STJ.v[SJS.ind]), 1, mean)
  SJS.z[SJS.ind]    <- apply(cbind(PFJ.z[SJS.ind], STJ.z[SJS.ind]), 1, mean)
  
  ## Setze korrespondierende PFJ.* und STJ.* gleich NA
  PFJ.lat[SJS.ind] <- NA;  PFJ.u[SJS.ind]   <- NA
  PFJ.v[SJS.ind]   <- NA;  PFJ.v[SJS.ind]   <- NA
  STJ.lat[SJS.ind] <- NA;  STJ.u[SJS.ind]   <- NA
  STJ.v[SJS.ind]   <- NA;  STJ.z[SJS.ind]   <- NA
  
  
  # Übergabe der Variablen
  list.model.jet <- list("PFJ.lat" = PFJ.lat, "PFJ.u" = PFJ.u, "PFJ.v" = PFJ.v, "PFJ.z" = PFJ.z,
                         "STJ.lat" = STJ.lat, "STJ.u" = STJ.u, "STJ.v" = STJ.v, "STJ.z" = STJ.z,
                         "SJS.lat" = SJS.lat, "SJS.u" = SJS.u, "SJS.v" = SJS.u, "SJS.z" = SJS.z)
  return(list.model.jet)
}

