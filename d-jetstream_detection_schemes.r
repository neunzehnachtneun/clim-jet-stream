## source("e-jetstream_detection_schemes.r")
##
## ROUTINEN ZUM AUFFINDEN DES/DER JETSTREAMS! ####
## MAXIMUM, CHEBYSHEV-LS-FIT, DIJKSTRA
####
####

## BENÖTIGTE PACKAGES
# install.packages("pckg.cheb_0.9.5-2.tar.gz", repos = NULL, type = "source")
library(pckg.cheb) # Least Squares Fit u Nullstellen d Ableitung
library(igraph)


## Methode 0: Maximum des Zonalwinds in Meridionalrichtung 
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

## Methode 1a: Polynomfit des Zonalwinds in Meridionalrichtung, zwei stärkste Maxima als PFJ und STJ
#library(parallel) # Paralleles Rechnen m Apply
find.jets.chebpoly.2d <- function(matrix, axis, n.order = 8) {
  
  ####
  ## VARIABLEN UND PARAMETER ####
  ####
  library(pckg.cheb) # Least Squares Fit u Nullstellen d Ableitung
  
  
  ####
  ## LEAST SQUARES FIT                   ####
  ## CHEBYSHEV POLYNOME 8-TER ORDNUNG      #
  ## AN ZONAL WIND IN MERIDIONALER RICHTUNG #
  ####
  ####
  
  list.max <- apply(matrix, 1, cheb.find.max, x.axis = lat, n = n.order)
  list.max.len <- length(list.max)
  
  ## Maxima des Modells (Positionen und Werte)
  model.max.lat <- sapply(list.max, "[[", 1)
  model.max.u <- sapply(list.max, "[[", 2)
  n.max.max <- max(sapply(model.max.lat, length))
  model.max.lat <- sapply(model.max.lat, fun.fill, n = n.max.max)
  #model.max.lat <- apply(array(model.max.lat, c(n.max.max, list.max.dim)), c(1,3), t)
  model.max.u <- sapply(model.max.u, fun.fill, n = n.max.max)
  #model.max.u <- apply(array(model.max.u, c(n.max.max, list.max.nrow, list.max.ncol)), c(1,3), t)
  
  ## entscheidungsschema für pfj und stj
  ## Unterscheidung von polarem und subtropischem Jetstream
  ## Filterung der zwei stärksten Maxima
  model.max.2 <- apply(model.max.u, 2, diff.max)
  # Positionen als Indizes
  model.max.2.x <- sapply(model.max.2, "[[", 1) 
  # Umrechnung von Indizes zu Breitengraden
  model.max.2.lat <- matrix(NA,nrow(matrix), ncol = 2)
  for (i in 1:nrow(matrix)) {
    #print(model.max.lat[model.max.2.x[,i],i])
    model.max.2.lat[i,] <- model.max.lat[model.max.2.x[,i],i]
  }
  # Werte der Maxima (U-Wind)
  model.max.2.u <- sapply(model.max.2, "[[", 2) 
  
  # Annahmen: PFJ nördliches Maximum, STJ südliches Maximum
  PFJ.lat <- rep(NA, nrow(matrix)); 
  STJ.lat <- PFJ.lat; MaxJ.lat <- PFJ.lat
  PFJ.u <- PFJ.lat; STJ.u <- PFJ.lat; MaxJ.u <- PFJ.lat
  for (i in 1:192) {
    #print(i)
    PFJ.ind <- which.max(model.max.2.lat[i,])
    STJ.ind <- which.min(model.max.2.lat[i,])
    if (length(PFJ.ind) >= 1 | length(STJ.ind) >= 1) {
      PFJ.lat[i] <- model.max.2.lat[i,PFJ.ind]
      STJ.lat[i] <- model.max.2.lat[i,STJ.ind]
      PFJ.u[i] <- model.max.2.u[PFJ.ind,i]
      STJ.u[i] <- model.max.2.u[STJ.ind,i]
    } else {
      PFJ.lat[i] <- NA; STJ.lat[i] <- NA;
      PFJ.u[i] <- NA; STJ.u[i] <- NA
    }
    PFJ.ind <- NA; STJ.ind <- NA;
    # Position d stärksten Jets
    MaxJ.ind <- which.max(model.max.2.u[,i])
    if (length(MaxJ.ind) >= 1) {
      MaxJ.lat[i] <- model.max.2.lat[i, MaxJ.ind]
      MaxJ.u[i] <- model.max.2.u[MaxJ.ind, i]
    } else {
      MaxJ.lat[i] <- NA; MaxJ.u[i] <- NA;
    }
  }
  
  ## Löschen von temporär benötigten Daten
  # rm(list.max.dim, list.max.nrow, list.max.ncol)
  
  ## Übergabe von Variablen
  list.model.jet <- list("MaxJ.lat" = MaxJ.lat, "MaxJ.u" = MaxJ.u,
                         "PFJ.lat" = PFJ.lat, "PFJ.u" = PFJ.u,
                         "STJ.lat" = STJ.lat, "STJ.u" = STJ.u)
  return(list.model.jet)
}

## Methode 1b: Polynomfit des Zonalwinds in Meridionalrichtung, zwei stärkste Maxima als PFJ und STJ
find.jets.chebpoly.fit.2d <- function(matrix.u, matrix.v, axis.x, axis.y, n.order = 8) {
  
  ## Aufruf von find.jet.chebpoly.2d zum Auffinden der Jets
  jets <- find.jets.chebpoly.2d(matrix = matrix.u, axis = axis.y, n.order = n.order)
  PFJ.lat <- jets$PFJ.lat; PFJ.u   <- jets$PFJ.u
  STJ.lat <- jets$STJ.lat; STJ.u   <- jets$STJ.u
  
  PFJ.lat.fit <- cheb.fit(PFJ.lat[c(97:192, 1:192, 1:96)], c(1:384), 24)
  PFJ.lat.fit <- PFJ.lat.fit[97:288]
  STJ.lat.fit <- cheb.fit(STJ.lat[c(97:192, 1:192, 1:96)], c(1:384), 24)
  STJ.lat.fit <- STJ.lat.fit[97:288]
  
  # Erkennen der zu den Breitengraden passenden Zonalwinde
  PFJ.u <- rep(NA, length.out = length(lon)); PFJ.v <- rep(NA, length.out = length(lon))
  STJ.u <- rep(NA, length.out = length(lon)); STJ.v <- rep(NA, length.out = length(lon))
  for (i in 1:length(lon)) {
    if (!is.na(PFJ.lat.fit[i])) {
      PFJ.u[i] <- matrix.u[i, which.min(abs(lat - PFJ.lat.fit[i]))]
      PFJ.v[i] <- matrix.v[i, which.min(abs(lat - PFJ.lat.fit[i]))]
    }
    if (!is.na(STJ.lat.fit[i])) {
      STJ.u[i] <- matrix.u[i, which.min(abs(lat - STJ.lat.fit[i]))]
      STJ.v[i] <- matrix.v[i, which.min(abs(lat - STJ.lat.fit[i]))]
    }
  }
  
  ## Übergabe von Variablen
  list.model.jet <- list("PFJ.lat" = PFJ.lat.fit, "PFJ.u" = PFJ.u, "PFJ.v" = PFJ.v,
                         "STJ.lat" = STJ.lat.fit, "STJ.u" = STJ.u, "STJ.v" = STJ.v)
  return(list.model.jet)
}

## Methode2: Kürzester Pfad mittels Dijkstra-Algorithmus. Nach Molnos/PIK.
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
  mat.lat <- matrix(lat, nr = nlon, nc = nlat, byrow = TRUE)
  mat.lon <- matrix(lon, nr = nlon, nc = nlat)
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
  
  # Übergabe der Variablen
  list.model.jet <- list("STJ.lon" = STJ$SP.J.lon, "STJ.lat" = STJ$SP.J.lat, 
                         "STJ.u"   = STJ$SP.J.u,   "STJ.v"   = STJ$SP.J.v,
                         "PFJ.lon" = PFJ$SP.J.lon, "PFJ.lat" = PFJ$SP.J.lat,
                         "PFJ.u"   = PFJ$SP.J.u,   "PFJ.v"   = PFJ$SP.J.v)
  return(list.model.jet)
}

