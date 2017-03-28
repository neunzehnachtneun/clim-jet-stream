## source('~/01-Master-Thesis/02-code-git/j-shortest-path-dijkstra.r')
##
## ROUTINE ZUM AUFFINDEN DES JETSTREAMS ÜBER DIE ####
## BESTIMMUNG DES KUERZESTEN PFADES IN EINEM GRAPHEN
## MITTELS DIJKSTRA ALGORITHMUS VGL PIK MOLNOS ETAL 2017
####
####


####
## FUNKTION ZUR NORMIERUNG VON VEKTOREN ####
## SCALE() LIEFERT KEIN VERGLEICHBARES ERGEBNIS.
####
norm.vec <- function(vec) {
  normalized.vec <- vec / sqrt( sum(vec ** 2))
  return(normalized.vec)
}


####
## FUNKTION ZUR BESTIMMUNG DES KÜRZESTEN PFADES IN EINEM GRAPHEN ####
## MITTELS DES DIJKSTRA-ALGORITHMUS ÜBER KNOTEN, VEBINDENDE KANTEN UND KANTENGEWICHTE
## GEWICHTE SIND ABHÄNGIG VON WINDGESCHWINDIGKEIT, -RICHTUNG, UND DEM BREITENGRAD
## METHODIK NACH PIK - MOLNOS ETAL 2017
####

find.jet.dijkstra.2d <- function(u, v, lon, lat, w1 = 0.49846, w2 = 0.00154, w3 = 0.5, clim.jet) {
  # Paket zur Berechnung von Distanzen in Graphen
  require(igraph)
  # Dimensionen des 2d-Datensatzes
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
    print(c(nghbrs.ttl[i], nghbrs.xx[i], nghbrs.yy[i]))
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
  
  # setzen der kanten
  nodes[,6] <- w1 * nodes[,3] + w2 * nodes[,4] + w3 * nodes[,5]
  
  # definieren des graphen über knoten und kanten gewichte
  #rownames(nodes) <- NULL
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
  
  # übergabe der variablen
  return.list <- list(lon.jet = lon.jet[-nlon],lat.jet = lat.jet[-nlon], u.jet = u.jet[-nlon], v.jet = v.jet[-nlon])
  return(return.list)
}
