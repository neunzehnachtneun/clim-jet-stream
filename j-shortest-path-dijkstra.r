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

find.jet.dijkstra <- function(u, v, lon, lat, w1 = 0.49846, w2 = 0.00154, w3 = 0.5, clim.jet) {
  # Paket zur Berechnung von Distanzen in Graphen
  require(igraph)
  # Dimensionen des 2d-Datensatzes
  nlon <- length(lon); nlat <- length(lat);
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
  for (i in 1:len) {
    # ausschließen der letzten lon-datenreihe
    if ( i %% nlon != 0) { 
      int.st <- c(i, i + 1)
      int.sw <- if (i > nlon) c(i, i + 1 - nlon)
      int.nw <- if (i < (len - nlon)) c(i, i + 1 + nlon)
      
      # parameter für windgeschwindigkeit
      max.uv <- max(sqrt(u ** 2 + v ** 2))
      x.st <- 1 - ((sqrt(u[i] ** 2 + v[i] ** 2) + 
                      sqrt(u[int.st[2]] ** 2 + v[int.st[2]] ** 2)) /
                     (2 * max.uv))
      x.sw <- if (i > nlon) 1 - ((sqrt(u[i] ** 2 + v[i] ** 2) + 
                                    sqrt(u[int.sw[2]] ** 2 + v[int.sw[2]] ** 2)) /
                                   (2 * max.uv))
      x.nw <- if (i < (len - nlon)) 1 - ((sqrt(u[i] ** 2 + v[i] ** 2) + 
                                            sqrt(u[int.nw[2]] ** 2 + v[int.nw[2]] ** 2)) /
                                           (2 * max.uv))
      # parameter für windrichtung
      y.st <- (1 - norm.vec(c(u[i], v[i])) %*% norm.vec(c(mat.lon[int.st[2]], mat.lat[int.st[2]]) - c(mat.lon[i], mat.lat[i]))) / 2 
      y.sw <- if (i > nlon) (1 - norm.vec(c(u[i], v[i])) %*% norm.vec(c(mat.lon[int.sw[2]], mat.lat[int.sw[2]]) - c(mat.lon[i], mat.lat[i]))) / 2 
      y.nw <- if (i < (len - nlon)) (1 - norm.vec(c(u[i], v[i])) %*% norm.vec(c(mat.lon[int.nw[2]], mat.lat[int.nw[2]]) - c(mat.lon[i], mat.lat[i]))) / 2 
      # parameter für breitengrad
      z.st <- (mat.lat[i] - clim.lat) ** 4 / (max(clim.lat, 90 - clim.lat)) ** 4
      z.sw <- if (i > nlon) (mat.lat[i] - clim.lat) ** 4 / (max(clim.lat, 90 - clim.lat)) ** 4
      z.nw <- if (i < (len - nlon)) (mat.lat[i] - clim.lat) ** 4 / (max(clim.lat, 90 - clim.lat)) ** 4
      # setzen der kanten
      int.st <- c(int.st, w1 * x.st + w2 * y.st + w3 * z.st)
      int.sw <- if (i > nlon) c(int.sw, w1 * x.sw + w2 * y.sw + w3 * z.sw)
      int.nw <- if (i < (len - nlon)) c(int.nw, w1 * x.nw + w2 * y.nw + w3 * z.nw)
    }
    nodes <- rbind(nodes, int.sw, int.st, int.nw)
  }
  
  # definieren des graphen über knoten und kanten gewichte
  rownames(nodes) <- NULL
  g <- add_edges(make_empty_graph(len), t(nodes[,1:2]), weight = nodes[,3])
  
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
