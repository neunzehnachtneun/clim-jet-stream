####################################################################################################
## source('~/Master_Thesis/r-code-git/fun_chebyshev.r')
##
##


##
## skalierung der stützpunkte auf [-1,1]
fkt.cheb.scale <- function(x) {
  x.cheb.scaled <- (2 * (x - x[1]) / (max(x) - min(x))) - 1
  return(x.cheb.scaled)
}


##
## reskalierung
fkt.cheb.rescale <- function(x.cheb, x) {
  x.rescaled <- (1/2 * (x.cheb + 1) * (max(x) - min(x))) + x[1]
  return(x.rescaled)
}


##
## funktion zur berechnung der y-werte aus koeffizienten (chebyshev) und x-werten
## benötigt für nullstellensuche
fkt.cheb.val <- function(x, cheb.coeff) {
  n <- length(cheb.coeff) - 1
  m <- n + 1
  cheb.t.0 <- 1;  cheb.t.1 <- x; 
  cheb.t <- cbind(cheb.t.0, cheb.t.1)
  if (n >= 2) {
    for (i in 3:m) {
      cheb.t.i <- 2 * x * cheb.t[,(i - 1)] - cheb.t[,(i - 2)]
      cheb.t <- cbind(cheb.t, cheb.t.i)
      rm(cheb.t.i)
    }
  }
  cheb.model <- cheb.t %*% cheb.coeff
  return(cheb.model)
}


##
## funktion zur berechnung der werte der ableitung
## benötigt zum auffinden des maximums => nullstelle der ableitung
fkt.cheb.deriv <- function(x, cheb.coeff) {
  if(length(x) != 0) {
    n <- length(cheb.coeff) - 1
    m <- n + 1
    cheb.u.0 <- 1; cheb.u.1 <-  2*x
    cheb.u <- cbind(cheb.u.0, cheb.u.1)
    if (n >= 2) {
      for (i in 3:m) {
        cheb.u.i <- 2 * x * cheb.u[,(i - 1)] - cheb.u[,(i - 2)]
        cheb.u <- cbind(cheb.u, cheb.u.i)
        rm(cheb.u.i)
      }}
    # berechnung der ableitung der polynome erster art
    # rekursionsformel 0
    # dT/dx = n * U_(n-1)
    cheb.t.deriv <- if (length(x) == 1) (2:m)*t(cheb.u[,1:n]) else t((2:m)*t(cheb.u[,1:n]))
    cheb.model.deriv <- cheb.t.deriv %*% cheb.coeff[2:m]
    return(cheb.model.deriv)
  }
}


## Variante 1 - Least squares fit über gesamten Datensatz
#
#
fkt.cheb.fit <- function(){
  
}

## Variante 2 - Least squares fit über Sequenzierungen des Datensatzes
## Schnitt des Datensatzes in <split> Sequenzen

fkt.cheb.fit.seq <- function(x, d, n, split, dx.hr){
  ## funktion zur rechnung mit chebyshev polynomen
  ## bestimmung der polynome erster und zweiter art, polynomfit nter ordnung,
  ## ermittelung der ableitung der polynome und entwicklung von modellen des fits und der ableitung.
  ## inputs: d datensatz, der approximiert werden soll, 
  ## x stützpunkte für f(x), zb längengrad [0,90],
  ## n ordnung des polynom fit, dx.h auflösung des hochaufgelösten gitters, 
  ## split die unterteilung der daten in sektoren mit der länge split
  ## outputs: koeffizienten und ableitung, modell und ableitung
  ##

  x.mat <- t(matrix(x,ncol = split))
  d.mat <- t(matrix(d,ncol = split))
  
  # schleife über sequenzen des Datensatzes
  for (i in 1:length(x.mat[,1])) {
    # print(i)
    # erstellung der sequenzen
    x.seq <- if (i == 1) c(x.mat[i,]) else c(x.mat[(i - 1), dim(x.mat)[2]], x.mat[i,])
    d.seq <- if (i == 1) c(d.mat[i,]) else c(d.mat[(i - 1), dim(d.mat)[2]], d.mat[i,])
    # skalierung der stützpunkte auf [-1,1]
    x.cheb <- fkt.cheb.scale(x.seq)
    # polynom *nullter* ordnung äquivalent zu *erstem* eintrag in vektor
    # daher wird ein hilfsskalar benutzt
    m <- (n + 1)
    ## erzeugung der chebyshev polynome erster art
    # rekursionsformel nach bronstein et al s952
    cheb.t.0 <- 1;  cheb.t.1 <- x.cheb; 
    cheb.t <- cbind(cheb.t.0, cheb.t.1)
    if (n >= 2) {
      for (j in 3:m) {
        cheb.t.i <- 2*x.cheb*cheb.t[,(j - 1)] - cheb.t[,(j - 2)]
        cheb.t <- cbind(cheb.t, cheb.t.i)
        rm(cheb.t.i)
      }} 
    
    ## erzeugung der chebyshev polynome zweiter art
    # rekursionsformel wiki???
    cheb.u.0 <- 1; cheb.u.1 <-  2*x.cheb
    cheb.u <- cbind(cheb.u.0, cheb.u.1)
    if (n >= 2) {
      for (k in 3:m) {
        cheb.u.i <- 2*x.cheb*cheb.u[,(k - 1)] - cheb.u[,(k - 2)]
        cheb.u <- cbind(cheb.u, cheb.u.i)
        rm(cheb.u.i)
      }}
    
    # berechnung der ableitung der polynome erster art
    # rekursionsformel wiki???
    # dT/dx = n * U_(n-1)
    cheb.t.deriv <- t((2:m)*t(cheb.u[,2:m]))

    ## modell berechnungen
    # berechnung der koeffizienten des polyfits
    cheb.coeff.seq <- solve(t(cheb.t) %*% cheb.t) %*% t(cheb.t) %*% d.seq
    cheb.coeff <- if (i == 1) cheb.coeff.seq else cbind(cheb.coeff, cheb.coeff.seq)
    # berechnung des gefilterten modells
    cheb.model.seq <- cheb.t %*% cheb.coeff.seq
    end <- length(cheb.model.seq)
    cheb.model <- if (i == 1) cheb.model.seq else c(cheb.model, cheb.model.seq[2:end])
    # berechnung des abgeleiteten modells
    cheb.model.deriv.seq <- cheb.t.deriv %*% cheb.coeff.seq[2:m]
    cheb.model.deriv <- if (i == 1) cheb.model.deriv.seq else c(cheb.model.deriv, cheb.model.deriv.seq[2:end])
    
    # berechnung der nullstellen
    extr.seq <- uniroot.all(fkt.cheb.deriv, cheb.coeff = cheb.coeff.seq, lower = (-1), upper = 1)
    # reskalierung der Nullstellen auf normale Lat- Achse
    x.extr.seq <- fkt.cheb.rescale(extr.seq, x = x.seq)
    x.extr <- if (i == 1) x.extr.seq else c(x.extr, x.extr.seq)
    # filtern der maxima aus nullstellen d. ableitung
    y.extr.seq <- if (length(extr.seq) != 0) fkt.cheb.val(x = extr.seq, cheb.coeff = cheb.coeff.seq)
    #y.extr <- if (exists(y.extr) == F &) y.extr.seq else c(y.extr, y.extr.seq)
    if (exists("y.extr.seq") == T & exists("y.extr") == F) {
      y.extr <- y.extr.seq
    } else if (exists("y.extr.seq") == T & exists("y.extr") == T) {
      y.extr <- c(y.extr, y.extr.seq)
    }
    ## löschung der übergangsvariablen
#    print(i)
#    print(cheb.coeff.seq)
    rm(cheb.coeff.seq, cheb.model.seq, cheb.model.deriv.seq, extr.seq, x.extr.seq, y.extr.seq)
  
    #print(i) 
  }
  
  ## übergabe der variablen als liste
  cheb.list <- list(cheb.coeff = cheb.coeff, cheb.model = cheb.model, cheb.model.deriv = cheb.model.deriv, extr.x = x.extr, extr.y = y.extr)#, y.extr = y.extr)
  return(cheb.list)
}
  
## Variante 3 - Least squares fit über 'fließende' sequenzierungen
## 
  
  