## source('~/Master_Thesis/pckg.cheb/R/pckg.cheb.r')
##
##

#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


##
#' @title Scaling of X-Axis
#' @param x.axis ursprüngliche beliebige X-Achse (Vektor)
#' @return x.cheb.scaled skalierte X-Achse (Vektor)
#' @description
#' \code{cheb.scale} skaliert beliebige X-Achse auf Achse, die für Polynom-fits verträglich ist.
#' @examples
#' x.axis <- c(0:30)
#' x.cheb.scaled <- cheb.scale(x.axis)
cheb.scale <- function(x.axis) {#, scale) {
  ## Funktion zur Skalierung von Stützpunkten
  ## von beliebigen Gittern auf [-1, 1]
  ## ##
  #  if (type == "cheb") {
  x.cheb.scaled <- (2 * (x.axis - x.axis[1]) / (max(x.axis) - min(x.axis))) - 1
  #  }
  return(x.cheb.scaled)
}


##
#' @title Rescaling of X-Axis
#' @param x.cheb skalierte X-Achse (Skalar oder Vektor)
#' @param x.axis beliebige X-Achse (Vektor)
#' @return x.rescaled reskalierte X-Achse (Skalar oder Sektor)
#' @description
#' cheb.rescale reskaliert die für den Fit erzeugte Achse auf die Ursprüngliche
#' @examples
#' x.rescaled <- cheb.rescale(x.cheb, x.axis)
cheb.rescale <- function(x.cheb, x.axis) {
  ## Funktion zur Reskalierung vom [-1, 1]-Gitter
  ## auf das Ursprungsgitter (in diesem Fall - Lat)
  ## ##
  x.rescaled <- (1/2 * (x.cheb + 1) * (max(x.axis) - min(x.axis))) + x.axis[1]
  return(x.rescaled)
}


##
#' @title Generating Chebyshev Polynomials of first kind
#' @param x.axis beliebige X-Achse (Vektor)
#' @param n Ordnung des Polynoms (Skalar)
#' @return cheb.t Chebyshev-Polynome Erster Art (Vektor)
#' @description
#' cheb.1st erzeugt Chebyshev Polynome erster Art aus beliebiger X-Achse
#' @examples
#' cheb.t <- cheb.1st(x.axis, n)
cheb.1st <- function(x.axis, n){
  ## Funktion zur Erzeugung von Chebyshev-Polynomen Erster Art
  ## ##
  x.cheb <- if (max(x.axis) - min(x.axis) > 2) cheb.scale(x.axis) else x.axis  ### ###
  m <- n + 1
  # Rekursionsformel Wiki / Bronstein
  cheb.t.0 <- 1;  cheb.t.1 <- x.cheb;
  cheb.t <- cbind(cheb.t.0, cheb.t.1)
  if (n >= 2) {
    for (i in 3:m) {
      cheb.t.i <- 2 * x.cheb * cheb.t[,(i - 1)] - cheb.t[,(i - 2)]
      cheb.t <- cbind(cheb.t, cheb.t.i)
      rm(cheb.t.i)
    }
  }
  return(cheb.t)
}


##
#' @title Generating Chebyshev Polynomials of second kind
#' @param x.axis beliebigie X-Achse (Vektor)
#' @param n Ordnung des Polynoms (Skalar)
#' @return cheb.u Chebyshev-Polynome Zweiter Art (Vektor)
#' @description
#' cheb.2nd erzeugt Chebyshev Polynome zweiter Art aus beliebiger X-Achse
#' @examples
#' cheb.u <- cheb.2nd(x.axis, n)
cheb.2nd <- function(x.axis, n){
  ## Funktion zur Erzeugung von Chebyshev-Polynomen Zweiter Art
  ## ##
  x.cheb <- if (max(x.axis) - min(x.axis) > 2) cheb.scale(x.axis) else x.axis
  m <- n + 1
  cheb.u.0 <- 1; cheb.u.1 <-  2*x.cheb
  cheb.u <- cbind(cheb.u.0, cheb.u.1)
  if (n >= 2) {
    for (i in 3:m) {
      cheb.u.i <- 2 * x.cheb * cheb.u[,(i - 1)] - cheb.u[,(i - 2)]
      cheb.u <- cbind(cheb.u, cheb.u.i)
      rm(cheb.u.i)
    }
  }
  return(cheb.u)
}


##
#' @title Calculation of Values of the model fit
#' @param x.axis beliebige X-Achse (Skalar oder Vektor)
#' @param cheb.coeff Chebyshev-Koeffizienten aus Least-Squares-Verfahren (Vektor)
#' @return cheb.model gefiltertes Modell (Skalar oder Vektor)
#' @description
#' cheb.val berechnet aus den Chebyshev-Koeffizienten die Y-Werte
#' @examples
#' cheb.model <- cheb.val(x.axis, cheb.coeff)
cheb.val <- function(x.axis, cheb.coeff) {
  ## Funktion zur Berechnung der Y-Werte aus X-Stellen und Cheb-Koeffizienten
  ## ##
  n <- length(cheb.coeff) - 1
  cheb.t <- cheb.1st(x.axis, n)
  cheb.model <- cheb.t %*% cheb.coeff
  return(cheb.model)
}


##
#' @title Calculation of the values of the first derivation
#' @param x.axis beliebige X-Achse (Skalar oder Vektor)
#' @param cheb.coeff Chebyshev-Koeffizienten aus Least-Squares-Verfahren (Vektor)
#' @return cheb.model.deriv Erste Ableitung des gefilterten Modells (Skalar oder Vektor)
#' @description
#' cheb.deriv.1st berechnet aus den Chebyshev-Koeffizienten die Werte der ersten Ableitung
#' @examples
#' cheb.model.deriv <- cheb.deriv.1st(x.axis, cheb.coeff)
cheb.deriv.1st <- function(x.axis, cheb.coeff) {
  ## Funktion zur Berechnung der Y-Werte der Ableitung des Modells
  ## aus X-Stellen und Chebyshev-Koeffizienten
  ## ##
  if (length(x.axis) != 0) { ### Überprüfen, ob nötig
    n <- length(cheb.coeff) - 1
    m <- n + 1
    cheb.u <- cheb.2nd(x.axis, n)

    # berechnung der ableitung der polynome erster art
    # rekursionsformel 0
    # dT/dx = n * U_(n-1)
    cheb.t.deriv <- if (length(x.axis) == 1) (2:m)*t(cheb.u[,1:n]) else t((2:m)*t(cheb.u[,1:n]))
    cheb.model.deriv <- cheb.t.deriv %*% cheb.coeff[2:m]
    return(cheb.model.deriv)
  }
}


##
#' @title Calculation of the values of the second derivation
#' @param x.axis beliebige X-Achse (Skalar oder Vektor)
#' @param cheb.coeff Chebyshev-Koeffizienten aus Least-Squares-Verfahren (Vektor)
#' @return cheb.model.deriv Zweite Ableitung des gefilterten Modells (Skalar oder Vektor)
#' @description
#' cheb.deriv.2nd berechnet aus den Chebyshev-Koeffizienten die Werte der zweiten Ableitung
#' @examples
#' cheb.model.deriv.2nd <- cheb.deriv.2nd(x.axis, cheb.coeff)
cheb.deriv.2nd <- function(x.axis, cheb.coeff) {
  cheb.model.deriv.2nd <- NA
  return(cheb.model.deriv.2nd)
}


##
#' @title Curve Fitting with Chebyshev Polynomials
#' @param d Zu fittender Datensatz/Zeitreihe (Vektor)
#' @param x.axis Beliebige X-Achse (Vektor)
#' @param n Ordnung des Polynoms (Skalar)
#' @return cheb.list Berechnete Parameter (Koeffizienten, gefiltertes Modell, erste und zweite Ableitung des gefilterten Modells, Extremstellen und -Werte) (Liste)
#' @description
#' \code{cheb.fit} fittet ein Chebyshev-Polynom beliebiger Ordnung an einen Datensatz/Zeitreihe mittels Least Squares Verfahren
#' @examples
#' cheb.list <- cheb.fit(d, x.axis, n)
cheb.fit <- function(d, x.axis, n){
  x.cheb <- cheb.scale(x.axis)
  cheb.t <- cheb.1st(x.axis, n)
  cheb.u <- cheb.2nd(x.axis, n)
  m <- n + 1
  ## modell berechnungen
  # berechnung der koeffizienten des polyfits
  cheb.coeff <- solve(t(cheb.t) %*% cheb.t) %*% t(cheb.t) %*% d
  # berechnung des gefilterten modells
  cheb.model <- cheb.val(x.cheb, cheb.coeff)
  # berechnung des abgeleiteten modells
  cheb.model.deriv <- cheb.deriv.1st(x.cheb, cheb.coeff)

  # berechnung der nullstellen
  extr <- rootSolve::uniroot.all(cheb.deriv.1st, cheb.coeff = cheb.coeff, lower = (-1), upper = 1)
  # reskalierung der Nullstellen auf normale Lat- Achse
  x.extr <- cheb.rescale(extr, x.axis = x.axis)
  y.extr <- if (length(extr) != 0) cheb.val(x.axis = extr, cheb.coeff = cheb.coeff)

  cheb.list <- list(cheb.coeff = cheb.coeff, cheb.model = cheb.model, cheb.model.deriv = cheb.model.deriv, x.extr = x.extr, y.extr = y.extr)
  return(cheb.list)
}

##
#' @title Curve Fitting with Chebyshev Polynomials over Sequences
#' @description
#' Fittet ein Chebyshev Polynom beliebiger Ordnung an einen sequenzierten Datensatz/Zeitreihe mittels Least Squares Verfahren
#' @examples
#' cheb.fit.seq(d, x.axis, n, split)
cheb.fit.seq <- function(d, x.axis, n, split, dx.hr){
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
    cheb.t <- fkt.cheb.1st(x.cheb, n)

    ## erzeugung der chebyshev polynome zweiter art
    cheb.u <- fkt.cheb.2nd(x.cheb, n)

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
  cheb.list <- list(cheb.coeff = cheb.coeff, cheb.model = cheb.model, cheb.model.deriv = cheb.model.deriv, extr.x = x.extr, extr.y = y.extr)
  return(cheb.list)
}

