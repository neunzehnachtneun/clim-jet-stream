## source('~/Master_Thesis/pckg.cheb/R/functions-chebyshev.r')
##
## library(devtools)
## library(roxygen2)
##
## Build and Reload Package:  'Ctrl + Shift + B'
## Check Package:             'Ctrl + Shift + E'
## Test Package:              'Ctrl + Shift + T'


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
#' cheb.model berechnet aus den Chebyshev-Koeffizienten die Y-Werte
#' @examples
#' cheb.model <- cheb.model.filter(x.axis, cheb.coeff)
cheb.model.filter <- function(x.axis, cheb.coeff) {
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
#' @return cheb.model.deriv.1st Erste Ableitung des gefilterten Modells (Skalar oder Vektor)
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
    cheb.model.deriv.1st <- cheb.t.deriv %*% cheb.coeff[2:m]
    return(cheb.model.deriv.1st)
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
  n <- length(cheb.coeff) - 1
  m <- n + 1
  cheb.t <- cheb.1st(x.axis, n)
  cheb.u <- cheb.2nd(x.axis, n)
  x.cheb <- cheb.scale(x.axis)
  cheb.t.deriv.2nd <- t((((1:m) ** 2) + (1:m)) %*% t(1 / (x.cheb ** 2 - 1)) * t(cheb.t - cheb.u))
  cheb.t.deriv.2nd[1,] <- (-1) * ((1:m) ** 4 - (1:m) ** 2) / (3)
  cheb.t.deriv.2nd[length(x.cheb),] <- ((1:m) ** 4 - (1:m) ** 2) / (3)
  cheb.model.deriv.2nd <- cheb.t.deriv.2nd %*% cheb.coeff
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
  library(rootSolve)
  x.cheb <- cheb.scale(x.axis)
  cheb.t <- cheb.1st(x.axis, n)
#  cheb.u <- cheb.2nd(x.axis, n)
#  m <- n + 1
  ## modell berechnungen
  # berechnung der koeffizienten des polyfits
  cheb.coeff <- solve(t(cheb.t) %*% cheb.t) %*% t(cheb.t) %*% d
  # berechnung des gefilterten modells
  cheb.model <- cheb.model.filter(x.cheb, cheb.coeff)
  # berechnung des abgeleiteten modells
  cheb.model.deriv.1st <- cheb.deriv.1st(x.cheb, cheb.coeff)

  # berechnung der nullstellen
  extr <- rootSolve::uniroot.all(cheb.deriv.1st, cheb.coeff = cheb.coeff, lower = (-1), upper = 1)
  # reskalierung der Nullstellen auf normale Lat- Achse
  x.extr <- if (length(extr) != 0) cheb.rescale(extr, x.axis = x.axis)
  y.extr <- if (length(extr) != 0) cheb.model.filter(x.axis = extr, cheb.coeff = cheb.coeff)

  cheb.list <- list(cheb.coeff = cheb.coeff, cheb.model = cheb.model, cheb.model.deriv.1st = cheb.model.deriv.1st, x.extr = x.extr, y.extr = y.extr)
  return(cheb.list)
}



##
#' @title Curve Fitting with Chebyshev Polynomials over Sequences
#' @description
#' Fittet ein Chebyshev Polynom beliebiger Ordnung an einen sequenzierten Datensatz/Zeitreihe mittels Least Squares Verfahren
#' @examples
#' cheb.fit.seq(d, x.axis, n, l)
cheb.fit.seq <- function(d, x.axis, n, l){
  library(rootSolve)
  x.mat <- matrix(x.axis, ncol = l, byrow = TRUE)
  d.mat <- matrix(d, ncol = l, byrow = TRUE)

  # schleife über sequenzen des Datensatzes
  for (i in 1:length(x.mat[,1])) {
    # print(i)
    # erstellung der sequenzen
    x.seq <- if (i == 1) c(x.mat[i,]) else c(x.mat[(i - 1), dim(x.mat)[2]], x.mat[i,])
    x.cheb.seq <- cheb.scale(x.seq)
    d.seq <- if (i == 1) c(d.mat[i,]) else c(d.mat[(i - 1), dim(d.mat)[2]], d.mat[i,])
    cheb.t.seq <- cheb.1st(x.seq, n)
    ## modell berechnungen
    # berechnung der koeffizienten des polyfits
    cheb.coeff.seq <- solve(t(cheb.t.seq) %*% cheb.t.seq) %*% t(cheb.t.seq) %*% d.seq
    cheb.coeff <- if (i == 1) cheb.coeff.seq else cbind(cheb.coeff, cheb.coeff.seq)
    # berechnung des gefilterten modells
    cheb.model.seq <- cheb.model.filter(x.cheb.seq, cheb.coeff.seq)
    cheb.model <- if (i == 1) cheb.model.seq else c(cheb.model, cheb.model.seq[2:(l+1)])
    # berechnung des abgeleiteten modells
    cheb.model.deriv.1st.seq <- cheb.deriv.1st(x.cheb.seq, cheb.coeff.seq)
    cheb.model.deriv.1st <- if (i == 1) cheb.model.deriv.1st.seq else c(cheb.model.deriv.1st, cheb.model.deriv.1st.seq[2:(l+1)])

    # berechnung der nullstellen
    extr.seq <- rootSolve::uniroot.all(cheb.deriv.1st, cheb.coeff = cheb.coeff.seq, lower = (-1), upper = 1)
    # reskalierung der Nullstellen auf normale Lat- Achse
    x.extr.seq <- if (length(extr.seq) != 0) cheb.rescale(extr.seq, x.axis = x.seq)
    y.extr.seq <- if (length(extr.seq) != 0) cheb.model.filter(x.axis = extr.seq, cheb.coeff = cheb.coeff.seq)
    #
    if (exists("x.extr.seq") == TRUE & exists("x.extr") == FALSE) {
      x.extr <- x.extr.seq
      y.extr <- y.extr.seq
    } else if (exists("x.extr.seq") == TRUE & exists("x.extr") == TRUE) {
      x.extr <- c(x.extr, x.extr.seq)
      y.extr <- c(y.extr, y.extr.seq)
    }
  }

  ## übergabe der variablen als liste
  cheb.list <- list(cheb.model = cheb.model, cheb.model.deriv.1st = cheb.model.deriv.1st, extr.x = x.extr, extr.y = y.extr)
  return(cheb.list)
}

