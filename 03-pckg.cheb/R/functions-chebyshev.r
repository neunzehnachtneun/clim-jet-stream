## source('~/01-Master-Thesis/02-code-git/03-pckg.cheb/R/functions-chebyshev.r')
##
## library(devtools)
## library(roxygen2)
## library(testthat)
##
## Build and Reload Package:  'Ctrl + Shift + B'
## Check Package:             'Ctrl + Shift + E'
## Test Package:              'Ctrl + Shift + T'
## Document:                  'Ctrl + Shift + D'


##
#' @title Scaling of X-Axis (as vector or as scalar) to [-1,1]
#' @param x.axis ursprüngliche beliebige X-Achse (Vektor)
#' @param x.val Einzelwert auf X-Achse (Skalar)
#' @return x.cheb.scaled Abhängig von Eingangsparametern: Skalierte X-Achse (Vektor) oder skalierter Einzelwert anhand vorgegebener X-Achse
#' @description
#' \code{cheb.scale} skaliert beliebige X-Achse auf Achse, die für Polynom-fits verträglich ist.
#' @export
cheb.scale <- function(x.axis, x.val = NA) {#, scale) {
  ## Funktion zur Skalierung von Stützpunkten
  ## von beliebigen Gittern auf [-1, 1]
  ## ##
  if ( is.na(x.val) == TRUE ) { # Für Skalierung von vektoriellen Achsen
    x.cheb.scaled <- 2 * (x.axis - min(x.axis)) / (max(x.axis) - min(x.axis)) - 1
  }
  if ( is.na(x.val) == FALSE ) { # Für Skalierung von Einzelwert(en) auf Achse
    x.cheb.scaled <- 2 * (x.val - min(x.axis)) / (max(x.axis) - min(x.axis)) - 1
  }
  return(x.cheb.scaled)
}


##
#' @title Rescaling of X-Axis (as vector or as scalar)
#' @param x.cheb skalierte X-Achse (Skalar oder Vektor)
#' @param x.axis beliebige X-Achse (Vektor)
#' @return x.rescaled reskalierte X-Achse (Skalar oder Sektor)
#' @description
#' cheb.rescale reskaliert die für den Fit erzeugte Achse auf die Ursprüngliche
#' @export
cheb.rescale <- function(x.cheb, x.axis) {
  ## Funktion zur Reskalierung vom [-1, 1]-Gitter
  ## auf das Ursprungsgitter (in diesem Fall - Lat)
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
#' @export
cheb.1st <- function(x.axis, n){
  ## Funktion zur Erzeugung von Chebyshev-Polynomen Erster Art
  ## ## Fehlerabfrage nur mit Schwellwert. Korrigieren! Auch cheb.2nd
  x.cheb <- if (length(x.axis) > 7) cheb.scale(x.axis) else x.axis
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
#' @export
cheb.2nd <- function(x.axis, n){
  ## Funktion zur Erzeugung von Chebyshev-Polynomen Zweiter Art
  ## ##
  x.cheb <- if (length(x.axis) > 7) cheb.scale(x.axis) else x.axis
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
#' @export
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
#' @export
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

    cheb.t.deriv.1st <- if (length(x.axis) == 1) (1:n)*t(cheb.u[,1:n]) else t((1:n) * t(cheb.u[,1:n]))
    cheb.t.deriv.1st <- cbind(0, cheb.t.deriv.1st)
    cheb.model.deriv.1st <- cheb.t.deriv.1st %*% cheb.coeff

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
#' @export
cheb.deriv.2nd <- function(x.axis, cheb.coeff) {
  n <- length(cheb.coeff) - 1
  #  m <- n + 1
  l <- length(x.axis)
  cheb.t <- cheb.1st(x.axis, n)
  cheb.u <- cheb.2nd(x.axis, n)
  x.cheb <- cheb.scale(x.axis)

  cheb.t.deriv.2nd <- t((0:n) * t(t((0:n + 1) * t(cheb.t )) - cheb.u)) / (x.cheb ** 2 - 1)
  cheb.t.deriv.2nd[1,] <- (-1) ** (0:n) * ((0:n) ** 4 - (0:n) ** 2) / (3)
  cheb.t.deriv.2nd[l,] <- ((0:n) ** 4 - (0:n) ** 2) / (3)

  cheb.model.deriv.2nd <- cheb.t.deriv.2nd %*% cheb.coeff

  return(cheb.model.deriv.2nd)
}


##
#' @title Curve Fitting with Chebyshev Polynomials
#' @param d Zu fittender Datensatz/Zeitreihe (Vektor)
#' @param x.axis Beliebige X-Achse (Vektor)
#' @param n Ordnung des Polynoms (Skalar)
#' @return cheb.model Werte des gefilterten Modells
#' @description
#' \code{cheb.fit} fittet ein Chebyshev-Polynom beliebiger Ordnung an einen Datensatz/Zeitreihe mittels Least Squares Verfahren
#' @export
cheb.fit <- function(d, x.axis, n){
  ## Herausfiltern von fehlenden Werten (NAs)
  mss.ind  <- which(is.na(d))

  # Fallunterscheidung für NAs im Datensatz
  if (length(mss.ind) >= 1) {
    cheb.t <- cheb.1st(x.axis[-mss.ind], n)
  } else {
    cheb.t <- cheb.1st(x.axis, n)
  }

  ## modell berechnungen
  # berechnung der koeffizienten des polyfits
  if (length(mss.ind) >= 1) {
    cheb.coeff <- solve(t(cheb.t) %*% cheb.t) %*% t(cheb.t) %*% d[-mss.ind]
  } else {
    cheb.coeff <- solve(t(cheb.t) %*% cheb.t) %*% t(cheb.t) %*% d
  }

  # berechnung des gefilterten modells
  x.cheb <- cheb.scale(x.axis)
  cheb.model <- cheb.model.filter(x.cheb, cheb.coeff)

  # Übergabe der Variablen
  return(cheb.model)
}


##
#' @title Curve Fitting with Chebyshev Polynomials and Finding of its Roots
#' @param d Zu fittender Datensatz/Zeitreihe (Vektor)
#' @param x.axis Beliebige X-Achse (Vektor)
#' @param n Ordnung des Polynoms (Skalar)
#' @param roots.bound.l Unterer Rand des Bereichs der Nullstellensuche
#' @param roots.bound.u Oberer Rand des Bereichs der Nullstellensuche
#' @return cheb.list Liste berechneter Parameter (Koeffizienten, gefiltertes Modell, erste und zweite Ableitung des gefilterten Modells, Extremstellen und -Werte)
#' @description
#' \code{cheb.fit.extr} fittet ein Chebyshev-Polynom beliebiger Ordnung an einen Datensatz/Zeitreihe mittels Least Squares Verfahren
#' @export
#' @importFrom rootSolve uniroot.all
cheb.find.extr <- function(d, x.axis, n, roots.bound.l = NA, roots.bound.u = NA){

  x.cheb <- cheb.scale(x.axis)
  cheb.t <- cheb.1st(x.axis, n)

  ## modell berechnungen
  # berechnung der koeffizienten des polyfits
  cheb.coeff <- solve(t(cheb.t) %*% cheb.t) %*% t(cheb.t) %*% d

  # Berechnung der Extrema über Nullstellen der ersten Ableitung
  extr <- uniroot.all(cheb.deriv.1st, cheb.coeff = cheb.coeff, lower = -1, upper = 1)
  # reskalierung der Extrema auf normale Lat- Achse
  extr.x <- if (length(extr) != 0) cheb.rescale(extr, x.axis = x.axis)

  # Überprüfen, welche Extrema im gesuchten Bereich liegen
  if (!is.na(roots.bound.l) | !is.na(roots.bound.u)) {
    ind <- which(extr.x >= roots.bound.l & extr.x <= roots.bound.u)
    extr <- extr[ind]
    extr.x <- extr.x[ind]
  }
  extr.y <- if (length(extr.x) != 0) cheb.model.filter(x.axis = extr, cheb.coeff = cheb.coeff)
  extr.deriv.2nd <- if (length(extr) != 0) cheb.deriv.2nd(x.axis = extr, cheb.coeff = cheb.coeff)

  cheb.list <- list(extr.x = extr.x,
                    extr.y = extr.y,
                    extr.deriv.2nd = extr.deriv.2nd)
  return(cheb.list)
}


##
#' @title Routine to find maximum values of a curve
#' @param d Zu fittender Datensatz
#' @param x.axis beliebiege X-Achse
#' @param n Ordnung des Skalars
#' @param max.bound.l Unterer Rand des Bereichs der Maxima-Suche
#' @param max.bound.u Oberer Rand des Bereichs der Maxima-Suche
#' @return max.list Positionen und Werte der Maxima als Liste
#' @description
#' \code{cheb.find.max} fittet ein Polynom und sucht mittel dessen Ableitung die Maxima des Datensatzes
#' @export
#' @importFrom rootSolve uniroot.all
cheb.find.max <- function(d, x.axis, n, max.bound.l = NA, max.bound.u = NA){
  x.cheb <- cheb.scale(x.axis)
  cheb.t <- cheb.1st(x.axis, n)

  ## modell berechnungen
  # koeffizienten d poly fits
  cheb.coeff <- solve(t(cheb.t) %*% cheb.t) %*% t(cheb.t) %*% d

  # berechnen d maxima
  # nullstellen d ableitung
  extr <- uniroot.all(cheb.deriv.1st, cheb.coeff = cheb.coeff, lower = -1, upper = 1)

  # reskalierung der Extrema auf normale Lat- Achse
  extr.x <- cheb.rescale(extr, x.axis = x.axis)

  # Überprüfen, welche Extrema im gesuchten Bereich liegen
  if (!is.na(max.bound.l) | !is.na(max.bound.u)) {
    ind <- which(extr.x >= max.bound.l & extr.x <= max.bound.u)
    extr <- extr[ind]
    extr.x <- extr.x[ind]
  }

  # Überprüfen, welche Extrema Maxima sind
  if (length(extr) != 0) {
    extr.deriv.2nd <- cheb.deriv.2nd(x.axis = extr, cheb.coeff = cheb.coeff)

    # filtern der maxima | wert d zweiten ableitung < 0
    ind.max <- which(extr.deriv.2nd < 0)
    if (length(ind.max) != 0) {
      max.x <- extr.x[ind.max]
      max.y <- cheb.model.filter(x.axis = extr[ind.max], cheb.coeff = cheb.coeff)
    } else {
      max.x <- NA; max.y <- NA
    }
  } else {
    max.x <- NA; max.y <- NA;
  }
  max.list <- list(max.x = max.x,
                   max.y = max.y)
  return(max.list)
}





