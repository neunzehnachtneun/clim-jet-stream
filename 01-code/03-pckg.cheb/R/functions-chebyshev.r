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
#' @param x_axis urspruengliche beliebige X-Achse (Vektor)
#' @param x_val Einzelwert auf X-Achse (Skalar)
#' @return x_cheb_scaled Abhaengig von Eingangsparametern: Skalierte X-Achse (Vektor) oder skalierter Einzelwert anhand vorgegebener X-Achse
#' @description
#' \code{cheb_scale} skaliert beliebige X-Achse auf Achse, die fuer Polynom-fits vertraeglich ist.
#' @export
cheb_scale <- function(x_axis, x_val = NA) {#, scale) {
  ## Funktion zur Skalierung von Stuetzpunkten
  ## von beliebigen Gittern auf [-1, 1]
  ## ##
  if ( is.na(x_val) == TRUE ) { # Fuer Skalierung von vektoriellen Achsen
    x_cheb_scaled <- 2 * (x_axis - min(x_axis)) / (max(x_axis) - min(x_axis)) - 1
  }
  if ( is.na(x_val) == FALSE ) { # Fuer Skalierung von Einzelwert(en) auf Achse
    x_cheb_scaled <- 2 * (x_val - min(x_axis)) / (max(x_axis) - min(x_axis)) - 1
  }
  return(x_cheb_scaled)
}


##
#' @title Rescaling of X-Axis (as vector or as scalar)
#' @param x_cheb skalierte X-Achse (Skalar oder Vektor)
#' @param x_axis beliebige X-Achse (Vektor)
#' @return x_rescaled reskalierte X-Achse (Skalar oder Sektor)
#' @description
#' cheb_rescale reskaliert die fuer den Fit erzeugte Achse auf die Urspruengliche
#' @export
cheb_rescale <- function(x_cheb, x_axis) {
  ## Funktion zur Reskalierung vom [-1, 1]-Gitter
  ## auf das Ursprungsgitter (in diesem Fall - Lat)
  x_rescaled <- (1/2 * (x_cheb + 1) * (max(x_axis) - min(x_axis))) + x_axis[1]
  return(x_rescaled)
}


##
#' @title Generating Chebyshev Polynomials of first kind
#' @param x_axis beliebige X-Achse (Vektor)
#' @param n Ordnung des Polynoms (Skalar)
#' @return cheb_t Chebyshev-Polynome Erster Art (Vektor)
#' @description
#' cheb_1st erzeugt Chebyshev Polynome erster Art aus beliebiger X-Achse
#' @export
cheb_1st <- function(x_axis, n){
  ## Funktion zur Erzeugung von Chebyshev-Polynomen Erster Art
  ## ## Fehlerabfrage nur mit Schwellwert. Korrigieren! Auch cheb_2nd
  x_cheb <- if (length(x_axis) > 7) cheb_scale(x_axis) else x_axis
  m <- n + 1
  # Rekursionsformel Wiki / Bronstein
  cheb_t_0 <- 1;  cheb_t_1 <- x_cheb;
  cheb_t <- cbind(cheb_t_0, cheb_t_1)
  if (n >= 2) {
    for (i in 3:m) {
      cheb_t_i <- 2 * x_cheb * cheb_t[,(i - 1)] - cheb_t[,(i - 2)]
      cheb_t <- cbind(cheb_t, cheb_t_i)
      rm(cheb_t_i)
    }
  }
  return(cheb_t)
}


##
#' @title Generating Chebyshev Polynomials of second kind
#' @param x_axis beliebigie X-Achse (Vektor)
#' @param n Ordnung des Polynoms (Skalar)
#' @return cheb_u Chebyshev-Polynome Zweiter Art (Vektor)
#' @description
#' cheb_2nd erzeugt Chebyshev Polynome zweiter Art aus beliebiger X-Achse
#' @export
cheb_2nd <- function(x_axis, n){
  ## Funktion zur Erzeugung von Chebyshev-Polynomen Zweiter Art
  ## ##
  x_cheb <- if (length(x_axis) > 7) cheb_scale(x_axis) else x_axis
  m <- n + 1
  cheb_u_0 <- 1; cheb_u_1 <-  2*x_cheb
  cheb_u <- cbind(cheb_u_0, cheb_u_1)
  if (n >= 2) {
    for (i in 3:m) {
      cheb_u_i <- 2 * x_cheb * cheb_u[,(i - 1)] - cheb_u[,(i - 2)]
      cheb_u <- cbind(cheb_u, cheb_u_i)
      rm(cheb_u_i)
    }
  }
  return(cheb_u)
}


##
#' @title Calculation of Values of the model fit
#' @param x_axis beliebige X-Achse (Skalar oder Vektor)
#' @param cheb_coeff Chebyshev-Koeffizienten aus Least-Squares-Verfahren (Vektor)
#' @return cheb_model gefiltertes Modell (Skalar oder Vektor)
#' @description
#' cheb_model berechnet aus den Chebyshev-Koeffizienten die Y-Werte
#' @export
cheb_model_filter <- function(x_axis, cheb_coeff) {
  ## Funktion zur Berechnung der Y-Werte aus X-Stellen und Cheb-Koeffizienten
  ## ##
  n <- length(cheb_coeff) - 1
  cheb_t <- cheb_1st(x_axis, n)
  cheb_model <- cheb_t %*% cheb_coeff
  return(cheb_model)
}


##
#' @title Calculation of the values of the first derivation
#' @param x_axis beliebige X-Achse (Skalar oder Vektor)
#' @param cheb_coeff Chebyshev-Koeffizienten aus Least-Squares-Verfahren (Vektor)
#' @return cheb_model_deriv_1st Erste Ableitung des gefilterten Modells (Skalar oder Vektor)
#' @description
#' cheb_deriv_1st berechnet aus den Chebyshev-Koeffizienten die Werte der ersten Ableitung
#' @export
cheb_deriv_1st <- function(x_axis, cheb_coeff) {
  ## Funktion zur Berechnung der Y-Werte der Ableitung des Modells
  ## aus X-Stellen und Chebyshev-Koeffizienten
  ## ##
  if (length(x_axis) != 0) { ### ueberpruefen, ob noetig
    n <- length(cheb_coeff) - 1
    m <- n + 1
    cheb_u <- cheb_2nd(x_axis, n)

    # berechnung der ableitung der polynome erster art
    # rekursionsformel 0
    # dT/dx = n * U_(n-1)

    cheb_t_deriv_1st <- if (length(x_axis) == 1) (1:n)*t(cheb_u[,1:n]) else t((1:n) * t(cheb_u[,1:n]))
    cheb_t_deriv_1st <- cbind(0, cheb_t_deriv_1st)
    cheb_model_deriv_1st <- cheb_t_deriv_1st %*% cheb_coeff

    return(cheb_model_deriv_1st)
  }
}


##
#' @title Calculation of the values of the second derivation
#' @param x_axis beliebige X-Achse (Skalar oder Vektor)
#' @param cheb_coeff Chebyshev-Koeffizienten aus Least-Squares-Verfahren (Vektor)
#' @return cheb_model_deriv Zweite Ableitung des gefilterten Modells (Skalar oder Vektor)
#' @description
#' cheb_deriv_2nd berechnet aus den Chebyshev-Koeffizienten die Werte der zweiten Ableitung
#' @export
cheb_deriv_2nd <- function(x_axis, cheb_coeff) {
  n <- length(cheb_coeff) - 1
  #  m <- n + 1
  l <- length(x_axis)
  cheb_t <- cheb_1st(x_axis, n)
  cheb_u <- cheb_2nd(x_axis, n)
  x_cheb <- cheb_scale(x_axis)

  cheb_t_deriv_2nd <- t((0:n) * t(t((0:n + 1) * t(cheb_t )) - cheb_u)) / (x_cheb ** 2 - 1)
  cheb_t_deriv_2nd[1,] <- (-1) ** (0:n) * ((0:n) ** 4 - (0:n) ** 2) / (3)
  cheb_t_deriv_2nd[l,] <- ((0:n) ** 4 - (0:n) ** 2) / (3)

  cheb_model_deriv_2nd <- cheb_t_deriv_2nd %*% cheb_coeff

  return(cheb_model_deriv_2nd)
}


##
#' @title Curve Fitting with Chebyshev Polynomials
#' @param d Zu fittender Datensatz/Zeitreihe (Vektor)
#' @param x_axis Beliebige X-Achse (Vektor)
#' @param n Ordnung des Polynoms (Skalar)
#' @return cheb_model Werte des gefilterten Modells
#' @description
#' \code{cheb_fit} fittet ein Chebyshev-Polynom beliebiger Ordnung an einen Datensatz/Zeitreihe mittels Least Squares Verfahren
#' @export
cheb_fit <- function(d, x_axis, n){
  ## Herausfiltern von fehlenden Werten (NAs)
  mss_ind  <- which(is.na(d))

  # Fallunterscheidung fuer NAs im Datensatz
  if (length(mss_ind) >= 1) {
    cheb_t <- cheb_1st(x_axis[-mss_ind], n)
  } else {
    cheb_t <- cheb_1st(x_axis, n)
  }

  ## modell berechnungen
  # berechnung der koeffizienten des polyfits
  if (length(mss_ind) >= 1) {
    cheb_coeff <- solve(t(cheb_t) %*% cheb_t) %*% t(cheb_t) %*% d[-mss_ind]
  } else {
    cheb_coeff <- solve(t(cheb_t) %*% cheb_t) %*% t(cheb_t) %*% d
  }

  # berechnung des gefilterten modells
  x_cheb <- cheb_scale(x_axis)
  cheb_model <- cheb_model_filter(x_cheb, cheb_coeff)

  # uebergabe der Variablen
  return(cheb_model)
}


##
#' @title Curve Fitting with Chebyshev Polynomials and Finding of its Roots
#' @param d Zu fittender Datensatz/Zeitreihe (Vektor)
#' @param x_axis Beliebige X-Achse (Vektor)
#' @param n Ordnung des Polynoms (Skalar)
#' @param roots_bound_lower Unterer Rand des Bereichs der Nullstellensuche
#' @param roots_bound_upper Oberer Rand des Bereichs der Nullstellensuche
#' @return cheb_list Liste berechneter Parameter (Koeffizienten, gefiltertes Modell, erste und zweite Ableitung des gefilterten Modells, Extremstellen und -Werte)
#' @description
#' \code{cheb_fit.extr} fittet ein Chebyshev-Polynom beliebiger Ordnung an einen Datensatz/Zeitreihe mittels Least Squares Verfahren
#' @export
#' @importFrom rootSolve uniroot.all
cheb_find_extr <- function(d, x_axis, n, roots_bound_lower = NA, roots_bound_upper = NA){

  x_cheb <- cheb_scale(x_axis)
  cheb_t <- cheb_1st(x_axis, n)

  ## modell berechnungen
  # berechnung der koeffizienten des polyfits
  cheb_coeff <- solve(t(cheb_t) %*% cheb_t) %*% t(cheb_t) %*% d

  # Berechnung der Extrema ueber Nullstellen der ersten Ableitung
  extr <- uniroot.all(cheb_deriv_1st, cheb_coeff = cheb_coeff, lower = -1, upper = 1)
  # reskalierung der Extrema auf normale Lat- Achse
  extr_x <- if (length(extr) != 0) cheb_rescale(extr, x_axis = x_axis)

  # ueberpruefen, welche Extrema im gesuchten Bereich liegen
  if (!is.na(roots_bound_lower) | !is.na(roots_bound_upper)) {
    ind <- which(extr_x >= roots_bound_lower & extr_x <= roots_bound_upper)
    extr <- extr[ind]
    extr_x <- extr_x[ind]
  }
  extr.y <- if (length(extr_x) != 0) cheb_model_filter(x_axis = extr, cheb_coeff = cheb_coeff)
  extr_deriv_2nd <- if (length(extr) != 0) cheb_deriv_2nd(x_axis = extr, cheb_coeff = cheb_coeff)

  cheb_list <- list(extr_x = extr_x,
                    extr.y = extr.y,
                    extr_deriv_2nd = extr_deriv_2nd)
  return(cheb_list)
}


##
#' @title Routine to find maximum values of a curve
#' @param d Zu fittender Datensatz
#' @param x_axis beliebiege X-Achse
#' @param n Ordnung des Skalars
#' @param maxima_bound_lower Unterer Rand des Bereichs der Maxima-Suche
#' @param maxima_bound_upper Oberer Rand des Bereichs der Maxima-Suche
#' @return max_list Positionen und Werte der Maxima als Liste
#' @description
#' \code{cheb_find_max} fittet ein Polynom und sucht mittel dessen Ableitung die Maxima des Datensatzes
#' @export
#' @importFrom rootSolve uniroot.all
cheb_find_max <- function(d, x_axis, n, maxima_bound_lower = NA, maxima_bound_upper = NA){
  x_cheb <- cheb_scale(x_axis)
  cheb_t <- cheb_1st(x_axis, n)

  ## modell berechnungen
  # koeffizienten d poly fits
  cheb_coeff <- solve(t(cheb_t) %*% cheb_t) %*% t(cheb_t) %*% d

  # berechnen d maxima
  # nullstellen d ableitung
  extr <- uniroot.all(cheb_deriv_1st, cheb_coeff = cheb_coeff, lower = -1, upper = 1)

  # reskalierung der Extrema auf normale Lat- Achse
  extr_x <- cheb_rescale(extr, x_axis = x_axis)

  # ueberpruefen, welche Extrema im gesuchten Bereich liegen
  if (!is.na(maxima_bound_lower) | !is.na(maxima_bound_upper)) {
    ind <- which(extr_x >= maxima_bound_lower & extr_x <= maxima_bound_upper)
    extr <- extr[ind]
    extr_x <- extr_x[ind]
  }

  # ueberpruefen, welche Extrema Maxima sind
  if (length(extr) != 0) {
    extr_deriv_2nd <- cheb_deriv_2nd(x_axis = extr, cheb_coeff = cheb_coeff)

    # filtern der maxima | wert d zweiten ableitung < 0
    ind_max <- which(extr_deriv_2nd < 0)
    if (length(ind_max) != 0) {
      max_x <- extr_x[ind_max]
      max_y <- cheb_model_filter(x_axis = extr[ind_max], cheb_coeff = cheb_coeff)
    } else {
      max_x <- NA; max_y <- NA
    }
  } else {
    max_x <- NA; max_y <- NA;
  }
  max_list <- list(max_x = max_x,
                   max_y = max_y)
  return(max_list)
}





