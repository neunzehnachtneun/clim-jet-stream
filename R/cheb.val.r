####################################################################################################
## source('~/Master_Thesis/pckg.cheb/R/cheb.val.r')
##
##

##
## Y-Werte aus X und Cheb-Koeff.
cheb.val <- function(x.axis, cheb.coeff) {
  ## Funktion zur Berechnung der Y-Werte aus X-Stellen und Cheb-Koeffizienten
  ## ##
  n <- length(cheb.coeff) - 1
  cheb.t <- pckg.cheb:::cheb.1st(x.axis, n)
  cheb.model <- cheb.t %*% cheb.coeff
  return(cheb.model)
}
