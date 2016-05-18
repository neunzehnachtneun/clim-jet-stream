####################################################################################################
## source('~/Master_Thesis/pckg.cheb/R/cheb.fit.r')
##
##

#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

##
## Variante 1 - Least squares fit über gesamten Datensatz
cheb.fit <- function(d, x.axis, n){
  x.cheb <- cheb.scale(x.axis)
  cheb.t <- cheb.1st(x.axis, n)
  cheb.u <- cheb.2nd(x.axis, n)
  m <- n + 1
  ## modell berechnungen
  # berechnung der koeffizienten des polyfits
  cheb.coeff <- solve(t(cheb.t) %*% cheb.t) %*% t(cheb.t) %*% d
  # berechnung des gefilterten modells
  cheb.model <- pckg:::cheb.val(x.cheb, cheb.coeff)
  # berechnung des abgeleiteten modells
  cheb.model.deriv <- pckg:::cheb.deriv(x.cheb, cheb.coeff)

  # berechnung der nullstellen
  extr <- uniroot.all(fkt.cheb.deriv, cheb.coeff = cheb.coeff, lower = (-1), upper = 1)
  # reskalierung der Nullstellen auf normale Lat- Achse
  x.extr <- pckg.cheb:::cheb.rescale(extr, x.axis = x.axis)
  y.extr <- if (length(extr) != 0) pckg.cheb:::cheb.val(x.axis = extr, cheb.coeff = cheb.coeff)

  cheb.list <- list(cheb.coeff = cheb.coeff, cheb.model = cheb.model, cheb.model.deriv = cheb.model.deriv, x.extr = x.extr, y.extr = y.extr)
  return(cheb.list)
}

