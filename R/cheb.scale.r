####################################################################################################
## source('~/Master_Thesis/pckg.cheb/R/cheb.scale.r')
##
##

##
## Skalierung
cheb.scale <- function(x.axis) {#, scale) {
  ## Funktion zur Skalierung von Stützpunkten
  ## von beliebigen Gittern auf [-1, 1]
  ## ##
  #  if (type == "cheb") {
  x.cheb.scaled <- (2 * (x.axis - x.axis[1]) / (max(x.axis) - min(x.axis))) - 1
  #  }
  return(x.cheb.scaled)
}
