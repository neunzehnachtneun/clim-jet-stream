####################################################################################################
## source('~/Master_Thesis/pckg.cheb/R/cheb.fit.seq.r')
##
##

## Variante 2 - Least squares fit über Sequenzierungen des Datensatzes
## Schnitt des Datensatzes in <split> Sequenzen

cheb.fit.seq <- function(x, d, n, split, dx.hr){
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
