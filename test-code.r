## source('~/Master_Thesis/Code/test-code.r')

## variablen für test
x <- seq(0:29)
d <- rnorm(30, mean = 0, sd = 2)
n <- 4
split <- 6
dx.hr <- 0.01

## test nullstellensuche
library(rootSolve)
fun <- function(x) cos(2*x) ** 3
curve(fun, from = -1, to = 1)


curve(cheb.poly.val(x, coeff.cheb = m.cheb.seq), from = (-1), to = (1))
roots <- uniroot.all(cheb.poly.val, coeff.cheb = m.cheb.seq,  lower = (-1), upper = 1)
points(roots, y = rep(0, length(roots)), pch = 16, cex = 1)

curve(cheb.poly.deriv(x, coeff.cheb = m.cheb.seq), from = (-1), to = (1))
abline(h = 0, lty = 3)
roots.deriv <- uniroot.all(cheb.poly.deriv, coeff.cheb = m.cheb.seq, lower = (-1), upper = 1)
points(roots.deriv, y = rep(0, length(roots.deriv)), pch = 16, cex = 1)

roots.deriv <- uniroot.all(fkt.cheb.deriv, cheb.coeff = tst.cheb.list$cheb.coeff[4,], lower = (-1), upper = 1)

##
a <- array(rnorm(8, mean = 0, sd = 2), dim=c(2,2,2))
b <- array(rnorm(8, mean = 0, sd = 2), dim=c(2,2,2))
c <- array(rnorm(8, mean = 0, sd = 2), dim=c(2,2,2))


## hohe auflösung
if (dx.hr > 0) {
  # hochaufgelöste stützpunkte für modellentwicklung
  x.cheb.hr <- seq(-1,1,dx.hr)
  # erzeugung der chebyshev polynome erster art
  cheb.t.hr.0 <- 1;  cheb.t.hr.1 <- x.cheb.hr;
  cheb.t.hr <- cbind(cheb.t.hr.0, cheb.t.hr.1)
  if (n >= 2) {
    for (ll in 3:m) {
      cheb.t.hr.i <- 2*x.cheb.hr*cheb.t.hr[,(ll - 1)] - cheb.t.hr[,(ll - 2)]
      cheb.t.hr <- cbind(cheb.t.hr, cheb.t.hr.i)
      rm(cheb.t.hr.i)
    }
  }
  # erzeugung der chebyshev polynome zweiter art
  # rekursionsformel quelle ??? wiki???
  cheb.u.hr.0 <- 1; cheb.u.hr.1 <-  2*x.cheb.hr
  cheb.u.hr <- cbind(cheb.u.hr.0, cheb.u.hr.1)
  if (n >= 2) {
    for (mm in 3:m) {
      cheb.u.hr.i <- 2*x.cheb.hr*cheb.u.hr[,(mm - 1)] - cheb.u.hr[,(mm - 2)]
      cheb.u.hr <- cbind(cheb.u.hr, cheb.u.hr.i)
      rm(cheb.u.hr.i)
    }
  }
  # berechnung der ableitung der polynome
  # rekursionsformel quelle ??? wiki???
  deriv.cheb.t.hr <- t((2:m)*t(cheb.u.hr[,2:m]))
}



tst.cheb.list <- fkt.cheb.fit.seq(x = lat.era.t63, d = d, n = n, split = split, dx.hr = 0)
str(tst.cheb.list)

plot(lat.era.t63, uwind.monmean[1,,1])
points(cheb.list[[1,1]]$extr.x, cheb.list[[1,1]]$extr.y, pch = 20)
lines(lat.era.t63, c(cheb.list[[1,1]]$cheb.model), lty = 3)

plot(lat.era.t63, uwind.monmean[2,,1])
points(cheb.list[[2,1]]$extr.x, cheb.list[[2,1]]$extr.y, pch = 20)
lines(lat.era.t63, c(cheb.list[[2,1]]$cheb.model), lty = 3)

plot(lat.era.t63, uwind.monmean[3,,1])
points(cheb.list[[3,1]]$extr.x, cheb.list[[3,1]]$extr.y, pch = 20)
lines(lat.era.t63, c(cheb.list[[3,1]]$cheb.model), lty = 3)


plot(lat.era.t63, uwind.monmean[4,,1])
points(cheb.list[[4,1]]$extr.x, cheb.list[[4,1]]$extr.y, pch = 20)
lines(lat.era.t63, c(cheb.list[[4,1]]$cheb.model), lty = 3)


plot(lat.era.t63, c(tst.cheb.list$cheb.model))
points(tst.cheb.list$extr.x, tst.cheb.list$extr.y, pch = 20)


## Schreiben von Datei aus Liste mit unterschiedlicher Länge
a <- c(NA, NA, NA)
b <- c(1,2)



vec.a <- cheb.list[[2,1]]$extr.x
while (length(vec.a) != 11) {
  vec.a <- c(vec.a, NA)
}



fkt.cheb.1st <- function(x, n){
  x.cheb <- fkt.cheb.scale(x)
  m <- n + 1
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

fkt.cheb.2nd <- function(x, n){
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







fkt.cheb.fit <- function(x, d, n){
  x.cheb <- fkt.cheb.scale(x)
  cheb.t <- fkt.cheb.1st(x, n)
  cheb.u <- fkt.cheb.2nd(x, n)
  m <- n + 1
  
  ## modell berechnungen
  # berechnung der koeffizienten des polyfits
  cheb.coeff <- solve(t(cheb.t) %*% cheb.t) %*% t(cheb.t) %*% d
  # berechnung des gefilterten modells
  cheb.model <- fkt.cheb.val(x.cheb, cheb.coeff)
  # berechnung des abgeleiteten modells
  cheb.model.deriv <- fkt.cheb.deriv(x.cheb, cheb.coeff)

  # berechnung der nullstellen
  extr <- uniroot.all(fkt.cheb.deriv, cheb.coeff = cheb.coeff, lower = (-1), upper = 1)
  # reskalierung der Nullstellen auf normale Lat- Achse
  x.extr <- fkt.cheb.rescale(extr, x = x)
  y.extr <- if (length(extr) != 0) fkt.cheb.val(x = extr, cheb.coeff = cheb.coeff)

  cheb.list <- list(cheb.coeff = cheb.coeff, cheb.model = cheb.model, cheb.model.deriv = cheb.model.deriv)#, x.extr = x.extr, y.extr = y.extr)
  return(cheb.list)
}
