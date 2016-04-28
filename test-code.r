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




