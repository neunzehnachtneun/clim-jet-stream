######################################################################
######################################################################
## ROUTINEN UND FUNKTIONEN FÜR PLOT TEMPLATES
## AUFBAUEND AUF PLOT(), IMAGE.PLOT(), CONTOUR(), POINTS()
## source('~/Master_Thesis/r-code-git/m-plots-master.r')
######################################################################
######################################################################


######################################################################
## AUFRUF WICHTIGER BIBLIOTHEKEN UND PAKETE
######################################################################
##


library(fields)
library(clim.pact)
library(RColorBrewer)

####
## BEISPIEL DATENSATZ
####

nx <- 192
ny <- 48

x1 <- seq(0, 360, length.out = nx)
x2 <- rnorm(nx, 45, 5)
x3 <- rnorm(nx, 45, 5)
x4 <- rnorm(nx, 45, 5)
y1 <- seq(0, 90, length.out = ny)
y2 <- rnorm(n = ny)

f <- matrix(rnorm(nx * ny), nx, ny)


####
## IMAGE.PLOT MIT CONTOUR UND POINTS
####


# mit titel für präsi
master.plt.imc.ttl <- function(x.ax, y.ax, xy.data, x.data, tit, x.lab, y.lab) {
  image.plot(x.ax, y.ax, xy.data, axes = FALSE,
             col = brewer.pal(11, "RdYlBu"), 
             main = tit, #sub = "Untertitel",
             xlab = x.lab, 
             ylab = y.lab,
             cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
  contour(x.ax, y.ax, xy.data, nlevels = 5, add = TRUE)
  points(x.ax, x.data, pch = 18, col = "gray0")
  axis(1, at = seq(min(x.ax), max(x.ax), length.out = 9))
  axis(2, at = seq(min(y.ax), max(y.ax), length.out = 6))
  box()
} 

# ohne titel
master.plt.imc <- function(x.ax, y.ax, xy.data, x.data, x.lab, y.lab) {
  image.plot(x.ax, y.ax, xy.data, axes = FALSE,
             col = brewer.pal(11, "RdYlBu"), 
             xlab = x.lab, 
             ylab = y.lab,
             cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
  contour(x.ax, y.ax, xy.data, nlevels = 5, add = TRUE)
  points(x.ax, x.data, pch = 18, col = "gray0")
  axis(1, at = seq(min(x.ax), max(x.ax), length.out = 9))
  axis(2, at = seq(min(y.ax), max(y.ax), length.out = 6))
  box()
} 


# master.plt.imc.ttl(x.ax = x1, y.ax = y1, xy.data = f, x.data = x2, tit = "Titel", x.lab = "X-Achse", y.lab = "Y-Achse")
# master.plt.imc(x.ax = x1, y.ax = y1, xy.data = f, x.data = x2, x.lab = "X-Achse", y.lab = "Y-Achse")



####
## IMAGE.PLOT MIT ADDLAND
####

# mit titel für präsi
master.plt.iml.ttl <- function(x.ax, y.ax, xy.data, x.data, tit, x.lab, y.lab) {
  image.plot(x.ax, y.ax, xy.data, axes = FALSE,
             col = brewer.pal(11, "RdYlBu"), 
             main = tit, #sub = "Untertitel",
             xlab = x.lab, 
             ylab = y.lab,
             cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
  addland(col = "black", lwd = 1)
  points(x.ax, x.data, pch = 18, col = "gray0")
  axis(1, at = seq(min(x.ax), max(x.ax), length.out = 9))
  axis(2, at = seq(min(y.ax), max(y.ax), length.out = 6))
  box()
} 

# ohne titel
master.plt.iml <- function(x.ax, y.ax, xy.data, x.data, x.lab, y.lab) {
  image.plot(x.ax, y.ax, xy.data, axes = FALSE,
             col = brewer.pal(11, "RdYlBu"), 
             xlab = x.lab, 
             ylab = y.lab,
             cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
  addland(col = "black", lwd = 1)
  points(x.ax, x.data, pch = 18, col = "gray0")
  axis(1, at = seq(min(x.ax), max(x.ax), length.out = 9))
  axis(2, at = seq(min(y.ax), max(y.ax), length.out = 6))
  box()
} 


master.plt.iml.ttl(x.ax = x1, y.ax = y1, xy.data = f,tit = "Titel", x.data = x2, x.lab = "X-Achse", y.lab = "Y-Achse")
master.plt.iml(x.ax = x1, y.ax = y1, xy.data = f, x.data = x2, x.lab = "X-Achse", y.lab = "Y-Achse")


####
## PLOT
####

# mit titel für präsi
master.plt.pts.ttl <- function(x.ax, x.data1, x.data2, x.data3, tit, x.lab, y.lab, leg.1, leg.2, leg.3) {
  plot(x.ax, x.data1, pch = 0, col = brewer.pal(3, "Dark2")[1],
       xlab = x.lab, 
       ylab = y.lab,
       main = tit,
       cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5,
       axes = FALSE)
  points(x.ax, x.data2, pch = 21, col = brewer.pal(3, "Dark2")[2])
  points(x.ax, x.data3, pch = 17, col = brewer.pal(3, "Dark2")[3])
  legend("topright", pch= c(0, 21, 17), col = brewer.pal(3, "Dark2"), c(leg.1, leg.2, leg.3))
  axis(1, at = seq(min(x.ax), max(x.ax), length.out = 9))
  axis(2, at = seq(floor(min(x.data1, x.data2, x.data3)), ceiling(max(x.data1, x.data2, x.data3)), length.out = 7))
  box()
}

# ohne titel für ma
master.plt.pts <- function(x.ax, x.data1, x.data2, x.data3, x.lab, y.lab, leg.1, leg.2, leg.3) {
  plot(x.ax, x.data1, pch = 0, col = brewer.pal(3, "Dark2")[1],
       xlab = x.lab, 
       ylab = y.lab,
       cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5,
       axes = FALSE)
  points(x.ax, x.data2, pch = 21, col = brewer.pal(3, "Dark2")[2])
  points(x.ax, x.data3, pch = 17, col = brewer.pal(3, "Dark2")[3])
  legend("topright", pch= c(0, 21, 17), col = brewer.pal(3, "Dark2"), c(leg.1, leg.2, leg.3))
  axis(1, at = seq(min(x.ax), max(x.ax), length.out = 9))
  axis(2, at = seq(floor(min(x.data1, x.data2, x.data3)), ceiling(max(x.data1, x.data2, x.data3)), length.out = 7))
  box()
}



master.plt.pts.ttl(x.ax = x1, x.data1 = x2, x.data2 = x3, x.data3 = x4, tit = "Titel", x.lab = "bla", y.lab = "alb", leg.1 = "a1", leg.2 = "b2", leg.3 = "c3")

master.plt.pts(x.ax = x1, x.data1 = x2, x.data2 = x3, x.data3 = x4, tit, x.lab = "bla", y.lab = "alb", leg.1 = "a1", leg.2 = "b2", leg.3 = "c3")





