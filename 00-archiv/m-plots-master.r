######################################################################
######################################################################
## ROUTINEN UND FUNKTIONEN FÜR PLOT TEMPLATES
## AUFBAUEND AUF PLOT(), IMAGE.PLOT(), CONTOUR(), POINTS()
## source('~/01-Master-Thesis/02-r-code-git/m-plots-master.r')
######################################################################
######################################################################


######################################################################
## AUFRUF WICHTIGER BIBLIOTHEKEN UND PAKETE
######################################################################
##


library(fields)
library(clim.pact)
library(RColorBrewer)

# ####
# ## BEISPIEL DATENSATZ
# ####

# nx <- 192
# ny <- 48
# 
# x1 <- seq(0, 360, length.out = nx)
# x2 <- rnorm(nx, 45, 5)
# x3 <- rnorm(nx, 45, 5)
# x4 <- rnorm(nx, 45, 5)
# y1 <- seq(0, 90, length.out = ny)
# y2 <- rnorm(n = ny)
# 
# f <- matrix(rnorm(nx * ny), nx, ny)




####
## IMAGE.PLOT MIT WAHLWEISE CONTOUR, LAND, PUNKTEN, TITEL ####
####

plt.image <- function(x.ax, y.ax, xy.data, colbreaks, nx = 9, ny = 6, x.dts = FALSE, y.dts = FALSE, label.title = NA, label.x, label.y, land = FALSE, cntr = FALSE, pnts = FALSE) {
  par(mar = c(5.1, 5.1, 4.1, 6.6)) 
  if (is.character(label.title)) {
    image(x.ax, y.ax, xy.data, axes = FALSE,
          col = brewer.pal(length(colbreaks) - 1, "RdYlBu"), breaks = colbreaks,
          main = label.title, xlab = label.x, ylab = label.y,
          cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
    if (x.dts) {axis.POSIXct(1, x = x.ax)} else {axis(1, at = round(seq(min(x.ax), max(x.ax), length.out = nx)))}
    if (y.dts) {axis.POSIXct(2, x = y.ax)} else {axis(2, at = round(seq(min(y.ax), max(y.ax), length.out = ny)))}
    if (cntr == TRUE) {contour(x.ax, y.ax, xy.data, nlevels = 5, add = TRUE)}
    if (land == TRUE) {addland(col = "black", lwd = 1)}
    box()
    image.plot(x.ax, y.ax, xy.data, col = brewer.pal(length(colbreaks) - 1, "RdYlBu"), breaks = colbreaks, legend.only = TRUE) 
  } else {
    image(x.ax, y.ax, xy.data, axes = FALSE,
          col = brewer.pal(length(colbreaks) - 1, "RdYlBu"), breaks = colbreaks, 
          xlab = label.x, ylab = label.y,
          cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5)
    if (x.dts) {axis.POSIXct(1, x = x.ax)} else {axis(1, at = round(seq(min(x.ax), max(x.ax), length.out = nx)))}
    if (y.dts) {axis.POSIXct(2, x = y.ax)} else {axis(2, at = round(seq(min(y.ax), max(y.ax), length.out = ny)))}
    if (cntr == TRUE) {contour(x.ax, y.ax, xy.data, nlevels = 5, add = TRUE)}
    if (land == TRUE) {  addland(col = "black", lwd = 1)}
    box()
    image.plot(x.ax, y.ax, xy.data, col = brewer.pal(length(colbreaks) - 1, "RdYlBu"), breaks = colbreaks, legend.only = TRUE) 
  }
  par(mar = c(5.1, 5.1, 4.1, 2.1)) 
} 





####
## PLOT
####

# mit titel für präsi
plt.points.title <- function(x.ax, x.data1, x.data2, x.data3, label.title, label.x, label.y, leg.1, leg.2, leg.3) {
  plot(x.ax, x.data1, pch = 0, col = brewer.pal(3, "Dark2")[1],
       main = label.title,
       xlab = label.x,
       ylab = label.y,
       cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5,
       axes = FALSE)
  points(x.ax, x.data2, pch = 21, col = brewer.pal(3, "Dark2")[2])
  points(x.ax, x.data3, pch = 17, col = brewer.pal(3, "Dark2")[3])
  legend("topright", pch = c(0, 21, 17), col = brewer.pal(3, "Dark2"), c(leg.1, leg.2, leg.3))
  axis(1, at = seq(min(x.ax), max(x.ax), length.out = 9))
  axis(2, at = seq(floor(min(x.data1, x.data2, x.data3)), ceiling(max(x.data1, x.data2, x.data3)), length.out = 7))
  box()
}

# ohne titel für ma
plt.points <- function(x.ax, x.data1, x.data2, x.data3, label.x, label.y, leg.1, leg.2, leg.3) {
  plot(x.ax, x.data1, pch = 0, col = brewer.pal(3, "Dark2")[1],
       xlab = label.x, 
       ylab = label.y,
       cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5,
       axes = FALSE)
  points(x.ax, x.data2, pch = 21, col = brewer.pal(3, "Dark2")[2])
  points(x.ax, x.data3, pch = 17, col = brewer.pal(3, "Dark2")[3])
  legend("topright", pch= c(0, 21, 17), col = brewer.pal(3, "Dark2"), c(leg.1, leg.2, leg.3))
  axis(1, at = seq(min(x.ax), max(x.ax), length.out = 9))
  axis(2, at = seq(floor(min(x.data1, x.data2, x.data3)), ceiling(max(x.data1, x.data2, x.data3)), length.out = 7))
  box()
}

##
# master.plt.pts.ttl(x.ax = x1, x.data1 = x2, x.data2 = x3, x.data3 = x4, label.title = "Titel", label.x = "bla", label.y = "alb", leg.1 = "a1", leg.2 = "b2", leg.3 = "c3")
#
# master.plt.pts(x.ax = x1, x.data1 = x2, x.data2 = x3, x.data3 = x4, label.x = "bla", label.y = "alb", leg.1 = "a1", leg.2 = "b2", leg.3 = "c3")





