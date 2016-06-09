#### Programm zur Datenvisualisierung
#### source('~/Master_Thesis/r-code-git/visualize.r')
#### 
#### 
#### 
#### 
## Vergleichsplot
## Modell vs Daten
## gefiltert über Chebyshev und Legendre bis Ordnung 5
##
plot(lat.era.t63, uwind.monmean[2,,1], type = "l")
lines(lat.era.t63, model.cheb[2,,1], col = "dark red")
lines(lat.era.t63, model.deriv.cheb[2,,1], col = "light blue")

plot(lat.era.t63, d, type="l")
lines(lat.model.cheb, model.cheb, col = "dark red")
lines(lat.model.cheb, model.deriv.cheb, col = "dark green")

lines(lat.era.t63, model.leg[2,1,], col="green")


image.plot(lon.era.t63, lat.era.t63, uwind.monmean[,,1])
addland(col="grey50",lwd=1)
image.plot(lon.era.t63,lat.era.t63,d.model.cheb[,,1])
addland(col="grey50",lwd=1)
image.plot(lon.era.t63,lat.era.t63,d.model.leg[,,1])
addland(col="grey50",lwd=1)

image.plot(lon.era.t63,lat.era.t63,uwind.monmean[,,1]-d.model.cheb[,,1])
addland(col="grey50",lwd=1)
image.plot(lon.era.t63,lat.era.t63,uwind.monmean[,,1]-d.model.leg[,,1])
addland(col="grey50",lwd=1)


image.plot(lon.era.t63,lat.era.t63,uwind.monmean[,,2]-d.model.cheb[,,2])
addland(col="grey50",lwd=1)
image.plot(lon.era.t63,lat.era.t63,uwind.monmean[,,2]-d.model.leg[,,2])
addland(col="grey50",lwd=1)

image.plot(lon.era.t63,lat.era.t63,uwind.monmean[,,3]-d.model.cheb[,,3])
addland(col="grey50",lwd=1)
image.plot(lon.era.t63,lat.era.t63,uwind.monmean[,,3]-d.model.leg[,,3])
addland(col="grey50",lwd=1)


image.plot(lon.era.t63, lat.era.t63, uwind.monmean[,,1])
lines(lon.era.t63, x.max[,1])

image.plot(lon.era.t63, lat.era.t63, uwind.monmean[,,2])
lines(lon.era.t63, x.max[,2])

image.plot(lon.era.t63, lat.era.t63, uwind.monmean[,,3])
lines(lon.era.t63, x.max[,3])




for (i in 1:664) {
  for (j in 1:192) {
    plot(lat.era.t63, uwind.monmean[j,,i])
    lines(lat.era.t63, cheb.model[j,,i], lty = 3)
    points(x.extr[j,,i], y.extr[j,,i],  pch = 20)
    print(i)
  }
}





####################################################################################################
########## visualisierung der daten ################################################################
####################################################################################################
##
## plot über gesamte zeit
image.plot(lon.era.t63,lat.era.t63,uwind.monmean[,,1])
addland(col="grey50",lwd=1)



## plot mean
#X11()
image.plot(lon.era.t63,lat.era.t63,uwind.mean)
addland(col="grey50",lwd=1)

## plot mean
#X11()
image.plot(lon.era.t63,lat.era.t63,uwind.std)
addland(col="grey50",lwd=1)