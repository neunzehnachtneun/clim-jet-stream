####################################################################################################
## source('~/Master_Thesis/r-code-git/locate_jetstream_polynomial_2d.r')
## era40, era-interim
library(ncdf)
library(fields)
library(clim.pact)
library(rootSolve)
library(parallel)
library(chron)
# eigenes package für polynom fit
# install.packages("pckg.cheb_0.1.tar.gz", repos = NULL, type = "source")
library(pckg.cheb)


path <- "data/"
file <- "era--t63_ua_monmean_300hpa_nh.nc"  # Nordhemisphäre
#file <- "era--t63_ua_monmean_300hpa_sh.nc"  # Südhemisphäre
#file <- "era--t63_ua_monmean_300hpa.nc"     # Globus


####################################################################################################
########## era-t63grid #############################################################################
########## einlesen ################################################################################
####################################################################################################
##
nc <- open.ncdf(paste(path, file, sep = ""))
lon.era.t63 <- get.var.ncdf(nc,"lon") #längengrad
lat.era.t63 <- get.var.ncdf(nc,"lat") #breitengrad
lev.era.t63 <- get.var.ncdf(nc,"lev") #höhenlevel
time.help <- get.var.ncdf(nc,"time")
time.era.t63 <- chron(time.help/24, origin. = c(month = 9,day = 1,year = 1957))
uwind.monmean <- get.var.ncdf(nc,"var131")
close.ncdf(nc)
rm(nc)


## alternative einleseroutine mit ncdf4
nc <- nc_open(paste(path, file, sep = ""))
time.help <- ncvar_get(nc, "time")
time.era.t63 = chron(time.help/24, origin. = c(month = 9,day = 1,year = 1957))


## nützliche variablen aus datensatz ziehen
##
n.lat <- length(lat.era.t63)
n.lat.diff <- n.lat - 1
n.lon <- length(lon.era.t63)


####################################################################################################
########## fitte polynom fünfter ordnung ###########################################################
########## an uwind in meridionaler richtung #######################################################
####################################################################################################
### chebyshev
##
lat <- lat.era.t63
n <- 23 # ordnung des polynoms

## chebyshev polynome
##
cheb.list <- apply(uwind.monmean[,,], c(1,3), pckg.cheb:::cheb.fit, x.axis = lat, n = n)
## Variante für paralleles Rechnen
cl <- makeCluster(getOption("cl.cores", 2))
cheb.list <- parApply(cl, uwind.monmean[,,1:2], c(1,3), pckg.cheb:::cheb.fit, x.axis = lat, n = n)
stopCluster(cl)

cheb.coeff <- sapply(cheb.list, "[[", 1)
cheb.coeff <- apply(array(data = cheb.coeff, dim = c(dim(cheb.coeff)[1], 192, 664)),  c(1,3), t)

cheb.model <- sapply(cheb.list, "[[", 2)
cheb.model <- apply(array(data = cheb.model, dim = c(dim(cheb.model)[1], 192, 664)),  c(1,3), t)

cheb.model.deriv.1st <- sapply(cheb.list, "[[", 3)
cheb.model.deriv.1st <- apply(array(data = cheb.model.deriv.1st, dim = c(dim(cheb.model.deriv.1st)[1], 192, 664)),  c(1,3), t)

x.extr <- sapply(cheb.list, "[[", 4)
y.extr <- sapply(cheb.list, "[[", 5)

fun.fill <- function(x, n) {
  while (length(x) < n) {
    x <- c(x, NA)
  }
  return(x)
}

x.extr <- sapply(x.extr, fun.fill, n = 24)
x.extr <- apply(array(x.extr, c(dim(x.extr)[1], 192, 664)), c(1,3), t)
y.extr <- sapply(y.extr, fun.fill, n = 24)
y.extr <- apply(array(y.extr, c(dim(y.extr)[1], 192, 664)), c(1,3), t)



residuals.cheb <- uwind.monmean - cheb.model
rmse <- sqrt(sum(residuals.cheb ** 2) / length(residuals.cheb))
## rmse = 0.4079846
## rmse = 0.2911683





for (i in 1:664) {
  for (j in 1:192) {
    plot(lat.era.t63, uwind.monmean[j,,i])
    lines(lat.era.t63, cheb.model[j,,i], lty = 3)
    points(x.extr[j,,i], y.extr[j,,i],  pch = 20)
    print(i)
  }
}




##### Anpassen der Variablennamen!!
## Zeitlich gemittelter Zonalwind
uwind.mean <- apply(uwind.monmean,c(1,2),mean)
uwind.std <- apply(uwind.monmean,c(1,2),sd)

## Meridional und zeitlich gemittelter Zonalwind
uwind.mon.mer.mean <- apply(uwind.monmean, 2, mean)
uwind.mon.mer.sd <- apply(uwind.mean, 2, sd)
plot(uwind.mon.mer.sd)

## Vermutung, dass Fehler mit (Breitengrad) ansteigt
## scheint nicht so
apply(residuals.cheb, 2, mean)

## Meridional gemittelter Zonalwind
uwind.monmean.mermean <- apply(uwind.monmean, c(2,3), mean)
apply(uwind.monmean, c(2,3), sd)
#####




## Vergleichsplot
## Modell vs Daten
## gefiltert über Chebyshev und Legendre bis Ordnung 5
##
plot(lat.era.t63, uwind.monmean[2,,1], type="l")
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




## über gesamten globus
##





####################################################################################################
########## ableitung des drehimpulses ##############################################################
########## aus zonal wind ##########################################################################
####################################################################################################
### ref: m = 
### formel noch inkorrekt
### keine schleife benutzen
##
# m <- matrix(NA,n.lon,n.lat)
# for (i in 1:n.lon){
#   for (j in 1:n.lat){
#     m[i,j] <- uwind.era.t63.monmean[i,j,1]*cos(lat.era.t63[j]) + 1/86400*uwind.era.t63.monmean[i,j,1]**2*cos(lat.era.t63[j])**2
#   }
# }
# #m <- uwind.era.t63.monmean*cos(lat.era.t63)
# 



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