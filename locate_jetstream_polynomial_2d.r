####################################################################################################
## source('~/Master_Thesis/r-code-git/locate_jetstream_polynomial_2d.r')
## era40, era-interim
library(ncdf)
library(fields)
library(clim.pact)
library(rootSolve)
library(parallel)
library(pckg.cheb)
#source('~/Master_Thesis/r-code-git/fun_chebyshev.r')
#source('~/Master_Thesis/Code/fun_legendre.r')


path <- "~/Master_Thesis/data/"
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
time.era.t63 <- get.var.ncdf(nc,"time")
uwind.monmean <- get.var.ncdf(nc,"var131")
close.ncdf(nc)
rm(nc)
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
split <- 6
dx.hr <- 0#0.01 # auflösung des hochaufgelösten gitters

## chebyshev polynome
##
cheb.list <- apply(uwind.monmean[,,1:2], c(1,3), pckg.cheb:::cheb.fit, x.axis = lat, n = n)
## Variante für paralleles Rechnen
cl <- makeCluster(getOption("cl.cores", 2))
cheb.list <- parApply(cl, uwind.monmean[,,1:2], c(1,3), pckg.cheb:::cheb.fit, x.axis = lat, n = n)
stopCluster(cl)
write.csv(cheb.list, "cheblist.csv")
cheb.list.2 <- read.csv(paste(path,"cheblist.csv", sep = ""))

cheb.coeff <- array(NA, dim = c(192, n + 1, 664))
cheb.model <- array(NA, dim = c(192, 48, 664))
cheb.model.deriv <- array(NA, dim = c(192, 48, 664))
#len.extr <- array(NA, dim = c(192, 664)) 

#for (ii in 1:dim(cheb.list)[1]) {
#  for (jj in 1:dim(cheb.list)[2]) {
#    len.extr[ii,jj] <- length(cheb.list[[ii,jj]]$extr.x)
#  }
#}
extr.x <- array(NA, dim = c(192, 11, 664))
extr.y <- array(NA, dim = c(192, 11, 664))

for (ii in 1:dim(cheb.list)[1]) {
  for (jj in 1:dim(cheb.list)[2]) {
#    cheb.coeff[ii,jj,] <- cheb.list[[ii,jj]]$cheb.coeff
    cheb.model[ii,,jj] <- cheb.list[[ii,jj]]$cheb.model
    cheb.model.deriv[ii,,jj] <- cheb.list[[ii,jj]]$cheb.model.deriv
    extr.x.h <- cheb.list[[ii,jj]]$extr.x
    extr.y.h <- cheb.list[[ii,jj]]$extr.y
    while (length(extr.x.h) != 11) {
      extr.x.h <- c(extr.x.h, NA)
      extr.y.h <- c(extr.y.h, NA)
    }
    extr.x[ii,,jj] <- extr.x.h
    extr.y[ii,,jj] <- extr.y.h
  }
}

rm(cheb.list, extr.x.h, extr.y.h)


residuals.cheb <- uwind.monmean - cheb.model
rmse <- sqrt(sum(residuals.cheb ** 2) / length(residuals.cheb))
## rmse = 0.4079846
for (i in 1:664) {
  plot(lat.era.t63, uwind.monmean[1,,i])
  lines(lat.era.t63, cheb.model[1,,i], lty = 3)
  points(extr.x[1,,i], extr.y[1,,i],  pch = 20)
  print(i)
}

##### Anpassen der Variablennamen!!
## Zeitlich gemittelter Zonalwind
uwind.mean <- apply(uwind.monmean,c(1,2),mean)
uwind.std <- apply(uwind.monmean,c(1,2),sd)

## Meridional und zeitliche gemittelter Zonalwind
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
