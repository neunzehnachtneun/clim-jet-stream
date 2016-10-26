####################################################################################################
## source('~/Master_Thesis/Code/locate_jetstream_2d.r')
## einleseroutine für ncdf-files 
## era40, era-interim
library(ncdf)
library(fields)
library(clim.pact)


####################################################################################################
########## era-t63grid #############################################################################
########## einlesen ################################################################################
####################################################################################################
##
# nc <- open.ncdf('/home/skiefer/era/era--t63_ua_monmean_300hpa.nc') ## ganzer globus
nc <- open.ncdf('/home/skiefer/era/era--t63_ua_monmean_300hpa_nh.nc')## nordhemisphäre
# nc <- open.ncdf('/home/johndoe/Master_Thesis/Code/era--t63_ua_monmean_300hpa_nh.nc')
lon.era.t63 <- get.var.ncdf(nc,"lon") #längengrad
lat.era.t63 <- get.var.ncdf(nc,"lat") #breitengrad
lev.era.t63 <- get.var.ncdf(nc,"lev") #höhenlevel
time.era.t63 <- get.var.ncdf(nc,"time")
uwind.monmean <- get.var.ncdf(nc,"var131")
close.ncdf(nc)
## nützliche variablen aus datensatz ziehen
##
uwind.mean <- apply(uwind.monmean,c(1,2),mean)
uwind.std <- apply(uwind.monmean,c(1,2),sd)
n.lat <- length(lat.era.t63)
n.lat.diff <- n.lat-1
n.lon <- length(lon.era.t63)




####################################################################################################
########## ableitung des zonalen windes ############################################################
########## in meridionaler richtung ################################################################
########## (diskretisiert) #########################################################################
####################################################################################################
##
## wert für delta phi 
##(gitterabstand in meridionaler richtung)
##
lat.diff <- matrix(0,1,n.lat.diff)
for (i in 1:n.lat.diff) {
 #print(i)
 lat.diff[i] <- abs(lat.era.t63[i]-lat.era.t63[i+1])
}
## zwischengitter in breitengrad richtung
##
lat.int <- matrix(0,1,n.lat.diff)
for (i in 1:n.lat.diff) {
 lat.int[i] <- lat.era.t63[i+1]-lat.diff[i]
}
## über einen längengrad
##
ijk <- 180
uwind.mer <- matrix(0,1,n.lat.diff)
for (i in 1:n.lat.diff) {
 uwind.mer[i] <- (uwind.monmean[ijk,i,1]-uwind.monmean[ijk,i+1,1])/lat.diff[i]
}
## über gesamten globus
##
uwind.mer.glob <- matrix(NA,n.lon,n.lat.diff)
t <- 5
for (i in 1:n.lon) {
 #  print(i)
 for (j in 1:n.lat.diff) {
   uwind.mer.glob[i,j] <- (uwind.monmean[i,j,t]-uwind.monmean[i,j+1,t])/lat.diff[j]
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
