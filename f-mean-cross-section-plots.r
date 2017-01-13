## source('~/Master_Thesis/02-r-code-git/d-analyse_seasonal_change.r')
## 
## BERECHNUNG VON GLEITENDEM MITTEL U SD ####
## ÜBER FÜNF JAHRE U SAISONAL              ## 
####
####

####
## AUFRUF WICHTIGER BIBLIOTHEKEN UND PAKETE ####
####

# library(raster)
# library(RColorBrewer)
# library(reshape2)
# library(ggplot2)
# library(egg)

library(fields)
library(clim.pact)

setwd("~/01-Master-Thesis/02-r-code-git/")
path <- "03-data-nc/"
filename <- "1958-2015-e4ei-t63-nh-uv-seasmean.nc"

source("a-read_era_ncdf.r")
source("b-locate_jetstream_1polynomial_2d.r")

####
## SEASONALLY YEARLY MEANS
####

setwd("~/01-Master-Thesis/02-r-code-git/")
path <- "03-data-nc/"
filename <- "1958-2015-e4ei-t63-nh-uv-seasmean.nc"

source("a-read_era_ncdf.r")
source("b-locate_jetstream_1polynomial_2d.r")


model.max.lat.mam <- model.max.lat[, which(dts.month == "Apr")]
model.max.lat.jja <- model.max.lat[, which(dts.month == "Jul")]
model.max.lat.son <- model.max.lat[, which(dts.month == "Oct")]
model.max.lat.djf <- model.max.lat[, which(dts.month == "Jan")]

model.max.u.mam <- model.max.u[, which(dts.month == "Apr")]
model.max.u.jja <- model.max.u[, which(dts.month == "Jul")]
model.max.u.son <- model.max.u[, which(dts.month == "Oct")]
model.max.u.djf <- model.max.u[, which(dts.month == "Jan")]







####
## RUNNING MEAN OVER FIVE YEARS
## (STEP +/- TWO YEARS)
####

dts.year.mn <- seq(1960, 2013)
n.time <- length(dts.year.mn)
for (seas in (c("mam", "jja", "son", "djf"))) {
  if (seas == "mam") {
    ind.seas <- which(dts.month == "Mar" | dts.month == "Apr" | dts.month == "May")
  } else if (seas == "jja") {
    ind.seas <- which(dts.month == "Jun" | dts.month == "Jul" | dts.month == "Aug")
  } else if (seas == "son") {
    ind.seas <- which(dts.month == "Sep" | dts.month == "Oct" | dts.month == "Nov")
  } else if (seas == "djf") {
    ind.seas <- which(dts.month == "Dec" | dts.month == "Jan" | dts.month == "Feb")
  }
  
  ## Mittelwerte global
  ## Vorbereiten der Matrizen
  u.seas.mean <- array(NA , dim = c(n.lon, n.lat, n.time))
  u.seas.sd <- array(NA , dim = c(n.lon, n.lat, n.time))
  model.max.u.seas.mean <- array(NA , dim = c(n.lon, n.time))
  model.max.u.seas.sd <- array(NA , dim = c(n.lon, n.time))
  model.max.lat.seas.mean <- array(NA , dim = c(n.lon, n.time))
  model.max.lat.seas.sd <- array(NA , dim = c(n.lon, n.time))
  
  for (i in seq(1, n.time)) {
    print(i)
    yr.i <- dts.year.mn[i]
    ind.yr <- which(dts.year == (yr.i - 2) | dts.year == (yr.i - 1) | dts.year ==  yr.i | dts.year == (yr.i + 1) | dts.year == (yr.i + 2))
    ind.seas.yr <- intersect(ind.yr, ind.seas)
    #  print(ind.seas.yr)
    u.seas.mean[,,i] <- apply(u.monmean[,, ind.seas.yr], c(1,2), mean)
    u.seas.sd[,,i] <- apply(u.monmean[,, ind.seas.yr], c(1,2), sd)
    model.max.u.seas.mean[,i] <- apply(model.max.u[, ind.seas.yr], 1, mean)
    model.max.u.seas.sd[,i] <- apply(model.max.u[, ind.seas.yr], 1, sd)
    model.max.lat.seas.mean[,i] <- apply(model.max.lat[, ind.seas.yr], 1, mean)
    model.max.lat.seas.sd[,i] <- apply(model.max.lat[, ind.seas.yr], 1, sd)
    
    
  }
  ## Variablenübergabe
  assign(paste0("u.seas.mean.", seas), u.seas.mean)
  assign(paste0("u.seas.sd.", seas), u.seas.sd)
  assign(paste0("model.max.u.seas.mean.", seas), model.max.u.seas.mean)
  assign(paste0("model.max.u.seas.sd.", seas), model.max.u.seas.sd)
  assign(paste0("model.max.lat.seas.mean.", seas), model.max.lat.seas.mean)
  assign(paste0("model.max.lat.seas.sd", seas), model.max.lat.seas.sd)
}




################################################################################
################################################################################
### PLOT FUER FARBENBLINDE !!!
###  
## Datenvorbereitung für ggplot2
gg.data.raw <- model.max.lat.seas.mean.djf

# gg.data.raw <- matrix(70/10368*(1:10368), ncol = 192, nrow = 54) #matrix(rnorm(10368,45,15), ncol = 192, nrow=54)
breaks <- c(-Inf,seq(30, 70, 5),Inf)
gg.data.hovm <- melt(gg.data.raw)
gg.data.hovm$color <- as.character(cut(gg.data.hovm$value, breaks = breaks, labels = FALSE))
col.pal <- brewer.pal(10, "RdYlBu")
names(col.pal) <- as.character(1:10)
## Plotroutine
gg.hovm <- ggplot(gg.data.hovm, aes(x = lon[Var1], y = dts.year.mn[Var2], z = value, fill = color)) + 
  geom_raster() + 
  scale_fill_manual(values = col.pal, guide_colourbar(title = "Latitude")) + 
  geom_contour(data = gg.data.hovm, aes(x = lon[Var1], y = dts.year.mn[Var2], z = value, fill = value), binwidth = 10, color = "gray0") +
  xlab("Longitude") + ylab("Year") +
  scale_x_continuous(breaks = seq(0, 360, 30)) +
  scale_y_continuous(breaks = seq(1960, 2010, 10)) +
  coord_fixed(3)


gg.data.hovm.yr <- aggregate(gg.data.hovm[,3], list(gg.data.hovm$Var2), mean)
colnames(gg.data.hovm.yr) <- c("year", "value")
gg.data.hovm.yr$color <- as.character(cut(gg.data.hovm.yr$value, breaks = breaks, labels = FALSE))

gg.hovm.yr <- ggplot(data = gg.data.hovm.yr, mapping = aes(x = "", y = dts.year.mn[year], z = value, fill = color)) +
  geom_raster() +
  scale_fill_manual(values = col.pal) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())


gg.data.hovm.mer <- aggregate(gg.data.hovm[,3], list(gg.data.hovm$Var1), mean)
colnames(gg.data.hovm.mer) <- c("lon", "value")
gg.data.hovm.mer$color <- as.character(cut(gg.data.hovm.mer$value, breaks = breaks, labels = FALSE))

gg.hovm.mer <- ggplot(data = gg.data.hovm.mer, mapping = aes(x = lon[lon], y = "", z = value, fill = color)) +
  geom_raster() +
  scale_fill_manual(values = col.pal) +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()) #+
#ggtitle(paste0("Position of Jet Stream - ", toupper(seas), "\n", "(five yearly moving average)"))

##
## Zusammenfügen der Plots
empty <- ggplot() + coord_fixed()
ggarrange(gg.hovm.mer, empty, gg.hovm, gg.hovm.yr, heights = c(1,10), widths = c(10,1))

##
## TEST FÜR 
## BOXPLOT ZONAL-WIND

a <- matrix(data = rnorm(192 * 54), nrow = 192, ncol = 54)
b <- matrix(data = rnorm(192 * 54), nrow = 192, ncol = 54)
c <- abind(a, b, along = 3)
d <- melt(c)

e <- melt(u.monmean)
boxp <- ggplot(e, aes(x = lat[Var2], y = value)) + geom_boxplot(aes(group = Var2))
print(boxp)

