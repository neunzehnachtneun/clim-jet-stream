######################################################################
## Berechnung von Mean und Sd
## über fünf Jahre & saisonal
######################################################################

library(fields)
library(reshape2)
library(ggplot2)
library(egg)


load(file = "monthly.RData")


dts.year.mn <- seq(1960, 2013)
n.time <- length(dts.year.mn)
seas <- "jja" # jja son djf
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


range(model.max.lat.seas.mean.mam, model.max.lat.seas.mean.jja, model.max.lat.seas.mean.son, model.max.lat.seas.mean.djf)



## Vorbereiten der Daten für ggplot2
gg.data.hovm <- melt(model.max.lat.seas.mean.son)

## Plotroutine
gg.hovm <- ggplot(data = gg.data.hovm, mapping = aes(x = lon[Var1], y = dts.year.mn[Var2], z = value, fill = value))
gg.hovm <- gg.hovm + geom_raster() + scale_fill_gradientn(
  colours = rev(tim.colors(14)), 
  limits = c(20, 75),
  guide_colourbar(title = "Latitude")) +
  theme(legend.position = "bottom") +
  geom_contour(binwidth = 7.5, color = "gray0") +
  xlab("Longitude") + ylab("Year") +
#  ggtitle(paste0("Position of Jet Stream - ", toupper(seas), "\n", "(five yearly moving average)")) +
  scale_x_continuous(breaks = seq(0, 360, 30)) +
  scale_y_continuous(breaks = seq(1960, 2010, 10))+
  coord_fixed()


gg.data.hovm.yr <- aggregate(gg.data.hovm[,3], list(gg.data.hovm$Var2), mean)
colnames(gg.data.hovm.yr) <- c("year", "Value")
# gg.data.hovm.yr <- (apply(model.max.lat.seas.mean, 2, mean))
gg.hovm.yr <- ggplot(data = gg.data.hovm.yr, mapping = aes(x = "", y = dts.year.mn[year], fill = Value)) +
  geom_raster() +
  scale_fill_gradientn(limits=c(20, 75),
                       colours = rev(tim.colors(14))) +
  theme(legend.position="none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

gg.data.hovm.mer <- aggregate(gg.data.hovm[,3], list(gg.data.hovm$Var1), mean)
colnames(gg.data.hovm.mer) <- c("Lon", "Value")
gg.hovm.mer <- ggplot(data = gg.data.hovm.mer, mapping = aes(x = lon[Lon], y = "", fill = Value)) +
  geom_raster() +
  scale_fill_gradientn(limits=c(20, 75),
                       colours = rev(tim.colors(14))) +
  theme(legend.position="none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())




##
## Zusammenfügen der Plots
empty <- ggplot()
ggarrange(gg.hovm.mer, empty, gg.hovm, gg.hovm.yr, heights = c(1,10), widths = c(10,1))





