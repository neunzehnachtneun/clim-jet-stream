######################################################################
## Berechnung von Mean und Sd
## über fünf Jahre & saisonal
######################################################################

# library(fields)
library(raster)
library(RColorBrewer)
library(reshape2)
library(ggplot2)
library(egg)


load(file = "monthly.RData")


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



# 
# seas <- "jja"
# 
# ## Vorbereiten der Daten für ggplot2
# ##
# range(model.max.lat.seas.mean.mam, model.max.lat.seas.mean.jja, model.max.lat.seas.mean.son, model.max.lat.seas.mean.djf)
# gg.data.hovm <- melt(model.max.lat.seas.mean.jja)
# 
# 
# ## Plotroutine
# gg.hovm <- ggplot(data = gg.data.hovm, mapping = aes(x = lon[Var1], y = dts.year.mn[Var2], z = value, fill = value))
# gg.hovm <- gg.hovm + geom_raster() + scale_fill_gradientn(
#   colours = brewer.pal(11, "RdYlBu"),
#   limits = c(20, 75),
#   guide_colourbar(title = "Latitude")) +
#   theme(legend.position = "bottom") +
#   geom_contour(binwidth = 5, color = "gray0") +
#   xlab("Longitude") + ylab("Year") +
#   scale_x_continuous(breaks = seq(0, 360, 30)) +
#   scale_y_continuous(breaks = seq(1960, 2010, 10)) +
#   coord_fixed(3)
# 
# 
# gg.data.hovm.yr <- aggregate(gg.data.hovm[,3], list(gg.data.hovm$Var2), mean)
# colnames(gg.data.hovm.yr) <- c("year", "Value")
# # gg.data.hovm.yr <- (apply(model.max.lat.seas.mean, 2, mean))
# gg.hovm.yr <- ggplot(data = gg.data.hovm.yr, mapping = aes(x = "", y = dts.year.mn[year], fill = Value)) +
#   geom_raster() +
#   scale_fill_gradientn(limits = c(20, 75),
#                        colours = brewer.pal(11, "RdYlBu")) +
#   theme(legend.position = "none",
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank())
# 
# gg.data.hovm.mer <- aggregate(gg.data.hovm[,3], list(gg.data.hovm$Var1), mean)
# colnames(gg.data.hovm.mer) <- c("Lon", "Value")
# gg.hovm.mer <- ggplot(data = gg.data.hovm.mer, mapping = aes(x = lon[Lon], y = "", fill = Value)) +
#   geom_raster() +
#   scale_fill_gradientn(limits = c(20, 75),
#                        colours = brewer.pal(11, "RdYlBu")) +
#   theme(legend.position = "none",
#         axis.title.x = element_blank(),
#         axis.title.y = element_blank(),
#         axis.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank()) +
#   ggtitle(paste0("Position of Jet Stream - ", toupper(seas), "\n", "(five yearly moving average)"))
# 
# 
# 
# 
# 
# ##
# ## Zusammenfügen der Plots
# empty <- ggplot() + coord_fixed()
# ggarrange(gg.hovm.mer, empty, gg.hovm, gg.hovm.yr, heights = c(1,10), widths = c(10,1))
# 






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




