library(maptools)
library(maps)
library(ggmap)

#-------------------------------------
#   make a US map with sites labeled
#-------------------------------------
xlim<-c(-125, -110)
ylim<-c(43, 49)
domain<-map("state", regions = c("idaho","Montana","Wyoming","oregon", "washington", "california", "Nevada", "utah", "colorado", "new mexico", "arizona"), plot = FALSE, fill = TRUE)
IDs<-sub("^idaho,", "", domain$names)
domain_sp<-map2SpatialPolygons(domain, IDs, CRS("+proj=longlat"))
sites<-cbind(-113.0283, 43.40202)
sites<-rbind(sites, cbind(-116.2314, 45.40276))
sp<-SpatialPoints(sites, proj4string=CRS("+proj=longlat +datum=WGS84"))
plot(domain_sp, axes = TRUE, xlim=xlim, ylim=ylim)
plot(sp, add=TRUE, pch = 19)
savePlot(filename="/home/natalie/Desktop/pacnw_map.png", type="png")

#==========================================
#  butte
#==========================================

#-------------------------------------
#   Make spatial points
#-------------------------------------
#surface sensors
locations<-read.table('/home/natalie/bsb_locations.txt', sep=",", skip = 1, header=FALSE)
colnames(locations) <- c('id', 'lat', 'lon', 'z')

#sodars, sonics, etc.
otherSensors <- as.data.frame(cbind('WSU1', 43.3325, -113.102756), stringsAsFactors = FALSE)
otherSensors <- rbind(otherSensors, cbind('WSU2', 43.4087, -113.0041))
otherSensors <- rbind(otherSensors, cbind('NOAA1', 43.3228, -113.1050))
otherSensors <- rbind(otherSensors, cbind('NOAA2', 43.2941, -113.1816))
#otherSensors <- rbind(otherSensors, cbind('SUM', 43.3963, -113.0218))
colnames(otherSensors) <- c('id', 'lat', 'lon')
otherSensors$lat <- as.numeric(otherSensors$lat)
otherSensors$lon <- as.numeric(otherSensors$lon)

#-------------------------------------
#   Get site map and add points
#-------------------------------------
bsbMap<-get_map(location = c(lon = -113.04, lat = 43.38), zoom = 12, maptype = 'terrain')

#label subset of points
locations$id1 <- locations$id
idx <- locations$id1 %in% c("R1", "R2", "R3", "R4", "R5", "R6", "R18", "R20", "R33", "TSW13",
        "R23_2", "TWSW11", "R7", "TSW1", "R9", "R13", "TSW12", "TSW11", "TSW10",
        "R24", "R22", "TSW2")
locations$id1[!idx] <- NA

m <- ggmap(bsbMap) + geom_point(data=locations, aes(x=lon, y=lat), colour = "black", size = 2)
m <- m + geom_text(data=locations, aes(x = lon+0.001, y = lat, label = id1),
         colour="black", size=3, hjust=0, vjust=1) + theme(legend.position = "none")
m <- m + annotate("text", x = -113.072, y = 43.384, label = "TWSW1", size = 3)
m <- m + annotate("text", x = -113.0018, y = 43.3945, label = "R17", size = 3)
m <- m + annotate("text", x = -113.014, y = 43.4095, label = "R19", size = 3)

m <- m + geom_point(data=otherSensors, aes(x=lon, y=lat), pch = 23, colour = "red", size = 2)
m <- m + geom_text(data=otherSensors, aes(x = lon+0.001, y = lat, label = id),
         colour="red", size=3, hjust=0, vjust=1) + theme(legend.position = "none")
m <- m + xlab("") + ylab("")
savePlot(filename="/home/natalie/Desktop/bsb_map.png", type="png")

#zoomed in map with rest of points labeled on the butte
#label some points manually
locations$id1 <- locations$id
idx <- locations$id1 %in% c("R28", "R8", "R34", "TWSW10", "R17")
locations$id1[idx] <- NA

bsbMap<-get_map(location = c(lon = -113.027724, lat = 43.402726), zoom = 14, maptype = 'terrain')
m <- ggmap(bsbMap)+ geom_point(data=locations, aes(x=lon, y=lat), colour = "black", size = 2)
m <- m + geom_text(data=locations, aes(x = lon+0.0005, y = lat, label = id1),
         colour="black", size=3, hjust=0, vjust=1) + theme(legend.position = "none")
m <- m + annotate("text", x = -113.0477, y = 43.4050, label = "R8", size = 3)
m <- m + annotate("text", x = -113.0418, y = 43.4125, label = "R28", size = 3)
m <- m + annotate("text", x = -113.0260, y = 43.4024, label = "R34", size = 3)
m <- m + annotate("text", x = -113.0035, y = 43.4025, label = "TWSW10", size = 3)
m <- m + annotate("text", x = -113.0033, y = 43.3964, label = "R17", size = 3)
m <- m + xlab("") + ylab("")
savePlot(filename="/home/natalie/Desktop/bsb_map_zoomed.png", type="png")

#==========================================
#  salmon
#==========================================

#-------------------------------------
#   Make spatial points
#-------------------------------------
#surface sensors
locations<-read.table('/home/natalie/salmon_locations.txt', sep=",", skip = 1, header=FALSE)
colnames(locations) <- c('id', 'lat', 'lon', 'z')

#sodars, sonics, etc.
otherSensors <- as.data.frame(cbind('ST1', 45.391258, -116.233891), stringsAsFactors = FALSE)
otherSensors <- rbind(otherSensors, cbind('ST2', 45.405656, -116.235601))
otherSensors <- rbind(otherSensors, cbind('ST3', 45.402938, -116.225631))
otherSensors <- rbind(otherSensors, cbind('ST4', 45.420509, -116.219399))
colnames(otherSensors) <- c('id', 'lat', 'lon')
otherSensors$lat <- as.numeric(otherSensors$lat)
otherSensors$lon <- as.numeric(otherSensors$lon)

#-------------------------------------
#   Get site map and add points
#-------------------------------------

srcMap<-get_map(location = c(lon = -116.228889, lat = 45.40350), zoom = 14, maptype = 'terrain')

#only label subset of points
locations$id1 <- locations$id
idx <- locations$id1 %in% c("SM4", "Natalie3")
locations$id1[idx] <- NA

otherSensors$id1 <- otherSensors$id
idx <- otherSensors$id1 %in% c("ST1")
otherSensors$id1[idx] <- NA

m <- ggmap(srcMap)+ geom_point(data=locations, aes(x=lon, y=lat), colour = "black", size = 2)
m <- m + geom_text(data=locations, aes(x = lon+0.001, y = lat, label = id1),
         colour="black", size=3.5, hjust=0, vjust=1) + theme(legend.position = "none")
m <- m + annotate("text", x = -116.2333, y = 45.4005, label = "SM4", size = 3.5)
m <- m + annotate("text", x = -116.2252, y = 45.3857, label = "Natalie3", size = 3.5)

m <- m + geom_point(data=otherSensors, aes(x=lon, y=lat), colour = "red", pch = 23, size = 2)
m <- m +  geom_text(data=otherSensors, aes(x = lon+0.001, y = lat, label = id1),
         colour="red",  size=3.5, hjust=0, vjust=1) + theme(legend.position = "none")
m <- m + annotate("text", x = -116.2351, y = 45.3920, label = "ST1", color = 'red', size = 3.5)
m <- m + xlab("") + ylab("")

savePlot(filename="/home/natalie/Desktop/salmon_map.png", type="png")



#r<-raster('/home/natalie/windninja/test_runs/big_butte.asc')
#r<-raster('/home/natalie/windninja/test_runs/salmonriver_dem.asc')
#contour(r, nlev = 10)





