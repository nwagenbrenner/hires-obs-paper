library(raster)

#----------------------
#  BSB
#-----------------------
locations<-read.table('/home/natalie/bsb_locations.txt', sep=",", skip = 1, header=FALSE)
colnames(locations) <- c('id', 'lat', 'lon', 'z')

r<-raster('/home/natalie/src/windninja/test_runs/big_butte.asc')

#warp to WGS84
r.p<-projectRaster(r, crs="+proj=longlat +datum=WGS84", method='ngb')

points<-cbind(locations$lon, locations$lat)
sp<-SpatialPoints(points, proj4string=CRS("+proj=longlat +datum=WGS84"))
#sp_utm <- spTransform(sp, CRS(proj4string(r)))

e<-extract(r.p, sp)
d<-cbind(locations, e)

write.table(d, file = "bsb.csv", append = FALSE, quote = FALSE, sep = ",",
                 eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                 col.names = TRUE, qmethod = c("escape", "double"),
                 fileEncoding = "")

#-----------------------
#  SRC
#-----------------------
locations<-read.table('/home/natalie/salmon_locations.txt', sep=",", skip = 1, header=FALSE)
colnames(locations) <- c('id', 'lat', 'lon', 'z')

r<-raster('/home/natalie/src/windninja/test_runs/salmonriver_dem.asc')

#warp to WGS84
r.p<-projectRaster(r, crs="+proj=longlat +datum=WGS84", method='ngb')

points<-cbind(locations$lon, locations$lat)
sp<-SpatialPoints(points, proj4string=CRS("+proj=longlat +datum=WGS84"))
#sp_utm <- spTransform(sp, CRS(proj4string(r)))

e<-extract(r.p, sp)
d<-cbind(locations, e)

write.table(d, file = "src.csv", append = FALSE, quote = FALSE, sep = ",",
                 eol = "\n", na = "NA", dec = ".", row.names = FALSE,
                 col.names = TRUE, qmethod = c("escape", "double"),
                 fileEncoding = "")
