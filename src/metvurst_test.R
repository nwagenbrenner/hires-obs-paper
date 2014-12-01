library(metvurst)

#src
src<-readData('/home/natalie/observations_paper/salmon_obs_alldata.txt')

#sub<-subsetOnSpeed(src, 'NM1', '>', 5.0)
sub2<-subset(src, subset=(plot=='NM4'))
hr <- substr(as.character(sub2$datetime), 12, 13)

windContours(hour = hr,
             wd = sub2$obs_dir,
             ws = sub2$obs_speed,
             speedlim = 15,
             spacing = 1,
             colour = rev(brewer.pal(11, "Spectral")),
             keytitle = "hourly wind frequencies for NM4 [%]")

savePlot(filename="/home/natalie/Desktop/dirunal_src_nm4.png", type="png")

map<-get_map(location = c(lon = -113.027724, lat = 43.602726), zoom = 9, maptype = 'terrain')
ggmap(map)

#bsb
d<-readData('/home/natalie/observations_paper/bsb_obs_alldata.txt')

dsub<-subsetOnSpeed(d, 'R2', '<', 6.0)

sub2<-subset(dsub, subset=(plot=='R15'))
hr <- substr(as.character(sub2$datetime), 12, 13)

windContours(hour = hr,
             wd = sub2$obs_dir,
             ws = sub2$obs_speed,
             speedlim = 30,
             spacing = 1,
             colour = rev(brewer.pal(11, "Spectral")),
             keytitle = "hourly wind frequencies for R15 [%]")

savePlot(filename="/home/natalie/Desktop/diurnal_r15.png", type="png")

map<-get_map(location = c(lon = -113.027724, lat = 43.602726), zoom = 9, maptype = 'terrain')
ggmap(map)
