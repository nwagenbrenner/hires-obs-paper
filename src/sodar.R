library(ggplot2)
library(windtools)
library(RColorBrewer)
library(grid)

#f<-'/home/natalie/observations_paper/sodar/SunsetRadar_Test1.csv' #sunset lake radar
#f<-'/home/natalie/observations_paper/sodar/100715DV.csv' #noaa GRI
#f<-'/home/natalie/observations_paper/sodar/sunsetlake.csv' #noaa sodar sunset lake (garbage)
#f<-'/home/natalie/observations_paper/sodar/sodar.csv' #wsu bsb downwind
f<-'/home/natalie/observations_paper/sodar/110831.csv' #wsu src
#f<-'/home/natalie/observations_paper/sodar/radar/raw_files/W10222.csv' #GRI radar speed/dir
#f<-'/home/natalie/observations_paper/sodar/sodar_coxswell.csv' #noaa cox's well

sodar<-read.table(f, sep=",", header=TRUE, stringsAsFactors=FALSE)
sodar$datetime<-as.POSIXct(sodar$datetime, format="%m/%d/%Y %H:%M")

#convert from MST to MDT for NOAA sodars
sodar$datetime<-sodar$datetime - 60*60

#s<-subset(sodar, subset=(as.POSIXlt(datetime)$hour %in% c(12,13) & as.POSIXlt(datetime)$mday == 1))
s<-subset(sodar, subset=(as.POSIXlt(datetime)$hour == 9 & as.POSIXlt(datetime)$mday == 18 & as.POSIXlt(datetime)$min ==30))
#s<-subset(sodar, subset=(as.POSIXlt(datetime)$hour == 10 )) # & as.POSIXlt(datetime)$min ==55))

#-------------------------------------------
# set NA values and speed, dir if necessary
#-------------------------------------------
#noaa ASC (e.g., cox's well, GRI)
#s$SPD[s$SPD==99.99]<-NA
#s$DIR[s$DIR==9999]<-NA
#s$W[s$W==99.99]<-NA
#title<-"GRI"
#title<-"Cox's Well"
#colnames(s)<-c('datetime','HT','SPD','DIR','W','SDW','IW','GPSD','GDIR','U','SDU','NU','IU','SNRU','V','SDV','NV','IV','SNRV','NW','SNRW')
#s <- na.omit(s)
#plots<-Sodar(s)

#wsu src scintech
s$SPD[s$SPD==99.99]<-NA
s$DIR[s$DIR==999.9]<-NA
s$W[s$W==99.99]<-NA
title<-"WSU SRC"
s <- na.omit(s)
plotSodar(s)

#wsu bsb scintech
#s$speed[s$speed==99.99]<-NA
#s$dir[s$dir==999.9]<-NA
#s$w[s$w==99.99]<-NA
#title<-"WSU"
#colnames(s)<-c('datetime','HT','SPD','DIR','W')
#s <- na.omit(s)
#plotSodar(s)

#noaa sunset lake sodar
#s$speed[s$speed=='***']<-NA
#s$dir[s$dir=='***']<-NA
#s$w[s$w=='***']<-NA
#s$speed<-mapply(uv2speed, s$u, s$v)
#s$dir<-mapply(uv2dir, s$u, s$v)
#title<-"Sunset Lake Sodar"
#colnames(s)<-c('datetime', 'HT', 'u', 'v', 'W', 'SPD', 'DIR')
#plotSodar(s)

#noaa sunset lake radar
#s$SPD[s$SPD==999999]<-NA
#s$DIR[s$DIR==999999]<-NA
#s$HT<-s$HT*1000 #convert km to m
#title<-"GRI Radar"
#plotRadar(s)
#------------------------------------
# combined speed, w, dir plots
#------------------------------------
#plotRadar(s)

plotSodar(s)

#------------------------------------
# individual plots for speed, dir, w
#------------------------------------
p_spd<-ggplot(s, aes(x=SPD, y=HT)) +
        geom_point(shape=19, size=1.5, alpha = 1, colour='darkblue') +
        xlab("Speed (m/s)") + ylab("Height AGL (m)") +
        theme_bw() +
        ggtitle(title) + 
        theme(axis.text = element_text(size = 14)) +
        theme(axis.title = element_text(size = 14)) +
        #geom_point(data=s, aes(x=W, y=z, colour='red'),shape = 19, size=1.5) +
        facet_grid(. ~ datetime)

p_dir<-ggplot(s, aes(x=DIR, y=HT)) +
        geom_point(shape=19, size=1.5, alpha = 1, colour='red') +
        xlab("Direction (m/s)") + ylab("Height AGL (m)") +
        theme_bw() +
        ggtitle(title) +
        theme(axis.text = element_text(size = 14)) +
        theme(axis.title = element_text(size = 14)) +
        facet_grid(. ~ datetime)


p_w<-ggplot(s, aes(x=W, y=HT)) +
        geom_point(shape=19, size=1.5, alpha = 1, colour='red') +
        xlab("W (m/s)") + ylab("Height AGL (m)") +
        theme_bw() +
        ggtitle(title) +
        theme(axis.text = element_text(size = 14)) +
        theme(axis.title = element_text(size = 14)) +
        facet_grid(. ~ datetime)


#====================================
# plot surface obs for comparison
#====================================

#-------------
# time series
#-------------
#src
#file = '/home/natalie/observations_paper/salmon_obs_alldata.txt'
#bsb
file = '/home/natalie/observations_paper/bsb_obs_alldata.txt'
speed <- readData(file)

t1<-as.POSIXct(strptime("2010-7-14 00:00:00", '%Y-%m-%d %H:%M:%S'))
t2<-as.POSIXct(strptime("2010-7-20 00:00:00", '%Y-%m-%d %H:%M:%S'))

sub<-subset(speed, subset=(datetime > t1 & datetime < t2 & plot == 'R5'))

#single plot with vectors for direction
u_scaled<-mapply(speed2u, 1, sub$obs_dir)
v_scaled<-mapply(speed2v, 1, sub$obs_dir)
sub <- cbind(sub, u_scaled, v_scaled)

p1<-ggplot(sub, aes(x=datetime, y=obs_speed)) + 
        geom_point(shape=19, size=1.5, alpha = 1, colour='darkblue') +
        geom_line(colour='darkblue') +
        xlab("Datetime") + ylab("Speed (m/s)") +
        theme_bw() +
        ggtitle("R2") +
        theme(axis.text.y = element_text(colour = 'darkblue', size=rel(1.5))) + 
        theme(axis.title.y = element_text(colour = 'darkblue', size=rel(1.5))) +
        scale_y_continuous(limits = c(0, 20)) +
        geom_segment(data=sub, aes(x=datetime+u_scaled*60*60, y=15+v_scaled,
         xend=datetime-u_scaled*60*60, yend=15-v_scaled), arrow = arrow(ends="first", length = unit(0.2, "cm")), size = 0.7)


# two plots
p1<-ggplot(sub, aes(x=datetime, y=obs_speed)) +
        geom_point(shape=19, size=1.5, alpha = 1, colour='darkblue') +
        geom_line(colour='darkblue') +
        xlab("Datetime") + ylab("Speed (m/s)") +
        theme_bw() +
        ggtitle("R2") +
        theme(axis.text.y = element_text(colour = 'darkblue', size=rel(1.5))) + 
        theme(axis.title.y = element_text(colour = 'darkblue', size=rel(1.5)))
   
p2<-ggplot(sub, aes(x=datetime, y=obs_dir)) +
        geom_point(shape=19, size=1.5, alpha = 1, colour='red') +
        geom_line(colour='red') +
        ylab("Direction") +
        theme(panel.background = element_rect(fill = NA)) + 
        theme(axis.text.y = element_text(colour = 'red', size=rel(1.5))) + 
        theme(axis.title.y = element_text(colour = 'red', size=rel(1.5)))

plotSpeedDirection(p1, p2)

#--------------
# vector plots
#--------------
file = '/home/natalie/observations_paper/bsb_obs_alldata.txt'
speed <- readData(file)

u<-mapply(speed2u, speed$obs_speed, speed$obs_dir)
v<-mapply(speed2v, speed$obs_speed, speed$obs_dir)
speed<-as.data.frame(cbind(speed,u,v))

dt<-as.POSIXct(strptime("2010-7-17 6:00:00", '%Y-%m-%d %H:%M:%S'))
sub<-subset(speed, subset=(datetime == dt))

lon = -113.027724
lat = 43.402726
maptype = 'terrain'
zoom = 12

m<-makeVectorMap(sub, lat, lon, zoom, maptype, colorscale='discrete', axis_labels=FALSE)


#==================================
#  functions
#==================================

plotSpeedDirection<-function(p1, p2){
    grid.newpage()

    # extract gtable
    g1 <- ggplot_gtable(ggplot_build(p1))
    g2 <- ggplot_gtable(ggplot_build(p2))

    # overlap the panel of 2nd plot on that of 1st plot
    pp <- c(subset(g1$layout, name == "panel", se = t:r))
    g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
        pp$l, pp$b, pp$l)

    # axis tweaks
    ia <- which(g2$layout$name == "axis-l")
    ga <- g2$grobs[[ia]]
    ax <- ga$children[[2]]
    ax$widths <- rev(ax$widths)
    ax$grobs <- rev(ax$grobs)
    ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
    g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
    g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

    # draw it
    grid.draw(g)
}

plotRadar<-function(s){
    s <- na.omit(s)
    u_scaled<-mapply(speed2u, 0.5, s$DIR)
    v_scaled<-mapply(speed2v, 0.5, s$DIR)
    s <- cbind(s, u_scaled, v_scaled)

    p<-ggplot(s, aes(x=SPD, y=HT)) +
        xlab("Speed (m/s)") + ylab("Height AGL (m)") +
        theme_bw() +
        ggtitle(title) + 
        scale_x_continuous(limits = c(0, 20)) +
        scale_y_continuous(limits = c(0, 4000)) +
        theme(axis.text = element_text(size = 14)) +
        theme(axis.title = element_text(size = 14)) +
        geom_segment(data=s, aes(x=SPD+u_scaled, y=HT+v_scaled*150, xend=SPD-u_scaled, yend=HT-v_scaled*150), 
                     arrow = arrow(ends="first", length = unit(0.2, "cm")), size = 0.7) +
        facet_grid(. ~ datetime)

    return(p)
}

plotSodar<-function(s){
    u_scaled<-mapply(speed2u, 1.5, s$DIR)
    v_scaled<-mapply(speed2v, 1.5, s$DIR)

    s <- cbind(s, u_scaled, v_scaled)
    myPalette <- colorRampPalette(c("darkblue", "blue", "lightblue", "darkorange", "red"),space = "rgb")
    sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(-1, 1), 
       breaks=c(-1.0, -0.5, 0, 0.5, 1.0), name="w (m/s)")

    p<-ggplot(s, aes(x=SPD, y=HT)) +
        #geom_point(shape=19, size=1.5, alpha = 1, colour='darkblue') +
        xlab("Speed (m/s)") + ylab("Height AGL (m)") +
        theme_bw() +
        ggtitle(title) + 
        theme(axis.text = element_text(size = 14)) +
        theme(axis.title = element_text(size = 14)) +
        scale_x_continuous(limits = c(-2, 15)) +
        scale_y_continuous(limits = c(0, 300)) +
        geom_segment(data=s, aes(x=SPD+u_scaled, y=HT+v_scaled*4,
         xend = SPD-u_scaled, yend = HT-v_scaled*4, 
         colour = s$W), arrow = arrow(ends="first", length = unit(0.2, "cm")), size = 0.7) +
        sc +
        facet_grid(. ~ datetime)

    return(p)
}

