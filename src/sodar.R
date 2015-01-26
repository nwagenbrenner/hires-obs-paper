library(ggplot2)

f<-'/home/natalie/observations_paper/sodar/sodar.csv'
f<-'/home/natalie/observations_paper/sodar/sodar_coxswell.csv'

sodar<-read.table(f, sep=",", header=TRUE, stringsAsFactors=FALSE)

sodar$datetime<-as.POSIXct(sodar$datetime, format="%m/%d/%Y %H:%M")

#s<-subset(sodar, subset=(as.POSIXlt(datetime)$hour == 11 & as.POSIXlt(datetime)$min == 0))
s<-subset(sodar, subset=(as.POSIXlt(datetime)$hour == 11 & as.POSIXlt(datetime)$mday == 16))
#s<-subset(sodar, subset=(as.character(datetime) == "2010-07-19 03:00"))

s<-subset(sodar, subset=(as.POSIXlt(datetime)$hour == 16 & as.POSIXlt(datetime)$mday == 15))

#t<-as.POSIXlt(sodar$datetime[1])

#---------------------
# set NA values
#---------------------
s$SPD[s$SPD==99.99]<-NA
s$DIR[s$DIR==9999]<-NA
s$W[s$W==99.99]<-NA


p<-ggplot(s, aes(x=SPD, y=z)) +
        geom_point(shape=19, size=1.5, alpha = 1) +
        xlab("Speed (m/s)") + ylab("Height AGL (m)") +
        theme_bw() +
        ggtitle("Cox's Well Sodar")
   
p <- p + theme(axis.text = element_text(size = 14))
p <- p + theme(axis.title = element_text(size = 14))

p <- p + facet_grid(. ~ datetime)



