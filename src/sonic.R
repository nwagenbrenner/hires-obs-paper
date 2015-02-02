library(ggplot2)
library(windtools)

f<-'/home/natalie/observations_paper/sonic/sonic_summary.csv' 

sonic<-read.table(f, sep=",", header=TRUE, stringsAsFactors=FALSE)
sonic$date<-as.POSIXct(sonic$date, format="%m/%d/%Y %H:%M")
sonic$z<-as.factor(sonic$z)

#s<-subset(sonic, subset=(as.POSIXlt(date)$hour == 10 & as.POSIXlt(date)$mday == 17))

t1<-as.POSIXct(strptime("2010-7-17 00:00:00", '%Y-%m-%d %H:%M:%S'))
t2<-as.POSIXct(strptime("2010-7-18 00:00:00", '%Y-%m-%d %H:%M:%S'))

s<-subset(sonic, subset=(date > t1 & date < t2))

u_scaled<-mapply(speed2u, 0.5, s$dir.1)
v_scaled<-mapply(speed2v, 0.5, s$dir.1)
s <- cbind(s, u_scaled, v_scaled)

#reduce overplotting
s<-subset(s, subset=(as.POSIXlt(date)$min == 0)) # & as.POSIXlt(date)$mday == 17))

p_sonic<-ggplot(s, aes(x=date, y=windspd, colour=z)) +
        geom_point(shape=19, size=1.5, alpha = 1) +
        #scale_colour_manual(values=c("red","green","blue"), name="Height AGL (m)") +
        scale_colour_discrete(name="Height AGL (m)") +
        xlab("Datetime") + ylab("Speed (m/s)") +
        theme_bw() +
        ggtitle("WSU Sonic Data") + 
        theme(axis.text = element_text(size = 14)) +
        theme(axis.title = element_text(size = 14)) +
        geom_segment(data=s, aes(x=date+u_scaled*60*60, y=10+as.numeric(z)+v_scaled/4,
         xend=date-u_scaled*60*60, yend=10+as.numeric(z)-v_scaled/4), arrow = arrow(ends="first", length = unit(0.2, "cm")), size = 0.7)
        

