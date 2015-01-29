library(ggplot2)
library(windtools)

f<-'/home/natalie/observations_paper/sonic/sonic_summary.csv' 

sonic<-read.table(f, sep=",", header=TRUE, stringsAsFactors=FALSE)
sonic$date<-as.POSIXct(sonic$date, format="%m/%d/%Y %H:%M")
sonic$z<-as.factor(sonic$z)

s<-subset(sonic, subset=(as.POSIXlt(date)$hour == 10 & as.POSIXlt(date)$mday == 17))

t1<-as.POSIXct(strptime("2010-7-16 00:00:00", '%Y-%m-%d %H:%M:%S'))
t2<-as.POSIXct(strptime("2010-7-17 00:00:00", '%Y-%m-%d %H:%M:%S'))

s<-subset(sonic, subset=(date > t1 & date < t2))


p_sonic<-ggplot(sonic, aes(x=date, y=wT, colour=z)) +
        geom_point(shape=19, size=1.5, alpha = 1) +
        #scale_colour_manual(values=c("red","green","blue"), name="Height AGL (m)") +
        scale_colour_discrete(name="Height AGL (m)") +
        xlab("Datetime (m/s)") + ylab("w'T'") +
        theme_bw() +
        ggtitle("WSU Sonic Data") + 
        theme(axis.text = element_text(size = 14)) +
        theme(axis.title = element_text(size = 14)) 
        

