#BSB=================
test<-subset(dsubhour, subset=(plot %in% c('TSW3', 'TSW4', 'TSW5', 'TWSW3', 'TWSW4', 'R12', 
            'R22', 'R19', 'R16')))
test$z <- as.factor(c('high', 'high', 'mid', 'low', 'low', 'mid', 'high', 'low', 'mid'))
test$transect<-as.factor(c('TWSW', 'R', 'R', 'R', 'TSW', 'TSW', 'TSW', 'TWSW', 'TWSW'))
test$plot<-as.factor(test$plot)
order <-c('TSW3', 'TSW4', 'TSW5', 'TWSW3', 'TWSW4', 'R12',
            'R22', 'R19', 'R16')
test$plot <- factor(test$plot, levels = order)
order <-c('low', 'mid', 'high')
test$z <- factor(test$z, levels = order)

#=====downslope flow plot=====================================
test$labels <- c('R12', 'R16', NA, NA, NA, 'TSW4', 'TSW5', NA, NA)

p<-ggplot(test, aes(x=z, y=obs_speed, colour=transect, shape=transect)) +
   geom_point(size=4.0, alpha = 1) +
   geom_line(aes(group=transect)) +
   xlab("Slope Location") + ylab("Speed (m/s)") +
   theme_bw()
   p <- p + theme(axis.text = element_text(size = 16))
   p <- p + theme(axis.title = element_text(size = 16))
   p <- p + geom_text(data=test, aes(x = z, y = obs_speed, label = labels),
         colour="black", size=5, hjust=-0.3, vjust=0) + theme(legend.position = "none")
   p <- p + annotate("text", x = 0.76, y = 3.1, label = "TSW3", size = 5)
   p <- p + annotate("text", x = 0.88, y = 3.30, label = "R22", size = 5)
   p <- p + annotate("text", x = 0.81, y = 2.9, label = "TWSW3", size = 5)
   p <- p + annotate("text", x = 1.90, y = 3.7, label = "R19", size = 5)
   p <- p + annotate("text", x = 1.95, y = 3.15, label = "TWSW4", size = 5)

savePlot('/home/natalie/Desktop/downslope_transect.png')
   

#=============upslope flow plot===============================
test$labels <- c('R12', 'R16', NA, NA, NA, NA, 'TSW5', NA, NA)

p<-ggplot(test, aes(x=z, y=obs_speed, colour=transect, shape=transect)) +
   geom_point(size=4.0, alpha = 1) +
   geom_line(aes(group=transect)) +
   xlab("Slope Location") + ylab("Speed (m/s)") +
   theme_bw()
   p <- p + theme(axis.text = element_text(size = 16))
   p <- p + theme(axis.title = element_text(size = 16))
   p <- p + geom_text(data=test, aes(x = z, y = obs_speed, label = labels),
         colour="black", size=5, hjust=-0.3, vjust=0) + theme(legend.position = "none")
   p <- p + annotate("text", x = 0.80, y = 2.6, label = "TSW3", size = 5)
   p <- p + annotate("text", x = 0.85, y = 2.67, label = "R22", size = 5)
   p <- p + annotate("text", x = 0.75, y = 2.48, label = "TWSW3", size = 5)
   p <- p + annotate("text", x = 1.75, y = 2.79, label = "TWSW4", size = 5)
   p <- p + annotate("text", x = 2.23, y = 2.77, label = "TSW4", size = 5)
   p <- p + annotate("text", x = 1.86, y = 2.98, label = "R19", size = 5)
   
savePlot('/home/natalie/Desktop/upslope_transect.png')
   
    
#SRC=================
test<-subset(dsubhour, subset=(plot %in% c('NM4', 'NM3', 'NM1', 'SW4', 'SW3', 'SW2',
            'SE5', 'SE4', 'SE3', 'NW4', 'NW3', 'NW2', 'NE4', 'NE3', 'NE1')))
test$z <- as.factor(c('high', 'mid', 'low', 'high', 'mid', 'low', 'high', 'mid', 'low',
                       'high', 'mid', 'low', 'high', 'mid', 'low'))
test$transect<-as.factor(c('NE', 'NE', 'NE', 'NM', 'NM', 'NM', 'NW', 'NW', 'NW',
                            'SE', 'SE', 'SE', 'SW', 'SW', 'SW'))
test$aspect<-as.factor(c('N', 'N', 'N', 'N', 'N', 'N', 'N', 'N', 'N',
                            'S', 'S', 'S', 'S', 'S', 'S'))
test$plot<-as.factor(test$plot)
order <-c('NM4', 'NM3', 'NM1', 'SW4', 'SW3', 'SW2','SE5', 'SE4', 'SE3', 'NW4', 'NW3', 
            'NW2', 'NE4', 'NE3', 'NE1' )
test$plot <- factor(test$plot, levels = order)
order <-c('low', 'mid', 'high')
test$z <- factor(test$z, levels = order)

#savePlot('/home/natalie/Desktop/synoptic_downvalley_1100.png')
#=============upslope flow plot===============================
test$labels <- c('NE1', 'NE3', 'NE4', 'NM1', 'NM3', 'NM4', 'NW2', 'NW3', 'NW4',
                 'SE3', 'SE4', 'SE5', 'SW2', 'SW3', 'SW4')

p<-ggplot(test, aes(x=z, y=obs_speed, colour=aspect, shape=aspect)) +
   geom_point(size=4.0, alpha = 1) +
   geom_line(aes(group=transect)) +
   xlab("Slope Location") + ylab("Speed (m/s)") +
   theme_bw()
   p <- p + theme(axis.text = element_text(size = 16))
   p <- p + theme(axis.title = element_text(size = 16))
   p <- p + geom_text(data=test, aes(x = z, y = obs_speed, label = labels),
         colour="black", size=5, hjust=-0.3, vjust=0) + theme(legend.position = "none")
   #p <- p + ylim(0,4.5)
   #p <- p + ggtitle("1600")
   #p <- p + annotate("text", x = 0.80, y = 2.6, label = "TSW3", size = 5)
   #p <- p + annotate("text", x = 0.85, y = 2.67, label = "R22", size = 5)
   #p <- p + annotate("text", x = 0.75, y = 2.48, label = "TWSW3", size = 5)
   #p <- p + annotate("text", x = 1.75, y = 2.79, label = "TWSW4", size = 5)
   #p <- p + annotate("text", x = 2.23, y = 2.77, label = "TSW4", size = 5)
   #p <- p + annotate("text", x = 1.86, y = 2.98, label = "R19", size = 5)
   
savePlot('/home/natalie/Desktop/upslope_transect.png')
   

