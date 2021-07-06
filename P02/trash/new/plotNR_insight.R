library("ggplot2")
require("R.matlab")

PLRdata <- readMat('/Users/Yuta-PC/Documents/matlab/Insight/hiddenPictureAnalysis6.3.0/datashow/data3.mat')

d <- PLRdata$dataForR
ribbondata <- data.frame(x=c(d[1, ,1],d[2, ,1]),
                         ymin=c(d[1, ,2],d[2, ,2]), 
                         ymax=c(d[1, ,3],d[2, ,3]))

datalen = 421;
ribbondata <- rbind(cbind(ribbondata[1:datalen,], group = "NR"), 
                    cbind(ribbondata[datalen+1:datalen*2,], group = "R"))

dat <- data.frame(x=c(d[1, ,1],d[2, ,1]),
                  y=c(d[1, ,4],d[2, ,4]))
dat <- rbind(cbind(dat[1:datalen,], group = "NR"), 
             cbind(dat[datalen+1:datalen*2,], group = "R"))

g2 <- ggplot() + 
  geom_ribbon(data = ribbondata, alpha = .3,
              aes(x = x, ymin = ymin, ymax = ymax, 
                  group = group, fill = group)) + 
  geom_line(data = dat, 
            aes(x = x, y = y, colour = group, group = group, linetype = group)) + 
  # ggtitle("") +
  xlab("Time [s]") + ylab("Pupil Changes [%]") + 
  coord_cartesian(xlim=c(-0.2, 3), ylim=c(-0.5, 7))+
  # xlim(-0.2, 1.5) + ylim(0.99, 1.04)+
  # lims(x = c(-0.2, 1.5), y = c(0.99, 1.04))+
  scale_x_continuous(expand = c(0, 0))
g2

# 
g2+theme(
  # panel.grid.major = element_line(colour = "black"),
  #            # panel.background = element_rect(fill = 'white', colour = 'black'),
  panel.background = element_rect(fill = "transparent",color = 'black'),
  panel.grid.major = element_line(colour = "black", size = 0.1),
  panel.grid.minor = element_line(colour = NA)
  # legend.position = "left"
  #            # plot.background = element_rect(fill = "transparent",color = NA)
)

# g2 + theme(plot.background = element_rect(fill = 'black', colour = 'black'))
# 
# p1 <- ggplot() + 
#   
#   ggtitle("geom_line")
# p1