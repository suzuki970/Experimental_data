
source("/Users/yuta/Creative Cloud Files/Rscript/Insight/initialization.R")
setwd(paste(mfileLoc,"Insight/hiddenPic4.1.4/dataShow/", sep = ""))

PLRdata <- readMat('data4.mat')
g <- c('1:Low confidence','2:High confidence')
numOfbar = 4
numOfcondition = 2

numOfSub = dim(PLRdata$ind.data)[2]/numOfbar

ind_data <- data.frame(
  sample = rep(rep(g, times = c(numOfSub,numOfSub)),numOfcondition),
  y = PLRdata$ind.data[1,],
  cell   = rep(1:numOfcondition, times = c(numOfSub*numOfcondition,numOfSub*numOfcondition)),
  e_p   = c(rep(1:numOfSub, numOfcondition),rep((numOfSub+1):(numOfSub*numOfcondition),numOfcondition))
)

x = aggregate( y ~ cell * sample, data = ind_data, FUN = "mean") 
std_data = aggregate(y~cell*sample, data=ind_data, FUN = "sd") 
std_data$y <- std_data$y / sqrt(numOfSub)

SE_min <- x$y - std_data$y
SE_max <- x$y + std_data$y

config <- list(lim_x = c(0.5, 2.5),
               lim_y = c(-6, 14),
               title = "Glare",
               label_x = "Condition",
               label_y = "Pupil Changes [%]",
               gr = c("#FFFFFF","#808080")
)


jitterVal <- rep(c(rep(-0.225,numOfSub),rep(0.225,numOfSub)),numOfcondition)

g1 <- ggplot(x, aes(x = cell, y = y, fill = sample))+
  geom_bar(stat = "identity", position = "dodge", colour = "black")+
  geom_errorbar(aes(ymin = SE_min, ymax = SE_max),
                width = 0.1,
                position=position_dodge(.9)) +
  geom_line(aes(x=c(0.0,4,5,2.5),y=c(0,0,0,0)), color="black")+

  geom_point(data = ind_data,
             alpha = 0.3,
             aes(cell,y,color=sample),
             position=position_dodge(.9)) +
  
  geom_line(data = ind_data,
            size = 0.5,
            alpha = 0.1,
            aes(x = cell+jitterVal, y = y, group = e_p)) +

  scale_fill_nejm()+
  coord_cartesian(xlim=config$lim_x, ylim = config$lim_y)+
  scale_y_continuous(breaks = seq(config$lim_y[1],config$lim_y[2],2)) +
  
  xlab(config$label_x) + ylab(config$label_y) + 
  geom_signif(comparisons = list(1:2), 
              annotations="*", 
              y_position = 13.5, 
              textsize = 5,
              tip_length = 0.01,
              family="Times")+
  
  geom_signif(xmin=0.775,xmax=1.225, 
              annotations="n.s.", 
              y_position = 12, 
              textsize = 5,
              tip_length = 0.01,
              family="Times")+
  
  geom_signif(xmin=1.775,xmax=2.225, 
              annotations="n.s.", 
              y_position = 12,
              textsize = 5,
              tip_length = 0.01,
              family="Times")+
  
  
  scale_x_continuous(breaks = 1:numOfcondition, 
                     labels = c(expression(paste("P"["NN"])), expression(paste("P"["NR"]))))+
  scale_fill_manual(values = config$gr)

g1 = setFigureStyle(g1)

plot(g1)
ggsave(file = "/Users/yuta/Desktop/insight_exp2.pdf", plot = g1, dpi = 300, width = 7, height = 4.8)
