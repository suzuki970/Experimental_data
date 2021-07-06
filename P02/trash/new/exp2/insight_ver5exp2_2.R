
source("//Users/yuta/Dropbox/Rscript/Insight/initialization.R")

setwd(paste(mfileLoc,"Insight/hiddenPictureAnalysis6.3.0/dataShow/", sep = ""))

# Show the time course ----------------------------------------------------

PLRdata <- readMat('data5.mat')
g <- c("NR->NR","NR->R",'R->R')
d <- makeDataSet(PLRdata,g)

config <- list(lim_x = c(-0.2, 3),
               lim_y = c(-0.5, 8),
               alpha = 0.3,
               label_x = "Time [s]",
               label_y = "Pupil Changes [%]",
               grCol = c("#ECB01F","#F8766D","#619CFF"),
               gr_point = c("#F8766D","#ECB01F","#619CFF"),
               groups = g
)

p <- disp(d,config,1)
# p <- p + theme(legend.position = 'none')

print(p)

ggsave(file = "/Users/yuta/Desktop/insight_exp2.pdf", plot = p, dpi = 100, width = 4.0, height = 2.85)


## bar plot

numOfConditions = 3
numOfSub = dim(PLRdata$barDataRNR)[2]/numOfConditions

ind_data <- data.frame(
  sample = rep(c('NR->NR','NR->R','R->R'), times = c(numOfSub,numOfSub,numOfSub)),
  y = PLRdata$barDataRNR[1,],
  cell   = rep(1:numOfConditions, times = c(numOfSub,numOfSub,numOfSub)),
  e_p   = rep(1:numOfSub,numOfConditions)
)


x = aggregate(y~cell*sample, data=ind_data, FUN="mean") 
std_data=aggregate(y~cell*sample, data=ind_data, FUN="sd") 
std_data$y <- std_data$y / sqrt(numOfSub)

SE_min <- x$y - std_data$y
SE_max <- x$y + std_data$y

config <- list(lim_x = c(0.5, 3.5),
               lim_y = c(-1, 9),
               label_x = "Condition",
               label_y = "Pupil Changes [%]",
               gr = c("#FFFFFF","#808080","#1A1A1A"),
               gr_point = c("#F8766D","#ECB01F","#619CFF")
)

## 

# drawing the graph-----------------------------------------------------------------

g1 <- ggplot(x, aes(x = cell, y = y, fill = sample))+
  geom_bar(stat = "identity", colour = "black")+
  scale_fill_manual(values = config$gr) +
  
  geom_point(data = ind_data,
             alpha = 0.3,
             aes(cell,y,color=sample)) +
  
  geom_line(data = ind_data,
            size = 0.5,
            alpha = 0.1,
            aes(x = cell, y = y, group = e_p)
  )+
  
  geom_line(aes(x=c(0,config$lim_x[1],config$lim_x[2]+1),y=c(0,0,0), group = 1), color="black") +
  
  scale_color_manual(values = config$gr_point) +
  geom_errorbar(aes(ymin = SE_min, ymax = SE_max),
                width = 0.05) +
  
  # scale_fill_nejm()+
  coord_cartesian(xlim=config$lim_x, ylim = config$lim_y)+
  scale_y_continuous(breaks = seq(config$lim_y[1],config$lim_y[2],1)) +
  # scale_x_discrete("Condition",
  #                  breaks =  c("A","B","C","D"),
  #                  labels = c('Inverted','Upright',"Inv vs. Up", "InvNR vs. UpR"))
  xlab(config$label_x) + ylab(config$label_y) + 
  scale_x_continuous(breaks = 1:3, 
                     labels = c(expression(paste("P"["NN"])),
                                expression(paste("P"["NR"])),
                                expression(paste("P"["RR"]))))
  # 
  # geom_signif(xmin=1, xmax=2, 
  #             annotations="*", 
  #             y_position = 5.5, 
  #             tip_length = 0.01)+
  # geom_signif(xmin=1, xmax=3, 
  #             annotations="*", 
  #             y_position = 6,
  #             tip_length = 0.01)

# geom_line(aes(x=c(0,0.5,4),y=c(0,0,0)), color="black")


g1 <- g1 +  theme(
  axis.text.x = element_text(colour="black"),
  axis.text.y = element_text(colour="black"),
  panel.background = element_rect(fill = "transparent",color = 'black'),
  panel.grid.major = element_line(colour = NA),
  panel.grid.minor = element_line(colour = NA),
  text=element_text(size=16,family="Times")
)

## show the graph
plot(g1)
# ggsave(file = "/Users/yuta/Desktop/insight_exp2bar.pdf", plot = g1, dpi = 300, width = 6.4, height = 4.8)
