
# -------------setting path and initializing--------
currentLoc = dirname( sys.frame(1)$ofile )
root = strsplit(currentLoc , "Rscript")
root = paste(root[[1]][[1]], "Rscript/",sep="")
path_toolbox = paste(root, "toolbox/",sep="")
source(paste(path_toolbox, "initialization.R", sep = ""))
# ----------------------------------------------------

# source("/Users/yuta/Dropbox/Rcode/Rscript/toolbox/initialization.R")

# mfileLoc = "/Users/yuta/Creative Cloud Files/Insight/hiddenPic4.1.4/dataShow/"
# setwd(paste(mfileLoc,"Insight/hiddenPic4.1.4/dataShow/", sep = ""))

# PLRdata <- readMat('/Users/yuta/Dropbox/MATLAB/P02_Insight/hiddenPic4.1.4/dataShow/data3.mat')
PLRdata <- readMat('../../analysis//data3.mat')

x <- data.frame(
  sample = c('NR->NR','NR->R','R->R'),
  y = PLRdata$barDataForR[,1],
  cell   = 1:3
)

numOfSub = dim(PLRdata$ind.data)[2]/3
ind_data <- data.frame(
  sample = rep(c('NR->NR','NR->R','R->R'), times = c(numOfSub,numOfSub,numOfSub)),
  y = PLRdata$ind.data[1,],
  cell   = rep(1:3, times = c(numOfSub,numOfSub,numOfSub)),
  e_p   = rep(1:numOfSub,3)
)

SE_min <- PLRdata$barDataForR[,1] - PLRdata$barDataForR[,2]
SE_max <- PLRdata$barDataForR[,1] + PLRdata$barDataForR[,2]

config <- list(lim_x = c(0.5, 3.5),
               lim_y = c(-5, 7),
               label_x = "Condition",
               label_y = "Pupil Changes [%]",
               gr = c("#FFFFFF","#808080","#1A1A1A"),
               gr_point = c("#F8766D","#ECB01F","#619CFF")
)
jitterVal <- c(rep(-0.225,numOfSub),rep(0.225,numOfSub))

## drawing the graph
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
                                expression(paste("P"["RR"]))))+
  
  geom_signif(xmin=1, xmax=2, 
              annotations="*", 
              y_position = 5.5, 
              textsize = 5,
              tip_length = 0.01,
              family="Times") +
  
  geom_signif(xmin=1, xmax=3, 
              annotations="*", 
              y_position = 6.5,
              textsize = 5,
              tip_length = 0.01,
              family="Times")

g1 = setFigureStyle(g1)

## show the graph
plot(g1)
# ggsave(file = "/Users/yuta/Desktop/insight_exp1.pdf", plot = g1, dpi = 300, width = 6.4, height = 4.8)
