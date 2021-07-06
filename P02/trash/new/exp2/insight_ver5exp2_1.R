
# -------------setting path and initializing--------
currentLoc = dirname( sys.frame(1)$ofile )
root = strsplit(currentLoc , "Rscript")
root = paste(root[[1]][[1]], "Rscript/",sep="")
path_toolbox = paste(root, "toolbox/",sep="")
source(paste(path_toolbox, "initialization.R", sep = ""))
# ----------------------------------------------------
mfileLoc="/Users/yuta/Dropbox/MATLAB/P02_Insight"
setwd(paste(mfileLoc,"/hiddenPictureAnalysis6.3.0/dataShow/", sep = ""))

PLRdata <- readMat('data4.mat')

numOfbar = 8
numOfcondition = 4

numOfSub = dim(PLRdata$ind.data)[2]/numOfbar
ind_data <- data.frame(
  sample = rep(rep(c('NR','R'), times = c(numOfSub,numOfSub)),4),
  y = PLRdata$ind.data[1,],
  cell   = rep(1:numOfcondition, times = c(numOfSub*2,numOfSub*2,numOfSub*2,numOfSub*2)),
  e_p   = c(rep(1:numOfSub, 2),rep((numOfSub+1):(numOfSub*2),2),
            rep((numOfSub*2+1):(numOfSub*3),2),rep((numOfSub*3+1):(numOfSub*4),2))
)

x = aggregate( y ~ cell * sample, data = ind_data, FUN="mean") #mean
std_data = aggregate(y~cell*sample, data=ind_data, FUN = "sd") 
std_data$y <- std_data$y / sqrt(numOfSub)

SE_min <- x$y - std_data$y
SE_max <- x$y + std_data$y

config <- list(lim_x = c(-0.2, 4.0),
               lim_y = c(0, 9.5),
               title = "Glare",
               label_x = "Condition",
               label_y = "Pupil Changes [%]")

jitterVal <- rep(c(rep(-0.225,numOfSub),rep(0.225,numOfSub)),4)

g1 <- ggplot(x, aes(x = cell, y = y, fill = sample))+
  geom_bar(stat = "identity", position = "dodge", colour = "black")+
  geom_errorbar(aes(ymin = SE_min, ymax = SE_max),
                width = 0.1,
                position=position_dodge(.9)) +
  
  geom_point(data = ind_data,
             alpha = 0.3,
             aes(cell,y,color=sample),
             position=position_dodge(.9)) +

  geom_line(data = ind_data,
            size = 0.5,
            alpha = 0.1,
            aes(x = cell+jitterVal, y = y, group = e_p)
            )+
  
  geom_signif(xmin=0.775,xmax=1.225, 
              annotations="*", 
              y_position = 8, 
              textsize = 5,
              tip_length = 0.01,
              family="Times")+
  
  geom_signif(xmin=1,xmax=2, 
              annotations="*", 
              y_position = 9, 
              textsize = 5,
              tip_length = 0.01,
              family="Times")+
  
  geom_signif(xmin=1.775,xmax=2.225, 
              annotations="*", 
              y_position = 8, 
              textsize = 5,
              tip_length = 0.01,
              family="Times")+
  
  geom_signif(xmin=3.775,xmax=4.225, 
              annotations="*", 
              y_position = 8, 
              textsize = 5,
              tip_length = 0.01,
              family="Times")+
  
  scale_fill_nejm()+
  coord_cartesian(ylim = config$lim_y)+
  scale_y_continuous(breaks = seq(config$lim_y[1],config$lim_y[2],1)) +
  xlab(config$label_x) + ylab(config$label_y) + 
  scale_x_continuous(breaks = 1:numOfcondition, 
                     labels = c('Inverted','Upright',"Inv vs. Up", "InvNR vs. UpR"))

g1 = setFigureStyle(g1)

plot(g1)
# ggsave(file = "/Users/yuta/Desktop/insight_exp2.pdf", plot = g1, dpi = 300, width = 6.4, height = 4.8)
