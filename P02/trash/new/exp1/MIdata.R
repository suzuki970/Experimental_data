
source("/Users/Yuta-PC/Documents/Rscript/Insight/initialization.R")
source("/Users/Yuta-PC/Documents/Rscript/anovakun_480.R")
source("/Users/Yuta-PC/Documents/Rscript/outputResults.R")
PLRdata <- readMat('/Users/Yuta-PC/Documents/matlab/Insight/hiddenPic4.1.4/dataShow/data4.mat')

library('mi')
# library(VIM)

g <- c('NRNR-low',
       'NRNR-middle',
       'NRNR-high',
       'NRR-low',
       'NRR-middle',
       'NRR-high')

numOfSub=23
numOfcondition = 2

train.mi <- data.frame(
  sample = rep(g, times = c(numOfSub,numOfSub,numOfSub,numOfSub,numOfSub,numOfSub)),
  y = t(matrix(PLRdata$anovaData,nrow=1,byrow=F)),
  sub_num   = rep(1:numOfSub, length(g))
)

ind = which(train.mi$y == 0)
train.mi$y[ind] = c(NA)


mdf <- missing_data.frame(train.mi) # warnings about missingness patterns
mdf <- change(mdf, y = "y", what = "transformation", to = "identity")
imputations <- mi(mdf)

re_constData = imputations@data[["chain:2"]][["y"]]@data

train.re <- data.frame(
  cell = rep(1:2, times = c(numOfSub*3,numOfSub*3)),
  # sample = rep(g, times = c(numOfSub,numOfSub,numOfSub,numOfSub,numOfSub,numOfSub)),
  y = re_constData,
  sub_num = rep(1:numOfSub, length(g)),
  sample = rep(1:6, times = c(numOfSub,numOfSub,numOfSub,numOfSub,numOfSub,numOfSub)),
  gr = rep(rep(c("A","B","C"), times = c(numOfSub,numOfSub,numOfSub)),2)
)

x = aggregate( y ~ sample*sub_num, data = train.re, FUN="mean")
ind = order(x$sample)
x = x[ind,]
forAnova = x$y

ind_data <- data.frame(
  y = forAnova,
  cell = rep(1:2, times = c(numOfSub*3,numOfSub*3)),
  sub_num = rep(1:numOfSub, length(g)),
  gr = rep(rep(c("A","B","C"), times = c(numOfSub,numOfSub,numOfSub)),2),
  sample = rep(1:6, times = c(numOfSub,numOfSub,numOfSub,numOfSub,numOfSub,numOfSub)),
  e_p   = c(rep(1:numOfSub, 3),rep((numOfSub+1):(numOfSub*2),3))
)

forAnova = t(matrix(forAnova, nrow=6, byrow=T))
forAnova = forAnova[c(1:7,9:23),]

anovakun(forAnova, "sAB",2,3,eta = T)

x = aggregate( y ~ cell*gr, data = train.re, FUN="mean")
std_data = aggregate( y ~ cell*sample, data = train.re, FUN = "sd")
std_data$y <- std_data$y / sqrt(numOfSub)

SE_min <- x$y - std_data$y
SE_max <- x$y + std_data$y

config <- list(lim_x = c(0.5, 2.5),
               lim_y = c(-8, 12),
               title = "Glare",
               label_x = "Condition",
               label_y = "Pupil Changes [%]",
               gr = c("#FFFFFF","#808080","#1A1A1A"),
               gr_point = c("#F8766D","#ECB01F","#619CFF")
)


jitterVal <- rep(c(rep(-0.3,numOfSub),rep(0,numOfSub),rep(0.3,numOfSub)),numOfcondition)

g1 <- ggplot(x, aes(x = cell, y = y, fill = gr))+
  geom_bar(stat = "identity", position = "dodge", colour = "black")+
  geom_errorbar(aes(ymin = SE_min, ymax = SE_max),
                width = 0.1,
                position=position_dodge(.9)) +
  geom_line(aes(x=c(0,4,5,2,2,2),y=c(0,0,0,0,0,0)), color="black")+

  geom_point(data = ind_data,
             alpha = 0.3,
             aes(cell,y,color=gr),
             position=position_dodge(.9)) +

  geom_line(data = ind_data,
            size = 0.5,
            alpha = 0.1,
            aes(x = cell+jitterVal, y = y, group = e_p)) +

  scale_fill_nejm()+
  coord_cartesian(xlim=config$lim_x, ylim = config$lim_y)+
  scale_y_continuous(breaks = seq(config$lim_y[1],config$lim_y[2],2)) +
 
  xlab(config$label_x) + ylab(config$label_y) +
# +
  # geom_signif(comparisons = list(1:2),
  #             annotations="*",
  #             y_position = 13.5,
  #             textsize = 5,
  #             tip_length = 0.01,
  #             family="Times")+
  #
  # geom_signif(xmin=0.775,xmax=1.225,
  #             annotations="n.s.",
  #             y_position = 12,
  #             textsize = 5,
  #             tip_length = 0.01,
  #             family="Times")+
  #
  # geom_signif(xmin=1.775,xmax=2.225,
  #             annotations="n.s.",
  #             y_position = 12,
  #             textsize = 5,
  #             tip_length = 0.01,
  #             family="Times")+
  #

  scale_x_continuous(breaks = 1:numOfcondition,
                     labels = c(expression(paste("P"["NN"])), expression(paste("P"["NR"]))))+
  scale_fill_manual(values = config$gr)

g1 = setFigureStyle(g1)

plot(g1)
ggsave(file = "/Users/Yuta-PC/Desktop/miData.pdf", plot = g1, dpi = 300, width = 6.4, height = 4.8)
