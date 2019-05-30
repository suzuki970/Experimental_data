# -------------setting path and initializing--------
currentLoc = dirname( sys.frame(1)$ofile )
root = strsplit(currentLoc , "Rscript")
root = paste(root[[1]][[1]], "/Rscript/",sep="")
path_toolbox = paste(root, "toolbox/",sep="")
source(paste(path_toolbox, "initialization.R", sep = ""))
# ----------------------------------------------------

saveLoc = currentLoc

sTime = -0.2
eTime = 3
countFigNum = 1

#### file loading 
load(paste(currentLoc,"/dataset_e1e2.rda", sep = ""))
numOfSub = length(unique(data_e1$sub))
subName = NULL

# EEG power -------------------------------------------------------------
p <- ggplot(data_e1_power, aes(x = data_x, y = data_y, colour = Pattern)) +
  geom_line(aes(linetype = Pattern))+
  xlim(c(4,16))+
  ylim(c(0,0.35))+
  scale_color_manual(values = c("black", "gray28") )+
  xlab("Frequency[Hz]") + ylab("Evoked-Power[μV]")

p = setFigureStyle(p)

eval(parse(text=paste("p", countFigNum ,"=p", sep="")))
countFigNum = countFigNum+1


# peak SSVEP amplitude ----------------------------------------------------
data_e1_ave <- aggregate( data_y ~ Luminance*Pattern, data = data_e1, FUN = "mean")

data_e1_std <- aggregate( data_y ~ Luminance*Pattern, data = data_e1, FUN = "sd")
data_e1_std$data_y <- data_e1_std$data_y / sqrt(numOfSub)

data_e1_ave$SE_min <- data_e1_ave$data_y - data_e1_std$data_y
data_e1_ave$SE_max <- data_e1_ave$data_y + data_e1_std$data_y

config <- list( label_x = "Contrast condition",
                label_y = "Normalized SSVEP amplitude",
                title="average",
                grCol = c("black", "gray28", "gray54", "gray75", "gray88",
                          "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF", "#FFFFFF"),
                gr_outline = rep(c("black", "gray28", "gray54", "gray75", "gray88"),times=rep(2,5))
)

p <- dispLineGraph(data_e1_ave, config, c("Luminance","Pattern"))

p = setFigureStyle(p)

p <- p + theme(
  legend.position = 'none'
)
# print(p)
eval(parse(text=paste("p", countFigNum ,"=p", sep="")))
countFigNum = countFigNum+1

# Heatmap of the time frequency plot --------------------------------------
# fileLoc = "/Users/yuta/Dropbox/MATLAB/P01_GlareSSVEP/E1_EEG"
dat <- readMat(paste(currentLoc,"/matData/dat_mean_tf.mat", sep = ""))

sTime = 0.5
eTime = 3

dat_ft = data.frame(
  data_x = rep(rep( seq(min(dat$toi), max(dat$toi), length=length(dat$toi)), length(dat$foi)),2),
  data_y = rep(rep( seq(min(dat$foi), max(dat$foi), length=length(dat$foi)), times = rep(length(dat$toi),length(dat$foi))),2),
  data_z = rbind(matrix( t(dat$y[,,5]), ncol = 1),matrix( t(dat$y[,,10]), ncol = 1))
)
dat_ft$Pattern = rep(c("Glare","Control"),times=c(length(dat_ft$data_x)/2,length(dat_ft$data_x)/2))

dat_ft = dat_ft[dat_ft$data_x > sTime,]
dat_ft = dat_ft[dat_ft$data_x < eTime,]
dat_ft = dat_ft[dat_ft$data_y > 5,]
dat_ft = dat_ft[dat_ft$data_y < 17,]

config <- list(lim_x = c(sTime, eTime),
               lim_y = c(6,18),
               alpha = 1,
               stride = 1,
               label_x = "Time [sec]",
               label_y = "Freqency[Hz]"
)

p <- ggplot(dat_ft,aes(x = data_x, y=data_y)) + 
  geom_tile(aes(fill = data_z)) +
  scale_fill_distiller(palette = "Spectral") +
  scale_y_continuous(breaks = seq(config$lim_y[1],config$lim_y[2],config$stride),expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(config$lim_x[1],config$lim_x[2],0.5),expand = c(0, 0)) +
  xlab(config$label_x) + ylab(config$label_y)
p <- p + facet_grid(. ~ Pattern)

p = setFigureStyle(p)

eval(parse(text=paste("p", countFigNum ,"=p", sep="")))
countFigNum = countFigNum+1

# ANOVA -------------------------------------------------------------------
anovakun(data_e1,"sAB",gg=T,long=T, peta=T)
output2wayANOVA(forDrawingSigANOVA)


# # figure output -----------------------------------------------------------
p = ggarrange(p1, p2, p3,
              labels = c("B","C", "D"),
              ncol = 1, nrow = 3)
print(p)

width_fig=6
height_fig=4 
# 
# ggsave(file = paste(saveLoc,"/fig3.pdf", sep = ""),
#        plot = p3, dpi = 300,
#        width = width_fig, height = height_fig,
#        family="Times")

