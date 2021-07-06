# -------------setting path and initializing-------------
currentLoc = dirname( sys.frame(1)$ofile )
root = strsplit(currentLoc , "Rscript")
root = paste(root[[1]][[1]], "/Rscript/",sep="")
path_toolbox = paste(root, "toolbox/",sep="")
source(paste(path_toolbox, "initialization.R", sep = ""))
# ----------------------------------------------------

saveLoc = "/Users/yuta/Desktop/"

width_fig=7
height_fig=4.8
countFigNum = 1

# ##----- show color spectrum data --------
currentLoc = strsplit(currentLoc , "SI")
color_data <- read.csv(paste(currentLoc,"/colorData_SR3AR.csv", sep = ""))
color_data <- color_data[,-2:-4]

color_data1 <- as.matrix(color_data[25:dim(color_data)[1],seq(3,22,by=3)]) 
color_data1 <- t(matrix(color_data1,nrow=1))
color_data2 <- as.matrix(color_data[25:dim(color_data)[1],seq(4,23,by=3)])
color_data2 <- t(matrix(color_data2,nrow=1))

gColor <- c("Black","Green","Cyan","Yellow","Blue","Magenta","Red")

dat <- data.frame(
  loc = rep(1:2,times=c(length(color_data1),length(color_data1))),
  data_y = as.numeric(rbind(color_data1,color_data2)),
  data_x = rep(380:780,14),
  Color = rep(rep(1:7,times=rep(401,7)),2)
)

waveLen = length(dat[dat$Color == 'Black',]$data_x)
dat_area = aggregate( data_y ~ Color, data = dat, FUN = "sum")
# dat_area$data_y = dat_area$data_y / waveLen

dat$Color = gColor[dat$Color]
dat$Color <- factor(dat$Color, levels = c("Black","Blue","Cyan","Green","Yellow","Red","Magenta"))

dat_mean = aggregate( data_y ~ data_x*Color, data = dat, FUN = "mean")
dat_mean$ymin = rep(0,dim(dat_mean)[1])
dat_mean$ymax = dat_mean$data_y

config <- list(lim_x = c(380, 780),
               lim_y = c(0, 0.0045),
               stride = 0.001,
               alpha_val = 0.2,
               label_x = "Wavelength [nm]",
               label_y = expression(paste("Radiance [W*sr"^{"-1"},"*m"^{"-2"} ,"]" )),
               grCol = c("Black","Blue","Cyan","Green","Yellow","Red","Magenta"))

# p <- disp(dat_mean,config,1,c("Color","Color"))
p <- ggplot(dat_mean,aes(x = data_x, y = data_y, colour = Color, group = Color))+
  geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = Color), alpha = config$alpha, size = 0.05) +
  scale_fill_manual(values = config$grCol)+
  geom_line()

p <- p +
  scale_color_manual(values = config$grCol)+
  geom_vline(xintercept=0, colour='black', linetype='longdash', size = 0.1) +
  ggtitle(config$title) +
  xlab(config$label_x) + ylab(config$label_y) +
  coord_cartesian(xlim = config$lim_x, ylim=config$lim_y) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(
    legend.position = 'none'
  )

p <- setFigureStyle(p)
print(p)

eval(parse(text=paste("p", countFigNum ,"=p", sep="")))
countFigNum = countFigNum+1
