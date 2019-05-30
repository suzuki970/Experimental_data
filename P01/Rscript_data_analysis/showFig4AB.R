# -------------setting path and initializing--------
currentLoc = dirname( sys.frame(1)$ofile )
root = strsplit(currentLoc , "Rscript")
root = paste(root[[1]][[1]], "/Rscript/",sep="")
path_toolbox = paste(root, "toolbox/",sep="")
source(paste(path_toolbox, "initialization.R", sep = ""))
# ----------------------------------------------------

saveLoc = currentLoc

countFigNum = 1
sTime = 0
eTime = 3

numOfCondition = 10

#### file loading 
load(paste(currentLoc,"/dataset_e1e2.rda", sep = ""))
data_e2$data_y = data_e2$data_y * 100 - 100 # to percent from propotion
numOfSub = length(unique(data_e2$sub))

# time course of pupil ------------------------------------------------------------
data_e2_ave = aggregate( data_y ~ Luminance*Pattern*data_x, data = data_e2, FUN = "mean")

config <- list(lim_x = c(-0.2, eTime),
               lim_y = c(-20,5),
               alpha = 0.1,
               stride = 5,
               label_x = "Time [sec]",
               label_y = "Pupil Changes [%]",
               title = "pupil",
               grCol = rep(c("black", "gray28", "gray54", "gray75", "gray88"),2)
)

p <- disp(data_e2_ave,config, 0, c("Luminance","interaction(Pattern,Luminance)"))

p = setFigureStyle(p)

p <- p + theme(
  legend.position = 'none',
  axis.title.x = element_blank(),
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank()
)

# print(p)
eval(parse(text=paste("p", countFigNum ,"=p", sep="")))
countFigNum = countFigNum+1

# averaged pupil changes from 0 sec to 3 sec ------------------------------------
data_e2 = data_e2[data_e2$data_x > 0,]
data_e2_anova = aggregate( data_y ~ sub*Luminance*Pattern, data = data_e2, FUN = "mean")
data_e2_anova$sub = subName[data_e2_anova$sub]

#### ANOVA
anovakun(data_e2_anova,"sAB",long=T, peta=T)
output2wayANOVA(forDrawingSigANOVA)

## mean during time from 0sec to 3 sec
data_e2_plot = aggregate( data_y ~ Luminance*Pattern, data = data_e2_anova, FUN = "mean")
std_data = aggregate(data_y ~ Luminance*Pattern, data=data_e2_anova, FUN = "sd") 
std_data$data_y <- std_data$data_y / sqrt(numOfSub)

std_data$SE_min <- data_e2_plot$data_y - std_data$data_y
std_data$SE_max <- data_e2_plot$data_y + std_data$data_y

# latency -----------------------------------------------------------------
timeLen = length(data_e2[data_e2$sub == '1',]$data_x)/10 - 1
time_x = seq(sTime,eTime,length = timeLen)

gColor = c("0.58","0.63","0.67","0.70","0.72")
gPattern =  rep(c("Glare","Halo"),times=c(5,5))

data_e2_lat <- data.frame(
  sub = rep(1:numOfSub,times=rep(numOfCondition,numOfSub)),
  Luminance = gColor[1:(numOfCondition/2)],
  Pattern = gPattern[rep(1:numOfCondition,numOfSub)],
  data_y = matrix(tapply(data_e2$data_y,
                         list(data_e2$Luminance,data_e2$Pattern,data_e2$sub),
                         function(x){return (time_x[which.min(x)])}),ncol=1)
)

sd = aggregate( data_y ~ Pattern, data = data_e2_lat, FUN = "sd")
m = aggregate( data_y ~ Pattern, data = data_e2_lat, FUN = "mean")

## ANOVA
anovakun(data_e2_lat,"sAB",long=T, peta=T)
output2wayANOVA(forDrawingSigANOVA)

## velocity ----------------------------------------------------------------
data_e2_vel <- data.frame(
  sub = rep(1:numOfSub,times=rep(timeLen*numOfCondition,numOfSub)),
  Luminance = gColor[rep(rep(1:(numOfCondition/2),times = rep(timeLen,numOfCondition/2)),numOfSub)],
  Pattern = gPattern[rep(rep(1:numOfCondition,times = rep(timeLen,numOfCondition)),numOfSub)],
  data_y = unlist(tapply(data_e2$data_y, list(data_e2$Luminance,data_e2$Pattern,data_e2$sub),function(x){return (diff(x))})),
  data_x = time_x
)

data_e2_vel$data_y = data_e2_vel$data_y*120 # tranceform to (%/s)
data_e2_vel$lat = rep(data_e2_lat$data_y, times=rep(timeLen,dim(data_e2_lat)[1]))
data_e2_vel = data_e2_vel[data_e2_vel$data_x > data_e2_vel$lat,]

anova_vel = aggregate( data_y ~ sub*Luminance*Pattern, data = data_e2_vel, FUN = "mean")

#### ANOVA
anovakun(anova_vel,"sAB",long=T,peta=T)
output2wayANOVA(forDrawingSigANOVA)

#### mean among participants
mean_vel = aggregate( data_y ~ Luminance*Pattern, data = anova_vel, FUN = "mean")
sd_vel = aggregate( data_y ~ Luminance*Pattern, data = anova_vel, FUN = "sd")
sd_vel$data_y <- sd_vel$data_y / sqrt(numOfSub)

mean_vel$SE_min <- mean_vel$data_y - sd_vel$data_y
mean_vel$SE_max <- mean_vel$data_y + sd_vel$data_y

#### making graph
config$lim_y = c(2.5,7.5)
config$stride = 1

config$stride = round(seq(config$lim_y[1],config$lim_y[2],config$stride),1)
config$label_y = "velocity of the pupil change [%/s]"
p <- dispLineGraph(mean_vel, config, c("Luminance","Pattern"))

eval(parse(text=paste("p", countFigNum ,"=p", sep="")))
countFigNum = countFigNum+1


# time-freqency -----------------------------------------------------------
eeg_data = aggregate( data_y ~ Luminance*Pattern*data_x, data = data_e3, FUN = "mean")

p <- ggplot(eeg_data,aes(x = data_x, y = data_y,group = interaction(Pattern,Luminance))) +
  geom_vline(xintercept=0, colour='black', linetype='longdash', size = 0.1) +
  geom_line(aes(color=Luminance,linetype =Pattern))+
  ylab('Normalized SSVEP amplitude at 7.5Hz') +
  scale_color_manual(values = config$grCol)  +
  scale_x_continuous(breaks = seq(config$lim_x[1],config$lim_x[2],0.5),expand = c(0, 0))

p = setFigureStyle(p)

p <- p + theme(
  legend.position = 'none',
  axis.title.x = element_blank(),
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank()
)
# print(p)
eval(parse(text=paste("p", countFigNum ,"=p", sep="")))
countFigNum = countFigNum+1

p = combineGraphs(seq(1,countFigNum-1),'p', NULL)
print(p)

# width_fig=10
# height_fig=9
# ggsave(file = paste(saveLoc,"/fig4AB.pdf", sep = ""),
#        plot = p, dpi = 300,
#        width = width_fig, height = height_fig,
       # family="Times")
