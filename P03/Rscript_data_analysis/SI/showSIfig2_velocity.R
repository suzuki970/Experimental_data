# -------------setting path and initializing--------
currentLoc = dirname( sys.frame(1)$ofile )
root = strsplit(currentLoc , "Rscript")
root = paste(root[[1]][[1]], "/Rscript/",sep="")
path_toolbox = paste(root, "toolbox/",sep="")
source(paste(path_toolbox, "initialization.R", sep = ""))
# ----------------------------------------------------

# --------- velocity --------------------------------------------

#### file loading 
currentLoc = strsplit(currentLoc , "SI")
saveLoc = "/Users/yuta/Desktop/"
load(paste(currentLoc,"/dataset_e1.rda", sep = ""))
countFigNum = 1

####  making data for calculating velocity ---

sTime = min(data_e1$data_x)
eTime = max(data_e1$data_x)
timeLen = length(data_e1[data_e1$sub == 1 & data_e1$Pattern == 'Glare' & data_e1$Color == 'Black',]$data_y)

time_x = seq(sTime,eTime,length = timeLen)

numOfCondition = length( unique(data_e1$Color) ) * 2
numOfSub = length(unique(data_e1$sub))

data_e1 = data_e1[data_e1$data_x > 0,]
data_e1_vel = data_e1[data_e1$data_x != data_e1$data_x[1], ]
data_e1_vel = data_e1_vel[order(data_e1_vel$sub,data_e1_vel$Pattern,data_e1_vel$Color),]
data_e1_vel$data_y = unlist(tapply(data_e1$data_y, list(data_e1$Color,data_e1$Pattern,data_e1$sub),function(x){return (diff(x))}))
data_e1_vel$data_y = data_e1_vel$data_y * 1000

#### mean after 1,000ms after stimulus presentation among trial
data_e1_vel = data_e1_vel[data_e1_vel$data_x > 1, ]
anova_vel = aggregate( data_y ~ sub*Color*Pattern, data = data_e1_vel, FUN = "mean")
anova_vel = anova_vel[order(anova_vel$Pattern,anova_vel$sub),]

#### ANOVA
anovakun(anova_vel,"sAB",gg=T,long=T,peta=T)
output2wayANOVA(forDrawingSigANOVA)
sigPair = makeSigPair(forDrawingPost)

#### mean among participants
mean_vel = aggregate( data_y ~ Color*Pattern, data = anova_vel, FUN = "mean")
sd_vel = aggregate( data_y ~ Color*Pattern, data = anova_vel, FUN = "sd")
sd_vel$data_y <- sd_vel$data_y / sqrt(numOfSub)

mean_vel$SE_min <- mean_vel$data_y - sd_vel$data_y
mean_vel$SE_max <- mean_vel$data_y + sd_vel$data_y

#### making graph
config = list(lim_x = c(0.5,7.5),
               lim_y = c(0,0.6),
              stride = 0.2,
              label_x = "",
               label_y = "Velocity of pupil change [mm/s]",
               grCol = c("Black","Blue","Cyan","Green","Yellow","Red","Magenta")
)
config$stride = round(seq(config$lim_y[1],config$lim_y[2],config$stride),1)

p <- dispLineGraph(mean_vel, config, c("Color","Pattern"))
p <- drawSignificance(p,sigPair,0.45,0.02)
p <- p + scale_y_continuous(breaks = config$stride,expand = c(0, 0))

p <- p + theme(
  legend.position = 'none',
  axis.ticks.x = element_blank(),
  axis.text.x = element_text(angle = 30, hjust = 1),
  axis.line.x = element_blank()
)
eval(parse(text=paste("p", countFigNum ,"=p", sep="")))
countFigNum = countFigNum+1

## --------------- latency -----------------------

#### file loading
load(paste(currentLoc,"/dataset_e1.rda", sep = ""))
# data_e1 = data_e1[data_e1$sub != 14 & data_e1$sub != 7,]
sTime = 0
eTime = 4

data_e1 = data_e1[data_e1$data_x <= eTime & data_e1$data_x >= sTime,]
# config <- list(lim_x = c(-0.2, 4),
#                lim_y = c(-0.5, max(data_e1$data_y)),
#                alpha = 0.1,
#                stride = 0.1,
#                label_x = "Time [sec]",
#                label_y = "Pupil Changes [mm]",
#                title = "pupil",
#                grCol = rep(c("Black","Blue","Cyan","Green","Yellow","Red","Magenta"),2)
# )
# 
# p <- disp(data_e1,config,0,c("Color","Color"))
# p <- p + facet_grid(sub ~ Pattern)

ind <- which( abs(data_e1$data_x-sTime) == min(abs(data_e1$data_x-sTime)) )
data_e1_lat = data_e1[ind,]
data_e1_lat = data_e1_lat[order(data_e1_lat$sub,data_e1_lat$Pattern,data_e1_lat$Color),]

timeLen = length(data_e1$data_x[data_e1$Color == "Black" & data_e1$sub==1 & data_e1$Pattern=="Glare"])
time_x = seq(sTime,eTime,length=timeLen)

# data_e1_lat = transform(data_e1_lat,)

data_e1_lat$data_y = matrix(tapply(data_e1$data_y, 
                                   list(data_e1$Color,data_e1$Pattern,data_e1$sub),
                                   function(x){return (time_x[which.min(x)])}),ncol=1)

data_e1_lat$data_x = NULL

# #### ANOVA ##
anova_lat = data_e1_lat
anova_lat = anova_lat[order(anova_lat$Pattern,anova_lat$sub),]

anovakun(anova_lat,"sAB",gg=T,long=T,peta=T)
output2wayANOVA(forDrawingSigANOVA)
sigPair = makeSigPair(forDrawingPost)

# #### mean among paricipants
mean_lat = aggregate( data_y ~ Pattern*Color, data = anova_lat, FUN = "mean")
names(mean_lat)[3] <- 'data_y'
sd_lat = aggregate( data_y ~ Pattern*Color, data = data_e1_lat, FUN = "sd")
names(sd_lat)[3] <- 'data_y'

sd_lat$data_y <- sd_lat$data_y / sqrt(numOfSub)

mean_lat$SE_min <- mean_lat$data_y - sd_lat$data_y
mean_lat$SE_max <- mean_lat$data_y + sd_lat$data_y

#### making graph
config = list(lim_x = c(0.5,7.5),
              lim_y = c(0,1.2),
              label_x = "",
              stride = 0.2,
              label_y = "Time latency [s]",
              grCol = c("Black","Blue","Cyan","Green","Yellow","Red","Magenta")
)
config$stride = round(seq(config$lim_y[1],config$lim_y[2],config$stride),1)

p <- dispLineGraph(mean_lat, config, c("Color","Pattern"))
p <- drawSignificance(p,sigPair,1.1,0.01)
p <- p + scale_y_continuous(breaks = config$stride,expand = c(0, 0))

p <- p + theme(
  legend.position = 'none',
  axis.ticks.x = element_blank(),
  axis.text.x = element_text(angle = 30, hjust = 1),
  axis.line.x = element_blank()
)
eval(parse(text=paste("p", countFigNum ,"=p", sep="")))
countFigNum = countFigNum+1

# # show graph --------------------------------------------------------------
p = combineGraphs(seq(1,countFigNum-1),'p',NULL)
print(p)
# ggsave(file = paste(saveLoc,"/fig_velocity.pdf", sep = ""),
#        plot = p, dpi = 300,
#        width = 8, height = 4,
#        family="Times")

# ###### Coorelation between PLR and time latency --------------------------------
# coor_lat_vel = data.frame(
#   lat = anova_lat$data_y,
#    vel = anova_vel$data_y
# )
# 
# # res <- lm(lat ~ vel, data = coor_lat_vel)
# print(cor.test(coor_lat_vel$vel, coor_lat_vel$lat, method="pearson"))
# 
# p <- ggplot() +
#   # geom_abline(intercept = interc_val, slope = slope_val, size = 0.1, color = "gray") +
#   # geom_abline(intercept = newavg$reaction, slope = newavg$data_y, size = 1, color = "black") +
#   # geom_line(data = newavg, aes(x = data_y, y = reaction), col = "black", size = 1) +
#   geom_point(data = coor_lat_vel,
#              aes(x = vel, y = lat), size = 2)
# print(p)