# -------------setting path and initializing--------
currentLoc = dirname( sys.frame(1)$ofile )
root = strsplit(currentLoc , "Rscript")
root = paste(root[[1]][[1]], "/Rscript/",sep="")
path_toolbox = paste(root, "toolbox/",sep="")
source(paste(path_toolbox, "initialization.R", sep = ""))
# ----------------------------------------------------

saveLoc = currentLoc

load(paste(currentLoc,"/dataset_sup.rda", sep = ""))
g1_name = c("0.52","0.58","0.63","0.67","0.7","0.72","0.74")
g1_name = c(0.52,0.58,0.63,0.67,0.7,0.72,0.74)

numOfSub = length(unique(data_sup$sub))
subName = NULL

config <- list(lim_x = c(0.5,7.5),
               lim_y = c(0,1),
               stride =c(-0.3,-0.2,-0.1,-0.0,0.1,0.2),
               alpha_val = 0.5,
               label_x = "Contrast condition",
               label_y = "Probability",
               title="SI=0.5sec, CI=duration",
               gr_outline = c("black", "gray28", "gray54", "gray75", "gray88")
)

std_data = aggregate( data_y ~ Pattern*Luminance*Target*cell*n, data = data_sup, FUN = "sd")
data_sup_mean = aggregate( data_y ~ Pattern*Luminance*Target*cell*n, data = data_sup, FUN = "mean")

std_data$data_y <- std_data$data_y / sqrt(numOfSub)

data_sup_mean$SE_min <- data_sup_mean$data_y - std_data$data_y
data_sup_mean$SE_max <- data_sup_mean$data_y + std_data$data_y

p <- ggplot(data_sup_mean[data_sup_mean$Pattern=='Control',], aes(cell, data_y))+
  geom_point(shape=0, size = 2) +
  scale_color_manual(values = config$gr_outline)+
  xlab(config$label_x) + ylab(config$label_y) +
  geom_hline(yintercept=0.5, colour='black', linetype='solid', size = 0.1) +
  geom_errorbar(aes(ymin = SE_min, ymax = SE_max),width = 0.1, size=0.2) +
  scale_x_continuous(breaks = 1:7,label = g1_name) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = config$lim_y)

data_sup_mean = data_sup_mean[data_sup_mean$Pattern=='Glare',]
data_sup_mean$cell = rep(1:7,5)

g1_name = c("0.58","0.63","0.67","0.7","0.72")
# g1_name = c(0.58,0.63,0.67,0.7,0.72)

data_sup_mean$Target = rep(g1_name,times=rep(7,5))

p <- p +
  geom_point(data=data_sup_mean,aes(x = cell, y = data_y, color = Target),shape=16,size = 2)

# g1_name = c(0.58,0.63,0.67,0.7,0.72)
psyCurves = data.frame()
dat_th = NULL
for (i in 1:length(g1_name)){
  tx = data_sup_mean[data_sup_mean$Target == g1_name[i],]
  g = c(0.52,0.58,0.63,0.67,0.7,0.72,0.74)
  g = 1:7
  tx$cell = g[tx$cell]
  fit <- quickpsy(tx, cell, data_y, n)
  t = fit$curves
  t$Target = rep(g1_name[i], dim(fit$curves)[1])
  psyCurves = rbind(psyCurves,t)
  dat_th = rbind(dat_th, fit$thresholds[,"thre"])
}

# dat_th = dat_th*7

p <- p +
  geom_line(data= psyCurves, aes(x = x, y = y,color = Target)) + 
  geom_vline(xintercept=dat_th[1], colour='black', linetype='longdash', size = 0.1) +
  geom_vline(xintercept=dat_th[2], colour='black', linetype='longdash', size = 0.1) +
  geom_vline(xintercept=dat_th[3], colour='black', linetype='longdash', size = 0.1)

p = setBarFigureStyle(p)
print(p)

freqInd1 <- which( abs(psyCurves$x-dat_th[5]) == min(abs(psyCurves$x-dat_th[5])) )
psyCurves$x[281]

# 195 236 281 300 

# width_fig=8
# height_fig=6
# ggsave(file = paste(saveLoc,"/fig_SI.pdf", sep = ""),
#        plot = p, dpi = 300,
#        width = width_fig, height = height_fig,
#        family="Times")

# per perticipants --------------------------------------------------------
load(paste(currentLoc,"/dataset_sup.rda", sep = ""))
data_sup = data_sup[data_sup$Pattern=='Glare',]
data_sup$cell = rep(rep(1:7,times = rep(6,7)),5)
g1_name = c("0.58","0.63","0.67","0.7","0.72")
data_sup$Target = rep( rep(g1_name,times = rep(6,5)) ,times=rep(7,30))

psyCurves = data.frame()

dat_th = NULL
g1_name = c(0.58,0.63,0.67,0.7,0.72)
g = 1:7

for(iSub in 1:1){
  dat = data_sup[data_sup$sub == iSub,]
  for (i in 1:length(g1_name)){
    tx = dat[dat$Target == g1_name[i],]
    tx$cell = g[tx$cell]
    fit <- quickpsy(tx, cell, data_y, n)
    t = fit$curves
    t$Target = rep(g1_name[i],dim(fit$curves)[1])
    t$sub = rep(iSub,dim(fit$curves)[1])
    psyCurves = rbind(psyCurves,t)
    dat_th = rbind(dat_th,fit$thresholds[,"thre"])
  }
}
p <- ggplot(data_sup,aes(cell, data_y,color = Target))+
  geom_point(shape=0, size = 2) +
  geom_line(data= psyCurves, aes(x = x, y = y,color = interaction(sub,Target)))

print(p)

anova_data = data.frame(
  sub = rep(1:6,times=rep(5,6)),
  Luminance = rep(g1_name,6),
  data_y = dat_th
)
# anovakun(anova_data,"sA",gg=T,long=T, eta=T)
# output1wayANOVA(forDrawingSigANOVA)