# -------------setting path and initializing--------
currentLoc = dirname( sys.frame(1)$ofile )
root = strsplit(currentLoc , "Rscript")
root = paste(root[[1]][[1]], "/Rscript/",sep="")
path_toolbox = paste(root, "toolbox/",sep="")
source(paste(path_toolbox, "initialization.R", sep = ""))
# ----------------------------------------------------

saveLoc = currentLoc

countFigNum = 1

#### file loading 
load(paste(currentLoc,"/dataset_e1.rda", sep = ""))
numOfSub = length(unique(data_e1$sub))

data_e1$sub = subName[data_e1$sub]

data_e1 = data_e1[data_e1$data_x > 0,]
# data_e1 = data_e1[data_e1$sub != 's23',] #s23 is rejected because number trials is small(> 50% of all trials)

dat_mean = tapply(data_e1$data_y,list(data_e1$sub,data_e1$Color,data_e1$Pattern),min)
dat_mean = matrix(dat_mean,ncol = 1)
data_e1 = aggregate( data_y ~ sub*Color*Pattern, data = data_e1, FUN = "mean")
data_e1$data_y = dat_mean
data_e1_anova = data_e1

#### ANOVA
anovakun(data_e1_anova,"sAB",gg=F,long=T, peta=T)
output2wayANOVA(forDrawingSigANOVA)
sigPair = makeSigPair(forDrawingPost)

# ## mean during time from 0sec to 4sec
data_e1_plot = aggregate( data_y ~ Color*Pattern, data = data_e1_anova, FUN = "mean")
data_e1_plot$data_y = data_e1_plot$V1
data_e1_plot$V1 = NULL

std_data = aggregate(data_y ~ Color*Pattern, data=data_e1_anova, FUN = "sd")
std_data$data_y = std_data$V1
std_data$V1 = NULL
std_data$data_y <- std_data$data_y / sqrt(numOfSub)

std_data$SE_min <- data_e1_plot$data_y - std_data$data_y
std_data$SE_max <- data_e1_plot$data_y + std_data$data_y

### bar plot
config <- list(lim_x = c(0.5,7.5),
               lim_y = c(min(data_e1_plot$data_y), max(data_e1_plot$data_y)),
               stride =c(-0.3,-0.2,-0.1,-0.0,0.1,0.2),
               label_x = "Condition",
               label_y = "Peak Pupil Constriction [mm]",
               title="average",
               gr = c("Black","Blue","Cyan","Green","Yellow","Red","Magenta",
                      "#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF"),
               gr_outline = rep(c("Black","Blue","Cyan","Green","Yellow","Red","Magenta"),times=rep(2,7))
)

p <- ggplot(data_e1_plot, aes(x = Color, y = data_y, fill = interaction(Color,Pattern)))+
  geom_bar(stat = "identity", position = "dodge", colour = config$gr_outline)+
  # scale_fill_manual(values = c("blue","white")) +
  scale_fill_manual(values = config$gr) +

  geom_errorbar(data = std_data, aes(ymin = SE_min, ymax = SE_max),
                width = 0.3, size=0.2, position = position_dodge(.9) ) +

  geom_hline(yintercept=0, colour="black", linetype="solid", size = 0.5) +
  # ylim(config$lim_y) +
  xlab(config$label_x) + ylab(config$label_y)

p <- drawSignificance(p,sigPair,0.12,0.015)

p = setBarFigureStyle(p)
p <- p + theme(
  legend.position = 'none',
  axis.ticks.x = element_blank(),
  axis.text.x = element_text(angle = 30, hjust = 1),
  axis.line.x = element_blank()
)
eval(parse(text=paste("p", countFigNum ,"=p", sep="")))
countFigNum = countFigNum+1


# Difference --------------------------------------------------------------
y = data_e1[data_e1$Pattern == "Glare",]$data_y - data_e1[data_e1$Pattern == "Halo",]$data_y
data_e1_diff = data_e1[data_e1$Pattern == "Glare",]
data_e1_diff$data_y = y
data_e1_diff$Pattern = NULL

std_data = aggregate(data_y ~ Color, data=data_e1_diff, FUN = "sd")
std_data$data_y = std_data$V1
std_data$V1 = NULL
std_data$data_y <- std_data$data_y / sqrt(numOfSub)

data_e1_diff = aggregate(data_y ~ Color, data=data_e1_diff, FUN = "sd")
data_e1_diff$data_y = data_e1_diff$V1
data_e1_diff$V1 = NULL

std_data$SE_min <- data_e1_diff$data_y - std_data$data_y
std_data$SE_max <- data_e1_diff$data_y + std_data$data_y

config$gr_outline = c("Black","Blue","Cyan","Green","Yellow","Red","Magenta")

p <- ggplot(data_e1_diff, aes(x = Color, y = data_y, fill = Color))+
  geom_bar(stat = "identity", position = "dodge", colour = config$gr_outline)+
  # scale_fill_manual(values = c("blue","white")) +
  scale_fill_manual(values = config$gr) +
  
  geom_errorbar(data = std_data, aes(ymin = SE_min, ymax = SE_max),
                width = 0.3, size=0.2) +
  
  geom_hline(yintercept=0, colour="black", linetype="solid", size = 0.5) +
  # ylim(config$lim_y) +
  xlab(config$label_x) + ylab(config$label_y)

p = setBarFigureStyle(p)
p <- p + theme(
  legend.position = 'none',
  axis.ticks.x = element_blank(),
  axis.text.x = element_text(angle = 30, hjust = 1),
  axis.line.x = element_blank()
)
eval(parse(text=paste("p", countFigNum ,"=p", sep="")))
countFigNum = countFigNum+1

# # figure output -----------------------------------------------------------
# 
# p = ggarrange(p1, p2,
#               labels = c("(a)", "(b)"),
#               ncol = 2, nrow = 1)
plot(p1)

# width_fig=6
# height_fig=4
# ggsave(file = paste(saveLoc,"/fig3_3.pdf", sep = ""),
#        plot = p, dpi = 300,
#        width = width_fig, height = height_fig,
#        family="Times")
