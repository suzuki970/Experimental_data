# -------------setting path and initializing--------
currentLoc = dirname( sys.frame(1)$ofile )
root = strsplit(currentLoc , "Rscript")
root = paste(root[[1]][[1]], "/Rscript/",sep="")
path_toolbox = paste(root, "toolbox/",sep="")
source(paste(path_toolbox, "initialization.R", sep = ""))
# ----------------------------------------------------

saveLoc = currentLoc

sTime = -0.2
eTime = 4
countFigNum = 1

#### file loading 
load(paste(currentLoc,"/dataset_e1.rda", sep = ""))

numOfSub = length(unique(data_e1$sub))
subName = NULL
for( i in seq(max(data_e1$sub))){ subName = rbind(subName,paste("s", i, sep = ""))}
data_e1$sub = subName[data_e1$sub]

### show graph
data_e1_ave = aggregate( data_y ~ Color*Pattern*data_x, data = data_e1, FUN = "mean")

config <- list(lim_x = c(sTime, eTime),
               lim_y = c(min(data_e1_ave$data_y), max(data_e1_ave$data_y)),
               alpha = 0.1,
               stride = 0.1,
               label_x = "Time [sec]",
               label_y = "Pupil Changes [mm]",
               title = "pupil",
               grCol = rep(c("Black","Blue","Cyan","Green","Yellow","Red","Magenta"),2)
)

p <- disp(data_e1_ave,config,0,c("Color","Color"))
p <- p + facet_grid(. ~ Pattern)

p = setFigureStyle(p)

p <- p + theme(
  legend.position = 'none'
)
eval(parse(text=paste("p", countFigNum ,"=p", sep="")))
countFigNum = countFigNum+1

## mean during time from 0sec to 4sec
data_e1_anova = aggregate( data_y ~ sub*Color*Pattern, data = data_e1, FUN = "mean")

#### ANOVA
anovakun(data_e1_anova,"sAB",gg=T,long=T, peta=T)
output2wayANOVA(forDrawingSigANOVA)
sigPair = makeSigPair(forDrawingPost)

## mean during time from 0sec to 4sec
data_e1_plot = aggregate( data_y ~ Color*Pattern, data = data_e1_anova, FUN = "mean")
std_data = aggregate(data_y ~ Color*Pattern, data=data_e1_anova, FUN = "sd") 
std_data$data_y <- std_data$data_y / sqrt(numOfSub)

std_data$SE_min <- data_e1_plot$data_y - std_data$data_y
std_data$SE_max <- data_e1_plot$data_y + std_data$data_y

### bar plot
config <- list(lim_x = c(0.5,7.5),
               lim_y = c(min(std_data$SE_min), max(std_data$SE_max)),
               stride =c(-0.3,-0.2,-0.1,-0.0,0.1,0.2),
               label_x = "Condition",
               label_y = "Averaged Pupil Changes [mm]",
               title="average",
               gr = c("Black","Blue","Cyan","Green","Yellow","Red","Magenta",
                      "#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF","#FFFFFF"),
               gr_outline = rep(c("Black","Blue","Cyan","Green","Yellow","Red","Magenta"),2)
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

# figure output -----------------------------------------------------------

p = ggarrange(p1, p2,
              labels = c("(a)", "(b)"),
              ncol = 2, nrow = 1)
plot(p)

# width_fig=12
# height_fig=4
# ggsave(file = paste(saveLoc,"/fig3.pdf", sep = ""),
#        plot = p, dpi = 300,
#        width = width_fig, height = height_fig,
#        family="Times")
