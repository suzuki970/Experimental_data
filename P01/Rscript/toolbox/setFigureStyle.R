
setFigureStyle <- function(gData) {
  gData <- gData +theme(
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size=16),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(colour="black"),
    axis.text.y = element_text(colour="black"),
    panel.background = element_rect(fill = "transparent",color = 'black',size = 0.5),
    panel.grid.major = element_line(colour = "gray", size = 0.05),
    panel.grid.minor = element_line(colour = NA),
    axis.ticks = element_line(colour = "black",size = 0.5),
    # text = element_text(size = 16,family = "Times"),
    text = element_text(size = 16,family = "Helvetica"),
    legend.title = element_text(size=10),
    legend.text = element_text(size=10),
    legend.key=element_rect(colour="transparent", fill=NA),
    plot.background=element_rect(fill="transparent", colour=NA),
    legend.background=element_rect(fill="transparent", colour=NA)
    # axis.line = element_line(colour = 'black', size = 2)
  )
  return(gData)
}

setBarFigureStyle <- function(gData) {
  gData <- gData +theme(
    axis.title.x = element_text(size=16),
    axis.title.y = element_text(size=16),
    panel.border = element_blank(), 
    axis.line = element_line(),
    axis.text.x = element_text(colour="black"),
    axis.text.y = element_text(colour="black"),
    panel.background = element_rect(fill = "transparent",size = 0.5),
    # panel.grid.major = element_line(colour = NA),
    panel.grid.major.y = element_line(colour = "gray", size = 0.05),
    panel.grid.major.x = element_line(colour = NA),
    panel.grid.minor = element_line(colour = NA),
    axis.ticks = element_line(colour = "black",size = 0.5),
    text = element_text(size = 16,family = "Times"),
    legend.title = element_text(size=10),
    legend.text = element_text(size=10),
    legend.key=element_rect(colour="transparent", fill=NA),
    plot.background=element_rect(fill="transparent", colour=NA),
    legend.background=element_rect(fill="transparent", colour=NA)
    
    )
  return(gData)
}