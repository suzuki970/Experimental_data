library(gtable)
require(gtable)
library(foreach)
require(foreach)

unit.def <- 'inches'
plot.length.def <- 6
legend.length.def <- 1.5

as.unit <- function(n){return(unit(n, unit.def))}

construct_gtable <- function(grob_matrix, widths, heights, name='layout'){
  gtable_matrix(name = name, grobs=grob_matrix, widths=widths, heights=heights)
}

tidy_plot <- function(plot, plot.length, legend.length=0, legend.position='right'){
  g0 <- ggplotGrob(plot)
  p0 <- list(g0[3:5, 2:4])
  w <- plot.length
  if(!legend.length==0){
    if(length(g0$widths)>5){
      stopifnot(legend.position=='right') # only works if legend.position='right'
      legend <- g0[3:5, 5]
      p0 <- append(p0, list(legend))
      w <- c(w, legend.length)
    }
  }
  return(matrix(list(p0, as.unit(w), as.unit(h <- plot.length)), ncol=1)) # for future compatibility
}

combine_plots.vertical <- function(plots, plot.length, legend.length){
  plot.length <- ifelse(is.null(plot.length), plot.length.def, plot.length)
  legend.length <- ifelse(is.null(legend.length), legend.length.def, legend.length)
  r <- foreach(p=plots, .combine='cbind') %do% tidy_plot(p, plot.length, legend.length)
  grobs <- foreach(l=r[1,], .combine='c') %do% l
  w <- foreach(v=r[2,], .combine='unit.c') %do% v
  return(construct_gtable(matrix(grobs, nrow=1), widths=w, heights=as.unit(plot.length)))
}

combine_plots <- function(plots, vertical=T, plot.length=NULL, legend.length=NULL){
  stopifnot(vertical=T) # only vertical combining supported
  combine_plots.vertical(plots, plot.length, legend.length)
}
