
makeDataSet <- function(PLRdata,gr) {
  d <- PLRdata$dataForR
  data_x <- NULL
  data_y <- NULL
  ymin <- NULL
  ymax <- NULL
  for (i in 1 : dim(d)[1] ) {
    data_x <- c(data_x, d[i, ,1])
    ymin <- c(ymin, d[i, ,2])
    ymax <- c(ymax, d[i, ,3])
    data_y <- c(data_y, d[i, ,4])
  }
  
  # order_line <- c(6,1,2,3,7,5,4,13,8,9,10,14,12,11)
  # order_line <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
  
  order_line <- c(1,2,3,4,5,6,7)
  
  ribbondata <- data.frame(data_x,data_y,ymin,ymax)
  datalen = length(ymin)
  
  groups <- NULL
  order <- NULL
  for (i in 1 : dim(d)[1] ) {
    groups <- c(groups,rep(gr[i], dim(d)[2]))
    order <- c(order,rep(order_line[i], dim(d)[2]))
  }
  ribbondata  <- cbind(ribbondata,groups,order)
  return(ribbondata)
}

makeDataSetAll <- function(PLRdata,gr,range_x) {
  
  dat <- PLRdata$anovaData
  data_x <- NULL
  data_y <- NULL
  ymin <- NULL
  ymax <- NULL
  for (i in 1 : dim(dat)[3] ) {
    meanVal = apply(dat[,,i], 2, mean)
    data_x <- c(data_x, range_x)
    ymin <- c(ymin, meanVal - (apply(dat[,,i], 2, sd) / sqrt(dim(dat)[1])))
    ymax <- c(ymax, meanVal + (apply(dat[,,i], 2, sd) / sqrt(dim(dat)[1])))
    data_y <- c(data_y, meanVal)
  }
  
  # order_line <- c(6,1,2,3,7,5,4,13,8,9,10,14,12,11)
  # order_line <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
  
  order_line <- c(1,2,3,4,5,6,7)
  
  ribbondata <- data.frame(data_x,data_y,ymin,ymax)
  datalen = length(ymin)
  
  groups <- NULL
  order <- NULL
  for (i in 1 : dim(dat)[3] ) {
    groups <- c(groups,rep(gr[i], dim(dat)[2]))
    order <- c(order,rep(order_line[i], dim(dat)[2]))
  }
  ribbondata  <- cbind(ribbondata,groups,order)
  return(ribbondata)
}

disp <- function(ribbondata,config,shadeFl,factors){
  
  if (shadeFl == 1) {
    numOfSub = length(unique(ribbondata$sub))
    eval(parse(text=paste(
      "data_std = aggregate( data_y ~ data_x * ",factors[1],"*",factors[2],
      ", data = ribbondata, FUN = 'sd')",
      sep=""))) 
    eval(parse(text=paste(
      "ribbondata = aggregate( data_y ~ data_x * ",factors[1],"*",factors[2],
      ", data = ribbondata, FUN = 'mean')",
      sep="")))
    
    data_std$data_y <- data_std$data_y / sqrt(numOfSub)
    ribbondata$ymin <- ribbondata$data_y - data_std$data_y
    ribbondata$ymax <- ribbondata$data_y + data_std$data_y
    
    eval(parse(text=paste(
      "g2 <- ggplot(ribbondata,
      aes(x = data_x, y = data_y, colour = ", factors[1],", group = ",factors[1],"))+",
      # "annotation_raster(image, -Inf, Inf, -Inf, Inf) +",
      "geom_ribbon(aes(ymin = ymin, ymax = ymax, fill = ",factors[1], "), alpha = config$alpha, size = 0.05) +",
      "scale_fill_manual(values = config$grCol)+",
      # "geom_line(aes(linetype=", factors[1],"))",
      "geom_line()",
      sep="")))
    
    g2 <- g2 +
      scale_color_manual(values = config$grCol, name = factors[1])+
      geom_vline(xintercept=0, colour='black', linetype='longdash', size = 0.1) +
      ggtitle(config$title) +
      xlab(config$label_x) + ylab(config$label_y) +
      coord_cartesian(xlim = config$lim_x, ylim=config$lim_y) +
      scale_x_continuous(expand = c(0, 0)) 
      # scale_y_continuous(breaks = seq(config$lim_y[1],config$lim_y[2],config$stride),expand = c(0, 0))
    
  }else{ 
    
    eval(parse(text=paste(
      "g2 <- ggplot(ribbondata,aes(x = data_x, y = data_y, colour = ", factors[1],", group = ",factors[2],"))+", 
      "geom_line(aes(linetype = Pattern))",
      # "geom_line()",
      sep="")))
    
    g2 <- g2 + 
      geom_vline(xintercept=0, colour='black', linetype='longdash', size = 0.1) +
      # ggtitle(config$title) +
      scale_color_manual(values = config$grCol)  +
      xlab(config$label_x) + ylab(config$label_y) +
      coord_cartesian(xlim=config$lim_x, ylim=config$lim_y) +
      scale_x_continuous(expand = c(0, 0))
      # scale_y_continuous(expand = c(0, 0.1))
    # scale_y_continuous(breaks = seq(config$lim_y[1],config$lim_y[2],config$stride),expand = c(0, 0))
    
  }
  
  g2 = setBarFigureStyle(g2)
  
  return(g2)
}


dispLineGraph <- function(ribbondata, config, factors){
  
  eval(parse(text=paste(
    "g2 <- ggplot(ribbondata,aes(x = ", factors[1],
    ", y = data_y, colour = ", factors[1],
    ", group = ",factors[2],
    ", shape = ",factors[2],")) + ", 
    "geom_point(position = position_dodge(.2), size = 3)+",
    # "ggeom_line(linetype = 'solid') + ",
    # "geom_line(position = position_dodge(.2) )+",
    "geom_errorbar(data = ribbondata, aes(ymin = SE_min, ymax = SE_max),
                  width = 0.1, position = position_dodge(.2) )", sep="")))
  
  g2 <- g2 + 
    scale_color_manual(values = config$grCol)  + 
    xlab(config$label_x) + ylab(config$label_y) +
    coord_cartesian(ylim=config$lim_y)+
    
    theme(
      # legend.position = 'none',
      plot.margin = grid::unit(c(.5, 1, .5, 0), "cm")
    )
  
  g2 = setBarFigureStyle(g2)
  g2 <- g2 + theme(
    axis.ticks.x = element_blank(),
    # axis.text.x = element_text(angle = 30, hjust = 1),
    axis.line.x = element_blank()
  )
  return(g2)
}

makeDataSetFFT <- function(PLRdata,gr,range_x) {
  
  dat <- PLRdata$anovaData
  data_x <- NULL
  data_y <- NULL
  ymin <- NULL
  ymax <- NULL
  for (i in 1 : dim(dat)[3] ) {
    a <- array(0, dim=c(dim(dat)[1], dim(dat)[2]))
    for (j in 1:dim(dat)[1] ) {
      a[j,] = abs(fft( dat[j,,i] - mean(dat[j,,i]) ))
      a[j,] = (a[j,] / (dim(dat)[2]))
    }
    meanVal = apply(a, 2, mean)
    # meanVal = abs(fft(meanVal))
    data_x <- c(data_x, range_x)
    ymin <- c(ymin, meanVal - (apply(a, 2, sd) / sqrt(dim(dat)[1])))
    ymax <- c(ymax, meanVal + (apply(a, 2, sd) / sqrt(dim(dat)[1])))
    data_y <- c(data_y, meanVal)
  }
  
  # order_line <- c(6,1,2,3,7,5,4,13,8,9,10,14,12,11)
  # order_line <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)
  
  order_line <- c(1,2,3,4,5,6,7)
  
  ribbondata <- data.frame(data_x,data_y,ymin,ymax)
  datalen = length(ymin)
  
  groups <- NULL
  order <- NULL
  for (i in 1 : dim(dat)[3] ) {
    groups <- c(groups,rep(gr[i], dim(dat)[2]))
    order <- c(order,rep(order_line[i], dim(dat)[2]))
  }
  ribbondata  <- cbind(ribbondata,groups,order)
  return(ribbondata)
}

drawSignificance <- function(p,sigPair,y_pos,range) {
  if(!is.null(sigPair)){
    for(i in 1:dim(sigPair)[1]){
      p <- p + geom_signif(xmin=sigPair[i,1], xmax=sigPair[i,3],annotations="*", y_position = y_pos+(i-1)*range,
                           textsize = 5, size=0.2, tip_length = 0.00,family="Times")
    }
  }
  return(p)
}

combineGraphs <- function(graphNum,p,layout){
  
  titleStr = c("'(a)'", "'(b)'", "'(c)'", "'(d)'", "'(e)'", "'(f)'")
  st = paste(p,graphNum, sep = "", collapse=",")
  labelSt = titleStr[seq(1,length(graphNum))]
  labelSt = paste(labelSt, collapse=",")
  
  ncolNum = round(length(graphNum) / 2 )
  
  if (is.numeric(layout)){
    eval(parse(text=paste("p = grid.arrange(", 
                          st ,",layout_matrix = layout)",
                          sep="")))
  }else{
    eval(parse(text=paste("p = ggarrange(",
                          st ,",labels = c(",
                          labelSt,
                          "),ncol = 2, nrow =", ncolNum, ")",
                          sep="")))
  }
  # print(p)
  return(p)
}