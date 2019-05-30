
source(paste(path_toolbox, "clearFunc.R", sep = ""))
clearAll()

currentLoc = dirname( sys.frame(1)$ofile )
root = strsplit(currentLoc , "Rscript")
root = paste(root[[1]][[1]], "Rscript/",sep="")
path_toolbox = paste(root, "toolbox/",sep="")

source(paste(root, "shaded_error/shaded_errorbar.R", sep = ""))
source(paste(path_toolbox, "setFigureStyle.R", sep = ""))
source(paste(path_toolbox, "outputResults.R", sep = ""))
source(paste(path_toolbox, "tidy_plot.R", sep = ""))
source(paste(path_toolbox, "makeDataset.R", sep = ""))
# source("http://aoki2.si.gunma-u.ac.jp/R/src/covar_test.R")

## read ANOVAkun
if(exists(".anovakun.env")){
  sys.source(paste(path_toolbox,"anovakun_482.R", sep = ""), envir = .anovakun.env)
}else{
  .anovakun.env <- new.env()
  sys.source(paste(path_toolbox,"anovakun_482.R", sep = ""), envir = .anovakun.env)
  attach(.anovakun.env)
}


# Package install(just once) ---------------------------------------------------------
# liblist <- c("'R.matlab'","'ggsci'","'ggsignif'","'ggplot2'","'fastICA'","'quickpsy'","'plyr'",
#              "'devtools'","'gridExtra'","'png'","'jpeg'","'lme4'","'ggpubr'","'grid'",
#              "'plotly'","'MASS'","'plotly'","RColorBrewer","'multcomp'","'car'","'lmerTest'",
#              "'gridExtra'","'ggmcmc'","'simr'","'EMAtools'")
# 
# for (i in 1:length(liblist)){
#   eval(parse(text=paste("install.packages(",liblist[i], ")",sep="")))
# }
# devtools::install_github("trinker/plotflow")
# install.packages("foreach", repos="http://R-Forge.R-project.org")
# library(devtools)
# install_github("norimune/glmmstan")
# load library ----------------------------------------------------------
require("R.matlab")

liblist <- c("'MASS'","'gridExtra'","ggsci","ggsignif","ggplot2","grid","'fastICA'",
           "'lme4'","'fastICA'","'lme4'","'lmerTest'","'ggpubr'","quickpsy","'plotflow'","'png'",
           "'jpeg'","'jpeg'","'plotly'","RColorBrewer","'multcomp'","'car'","'gridExtra'","glmmstan",
           "ggmcmc","simr","EMAtools")

for (i in 1:length(liblist)){
  eval(parse(text=paste("library(",liblist[i], ")",sep="")))
}

# font.add("Arial", regular = "Arial.ttf", bold = "Arial Bold.ttf",italic = "Arial Italic.ttf", bolditalic = "Arial Bold Italic.ttf")
# loadfonts()

subName = NULL
for( i in seq(30)){ 
  if(i<10){subName = rbind(subName,paste("s0", i, sep = ""))} 
  else{subName = rbind(subName,paste("s", i, sep = ""))}
}