# -------------setting path and initializing--------
currentLoc = dirname( sys.frame(1)$ofile )
root = strsplit(currentLoc , "Rscript")
root = paste(root[[1]][[1]], "/Rscript/",sep="")
path_toolbox = paste(root, "toolbox/",sep="")
source(paste(path_toolbox, "initialization.R", sep = ""))
# ----------------------------------------------------

sTime = -0.2
eTime = 4

gColor = rep(c("Black","Blue","Cyan","Green","Yellow","Red","Magenta"),2)
gPattern = c("Glare","Halo")

# #### making dataset for e1 #################################
fileLoc = paste(currentLoc, "/matData/", sep = "")
dat <- readMat(paste(fileLoc,"data_e1.mat", sep = ""))
numOfTrial = dim(dat$y)[1]
numOfSub = length(unique(dat$sub))
lengthOfTime = dim(dat$y)[2]

x = seq(sTime,eTime,length=lengthOfTime)
sh = rep(c(1,2),times=c(7,7))

ind_data <- data.frame(
  sub =  rep( dat$sub, times = rep( lengthOfTime, numOfTrial)),
  data_y = t(matrix(t(dat$y),nrow=1)),
  data_x = rep( x, numOfTrial),
  Color = rep( dat$condition, times = rep(lengthOfTime, numOfTrial)),
  Pattern = rep( sh[dat$condition], times = rep(lengthOfTime, numOfTrial))
 )
ind_data$Color = gColor[ind_data$Color]
ind_data$Color <- factor(ind_data$Color, levels = c("Black","Blue","Cyan","Green","Yellow","Red","Magenta"))

ind_data$Pattern = gPattern[ind_data$Pattern]


# ind_data = ind_data[ind_data$data_x > 1,]
data_e1 = aggregate( data_y ~ sub*Color*Pattern*data_x, data = ind_data, FUN = "mean")

dat_mean = unlist(tapply(data_e1$data_y,list(data_e1$sub,data_e1$Color,data_e1$Pattern),min))
dat_mean = matrix(dat_mean,ncol = 1)

save(data_e1,file = paste(currentLoc,"/dataset_e1.rda", sep = ""))

# #### making dataset for e2 #################################
dat = readMat(paste(fileLoc,"data_e2.mat", sep = ""))

numOfTrial = dim(dat$datRes)[1]
numOfSub = length(unique(dat$sub))
sType = rep(1:2,times=c(7,7))
ind_data = data.frame(
  sub =  dat$sub,
  data_y = dat$datRes,
  Color = dat$condition,
  Pattern = sType[dat$condition]
 )

ind_data$Color = gColor[ind_data$Color]
ind_data$Color = factor(ind_data$Color, levels = c("Black","Blue","Cyan","Green","Yellow","Red","Magenta"))

ind_data$Pattern = gPattern[ind_data$Pattern]
ind_data$Pattern = factor(ind_data$Pattern, levels = gPattern)

ind_data = ind_data[order(ind_data$sub, ind_data$Color, ind_data$Pattern),]
dat_mean = matrix(tapply(ind_data$data_y,list(ind_data$Pattern,ind_data$Color,ind_data$sub),mean),ncol = 1)
dat_sd = matrix(tapply(ind_data$data_y,list(ind_data$Pattern,ind_data$Color,ind_data$sub),sd),ncol = 1)

dat_sd = dat_sd*3
ind_data$dat_mean = rep(dat_mean,times = rep(8,14*numOfSub))
ind_data$minsd = ind_data$dat_mean - rep(dat_sd,times = rep(8,14*numOfSub))
ind_data$maxsd = ind_data$dat_mean + rep(dat_sd,times = rep(8,14*numOfSub))

ind_data = ind_data[ind_data$data_y < ind_data$maxsd,]
ind_data = ind_data[ind_data$data_y > ind_data$minsd,]

data_e2 = aggregate( data_y ~ sub*Color*Pattern, data = ind_data, FUN = "mean")

cri = data_e2[ data_e2$Color == "Black" & data_e2$Pattern == "Halo",]
cri = rep(cri$data_y,14)
data_e2$data_afy = mapply(function(x,y){return (x/y)},data_e2$data_y,cri)

data_e2$data_afy = data_e2$data_afy*100 -100

save(data_e2,file = paste(currentLoc,"/dataset_e2.rda", sep = ""))

#### making dataset for e3 #################################
dat <- readMat(paste(fileLoc,"data_e3.mat", sep = ""))

numOfTrial = dim(dat$y)[1]
numOfSub = length(unique(dat$sub))
lengthOfTime = dim(dat$y)[2]

x = seq(sTime,eTime,length=lengthOfTime)

ind_data <- data.frame(
  sub =  rep( dat$sub, times = rep( lengthOfTime, numOfTrial)),
  data_y = t(matrix(t(dat$y),nrow=1)),
  data_x = rep( x, numOfTrial),
  Color = rep( gColor[dat$condition], times = rep(lengthOfTime, numOfTrial))
    )
ind_data$Pattern = "Homogeneous"

data_e3 = aggregate( data_y ~ sub*Color*Pattern*data_x, data = ind_data, FUN = "mean")
g <- c("Black","Blue","Cyan","Green","Yellow","Red","Magenta")
data_e3$Color <- factor(data_e3$Color, levels = g)

save(data_e3,file = paste(currentLoc,"/dataset_e3.rda", sep = ""))

##################################################

# fileLoc = "/Users/yuta/Dropbox/MATLAB/P03_GlarePupil/colorGlareExp/analysis/dataAnalysis/"
# dat <- readMat(paste(fileLoc,"data5.mat", sep = ""))
# numOfTrial = dim(dat$y)[1]
# numOfSub = length(unique(dat$sub))
# lengthOfTime = dim(dat$y)[2]
# 
# x = seq(sTime,eTime,length=lengthOfTime)
# sh = rep(c(1,2),times=c(7,7))
# 
# ind_data <- data.frame(
#   sub =  dat$sub,
#   data_y = dat$y,
#   Color = dat$condition,
#   Pattern = sh[dat$condition]
# )
# 
# ind_data$Color = gColor[ind_data$Color]
# ind_data$Color <- factor(ind_data$Color, levels = c("Black","Blue","Cyan","Green","Yellow","Red","Magenta"))
# 
# ind_data$Pattern = gPattern[ind_data$Pattern]
# 
# data_e1 = aggregate( data_y ~ sub*Color*Pattern, data = ind_data, FUN = "mean")
# 
# # dat_mean = tapply(data_e1$data_y,list(data_e1$sub,data_e1$Color,data_e1$Pattern),min)
# # dat_mean = matrix(dat_mean,ncol = 1)
# 
# # data_e1 = aggregate( data_y ~ sub*Color*Pattern, data = ind_data, FUN = "mean")
# # data_e1$data_y = dat_mean
# # save(data_e1,file = paste(currentLoc,"/dataset_e1.rda", sep = ""))
# save(data_e1,file = paste(currentLoc,"/dataset_e1_min.rda", sep = ""))
# 
