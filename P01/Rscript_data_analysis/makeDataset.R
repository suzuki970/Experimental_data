# -------------setting path and initializing--------
currentLoc = dirname( sys.frame(1)$ofile )
root = strsplit(currentLoc , "Rscript")
root = paste(root[[1]][[1]], "/Rscript/",sep="")
path_toolbox = paste(root, "toolbox/",sep="")
source(paste(path_toolbox, "initialization.R", sep = ""))
# ----------------------------------------------------

sTime = -0.2
eTime = 3

# the condition types are described as below
f1 = "Luminance"
f2 = "Pattern"

g1_name = c("0.58","0.63","0.67","0.70","0.72")
g2_name = c("Glare","Control")

g1 <- c(rep(g1_name,2),"task")
g2 <- c(rep(g2_name,times=c(5,5)),"task")


# data power --------------------------------------------------------------
fileLoc = paste(currentLoc, "/matData/", sep = "")
dat <- readMat(paste(fileLoc,"dat_power.mat", sep = ""))
numOfTrial = dim(dat$y)[1]
lengthOfTime = dim(dat$y)[2]

data_e1_power <- data.frame(
  Pattern = g2_name[dat$condition],
  data_y = t(matrix(t(dat$y),nrow=1)),
  data_x = dat$x
)

# #### making dataset #################################
dat <- readMat(paste(fileLoc,"dat_pupil.mat", sep = ""))
numOfTrial = dim(dat$y)[1]
numOfSub = length(unique(dat$sub))
lengthOfTime = dim(dat$y)[2]

x = seq(sTime,eTime,length=lengthOfTime)

ind_data <- data.frame(
  sub =  rep( dat$sub, times = rep( lengthOfTime, numOfTrial)),
  data_y = t(matrix(t(dat$y),nrow=1)),
  data_x = rep( x, numOfTrial)
)
eval(parse(text=paste("ind_data$",f1,"=",
                      "rep( g1[dat$condition], times = rep(lengthOfTime, numOfTrial))",
                      sep="")))
eval(parse(text=paste("ind_data$",f1, "<- factor(ind_data$",f1, ",levels = c('",paste(g1_name,collapse="','"),"'))",sep="")))

eval(parse(text=paste("ind_data$",f2,"=",
                      "rep( g2[dat$condition], times = rep(lengthOfTime, numOfTrial))",
                      sep="")))
eval(parse(text=paste("ind_data$",f2, "<- factor(ind_data$",f2, ",levels = c('",paste(g2_name,collapse="','"),"'))",sep="")))

eval(parse(text=paste("data_e2 = aggregate( data_y ~ sub*",f1,"*",f2,"*data_x, data = ind_data, FUN = 'mean')",sep="")))


# #### making dataset eeg #################################
# EEG  :'1:RK','2:YS','3:YT','4"TS','5:KI','6:YN',7:AK','8:HT','9:RY'
# Pupil:'7:AK','8:HT','5:KI','1:RK','4:TS','6:YN','2:YS','0:YS2','3:YT','9:YT2'

dat <- readMat(paste(fileLoc,"dat_eeg.mat", sep = ""))

numOfSub = length(unique(dat$sub))
subNum = c(7,8,5,1,4,6,2,9,3,10)

ind_data <- data.frame(
  sub =  subNum[dat$sub],
  data_y = dat$y
)
eval(parse(text=paste("ind_data$",f1,"=","g1[dat$condition]",sep="")))
eval(parse(text=paste("ind_data$",f1, "<- factor(ind_data$",f1, ",levels = c('",paste(g1_name,collapse="','"),"'))",sep="")))

eval(parse(text=paste("ind_data$",f2,"=","g2[dat$condition]",sep="")))
eval(parse(text=paste("ind_data$",f2, "<- factor(ind_data$",f2, ",levels = c('",paste(g2_name,collapse="','"),"'))",sep="")))

eval(parse(text=paste("data_e1 = aggregate( data_y ~ sub*",f1,"*",f2,", data = ind_data, FUN = 'mean')",sep="")))


# #### making dataset time-frequency #################################
# EEG  :'1:RK','2:YS','3:YT','4"TS','5:KI','6:YN',7:AK','8:HT','9:RY'
# Pupil:'7:AK','8:HT','5:KI','1:RK','4:TS','6:YN','2:YS','0:YS2','3:YT','9:YT2'

# dat <- readMat(paste(fileLoc,"dat_tf.mat", sep = ""))
dat <- readMat(paste(fileLoc,"dat_tf.mat", sep = ""))

numOfSub = length(unique(dat$sub))

ind_data <- data.frame(
  sub =  dat$sub,
  data_y = dat$y,
  data_x = dat$x
)

eval(parse(text=paste("ind_data$",f1,"=","g1[dat$condition]",sep="")))
eval(parse(text=paste("ind_data$",f1, "<- factor(ind_data$",f1, ",levels = c('",paste(g1_name,collapse="','"),"'))",sep="")))

eval(parse(text=paste("ind_data$",f2,"=","g2[dat$condition]",sep="")))
eval(parse(text=paste("ind_data$",f2, "<- factor(ind_data$",f2, ",levels = c('",paste(g2_name,collapse="','"),"'))",sep="")))

data_e3 = ind_data

data_e3 = data_e3[order(data_e3$sub,data_e3$Luminance,data_e3$Pattern),]
# data_e3 = data_e3[data_e3$data_x > 0.69 & data_e3$data_x < 3,]
data_e3 = data_e3[data_e3$data_x >= -0.2 & data_e3$data_x <= 3,]

# data_e3$data_y = unlist(tapply(data_e3$data_y,
#                                list(data_e3$sub,data_e3$Luminance,data_e3$Pattern),
#                                function(x){return (scale(x))}))

eval(parse(text=paste("data_e3 = aggregate( data_y ~ sub*",f1,"*",f2,"*data_x, data = data_e3, FUN = 'mean')",sep="")))
save(data_e1_power, data_e1,data_e2,data_e3,file = paste(currentLoc,"/dataset_e1e2.rda", sep = ""))
