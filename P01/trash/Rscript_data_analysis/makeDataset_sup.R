# -------------setting path and initializing--------
currentLoc = dirname( sys.frame(1)$ofile )
root = strsplit(currentLoc , "Rscript")
root = paste(root[[1]][[1]], "/Rscript/",sep="")
path_toolbox = paste(root, "toolbox/",sep="")
source(paste(path_toolbox, "initialization.R", sep = ""))
# ----------------------------------------------------

# the condition types are described as below
f1 = "Luminance"
f2 = "Pattern"

g1_name = c("0.52","0.58","0.63","0.67","0.7","0.72","0.74")
g1_name = c(0.52,0.58,0.63,0.67,0.7,0.72,0.74)

taLum = c(rep(g1_name,times=rep(2,length(g1_name))), rep(g1_name,times=rep(10,7)))
xdur = c(rep(c(1:7),times=rep(2,7)),rep(rep(c(2:6),times=rep(2,5)),7))


g1_name_con = NULL
for(i in 1:length(g1_name)){
  g1_name_con = cbind(g1_name_con,paste(g1_name[i],g1_name[i], sep = " vs. "))
}
g1_name_con = rep(g1_name_con,time=rep(2,length(g1_name_con)))

g1_name_gl = NULL
for(i in 1:length(g1_name)){
  for(j in 2:6){
    g1_name_gl = cbind(g1_name_gl,paste(g1_name[i],g1_name[j], sep = " vs. "))
  }
}
g1_name_gl = rep(g1_name_gl,time=rep(2,length(g1_name_gl)))

g1_name = c(g1_name_con,g1_name_gl)

g2_name = rep(c("Control","Glare"),times=c(length(g1_name_con),length(g1_name_gl)))

# #### making dataset #################################
fileLoc = paste(currentLoc, "/matData/", sep = "")
dat <- readMat(paste(fileLoc,"/dat_SI.mat", sep = ""))

# 0:left, 1:right

ind_data <- data.frame(
  sub =  dat$sub,
  data_y = dat$res,
  Luminance = g1_name[dat$condition],
  Target = taLum[dat$condition],
  Pattern = g2_name[dat$condition],
  loc = dat$loc,
  cell = xdur[dat$condition],
  n = matrix(1,dim(dat$res)[1],1)
)

data_sup = aggregate( data_y ~ sub*Pattern*Luminance*Target*cell*n*loc, data = ind_data, FUN = "mean")

data_sup$data_y[data_sup$loc == 1] = 1 - data_sup$data_y[data_sup$loc == 1]

data_sup = aggregate( data_y ~ sub*Pattern*Luminance*Target*cell*n, data = data_sup, FUN = "mean")

save(data_sup,file = paste(currentLoc,"/dataset_sup.rda", sep = ""))

