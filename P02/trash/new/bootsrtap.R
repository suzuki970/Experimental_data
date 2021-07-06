# install.packages("boot")
# library("boot")

s_data <- read.csv("/Users/Yuta-PC/Documents/matlab/Insight/hiddenPic4.1.4/dataShow/insightRNR.csv") 

 newData <-  array(0,c(240,23))
 
# for (sub in 1:5) {
  for (i in 1:240) {
    a <- list()
    b <- list()
    if (i < 10) {       
      eval(parse(text=paste("y <- s_data$PLR_..",i, sep="") ))
    } else if (i < 100) {
      eval(parse(text=paste("y <- s_data$PLR_.", i, sep="") ))
    } else{
      eval(parse(text=paste("y <- s_data$PLR_",  i, sep="") ))
    }
    
    # yy = y[ 1:( length(y) %/% 2 ) * 2 ]
    yy = y[ seq(1,  length(y), by = 2) ]
    force <- function(yy){yy}
    t = tsboot(yy, force, R=23, l=10, sim="fixed")$t
    ave = apply(t, 1, mean)
    newData[i,] <- ave
  }
# }
#  for (sub in 1:5) {
#   # y = nls(y ~ newData[sub,])
 # matplot(1:240,newData,type="l")
#    ,col = rainbow(sub)[sub],type="l")
#  }
# 
# x <- 1:10
# c(1:3)
# c(2:4)
# c(3:5)
# 
# 
# aveyy = mean(yy)
# 