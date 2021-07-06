source( "/Users/Yuta-PC/Documents/Rscript/clearFunc.R" )
clearAll()

source( "/Users/Yuta-PC/Documents/Rscript/clearFunc.R" )
if( dev.cur() > 1 ) dev.off()

s_data <- read.csv("/Users/Yuta-PC/Documents/matlab/Insight/hiddenPic4.1.4/dataShow/insightPriorRNR.csv") 

# m1 <- lmer(PLR ~ condition + (1|sibject),data=s_data)
library("lme4")
allListIntercept <- list()
allListCondition <- list()
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
  c1 <- s_data$conditionRNR
  c2 <- s_data$condition45
  s <- s_data$subject
  
  s_datatmp <- data.frame(
    conditionRNR = c1,
    condition45 = c2,
    PLR = y,
    subject = s
  )
  
  m1 <- lmer(PLR ~ conditionRNR*condition45 + (1|subject)+(1|subject:conditionRNR)+(1|subject:condition45),data=s_datatmp)
  anova(m1)
  t <- summary(m1)$coefficients[,"t value"]
  t <- as.numeric(t)
  
  a <- cbind(a,t[1])
  b <- cbind(b,t[2])
  allListIntercept <- rbind(allListIntercept,a)
  allListCondition <- rbind(allListCondition,b)
}


startTime <- -0.5
endTime <- 1.5
xx <-seq(startTime, endTime, length = 240)
if( dev.cur() > 1 ) dev.off()
# plot(xx,allListIntercept[,i],type="l",col=i, xlim=c(startTime,endTime), ylim=c(-10,3), ylab="", lty=2)
# par(new=T)
plot(xx,allListCondition,type="l",col=i, xlim=c(startTime,endTime),ylab="t-Value", lty=1)
par(new=T)
legend("topleft",
       legend=c("1", "2"),
       lty=c(1,1),
       col=c(1,2)