source( "/Users/Yuta-PC/Documents/Rscript/clearFunc.R" )
clearAll()

source( "/Users/Yuta-PC/Documents/Rscript/clearFunc.R" )
source( "/Users/Yuta-PC/Documents/Rscript/MMCplot.R" )
if( dev.cur() > 1 ) dev.off()

s_data <- read.csv("/Users/Yuta-PC/Documents/matlab/Insight/hiddenPic4.1.4/dataShow/insightPriorRNR.csv") 

library("lme4")

plot(s_data$condition45,s_data$PLR)

m1 <- lmer(PLR ~ conditionRNR + (1|subject),data=s_data)
m1 <- lmer(PLR ~ condition45 + (1|subject),data=s_data)
summary(m1)

m1 <- lmer(PLR ~ conditionRNR*condition45 + (1|subject)+(1|subject:conditionRNR)+(1|subject:condition45),data=s_data)
anova(m1)

t <- summary(m1)$coefficients[,"t value"]
  t <- as.numeric(t)
  
